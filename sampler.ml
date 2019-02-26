(* Functions for creating and invoking samplers *)

open Parsetree
open Ast_helper
open Asttypes
open Longident

exception Unsupported_operation of string

let loc = Location.none

let ident name = { txt = Lident name ; loc }

let exp_ident name = Exp.ident (ident name)

let make_function name body =
  let pat = Pat.var { txt = name ; loc } in
  [%expr fun [%p pat] -> [%e body]]

(* This converts a list of strings in OCaml to its AST representation *)
let rec ast_list_of_strings l =
  begin match l with
  | [] -> Exp.construct { txt = Lident "[]" ; loc = loc  } None
  | hd :: tl ->
     let hd_expr = Exp.ident { txt = Lident hd ; loc = loc } in
     let tl_expr = ast_list_of_strings tl in
     let tup = Exp.tuple [hd_expr;tl_expr] in
     Exp.construct { txt = Lident "::" ; loc = loc } (Some tup)
  end

module Untyped = struct

  open Ast_iterator

  (* Collect all the type declarations *)

  let (type_decls : type_declaration list ref) = ref []

  let type_declaration_iterator _iterator type_decl =
    type_decls := !type_decls @ [type_decl]

  let type_decl_iterator =
    {
      default_iterator with
      type_declaration = type_declaration_iterator
    }

  let (recursive_samplers : string list ref) = ref []

  (* Main function for making samplers *)
  
  let rec make_sampler type_decl =
    let { ptype_name = { txt = name ; _ }
        ; ptype_params
        ; ptype_kind
        ; ptype_manifest ; _
        } = type_decl in
    let sampler_pattern = Pat.var { txt = "sample_" ^ name ; loc } in

    let sampler_params expr =
      let sampler_name ctype =
        begin match ctype.ptyp_desc with
        | Ptyp_var name -> "sample_" ^ name
        | Ptyp_constr ({ txt = Lident name ; _ }, _) ->
           "sample_" ^ name
        | _ -> raise (Unsupported_operation "sampler_params")
        end
      in
      let params = List.map sampler_name (List.map (fun x -> fst x) ptype_params) in
      List.fold_right make_function params expr
    in
    
    begin match ptype_kind, ptype_manifest with
    | Ptype_abstract, Some manifest ->
       begin match manifest.ptyp_desc with
       | Ptyp_arrow (_, t1, t2) ->
          (* ex. type t = t1 -> t2 *)
          let exn_msg =
            Exp.constant
              (Pconst_string
                 (Format.asprintf
                    "Please provide functions of type %a"
                    Pprintast.core_type manifest, None))
          in
          let exn_expr = Exp.construct (ident "Function_sampler") (Some exn_msg) in
          let raise_expr = Exp.apply (exp_ident "raise") [Nolabel, exn_expr] in
          Vb.mk sampler_pattern @@ sampler_params [%expr fun () -> [%e raise_expr]]
       | Ptyp_tuple ts ->
          (* ex. type t = t_1 * t_2 * ... * t_n *)
          let sampler_calls = List.map call_sampler ts in
          let expr = Exp.tuple sampler_calls in
          Vb.mk sampler_pattern @@ sampler_params [%expr fun () -> [%e expr]]
       | Ptyp_constr ({ txt = Lident name' }, ts) ->
          (* ex. type t = int; type t = int list; type 'a t = 'a list *)
          let expr = call_sampler manifest in
          Vb.mk sampler_pattern @@ sampler_params [%expr fun () -> [%e expr]]
       | _ -> raise (Unsupported_operation "make_sampler :: ptype_abstract, some manifest")
       end
    | Ptype_variant constructors, None ->
       (* type t = A | B | C | ... *)
       let check_rec = check_recursive_variant name constructors in
       let constructor_fn_names =
         List.map (fun { pcd_name = { txt = name ; _ } ; _} -> constructor_pattern name) constructors
       in
       let expr = ast_list_of_strings constructor_fn_names in
       let expr = Exp.apply (exp_ident "sample_alternatively") [Nolabel, expr] in
       let expr = Exp.apply expr [Nolabel, exp_ident "()"] in
       let expr = List.fold_left make_constructor_function expr constructors in
         if check_rec
         then
           begin
             recursive_samplers := ("sample_" ^ name) :: !recursive_samplers;
             Vb.mk sampler_pattern @@ sampler_params ([%expr fun ~size:10  () -> [%e expr]])
           end
         else Vb.mk sampler_pattern @@ sampler_params ([%expr fun () -> [%e expr]])
    | Ptype_record labels, None ->
       (* type t = { l1 : T1 ; l2 : T2, ... } *)
       let check_rec = check_recursive_record name labels in
       let lv_pairs =
         List.map
           (fun { pld_name = { txt = name ; _ }; pld_type ; _ } -> ({ txt = Lident name; loc }, call_sampler pld_type))
           labels
       in
       let expr = Exp.record lv_pairs None in
       if check_rec
       then
         begin
           recursive_samplers := ("sample_" ^ name) :: !recursive_samplers;
           Vb.mk sampler_pattern @@ sampler_params ([%expr fun ~size:10 () -> [%e expr]])
         end
       else Vb.mk sampler_pattern @@ sampler_params ([%expr fun () -> [%e expr]])
    | _ -> raise (Unsupported_operation "make_sampler")
    end


  (* Returns an expression of the form "sample_[type] ()" *)
  and call_sampler ctype =
    begin match ctype.ptyp_desc with
    | Ptyp_constr ({ txt = Lident name ; _ }, ts) ->
       let args = List.map (fun x -> (Nolabel, pass_sampler x)) ts in
       let args = args@[Nolabel, exp_ident "()"] in
       Exp.apply (pass_sampler ctype) args
    | _ ->
       let sampler_call = pass_sampler ctype in
       let unit_expr = Exp.construct (ident "()") None in
       Exp.apply sampler_call [Nolabel, unit_expr]
    end


  (* Returns an expression of the form "sample_[type]" *)
  and pass_sampler ctype =
    begin match ctype.ptyp_desc with
    | Ptyp_var name -> exp_ident @@ "sample_" ^ name
    (* | Ptyp_arrow (_, t1, t2) ->
     *    "sample_" ^ (ctype_string t1) ^ "_arrow_" ^ (ctype_string t2) *)
    | Ptyp_tuple ctypes ->
       let tup = Exp.tuple @@ List.map call_sampler ctypes in
       [%expr fun () -> [%e tup]]
    | Ptyp_constr ({ txt = Lident name ; _ }, _) ->
       exp_ident @@ "sample_" ^ name
    | _ -> raise (Unsupported_operation "pass_sampler")
    end

  and ctype_string ctype =
    begin match ctype.ptyp_desc with
    | Ptyp_var name -> name
    | Ptyp_constr ({ txt = Lident name ; _ }, _) -> name
    | _ -> raise (Unsupported_operation "ctype_string")
    end

  (* Makes a function that constructs a value *)
  (* using one of the constructors *)
  (* All the constructor functions are nested together *)
  and make_constructor_function expr cnstr_decl =
    let { pcd_name = { txt = name ; _} ; pcd_args ; _ } = cnstr_decl in
    let pattern =
      Pat.var { txt = constructor_pattern name ; loc }
    in
    let body =
      begin match pcd_args with
      | Pcstr_tuple ctypes ->
         begin match ctypes with
         | [] ->
            Exp.construct ({ txt = Lident name; loc }) None           
         | _ ->
            let args = List.map call_sampler ctypes in
            Exp.construct ({ txt = Lident name; loc }) (Some (Exp.tuple args))
         end
      | _ -> raise (Unsupported_operation "make_constructor_function")
      end
    in
    let body = [%expr fun () -> [%e body]] in
    [%expr let [%p pattern] = [%e body] in [%e expr]]

  and constructor_pattern name =
    "cnstr_" ^ name

  and check_recursive_variant name constructors =
    let check_constructor { pcd_args } =
      begin match pcd_args with
      | Pcstr_tuple ctypes ->
         List.mem true (List.map (check_recursive name) ctypes)
      | Pcstr_record labels ->
         check_recursive_record name labels
      end
    in
    List.mem true (List.map check_constructor constructors)
  (* Just checks if the type name occurs in its constructors *)
  and check_recursive_record name labels =
    let check_label name { pld_type } =
      check_recursive name pld_type
    in
    List.mem true (List.map (check_label name) labels)

  and check_recursive name ctype =
    begin match ctype.ptyp_desc with
    | Ptyp_arrow (_,t1,t2) -> (check_recursive name t1) || (check_recursive name t2)
    | Ptyp_tuple ctypes ->
       List.mem true (List.map (check_recursive name) ctypes)
    | Ptyp_constr ({ txt = Lident name' ; _ } , ctypes) ->
       (name = name') || (List.mem true (List.map (check_recursive name) ctypes))
    | _ -> false
    end
      
  let run parse_tree =
    type_decl_iterator.structure type_decl_iterator parse_tree;
    Str.value Recursive (List.map make_sampler !type_decls)

end

module Typed = struct

  open Ast_helper
  open Types
  open Longident

  exception No_choice
         
  (* The typed module should take a type_expr and return the appropriate sampler *)
  (* or create one if it doesn't exist (ex. functions which take tuple arguments) *)

  let base_types =
    [
      Predef.type_int;
      Predef.type_bool;
      Predef.type_string
    ]

  let choose l =
    if List.length l = 0
    then raise No_choice
    else
      let
        choice = Random.int (List.length l)
      in
      List.nth l choice
  
  let rec collect_vars type_expr =
    begin match type_expr.desc with
    | Tvar _ -> [type_expr]
    | Tarrow (_, t1, t2, _) ->
       (collect_vars t1) @ (collect_vars t2)
    | Ttuple ts ->
       List.flatten (List.map collect_vars ts)
    | Tconstr (_, ts, _) ->
       List.flatten (List.map collect_vars ts)
    | Tlink t ->
       collect_vars t
    | _ -> []
    end

  and instantiate_vars vars =
    begin match vars with
    | [] -> []
    | hd :: tl ->
       (hd, choose base_types) :: (instantiate_vars tl)
    end

  and subst_vars var_map type_expr =
    begin match type_expr.desc with
    | Tvar _ -> List.assoc type_expr var_map 
    | Tarrow (lbl, t1, t2, comm) ->
       {type_expr with desc = Tarrow (lbl, subst_vars var_map t1, subst_vars var_map t2, comm)}
    | Ttuple ts ->
       {type_expr with desc = Ttuple (List.map (fun x -> subst_vars var_map x) ts)}
    | Tconstr (p, ts, abbrev) ->
       {type_expr with desc = Tconstr (p, List.map (fun x -> subst_vars var_map x) ts, abbrev)}
    | Tlink t -> subst_vars var_map t
    | _ -> type_expr
    end
      
  and get_sampler fn_sig argtypes =
    let fn_sig = Btype.repr fn_sig in
    let argtypes = List.map (Btype.repr) argtypes in
    let vars = collect_vars fn_sig in
    let var_map = instantiate_vars vars in
    let fn_sig = subst_vars var_map fn_sig in
    let argtypes = List.map (fun x -> subst_vars var_map x) argtypes in
    let argtypes' = List.map get_sampler' argtypes in
    let expr = Exp.tuple argtypes' in
    (fn_sig,  [%expr fun () -> [%e expr]])

  and get_sampler' type_expr =
    begin match type_expr.desc with
    | Tarrow (_, t1, t2, _) ->
       let exn_msg =
         Exp.constant
           (Pconst_string
              (Format.asprintf "Please provide functions of type %a"
                 Printtyp.type_expr type_expr, None))
       in
       let exn_expr = Exp.construct (ident "Function_sampler") (Some exn_msg) in
       let raise_expr = Exp.apply (exp_ident "raise") [Nolabel, exn_expr] in
       [%expr (fun () -> [%e raise_expr]) ()]
    | Ttuple ts ->
       let ts_samplers = List.map get_sampler' ts in
       let tup = Exp.tuple ts_samplers in
       [%expr (fun () -> [%e tup]) ()]
    | Tconstr (p, ts, _) ->
       let name = Path.name p in
       let sampler_name = "sample_" ^ name in
       let sampler_args = List.map (fun x -> (Nolabel, pass_sampler x)) ts in
       Exp.apply (exp_ident sampler_name) (sampler_args@[Nolabel, exp_ident "()"])
    | _ -> raise (Unsupported_operation "get_sampler'")
    end

  and pass_sampler type_expr =
    begin match type_expr.desc with
    | Ttuple ts ->
       let tup = Exp.tuple @@ List.map get_sampler' ts in
       [%expr fun () -> [%e tup]]
    | Tconstr (p, _, _) ->
       exp_ident @@ "sample_" ^ (Path.name p)
    | _ -> raise (Unsupported_operation "Typed.pass_sampler")
    end

  end
