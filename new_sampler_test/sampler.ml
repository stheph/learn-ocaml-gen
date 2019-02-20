open Parsetree
open Ast_helper
open Asttypes
open Longident

exception Unsupported_operation

let loc = Location.none

let ident name = { txt = Lident name ; loc }

let exp_ident name = Exp.ident (ident name)

let make_function name body =
  let pat = Pat.var { txt = name ; loc } in
  [%expr fun [%p pat] -> [%e body]]

let rec ast_list_of_strings l =
  begin match l with
  | [] -> Exp.construct { txt = Lident "[]" ; loc = loc  } None
  | hd :: tl ->
     let hd_expr = Exp.ident { txt = Lident hd ; loc = loc } in
     let tl_expr = ast_list_of_strings tl in
     let tup = Exp.tuple [hd_expr;tl_expr] in
     Exp.construct { txt = Lident "::" ; loc = loc } (Some tup)
  end
                               
let rec make_sampler type_decl =
  let { ptype_name = { txt = name ; _ }
      ; ptype_params
      ; ptype_kind
      ; ptype_manifest ; _
      } = type_decl in
  let sampler_pattern = Pat.var { txt = "sample_" ^ name ; loc } in

  let sampler_params expr =
    let params = List.map pass_sampler (List.map (fun x -> fst x) ptype_params) in
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
       | _ -> raise Unsupported_operation
       end
    | Ptype_variant constructors, None ->
       (* type t = A | B | C | ... *)
       let constructor_fn_names =
         List.map (fun { pcd_name = { txt = name ; _ } ; _} -> constructor_pattern name) constructors
       in
       let expr = ast_list_of_strings constructor_fn_names in
       let expr = Exp.apply (exp_ident "sample_alternatively") [Nolabel, expr] in
       let expr = Exp.apply expr [Nolabel, exp_ident "()"] in
       let expr = List.fold_left make_constructor_function expr constructors in
       Vb.mk sampler_pattern @@ sampler_params ([%expr fun () -> [%e expr]])
    (* | Ptype_record labels, None ->
     *    (\* type t = { l1 : T1 ; l2 : T2, ... } *\)
     *    (\* TODO *\)
     *    Vb.mk sampler_pattern ([%expr fun () -> ()]) *)
    | _ -> raise Unsupported_operation
  end

and call_sampler ctype =
  begin match ctype.ptyp_desc with
  | Ptyp_constr ({ txt = Lident name ; _ }, ts) ->
     let args = List.map (fun x -> (Nolabel, exp_ident @@ pass_sampler x)) ts in
     let args = args@[Nolabel, exp_ident "()"] in
     Exp.apply (exp_ident @@ pass_sampler ctype) args
  | _ ->
     let sampler_call = exp_ident @@ pass_sampler ctype in
     let unit_expr = Exp.construct (ident "()") None in
     Exp.apply sampler_call [Nolabel, unit_expr]
  end
               
and pass_sampler ctype =
  begin match ctype.ptyp_desc with
  | Ptyp_var name -> "sample_" ^ name
  | Ptyp_arrow (_, t1, t2) ->
     "sample_" ^ (ctype_string t1) ^ "_arrow_" ^ (ctype_string t2)
  | Ptyp_tuple ctypes ->
     let tup = String.concat "_" (List.map ctype_string ctypes) in
     "sample_" ^ tup
  | Ptyp_constr ({ txt = Lident name ; _ }, _) ->
     "sample_" ^ name
  | _ -> raise Unsupported_operation
  end

and ctype_string ctype =
  begin match ctype.ptyp_desc with
  | Ptyp_var name -> name
  | Ptyp_constr ({ txt = Lident name ; _ }, _) -> name
  | _ -> raise Unsupported_operation
  end

and make_constructor_function expr cnstr_decl =
  let { pcd_name = { txt = name ; _} ; pcd_args ; _ } = cnstr_decl in
  let pattern =
    Pat.var { txt = constructor_pattern name ; loc }
  in
  let body =
    begin match pcd_args with
    | Pcstr_tuple ctypes ->
       let args = List.map call_sampler ctypes in
       Exp.construct ({ txt = Lident name; loc }) (Some (Exp.tuple args))
    | _ -> raise Unsupported_operation
    end
  in
  [%expr let [%p pattern] = [%e body] in [%e expr]]

and constructor_pattern name =
    "cnstr_" ^ name
  
let generate_samplers type_decls =
  Str.value Recursive (List.map make_sampler type_decls)
