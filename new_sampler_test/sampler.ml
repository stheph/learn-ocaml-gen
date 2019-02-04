open Parsetree
open Ast_helper
open Asttypes
open Longident

exception Unsupported_operation

let loc = Location.none

let ident name = { txt = Lident name ; loc }

let exp_ident name = Exp.ident (ident name)

let call_on_unit name = [%expr [%e exp_ident name] ()]

let make_function name body =
  let pat = Pat.var { txt = name ; loc } in
  [%expr fun [%p pat] -> [%e body]]

(* Takes a list of strings, and converts it to a list in OCaml syntax *)
let rec ast_list_of_strings l =
  begin match l with
  | [] -> Exp.construct { txt = Lident "[]" ; loc = loc  } None
  | hd :: tl ->
     let hd_expr = Exp.ident { txt = Lident hd ; loc = loc } in
     let tl_expr = ast_list_of_strings tl in
     let tup = Exp.tuple [hd_expr;tl_expr] in
     Exp.construct { txt = Lident "::" ; loc = loc } (Some tup)
  end
    
let strip_apos str =
  if String.contains str '\''
  then
    String.concat "" @@ String.split_on_char '\'' str
  else
    str

(* TODO I'd like a store for valid samplers *)
(* that we check against *)
let get_sampler name = "sample_" ^ (strip_apos name)
let get_sampler_by_ctype ctype =
  get_sampler (Format.asprintf "%a" Pprintast.core_type ctype)

let rec call_sampler ctype =
  begin match ctype.ptyp_desc with
  | Ptyp_var name ->
     exp_ident @@ "sample_" ^ name
  | Ptyp_tuple ctypes ->
     let samplers = List.map call_sampler ctypes in
     Exp.tuple samplers
  | Ptyp_constr ({ txt = Lident name ; _ }, ctypes) ->
     let sampler = "sample_" ^ name in
     let samplers = List.map call_sampler ctypes in
     let args =
       begin match samplers with
       | [] -> None
       | [x] -> Some x
       | l -> Some (Exp.tuple l)
       end
     in
     Exp.apply (Exp.construct (ident sampler) args) [Nolabel, exp_ident "()"]
  | _ -> raise Unsupported_operation
  end
    
let generate_sampler type_decl =
  let { ptype_name = { txt = name ; _ }
      ; ptype_params
      ; ptype_kind
      ; ptype_manifest ; _
      } = type_decl in
  let sampler_name = "sample_" ^ name in
  let sampler_pattern = Pat.var { txt = sampler_name ; loc } in

  (* type 'a list -> "let rec sample_list sample_a () = ... " *)
  let sampler_params expr =
    let rec get_sampler_params params =
      begin match params with
      | [] -> []
      | hd :: tl ->
         begin match hd with
         | { ptyp_desc = Ptyp_var var_name ; _ } ->
            (get_sampler var_name) :: get_sampler_params tl
         | _ -> get_sampler_params tl
         end
      end
    in
    let params = List.map (fun x -> fst x) ptype_params in
    List.fold_right make_function (get_sampler_params params) expr
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
        Vb.mk sampler_pattern (sampler_params  [%expr fun () -> [%e raise_expr]])
     | Ptyp_tuple ts ->
        (* ex. type t = t_1 * t_2 * ... * t_n *)
        let samplers = List.map (get_sampler_by_ctype) ts in
        let sampler_calls = List.map call_on_unit samplers in
        let expr = Exp.tuple sampler_calls in
        Vb.mk sampler_pattern (sampler_params [%expr fun () -> [%e expr]])
     | Ptyp_constr ({ txt = Lident name' }, ts) ->
        (* ex. type t = int; type t = int list; type 'a t = 'a list *)
        let sampler_name' = get_sampler name' in
        let samplers = List.map get_sampler_by_ctype ts in
        let sampler_args = List.map (fun x -> (Nolabel, exp_ident x)) (samplers@["()"]) in
        let expr = Exp.apply (exp_ident sampler_name') sampler_args in
        Vb.mk sampler_pattern (sampler_params [%expr fun () -> [%e expr]])
     | _ -> raise Unsupported_operation
     end
  | Ptype_variant constructors, None ->
     (* type t = A | B | C | ... *)
     let constr_function_names =
       List.map
         (fun ({ pcd_name = { txt = constr_name ; _} ; _ }) -> "construct_" ^ constr_name)
         constructors
     in
     (* Converts a constructor C into a function *)
     (* let construct_C = C (<appropriate samplers>) *)
     let rec constr_functions constrs expr =
       begin match constrs with
       | [] -> expr
       | hd::tl ->
          let body = constr_functions tl expr in
          let { pcd_name = { txt = constr_name ; _ } ; pcd_args ; _ } = hd in
          let pat = Pat.var ({ txt = "construct_" ^ constr_name ; loc }) in
          let expr =
            begin match pcd_args with
            | Pcstr_tuple ctypes ->
               let samplers = List.map call_sampler ctypes in
               let args =
                 begin match samplers with
                 | [] -> None
                 | [x] -> Some x
                 | l -> Some (Exp.tuple l)
                 end
               in
               Exp.construct (ident constr_name) args
            | _ -> raise Unsupported_operation
            end
          in
          [%expr let [%p pat] = [%e expr] in [%e body]]
       end
     in
     let body =
       Exp.apply
         (Exp.apply
            (exp_ident "sample_alternatively")
            [Nolabel, ast_list_of_strings constr_function_names])
         [Nolabel, exp_ident "()"]
     in
     let expr = constr_functions constructors body in
     Vb.mk sampler_pattern (sampler_params [%expr fun () -> [%e expr]])
  | Ptype_record labels, None ->
     (* type t = { l1 : T1 ; l2 : T2, ... } *)
     (* TODO *)
     Vb.mk sampler_pattern (sampler_params [%expr fun () -> ()])
  | _ -> raise Unsupported_operation
  end

let generate_samplers type_decls =
  Str.value Recursive (List.map generate_sampler type_decls)
