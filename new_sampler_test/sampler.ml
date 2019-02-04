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

(* TODO I'd like a store for valid samplers *)
(* that we check against *)
let get_sampler name = "sample_" ^ name
let get_sampler_by_ctype ctype =
  get_sampler (Format.asprintf "%a" Pprintast.core_type ctype)

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
  | Ptype_record labels, None ->
  (* type t = { l1 : T1 ; l2 : T2, ... } *)
  | _ -> raise Unsupported_operation
  end

let generate_samplers type_decls =
  Str.value Recursive (List.map generate_sampler type_decls)
