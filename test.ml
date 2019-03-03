open Ast_helper
open Asttypes
open Parsetree
open Location
open Longident
open Types

let loc = Location.none

let (output_file : structure ref) = ref []

(* We need these in test.ml as per learn-ocaml instructions *)
let opens =
  [
    [%stri open Test_lib] ;
    [%stri open Report]
  ]

let exns =
  [
    [%stri exception Not_implemented]; [%stri exception Function_sampler of string]
  ] 

let exercises : string list ref = ref [] 

let process_fun (fn_info : Typediter.function_info) =
  let make_str str = { txt = str ; loc = loc } in
  let make_lident str = { txt = Lident str ; loc = loc } in
  let cons_id = make_lident "::" in
  let nil_id = make_lident "[]" in

  let (type_sig, sampler) = Sampler.Typed.get_sampler fn_info.fn_sig fn_info.argtypes in
  
  let exercise_name = "exercise_" ^ fn_info.name in
  exercises := exercise_name :: !exercises;
  let pattern = Pat.var (make_str exercise_name) in

  let nils_constr = Exp.construct nil_id None in
  let fn_name_const = Exp.constant (Pconst_string (fn_info.name,None)) in
  let code_constr = Exp.construct (make_lident "Code") (Some fn_name_const) in
  let tuple_1 = Exp.tuple [code_constr ; nils_constr] in
  let cons_code_nils = Exp.construct cons_id (Some tuple_1) in

  let fn_const = Exp.constant (Pconst_string ("Function:", None)) in
  let txt_constr = Exp.construct (make_lident "Text") (Some fn_const) in
  let tuple_2 = Exp.tuple [txt_constr;cons_code_nils] in
  let cons_txt_fn = Exp.construct cons_id (Some tuple_2) in

  let empty_list_constr = (Nolabel, Exp.construct nil_id None) in
  let sampler_const = (Labelled "sampler", sampler) in
  let gen_const = (Labelled "gen", Exp.constant (Pconst_integer ("10", None))) in
  let type_sig_str = Format.asprintf "[%%ty : %a]" Typediter.print_type type_sig in
  let type_constr = (Nolabel, Exp.construct (make_lident type_sig_str) None) in

  let tester_fn_name =
    Format.sprintf "test_function_%d_against_solution" fn_info.argcount in
  let tester_fn_ident = Exp.ident (make_lident tester_fn_name) in

  let apply_test_fn =
    Exp.apply tester_fn_ident
      [type_constr;(Nolabel,fn_name_const);gen_const;sampler_const;empty_list_constr]
  in

  let tuple_3 = Exp.tuple [cons_txt_fn ; apply_test_fn] in
  let section_constr = Exp.construct (make_lident "Section") (Some tuple_3) in

  let value_binding_list =
    [ { pvb_pat = pattern ;
        pvb_expr = section_constr ;
        pvb_attributes = [] ;
        pvb_loc = loc
    } ]
  in
  let stri = Str.value Nonrecursive value_binding_list
  in stri               

let rec make_ast_list str_list =
  begin match str_list with
  | [] -> Exp.construct { txt = Lident "[]" ; loc = loc } None
  | hd::tl ->
     Exp.construct
       { txt = Lident "::" ; loc = loc }
       (Some (Exp.tuple [ Exp.ident { txt = Lident hd ; loc = loc } ; make_ast_list tl]))
  end

let gen_main_fn () =
  let last_fun = (Nolabel, Exp.fun_ Nolabel None
                   (Pat.construct { txt = Lident "()" ; loc = loc } None)
                   (make_ast_list !exercises)) in
  let code_ast = (Nolabel, Exp.ident { txt = Lident "code_ast" ; loc = loc}) in
  let ast_sanity_check = Exp.ident { txt = Lident "ast_sanity_check" ; loc = loc } in
  let apply_ast_fns = (Nolabel, Exp.apply ast_sanity_check [code_ast]) in
  let double_at = Exp.ident { txt = Lident "@@"  ; loc = loc } in
  let apply_inner = (Nolabel, Exp.apply double_at [apply_ast_fns;last_fun]) in
  let set_result = (Nolabel, Exp.ident { txt = Lident "set_result" ; loc = loc }) in
  let apply_outer = Exp.apply double_at [set_result; apply_inner] in

  let pattern = Pat.construct { txt = Lident "()" ; loc = loc } None in
  let vbs = [ { pvb_pat = pattern ; pvb_expr = apply_outer ;
                pvb_attributes = [] ; pvb_loc = loc } ]
  in Str.value Nonrecursive vbs

let out_file parse_tree typed_tree dir =
  let fn_map = Typediter.run typed_tree in
  let (fn_infos : Typediter.function_info list ref) = ref [] in
  Typediter.FnMap.iter
    (fun _ (y : Typediter.function_info) -> fn_infos := y :: !fn_infos)
    fn_map;
  fn_infos := List.filter (fun (x : Typediter.function_info) -> List.mem x.name !Exercise.Solution.exercises) !fn_infos;
  output_file := !output_file @ opens;
  output_file := !output_file @ exns;
  (* output_file := !output_file @ choose_fn; *)
  let samplers = Sampler.Untyped.run parse_tree in
  output_file := !output_file @ [samplers];
  output_file := !output_file @ List.map (process_fun) !fn_infos;
  output_file := !output_file @ [gen_main_fn ()];
  let oc = open_out (dir ^ Filename.dir_sep ^ "test.ml") in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "@[%a@]@." (Pprintast.structure) !output_file;
  close_out oc
