open Ast_helper
open Asttypes
open Parsetree
open Location
open Longident
open Types

exception No_choice
exception Not_implemented
            
let loc = Location.none

let repr = Btype.repr

let () = Random.self_init ()

let choose l =
  if List.length l = 0
  then raise Not_implemented
  else
    let
      choice = Random.int (List.length l)
    in
    List.nth l choice
    
             
(* Let's start with some  *)
let (output_file : structure ref) = ref []

(* We need these in test.ml as per learn-ocaml instructions *)
let opens =
  [
    [%stri open Test_lib] ;
    [%stri open Report]
  ]

let exns =
  [
    [%stri exception No_choice]
  ]

(* Need to include this function to randomly select an element in a list *)
(* init the random number generator first *)
let choose_fn =
  [
    [%stri let () = Random.self_init () ];
    [%stri
     let choose l =
       if List.length l = 0
       then raise No_choice
       else
         let
           choice = Random.int (List.length l)
         in
         List.nth l choice
    ];
  ]

(* Replace type variables with instantiations *)
let rec replace_var var_map type_expr =
  let type_expr = repr type_expr in (* TODO Find out how to use this properly *)
  begin match type_expr.desc with
  | Tvar _ ->
     List.assoc type_expr.id var_map
  | Tarrow (lbl,t1,t2,comm) ->
     let desc =
       Tarrow (lbl, replace_var var_map t1, replace_var var_map t2, comm)
     in { type_expr with desc = desc }
  | Ttuple (ts) ->
     let desc =
       Ttuple (List.map (replace_var var_map) ts)
     in
     { type_expr with desc = desc }
  | Tconstr (p, ts, m) ->
     let desc =
       Tconstr (p, List.map (replace_var var_map) ts, m)
     in { type_expr with desc = desc }
  | _ -> raise Not_implemented
  end
    
let rec get_type_vars type_expr =
  let type_expr = repr type_expr in
  begin match type_expr.desc with
  | Tvar _ -> [type_expr.id]
  | Tconstr (_, ts, _)
  | Ttuple ts ->
     List.flatten @@ List.map get_type_vars ts
  | Tarrow (_, t1, t2, _) ->
     get_type_vars t1 @ get_type_vars t2
  | _ -> raise Not_implemented
  end

let rec print_var_maps ppf (var_maps : (int * string) list) =
  Format.fprintf ppf "@[%a@]@." print_var_maps' var_maps
and print_var_maps' ppf var_maps =
  begin match var_maps with
  | [] -> Format.fprintf ppf ""
  | hd::tl ->
     Format.fprintf ppf
       "%a, %a"
       print_var_map hd
       print_var_maps' tl
  end
and print_var_map ppf var_map =
  Format.fprintf ppf
    "(%d, %s)"
    (fst var_map)
    (snd var_map)

let rec print_samplers ppf samplers =
  Format.fprintf ppf
    "@[[%a]@]@."
    print_samplers' samplers
and print_samplers' ppf samplers =
  begin match samplers with
  | [] -> Format.fprintf ppf ""
  | hd::tl ->
     Format.fprintf ppf
       "%s, %a"
       hd
       print_samplers' tl
  end

let rec sampler_string argtype =
  let ending = string_of_type argtype in
  "sampler_" ^ ending
  
and string_of_type type_expr =
  begin match type_expr.desc with
  | Tconstr (p, ts, _) ->
     begin match ts with
     | [] -> Path.name p
     | x ->
        let x' = List.map string_of_type ts in
        let prepend = String.concat "_" x' in
        prepend ^ "_" ^ (Path.name p)
     end
  | Ttuple (ts) ->
     String.concat "_" (List.map string_of_type ts)
  end
  
let rec get_sampler fn_sig argtypes =
  let id_map = List.map (fun x -> (x.id, x.desc)) argtypes in
  let type_vars = List.map get_type_vars argtypes in
  let type_vars' = List.sort_uniq compare @@ List.flatten type_vars in
  let rec assign_types type_vars =
    begin match type_vars with
    | [] -> []
    | hd::tl ->
       (* Choose a random instantiation for the type var *)
       let hd' =
         (hd,
          choose [ Predef.type_int ; Predef.type_float ; Predef.type_bool ;
                   Predef.type_char ; Predef.type_string ])
       in
       hd' :: (assign_types tl)
    end
  in
  let var_map = assign_types type_vars' in
  let replaced = List.map (replace_var var_map) argtypes in
  let new_sig = replace_var var_map fn_sig in
  let samplers = List.map sampler_string replaced in
  let sampler_expr sampler =
    let sampler_exp =
      Make_sampler.make_expr
        (Pexp_ident (Make_sampler.make_id sampler))
    in let unit_expr =
         Make_sampler.make_expr
           (Pexp_construct (Make_sampler.make_id "()", None))
       in
       Make_sampler.make_expr (Pexp_apply (sampler_exp, [(Nolabel, unit_expr)]))
  in let sampler_exprs = List.map (sampler_expr) samplers in
     let sampler_tuple = Make_sampler.make_expr (Pexp_tuple (sampler_exprs)) in
     let sampler_fun =
       Exp.fun_ Nolabel None
         (Pat.construct (Make_sampler.make_id "()") None)
         sampler_tuple
     in
     (new_sig, sampler_fun)

let exercises : string list ref = ref []
       
let process_fun (fn_info : Typediter.function_info) =
  let make_str str = { txt = str ; loc = loc } in
  let make_lident str = { txt = Lident str ; loc = loc } in
  let cons_id = make_lident "::" in
  let nil_id = make_lident "[]" in

  let (type_sig, sampler) = get_sampler fn_info.fn_sig fn_info.argtypes in
  
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
  let type_constr = (Nolabel, Exp.constant (Pconst_string (type_sig_str, None))) in

  let tester_fn_name =
    Format.sprintf "test_function_%d_against_solution" fn_info.argcount in
  let tester_fn_ident = Exp.ident (make_lident tester_fn_name) in

  let apply_test_fn =
    Exp.apply tester_fn_ident
      [type_constr;gen_const;sampler_const;empty_list_constr]
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
  | [] -> Exp.construct (Make_sampler.make_id "[]") None
  | hd::tl ->
     Exp.construct
       (Make_sampler.make_id "::")
       (Some (Exp.tuple [ Exp.ident (Make_sampler.make_id hd) ; make_ast_list tl]))
  end

let gen_main_fn () =
  let last_fun = (Nolabel, Exp.fun_ Nolabel None
                   (Pat.construct (Make_sampler.make_id "()") None)
                   (make_ast_list !exercises)) in
  let code_ast = (Nolabel, Exp.ident (Make_sampler.make_id "code_ast")) in
  let ast_sanity_check = Exp.ident (Make_sampler.make_id "ast_sanity_check") in
  let apply_ast_fns = (Nolabel, Exp.apply ast_sanity_check [code_ast]) in
  let double_at = Exp.ident (Make_sampler.make_id "@@") in
  let apply_inner = (Nolabel, Exp.apply double_at [apply_ast_fns;last_fun]) in
  let set_result = (Nolabel, Exp.ident (Make_sampler.make_id "set_result")) in
  let apply_outer = Exp.apply double_at [set_result; apply_inner] in

  let pattern = Pat.construct (Make_sampler.make_id "()") None in
  let vbs = [ { pvb_pat = pattern ; pvb_expr = apply_outer ;
                pvb_attributes = [] ; pvb_loc = loc } ]
  in Str.value Nonrecursive vbs

let test parse_tree typed_tree =
  let fn_map = Typediter.iter typed_tree in
  let (fn_infos : Typediter.function_info list ref) = ref [] in
  Typediter.FnMap.iter
    (fun _ (y : Typediter.function_info) -> fn_infos := y :: !fn_infos)
    fn_map;
  output_file := !output_file @ opens;
  output_file := !output_file @ exns;
  output_file := !output_file @ choose_fn;
  output_file := !output_file @ [Make_sampler.make parse_tree];
  output_file := !output_file @ List.map (process_fun) !fn_infos;
  output_file := !output_file @ [gen_main_fn ()];
  Format.fprintf (Format.std_formatter) "@[%a@]@." (Pprintast.structure) !output_file;
  
    
  (* Typediter.FnMap.iter (fun x y -> args_to_samplers y) fn_map *)
  ()
