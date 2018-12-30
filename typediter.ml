open Typedtree
open TypedtreeIter
open Types

exception Too_many_args
       
type function_info =
  { name : string;
    fn_sig : Types.type_expr;
    argtypes : Types.type_expr list;
    argcount : int;
  }

let rec print_fn_info ppf fn_info =
  let { name = name ; fn_sig = fn_sig ; argtypes = argtypes ; argcount = argcount } = fn_info in
  Format.fprintf ppf
    "{@[<v 0>\n\tname : %s[%d];\n\tsig : %a;\n\targtypes : %a\n\t@]}@."
    name
    argcount
    print_type
    fn_sig
    print_type_list
    argtypes
    
and print_type ppf tp =
  Format.fprintf ppf
    "@[<h 0>%a@]"
    Printtyp.type_scheme
    tp

and print_type_list ppf tps =
  Format.fprintf ppf
    "@[<h 0>%a@]"
    print_type_list'
    tps
    
and print_type_list' ppf tps =
  begin match tps with
  | [] -> Format.fprintf ppf ""
  | [t] -> print_type ppf t
  | hd::tl -> Format.fprintf ppf
                "%a, %a"
                print_type
                hd
                print_type_list
                tl
  end

let print_argtypes tps =
  print_type_list (Format.std_formatter) tps
    
let print_info info = print_fn_info (Format.std_formatter) info

let print_map str info =
  Format.fprintf (Format.std_formatter)
    "@[<v 0>{%s = %a}]@."
    str
    print_fn_info
    info

let print_map_simple str info =
  Format.fprintf (Format.std_formatter)
    "@[<h 0>%s[%d] : %a; %a@]@."
    info.name
    info.argcount
    print_type
    info.fn_sig
    print_type_list
    info.argtypes
                
(* Map from function names to function_info *)
module FnMap = Map.Make(String)

let fn_map = ref (FnMap.empty)
                       
module CollectFnInfo = struct
  include DefaultIteratorArgument
  
  let rec collect_info structure_item =
    begin match structure_item with
    (* let _ id = expr *)
    | { str_desc =
          Tstr_value (_, [{ vb_pat = { pat_desc = Tpat_var (id, _);
                                                pat_type = typ };
                            vb_expr = expr }]) } ->
       let name = Ident.name id in
       let fn_sig = typ in
       let argtypes = arg_types expr in
       let argcount = List.length argtypes in
       let fn_info = { name = name ; fn_sig = fn_sig ; argtypes = argtypes ; argcount = argcount }
       in fn_map := FnMap.(add name fn_info !fn_map)
    (* Not a function *)
    | _ -> ()
    end
      
  and arg_types { exp_desc = expr_d } =
    begin match expr_d with
    (* fun c_lhs -> c_rhs *)
    | Texp_function { cases = [{ c_lhs = {pat_type = typ}; c_rhs = expr }] } ->
       typ :: (arg_types expr)
    | _ -> []
    end

  let enter_structure_item structure_item = collect_info structure_item
      
end

module FnInfoIter = TypedtreeIter.MakeIterator(CollectFnInfo)

(* let exercise_count = ref 0
 * 
 * let get_exercise_count () =
 *   let ex_cnt = !exercise_count in
 *   exercise_count := !exercise_count + 1;
 *   ex_cnt
 *                                               
 * let rec gen_test { name ; fn_sig ; argtypes ; argcount } =
 *   (\* exercise count *\)
 *   let ex_cnt = get_exercise_count () in
 *   if argcount <= 4 then
 *     let testing_fn = Format.sprintf "test_function_%d_against_solution" argcount in
 *     let type_str = gen_test_ty fn_sig in
 *     Format.sprintf
 *       "let exercise_%d =\n\t%s\n\t\t%s\n\t\t\"%s\""
 *       ex_cnt
 *       testing_fn
 *       type_str
 *       name
 *   else raise Too_many_args
 *                                               
 * and gen_test_ty fn_sig =
 *   let sig_str = print_type (Format.str_formatter) fn_sig;
 *                 Format.flush_str_formatter ()
 *   in
 *   Format.sprintf "[%%ty: %s]" sig_str *)
                                                          
let iter typed_tree =
  FnInfoIter.iter_structure typed_tree;
  !fn_map
