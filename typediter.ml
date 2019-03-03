open Typedtree
open TypedtreeIter
open Types

type function_info =
  { name : string;
    fn_sig : Types.type_expr;
    argtypes : Types.type_expr list;
    argcount : int;
  }

let rec print_type ppf tp =
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

let run typed_tree =
  FnInfoIter.iter_structure typed_tree;
  !fn_map
