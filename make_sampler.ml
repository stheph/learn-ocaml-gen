open Ast_iterator
open Parsetree
open Asttypes
open Longident

exception Not_implemented

let loc = Location.none

let base_types =
  [
    [%type: int] ;
    [%type: float] ;
    [%type: bool] ;
    [%type: char] ;
    [%type: string]
  ]

            
(* Make samplers from Parsetree type_decls *)
let (type_decls : type_declaration list ref) = ref []
                                                   
(* Iterate through the AST for type_decls *)
let type_declaration_iterator _iterator type_decl =
  type_decls := !type_decls @ [type_decl]

let type_decl_iterator =
  {
    default_iterator with
    type_declaration = type_declaration_iterator
  }

(* Make the AST elements we can't deal with with with metaquot *)
let default_id = { txt = Lident "default" ; loc = Location.none }
let make_id str = { default_id with txt = Lident str }
let default_str = { txt = "default" ; loc = Location.none }
let make_str str = { default_str with txt = str }
let default_expr = { pexp_desc = Pexp_ident (default_id) ;
                     pexp_loc = Location.none ;
                     pexp_attributes = []
                   }
let make_expr pdesc = { default_expr with pexp_desc = pdesc }
let default_pat = { ppat_desc = Ppat_any ; ppat_loc = Location.none ; ppat_attributes = [] }
let make_pat pdesc = { default_pat with ppat_desc = pdesc }

(* Functions for making functions *)
(* Given arguments arg1, arg2, ... arg_n *)
(* -> fun arg1 -> fun arg2 -> ... fun arg_n -> ... *)    
let rec compose_lets [%expr let [%p? pat] = [%e? e1] in [%e? e2]] e' =
  [%expr let [%p pat] = [%e e1] in [%e e']]

let compose_fns ([%expr fun [%p? pat] -> [%e? e]]) e' =
  [%expr fun [%p pat] -> [%e e']]
    
let rec create_fn args e =
  begin match args with
  | [] -> e
  | hd::tl ->
     let pat = make_pat (Ppat_var (make_str hd)) in
     compose_fns [%expr fun [%p pat] -> [%e e]] (create_fn tl e)
  end

(* Converts parameters into strings for sampler function names *)
(* Returns an option to carry information about where there were params or not *)
let rec string_of_params params =
  begin match params with
  | [] -> None
  | params' ->
     Some (String.concat "_" @@ List.map (fun x -> string_of_core_type @@ fst x) params')
  end
    
and string_of_core_type core_type =
  begin match core_type.ptyp_desc with
  | Ptyp_var v -> v
  | Ptyp_constr ( { txt = Lident id} , ctypes) ->
     id
  | Ptyp_tuple ctypes ->
     String.concat "_" @@ List.map string_of_core_type ctypes
  | x -> raise Not_implemented
  end

(* Gets the name of the sampler function corresponding to the given type *)
let get_sampler { ptyp_desc = Ptyp_constr ({ txt = Lident name}, ctypes)} =
  begin match ctypes with
  | [] -> "sampler_" ^ name
  | x ->
  let ctypes_string = String.concat "_" (List.map string_of_core_type ctypes) in
  "sampler_" ^ ctypes_string ^ "_" ^ name
  end
    
(* Create an AST representation of a constructor *)
let construct constr_name constr_args =
  let constr_id = make_id constr_name in
  let make_unit_appl gen_fn =
    let fn_expr = make_expr (Pexp_ident (make_id gen_fn)) in
    let unit_arg = make_expr (Pexp_ident (make_id "()")) in
    make_expr (Pexp_apply (fn_expr, [(Nolabel, unit_arg)]))
  in
  let arg_exprs = List.map make_unit_appl constr_args in
  let args_opt =
    begin match arg_exprs with
    | [] -> None
    | [x] -> Some x
    | l -> Some (make_expr (Pexp_tuple l))
    end
  in
  make_expr (Pexp_construct (constr_id, args_opt))
            
(* Creates a constructor function which takes unit *)
(* Returns a constructor with the appropriate generators as arguments *)
let constructor_function { pcd_name = { txt = constr_name } ; pcd_args = Pcstr_tuple args } =
  let constr_args = List.map get_sampler args in
  let constr_expr = construct constr_name constr_args in
  let constr_fns = create_fn ["()"] constr_expr in
  let constr_fn_name = "constr_" ^ (String.lowercase_ascii constr_name) in
  let constr_pat = make_pat (Ppat_var (make_str constr_fn_name)) in
  ([%expr let [%p constr_pat] = [%e constr_fns] in [%e default_expr]], constr_fn_name)
    
(* Create a list *)
(* We use this to choose constructors randomly *)
(* ex. choose [sampler_constr1;sampler_constr2] *)
let rec ast_list l =
  begin match l with
  | [] -> make_expr (Pexp_construct (make_id "[]", None))
  | hd :: tl ->
     let hd' = make_expr (Pexp_ident (make_id hd)) in
     let tl' = ast_list tl in
     let tup = make_expr (Pexp_tuple [hd';tl']) in
     make_expr (Pexp_construct (make_id "::", Some tup))
  end

let generate_sampler { ptype_name = { txt = name} ;
                     ptype_params = params ;
                     ptype_kind = kind ;
                     ptype_manifest = manifest} =
  begin match kind with
  | Ptype_variant constrs ->
     (* Get parameter strings, if there are any *)
     let params_string = string_of_params params in
     (* The name of the sampler function *)
     let sampler_pattern_string param_string =
       begin match param_string with
       | None -> "sampler_" ^ name
       | Some param_string' -> "sampler_" ^ param_string' ^ "_" ^ name
       end
     in
     let sampler_pattern = make_pat (Ppat_var (make_str @@ sampler_pattern_string params_string)) in
     
     (* Create a function for each constructor *)
     let constr_generators = List.map constructor_function constrs in
     (* We get back the expression representing the function for the AST *)
     (* as well as the name of the function to toss into our list *)
     let (gen_exprs, gen_list) = List.split constr_generators in
     
     (* Create an expression "(choose [sampler1; sampler2]) ()" *)
     let choose_expr = make_expr (Pexp_ident (make_id "choose")) in
     let unit_expr = make_expr (Pexp_construct (make_id "()", None)) in
     let apply_choose_expr = make_expr (Pexp_apply (choose_expr, [(Nolabel, ast_list gen_list)])) in
     let end_expr = make_expr (Pexp_apply (apply_choose_expr, [(Nolabel, unit_expr)])) in
     
     (* Now we compose all the constructor functions together with the choose expression *)
     let generator_expr =
       List.fold_right compose_lets gen_exprs end_expr in
     let generator_fn = create_fn ["()"] generator_expr in
     
     (* Finally, we end up with a function let rec [sampler_pattern] -> [generator_function] *)
     (* [%stri let rec [%p generator_pat] = [%e generator_fn]] *)
     (* Scratch that, value binding result for mutual recursion *)
     { pvb_pat = sampler_pattern ; pvb_expr = generator_fn ;
       pvb_attributes = [] ; pvb_loc = loc }
  | Ptype_abstract ->
     begin match manifest with
     | None -> raise Not_implemented
     | Some ctype ->
        begin match ctype.ptyp_desc with
        | Ptyp_constr ({ txt = type_name }, _) -> (* TODO Wild card needs to be filled eventually? *)
           let sampler_pattern =
             make_pat (Ppat_var (make_str @@ "sampler_" ^ name)) in
           let sampler_application sampler =
             let sampler_expr = make_expr (Pexp_ident (make_id sampler)) in
             let unit_expr = make_expr (Pexp_construct (make_id "()", None)) in
             make_expr (Pexp_apply (sampler_expr, [(Nolabel, unit_expr)]))
           in
           let ctype' = sampler_application @@ get_sampler ctype in
           let generator_fn = create_fn ["()"] ctype' in
           { pvb_pat = sampler_pattern ; pvb_expr = generator_fn ;
             pvb_attributes = [] ; pvb_loc = loc }   
        | Ptyp_tuple ctypes ->
           let sampler_pattern =
             make_pat (Ppat_var (make_str @@ "sampler_" ^ name)) in
           
           let sampler_application sampler =
             let sampler_expr = make_expr (Pexp_ident (make_id sampler)) in
             let unit_expr = make_expr (Pexp_construct (make_id "()", None)) in
             make_expr (Pexp_apply (sampler_expr, [(Nolabel, unit_expr)]))
           in
           let ctypes' = List.map sampler_application @@ List.map get_sampler ctypes in
           let end_expr = make_expr (Pexp_tuple ctypes') in
           let generator_fn = create_fn ["()"] end_expr in
           { pvb_pat = sampler_pattern ; pvb_expr = generator_fn ;
             pvb_attributes = [] ; pvb_loc = loc }
        | _ -> raise Not_implemented
        end
     end
  | _ -> raise Not_implemented
  end

(* Take the cross product of an arbitrary number of lists *)
(* Due to the type system, each list has to be a singleton list *)
(* cross_all : 'a list list list -> 'a list list *)
let rec cross_all l =
  begin match l with
  | [] -> []
  | hd::tl ->
     let rec cross l1 l2 =
       begin match l1, l2 with
       | l1', [] -> l1'
       | [], l2' -> l2'
       | [x], l2' -> List.map (fun x' -> x@x') l2'
       | l1', [y] -> List.map (fun y' -> y'@y) l1'
       | hd::tl, l2' ->
          let hd' = List.map (fun y -> hd@y) l2'
          in hd' @ (cross tl l2')
       end
     in
     cross hd (cross_all tl)
  end

(* Generate samplers for type_decls with type variables *)
(* This creates multiple, one for each base_type listed above *)
let rec generate_sampler_vars
      ({ptype_params = params ;
        ptype_kind = kind ; _ } as type_decl : type_declaration) =
  let params = List.map (fun (x, _) -> x.ptyp_desc) params in
  let rec assign_types params =
    begin match params with
    | [] -> []
    | hd::tl ->
       (* hd = a' -> [[(a', int)]; [(a', float)]; ...] *)
       let hd' = List.map (fun x -> [hd, x]) base_types in
       hd' :: (assign_types tl)
    end
  in
  let var_maps = cross_all @@ assign_types params in
  List.map (fun x -> generate_sampler_vars' type_decl x) var_maps
           
and generate_sampler_vars'
      ({ptype_name = _name ;
        ptype_params = params ;
        ptype_kind = kind ; _ } as type_decl : type_declaration) var_maps =
  begin match kind with
  | Ptype_variant constrs ->
     let replace_param param var_maps =
       let param' = (fun (x, _) -> x.ptyp_desc) param in
       begin match List.assoc_opt param' var_maps with
       | Some x -> (fun (_, y) -> (x, y)) param
       | None -> param
       end
     in
     let params' = List.map (fun x -> replace_param x var_maps) params in
     let constrs' =  Type_vars.instantiate_var var_maps constrs in
     generate_sampler { type_decl with ptype_params = params' ; ptype_kind = Ptype_variant constrs' }
  | _ -> raise Not_implemented
  end  

(* We use this to discriminate between decls with type variables and without *)
let has_type_variables ({ ptype_params = params ; _ } : type_declaration) =
  begin match params with
  | [] -> false
  | _ -> true
  end

let make parse_tree  =
  type_decl_iterator.structure type_decl_iterator parse_tree;
  let with_vars = List.filter has_type_variables !type_decls in
  let without_vars = List.filter (fun x -> not @@ has_type_variables x) !type_decls in
  let with_var_samplers = List.map generate_sampler_vars with_vars in
  let without_var_samplers = List.map generate_sampler without_vars in
  (* Each of those lists is a set of value bindings *)
  {
    pstr_desc = Pstr_value (Recursive, List.flatten @@ with_var_samplers@[without_var_samplers] ) ;
    pstr_loc = loc
  }
