(* Functions for creating and invoking samplers *)

exception No_choice
exception Not_implemented of string
            
let loc = Location.none

let repr = Ctype.repr

let (samplers : string list ref) = ref []

(* This is where we'll store all the samplers ast forms *)
(* We won't print this until the very end *)
(* given the Typed phase may introduce new ones *)
let (sampler_fns : Parsetree.value_binding list ref) = ref []

let rec register_sampler str =
  if not (sampler_exists str) then
    samplers := str :: !samplers

and sampler_exists str =
  List.mem str !samplers

let register_sampler_fn fn =
  sampler_fns := !sampler_fns @ [fn]
                        
let () = Random.self_init ();
  register_sampler "sample_int";
  register_sampler "sample_float";
  register_sampler "sample_bool";
  register_sampler "sample_char";
  register_sampler "sample_string"

let choose l =
  if List.length l = 0
  then raise No_choice
  else
    let
      choice = Random.int (List.length l)
    in
    List.nth l choice
               
(* The untyped version is mainly for producing samplers via type declarations *)
module Untyped = struct

  open Ast_helper
  open Types
  open Longident

  let base_types =
    [
      Predef.type_int;
      Predef.type_bool;
      Predef.type_string
    ]

  let rec sampler_name type_expr =
    let ending = sampler_string_of_type type_expr in
    "sample_" ^ ending
  and sampler_string_of_type type_expr =
    begin match type_expr.desc with
    | Tconstr (p, ts, _) ->
       begin match ts with
       | [] -> Path.name p
       | x ->
          let x' = List.map sampler_string_of_type ts in
          let prepend = String.concat "_" x' in
          prepend ^ "_" ^ (Path.name p)
       end
    | Ttuple  ts ->
       String.concat "_" (List.map sampler_string_of_type ts)
    end

  let rec get_sampler fn_sig argtypes =
    let id_desc_map = List.map (fun x -> (x.id, x.desc)) argtypes in
    let type_vars = List.map Typevars.Typed.collect_vars argtypes in
    let type_vars = List.sort_uniq compare @@ List.flatten type_vars in

    let rec assign_types type_vars =
      begin match type_vars with
      | [] -> []
      | hd::tl ->
         (* This time, we'll choose one random type assignment, rather than doing one of each *)
         let hd' = (hd, choose base_types) in
         hd' :: (assign_types tl)
      end
    in

    let var_map = assign_types type_vars in
    let instantiated = List.map (Typevars.Typed.instantiate_var var_map) argtypes in
    let fn_sig = Typevars.Typed.instantiate_var var_map fn_sig in
    let samplers = List.map sampler_name instantiated in
    let sampler_expr sampler =
      let sampler_expr = Exp.ident ( { txt = Lident sampler ; loc = loc } ) in
      let unit_expr =  Exp.ident ( { txt = Lident "()" ; loc = loc } ) in
      Exp.apply sampler_expr [Nolabel, unit_expr]
    in
    let sampler_exprs = List.map (sampler_expr) samplers in
    let sampler_tuple = Exp.tuple sampler_exprs in
    let sampler_fun = Exp.fun_ Nolabel None (Pat.construct { txt = Lident "()" ; loc = loc } None) sampler_tuple in
    (fn_sig, sampler_fun)
  
  open Ast_iterator
  open Asttypes
  open Longident
  open Parsetree

  let base_types =
    [
      [%type: int];
      [%type : bool];
      [%type : string]
    ]

  (* We collect all the type declarations here *)
  let (type_decls : type_declaration list ref) = ref []

  let type_declaration_iterator _iterator type_decl =
    type_decls := !type_decls @ [type_decl]

  let type_decl_iterator =
    {
      default_iterator with
      type_declaration = type_declaration_iterator
    }

  (* Some functions for making functions *)

  (* compose_let (let pat = e1 in e2) e' -> let pat = e1 in e' *)
  let compose_let [%expr let [%p? pat] = [%e? e1] in [%e? e2]] e' =
    [%expr let [%p pat] = [%e e1] in [%e e']]
                                     
  (* Same as above but with let rec *)
  let compose_let_rec [%expr let rec [%p? pat] = [%e? e1] in [%e? e2]] e' =
    [%expr let rec [%p pat] = [%e e1] in [%e e']]

  (* compose_fn (fun pat -> e) e' -> fun pat -> e' *)
  let compose_fn [%expr fun [%p? pat] -> [%e? e]] e' =
    [%expr fun [%p pat] -> [%e e']]

  (* create_fn [x1;x2;..;xn] e -> fun x1 -> fun x2 -> ... -> fun xn -> e *)
  let rec create_fn args e =
    begin match args with
    | [] -> e
    | hd::tl ->
       let pat = Pat.var ( { txt = hd ; loc = loc} ) in
       let e' = create_fn tl e in
       [%expr fun [%p pat] -> [%e e']]
    end

  (* We turn type declarations into strings for the sampler name *)
  (* ex. type test = ... -> sampler_test  *)
  (* For types with variables, we first instantiate the type variable to some value *)
  (* ex. type 'a tree  = ... -> sampler_int_tree *)
  let rec sampler_string_of_ctypes ctypes =
    begin match ctypes with
    | [] -> None
    | ctypes' ->
       Some (String.concat "_" @@ List.map sampler_string_of_ctype ctypes')
    end
  and sampler_string_of_ctype ctype =
    begin match ctype.ptyp_desc with
    | Ptyp_var v -> v
    | Ptyp_constr ( { txt = Lident id ; _ } , _ctypes) ->
       id                       (* TODO Why is ctypes unused? *)
    | Ptyp_tuple ctypes ->
       String.concat "_" @@ List.map sampler_string_of_ctype ctypes
    end

  (* Give it a type, and it returns the name of the sampler *)
  let sampler_name ctype =
    begin match ctype.ptyp_desc with
    | Ptyp_constr ( { txt = Lident name ; _ }, ctypes ) ->
       begin match ctypes with
       | [] -> "sample_" ^ name
       | _ ->
          let ctypes_string = sampler_string_of_ctypes ctypes in
          begin match ctypes_string with
          | None -> "sample_" ^ name
          | Some ctypes_string' ->
             "sample_" ^ ctypes_string' ^ "_" ^ name
          end
       end
    | _ -> raise (Not_implemented "sampler_name")
    end

  (* Given a constructor's name and arguments, we create a construct expression *)
  (* This is used for creating a random instance of that constructor *)
  (* so we assume the arguments are names of samplers *)
  let rec constructor constr_name sampler_args =
    let constr_id = { txt = Lident constr_name ; loc = loc } in
    let apply_to_unit name =
      let name_expr = Exp.ident { txt = Lident name ; loc = loc } in
      let unit_expr = Exp.ident { txt = Lident "()" ; loc = loc } in
      Exp.apply name_expr [Nolabel, unit_expr]
    in
    let sampler_exprs = List.map apply_to_unit sampler_args in
    let args =
      begin match sampler_exprs with
      | [] -> None
      | [x] -> Some x
      | l -> Some (Exp.tuple l)
      end
    in
    Exp.construct constr_id args

  (* Now we create functions which create constructors *)
  (* Takes a constructor_declaration *)
  (* Returns the function plus its name as a string *)
  and constructor_function { pcd_name = { txt = constr_name } ; pcd_args = Pcstr_tuple args } =
    let constr_args = List.map sampler_name args in
    let constr = constructor constr_name constr_args in
    let constr_fn = create_fn ["()"] constr in (* Shortuct *)
    let fn_name = "constr_" ^ (String.lowercase_ascii constr_name) in
    let constr_pat = Pat.var { txt = fn_name ; loc = loc } in

    (* This is just a placeholder until we compose all the lets together *)
    let default_expr = Exp.ident { txt = Lident "default" ; loc = loc } in
    
    ([%expr let [%p  constr_pat] = [%e constr_fn] in [%expr default_expr]], fn_name)

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

  (* Now we have our sampler functions which we'll make mutually recursive *)
  let generate_sampler { ptype_name = { txt = name} ;
                     ptype_params = params ;
                     ptype_kind = kind ;
                     ptype_manifest = manifest} =
    begin match kind with
    | Ptype_variant constrs ->
       let params = List.map (fun (x, y) -> x) params in
       let params_string = sampler_string_of_ctypes params in

       let sampler_pattern_string param_string =
         begin match param_string with
         | None -> "sample_" ^ name
         | Some param_string' -> "sample_" ^ param_string' ^ "_" ^ name
         end
       in
       let sampler_pattern = Pat.var { txt = sampler_pattern_string params_string ; loc = loc } in
       let constr_generators = List.map constructor_function constrs in
       let (gen_exprs, gen_list) = List.split constr_generators in

       let choose_expr = Exp.ident { txt = Lident "sample_alternatively" ; loc = loc } in
       let unit_expr = Exp.ident { txt = Lident "()" ; loc = loc } in
       let apply_choose_expr = Exp.apply choose_expr [Nolabel, ast_list_of_strings gen_list] in
       let apply_to_unit = Exp.apply apply_choose_expr [Nolabel, unit_expr] in

       let generator_expr =
         List.fold_right compose_let gen_exprs apply_to_unit in
       let generator_fn = create_fn ["()"] generator_expr in
       register_sampler @@ sampler_pattern_string params_string;
       Vb.mk sampler_pattern generator_fn
    | Ptype_abstract ->
       begin match manifest with
       | None -> raise (Not_implemented "generate_sampler :: match manifest")
       | Some ctype ->
          let sampler_name' = "sample_" ^ name in
          let sampler_pattern = Pat.var { txt = sampler_name' ; loc = loc } in
          
          let sampler_application sampler =
            let sampler_expr = Exp.ident { txt = Lident sampler ; loc = loc } in
            let unit_expr = Exp.construct { txt = Lident "()" ; loc = loc } None in
            Exp.apply sampler_expr [Nolabel, unit_expr]
          in
          begin match ctype.ptyp_desc with
          | Ptyp_constr ({ txt = type_name ; _ } ,_) ->
             let ctype_sampler = sampler_application @@ sampler_name ctype in
             let sampler_fn = create_fn ["()"] ctype_sampler in
             register_sampler sampler_name';
             Vb.mk sampler_pattern sampler_fn
          | Ptyp_tuple ctypes ->
             let ctypes_samplers = List.map sampler_application @@ List.map sampler_name ctypes in
             let samplers_tuple = Exp.tuple ctypes_samplers in
             let sampler_fn = create_fn ["()"] samplers_tuple in
             register_sampler sampler_name';
             Vb.mk sampler_pattern sampler_fn
          | _ ->
             raise (Not_implemented
               "generate_sampler :: match manifesh :: match ctype.ptyp_desc")
          end
       end
    | _ -> raise (Not_implemented "generate_sampler")
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

  let rec generate_sampler_with_type_vars ({ ptype_params = params ;
                                             ptype_kind = kind ; _ } as type_decl) =
    let params = List.map (fun (x, _) -> x.ptyp_desc) params in
    let rec assign_types params =
      begin match params with
      | [] -> []
      | hd::tl ->
         let hd' = List.map (fun x -> [hd, x]) base_types in
         hd' :: assign_types tl
      end
    in
    
    let var_maps = cross_all @@ assign_types params in
    List.map (fun x -> generate_sampler_with_type_vars' type_decl x) var_maps

  and generate_sampler_with_type_vars' ({ ptype_params = params ;
                                          ptype_kind = kind ; _ } as type_decl) var_maps =
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
       let constrs' =  Typevars.Untyped.instantiate_var var_maps constrs in
       generate_sampler
         { type_decl with ptype_params = params' ; ptype_kind = Ptype_variant constrs' }
    | _ -> raise (Not_implemented "generate_sampler_with_type_vars")
    end

      
  (* We use this to discriminate between decls with type variables and without *)
  let has_type_variables ({ ptype_params = params ; _ } : type_declaration) =
    begin match params with
    | [] -> false
    | _ -> true
    end

  let run parse_tree =
    type_decl_iterator.structure type_decl_iterator parse_tree;
    let with_vars = List.filter has_type_variables !type_decls in
    let without_vars = List.filter (fun x -> not @@ has_type_variables x) !type_decls in
    let with_var_samplers = List.map generate_sampler_with_type_vars with_vars in
    let without_var_samplers = List.map generate_sampler without_vars in
    List.iter register_sampler_fn (List.flatten @@ without_var_samplers :: with_var_samplers)
end
                   

(* The typed version is mostly for invoking samplers created by the untyped version *)
(* But some samplers may have been missed, so we create those as well *)
(* For example, any function which takes a tuple *)
(* f : a' * 'b -> 'c, there will most likely be samplers for instantiations of 'a and 'b *)
(* but unless 'a * 'b was declared as a type on its own, there is no sampler created *)
module Typed = struct
  open Ast_helper
  open Types
  open Longident

  let base_types =
    [
      Predef.type_int;
      Predef.type_bool;
      Predef.type_string
    ]

  (* Borrowed from Printtyp.ml *)
  (* Print a type expression *)

  let names = ref ([] : (type_expr * string) list)
  let name_counter = ref 0
  let named_vars = ref ([] : string list)

  let reset_names () = names := []; name_counter := 0; named_vars := []
  let add_named_var ty =
    match ty.desc with
      Tvar (Some name) | Tunivar (Some name) ->
                          if List.mem name !named_vars then () else
                            named_vars := name :: !named_vars
      | _ -> ()

  let rec new_name () =
    let name =
      if !name_counter < 26
      then String.make 1 (Char.chr(97 + !name_counter))
      else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
             string_of_int(!name_counter / 26) in
    incr name_counter;
    if List.mem name !named_vars
       || List.exists (fun (_, name') -> name = name') !names
    then new_name ()
    else name

  let name_of_type t =
    (* We've already been through repr at this stage, so t is our representative
     of the union-find class. *)
    try List.assq t !names with Not_found ->
                                 let name =
                                   match t.desc with
                                     Tvar (Some name) | Tunivar (Some name) ->
                                                         (* Some part of the type we've already printed has assigned another
                                                          * unification variable to that name. We want to keep the name, so try
                                                          * adding a number until we find a name that's not taken. *)
                                                         let current_name = ref name in
                                                         let i = ref 0 in
                                                         while List.exists (fun (_, name') -> !current_name = name') !names do
                                                           current_name := name ^ (string_of_int !i);
                                                           i := !i + 1;
                                                         done;
                                                         !current_name
                                     | _ ->
                                        (* No name available, create a new one *)
                                        new_name ()
                                 in
                                 (* Exception for type declarations *)
                                 if name <> "_" then names := (t, name) :: !names;
                                 name

  let check_name_of_type t = ignore(name_of_type t)

  let remove_names tyl =
    let tyl = List.map repr tyl in
    names := List.filter (fun (ty,_) -> not (List.memq ty tyl)) !names

  (* End *)
  let rec print_type_expr ppf expr =
    let {id = id; desc = tdesc} as expr= Btype.repr expr in
    begin match tdesc with
    | Tvar so ->
       let str = name_of_type expr 
       (* begin match so with
        * | Some s-> s
        * | None -> "-NONE-"
        * end *)
       in Format.fprintf ppf
            "Tvar(%s)[%d]" str id
    | Tarrow (_, t1, t2, _) ->
       Format.fprintf ppf
         "Tarrow (_, %a, %a, _)[%d]"
         print_type_expr t1
         print_type_expr t2
         id
    | Ttuple ts ->
       Format.fprintf ppf
         "Ttuple(%a)[%d]"
         print_type_expr_list ts
         id
    | Tconstr (p, ts, _) ->
       Format.fprintf ppf
         "Tconstr(%s, %a, _)[%d]"
         (Printtyp.string_of_path p)
         print_type_expr_list ts
         id
    | Tobject (t, pteor) ->
       begin match !pteor with
       | Some (p, ts) ->
          Format.fprintf ppf
            "Tobject(%a, (%s, %a))[%d]"
            print_type_expr t
            (Printtyp.string_of_path p)
            print_type_expr_list ts
            id
       | None ->
          Format.fprintf ppf
            "Tobject(%a, -NONE-)[%d]"
            print_type_expr t
            id
       end
    | Tfield (lbl, _, t1, t2) ->
       Format.fprintf ppf
         "Tfield(%s, _, %a, %a)[%d]"
         lbl
         print_type_expr t1
         print_type_expr t2
         id
    | Tnil -> Format.fprintf ppf "Tnil[%d]" id
    | Tlink t ->
       (* print_type_expr ppf t *)
       Format.fprintf ppf
         "Tlink(%a)[%d]"
         print_type_expr t
         id
    | _ -> Format.fprintf ppf "[unsupported type_desc]"
    end

  and print_type_expr_list ppf l =
    Format.fprintf ppf "[%a]" print_type_expr_list' l
                   
  and print_type_expr_list' ppf l =
    begin match l with
    | [] -> Format.fprintf ppf ""
    | hd::tl ->
       Format.fprintf ppf
         "%a, %a"
         print_type_expr hd
         print_type_expr_list' tl
    end

      
  let rec sampler_name type_expr =
    let ending = sampler_string_of_type type_expr in
    "sample_" ^ ending
  and sampler_string_of_type type_expr =
    print_type_expr (Format.std_formatter) type_expr;
    begin match type_expr.desc with
      | Tvar s ->
        begin match s with
          | None -> ""
          | Some s' -> s'
        end
    | Tconstr (p, ts, _) ->
       begin match ts with
       | [] -> Path.name p
       | x ->
          let x' = List.map sampler_string_of_type ts in
          let prepend = String.concat "_" x' in
          prepend ^ "_" ^ (Path.name p)
       end
    | Ttuple  ts ->
       String.concat "_" (List.map sampler_string_of_type ts)
    | Tarrow (_, t1, t2, _) ->
      (sampler_string_of_type t1) ^ "_arrow_" ^ (sampler_string_of_type t2)
    | _ -> "ERROR"
       
    end

  let rec get_sampler fn_sig argtypes =
    let id_desc_map = List.map (fun x -> (x.id, x.desc)) argtypes in
    (* Unlike in the untyped version, we're gonna use the id to represent a type var *)
    let type_vars = List.map Typevars.Typed.collect_vars argtypes in
    let type_vars = List.sort_uniq compare @@ List.flatten type_vars in

    let rec assign_types type_vars =
      begin match type_vars with
      | [] -> []
      | hd::tl ->
         (* This time, we'll choose one random type assignment, rather than doing one of each *)
         let hd' = (hd, choose base_types) in
         hd' :: (assign_types tl)
      end
    in

    let var_map = assign_types type_vars in
    let instantiated = List.map (Typevars.Typed.instantiate_var var_map) argtypes in
    let fn_sig = Typevars.Typed.instantiate_var var_map fn_sig in
    let _ = Format.fprintf (Format.std_formatter) "fn_sig:%a\n" Printtyp.type_expr fn_sig in
    let samplers = List.map sampler_name instantiated in
    let _ = List.iter (fun x -> print_string @@ x ^ "\n") samplers in
    
    (* Check that all the samplers we wanna use exist *)
    (* and create the ones that don't *)
    List.iter (check_samplers) (List.combine samplers argtypes);
    
    let sampler_expr sampler =
      let sampler_expr = Exp.ident ( { txt = Lident sampler ; loc = loc } ) in
      let unit_expr =  Exp.ident ( { txt = Lident "()" ; loc = loc } ) in
      Exp.apply sampler_expr [Nolabel, unit_expr]
    in
    let sampler_exprs = List.map (sampler_expr) samplers in
    let sampler_tuple = Exp.tuple sampler_exprs in
    let sampler_fun = Exp.fun_ Nolabel None (Pat.construct { txt = Lident "()" ; loc = loc } None) sampler_tuple in
    (fn_sig, sampler_fun)

  (* Copied from Untyped above *)
  and create_fn args e =
    let open Parsetree in
    begin match args with
    | [] -> e
    | hd::tl ->
       let pat = Pat.var ( { txt = hd ; loc = loc} ) in
       let e' = create_fn tl e in
       [%expr fun [%p pat] -> [%e e']]
    end

  and check_samplers (sampler, type_expr) =
    (* If the sampler doesn't yet exist, we make it *)
    (* This should only occur in the example above *)
    let type_expr = repr type_expr in
    if not (sampler_exists sampler) then
      begin match type_expr.desc with
      | Ttuple ts ->
         let sampler_names = List.map sampler_name ts in
         let sampler_expr sampler =
           let sampler_expr = Exp.ident ( { txt = Lident sampler ; loc = loc } ) in
           let unit_expr =  Exp.ident ( { txt = Lident "()" ; loc = loc } ) in
           Exp.apply sampler_expr [Nolabel, unit_expr]
         in
         let sampler_tuple = Exp.tuple (List.map sampler_expr sampler_names) in
         let sampler_fun = create_fn ["()"] sampler_tuple in
         let sampler_pattern = Pat.var { txt = sampler ; loc = loc } in
         let sampler = Vb.mk sampler_pattern sampler_fun in
         register_sampler_fn sampler
      | Tarrow (_, t1, t2, _) ->
        let sampler_body =
          Exp.apply
            (Exp.ident { txt = Lident "raise" ; loc = loc})
            [Nolabel,
             Exp.construct { txt = Lident "Not_implemented" ; loc = loc} None]
        in
         let sampler_fun = create_fn ["()"] sampler_body in
         let sampler_pattern = Pat.var { txt = sampler ; loc = loc } in
         let sampler = Vb.mk sampler_pattern sampler_fun in
         register_sampler_fn sampler
      | Tconstr (Path.Pident t, ts, _) ->
         begin match Ident.name t with
         | "list" ->
            let list_sampler = Exp.ident { txt = Lident "sample_list" ; loc = loc } in
            let sample_from = Exp.ident { txt = Lident (sampler_name @@ List.hd ts) ; loc = loc } in
            let sampler_body = Exp.apply list_sampler [Nolabel, sample_from] in
            let unit_expr =  Exp.ident ( { txt = Lident "()" ; loc = loc } ) in
            let sampler_body' = Exp.apply sampler_body [Nolabel, unit_expr] in
            let sampler_fun = create_fn ["()"] sampler_body in
            let sampler_pattern = Pat.var { txt = sampler ; loc = loc } in
            let sampler = Vb.mk sampler_pattern sampler_fun in
            register_sampler_fn sampler
         end
      | _ -> raise (Not_implemented ("check_samplers :: match desc :: " ^ sampler))
      end
  
end

let sampler_functions () =
  let open Ast_helper in
  Str.value Recursive !sampler_fns

