open Parsetree
    
(* Deal with the ones with type vars first: instantiate for each type *)
(* We do this by replacing instances of each type var in the tree with a core_type *)
(* listed as "base_types" above *)
(* instantiate_var is responsible for going into the constructors *)
(* Each constructor has a list of core_types, which the replace_var functions manipulate *)
(* with replace_var' doing the actual replacement *)
    
let rec instantiate_var var_map constrs =
  begin match constrs with
  | [] -> []
  | hd::tl ->
     let hd' =
       begin match hd with
       | { pcd_args = Pcstr_tuple args ; _} as constr ->
          { constr with pcd_args = Pcstr_tuple (replace_var var_map args) }
       (* label_decls is a list of lbl1 : t1, lbl2 : t2, ... *)
       | { pcd_args = Pcstr_record label_decls ; _ } as constr ->
          let label_decls' =
            List.map
              (fun ({ pld_type = typ ; _ } as x) -> { x with pld_type = replace_var' var_map typ })
              label_decls
          in { constr with pcd_args = Pcstr_record label_decls' }
       end
     in hd' :: (instantiate_var var_map tl)
  end
and replace_var var_map core_types =
  begin match core_types with
  | [] -> []
  | hd::tl ->
     replace_var' var_map hd :: replace_var var_map tl
  end
and replace_var' var_map core_type =
  let typ_desc' =
    begin match core_type.ptyp_desc with
    | Ptyp_var _ as var ->
       begin match List.assoc_opt var var_map with
       | Some x -> x.ptyp_desc
       | None -> var
       end
    | Ptyp_arrow (lbl, ctype1, ctype2) ->
       Ptyp_arrow (lbl,
                   replace_var' var_map ctype1,
                   replace_var' var_map ctype2)
    | Ptyp_tuple ctypes ->
       Ptyp_tuple (replace_var var_map ctypes)
    | Ptyp_constr (id, ctypes) ->
       Ptyp_constr (id, replace_var var_map ctypes)
    | Ptyp_class (id, ctypes) ->
       Ptyp_class (id, replace_var var_map ctypes)
    | Ptyp_alias (ctype, alias) ->
       Ptyp_alias (replace_var' var_map ctype, alias)
    | Ptyp_poly (id, ctype) ->
       Ptyp_poly (id, replace_var' var_map ctype)
    | x -> x
    end
  in { core_type with ptyp_desc = typ_desc' }
