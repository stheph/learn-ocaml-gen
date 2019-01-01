(* Functions for dealing with those pesky type variables *)

module Untyped = struct
  open Parsetree

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
  
end

module Typed = struct
  open Typedtree
  open Types

  exception Not_implemented of string
                                 
  let rec instantiate_var var_map type_expr =
    (* repr returns the canonical representation of a type *)
    (* TODO Not sure what this means *)
    (* Ctype is for manipulating types after type checking *)
    (* TODO Look into the necessity of correct_levels? *)
    let type_expr = Ctype.repr type_expr in
    begin
      match type_expr.desc with
      | Tvar _ ->
         (* If it's a variable, go ahead and replace it from the map *)
         List.assoc type_expr.id var_map
      (* For the rest of the cases, just recursively call, and replace in type_expr *)
      | Tarrow (lbl, t1, t2, comm) ->
         let desc =
           Tarrow (lbl, instantiate_var var_map t1, instantiate_var var_map t2, comm) in
         { type_expr with desc = desc }
      | Ttuple ts ->
         let desc =
           Ttuple (List.map (instantiate_var var_map) ts) in
         { type_expr with desc = desc }
      | Tconstr (p, ts, m) ->
         let desc =
           Tconstr (p, List.map (instantiate_var var_map) ts, m) in
         { type_expr with desc = desc }
      | _ -> raise (Not_implemented "instantiate_var")
    end

  let rec collect_vars type_expr =
    let type_expr = Ctype.repr type_expr in
    begin match type_expr.desc with
    | Tvar _ -> [type_expr.id]              (* This is a type variable, so store its id *)
    | Tconstr (_, ts, _)
    | Ttuple ts ->
       List.flatten @@ List.map collect_vars ts
    | Tarrow (_, t1, t2, _) ->
       collect_vars t1 @ collect_vars t2
    | _ -> raise (Not_implemented "collect_vars")
    end
      
end
                 
