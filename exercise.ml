(* Contains mappers for template.ml and solution.ml *)
(* They're pretty similar, except for two things *)
(* 1. template will "erase" content between the erase extension *)
(* -- this means the content will be overwritten by "YOUR CODE HERE" *)
(* 2. Solution will keep track of what exercises have the erase extension *)
(* erase should be the only extension left *)
(* so we need not worry about ignoring other extensions *)

open Ast_mapper
open Asttypes
open Parsetree

let loc = Location.none
       
module Template = struct

  let rec expr_mapper mapper expr =
    begin match expr with
    | { pexp_desc = Pexp_constraint (e, _) ; _ } ->
       expr_mapper mapper e
    | [%expr [%erase [%e? e]]] ->
       [%expr raise Not_implemented]
    | expr' -> default_mapper.expr mapper expr
    end

  let mapper =
    {
      default_mapper with
      expr = expr_mapper
    }

  let out_file tree dir =
    let tree' = mapper.structure mapper tree in
    let oc = open_out (dir ^ Filename.dir_sep ^ "template.ml") in
    let ppf = Format.formatter_of_out_channel oc in
    Format.fprintf ppf "@[%a@]@." Pprintast.structure tree';
    close_out oc

end

module Solution = struct

  let (current_function : string ref) = ref ""
  let (exercises : string list ref) = ref []

  let rec expr_mapper track mapper expr =
    begin match expr with
    | { pexp_desc = Pexp_constraint (e, _) ; _ } ->
       expr_mapper track mapper e
    | [%expr [%erase [%e? e]]] ->
       if track then
         exercises := !current_function :: !exercises;
       expr_mapper track mapper e
    | expr' -> default_mapper.expr mapper expr
    end

  let solution_mapper =
    {
      default_mapper with
      expr = (expr_mapper true)
    }

  let structure_item_mapper track mapper structure_item =
    begin match structure_item.pstr_desc with
    | Pstr_value (rec_flag, vbs) ->
       let rec map_vbs vbs =
         begin match vbs with
         | [] -> []
         | ({ pvb_pat =
                { ppat_desc =
                    Ppat_var { txt = name ; _ } ; _ } ; _ }  as hd) :: tl ->
            if track then
              current_function := name;
            (solution_mapper.value_binding mapper hd) :: (map_vbs tl)
         | hd::tl -> (solution_mapper.value_binding mapper hd) :: (map_vbs tl)
         end
       in
       let vbs' = map_vbs vbs in
       { structure_item with pstr_desc = Pstr_value (rec_flag, vbs')}
    | _ -> solution_mapper.structure_item mapper structure_item
    end

  (* Technically should just be an iterator *)
  (* but as a mapper, we can compose with the one above *)
  let exercise_tracker_mapper =
    {
      solution_mapper with
      structure_item = (structure_item_mapper true)
    }

  let stripper =
    {
      default_mapper with
      expr = (expr_mapper false)
    }

  let strip tree =
    stripper.structure stripper tree
      
  let out_file tree dir =
    let tree' = exercise_tracker_mapper.structure exercise_tracker_mapper tree in
    let oc = open_out (dir ^ Filename.dir_sep ^ "solution.ml") in
    let ppf = Format.formatter_of_out_channel oc in
    Format.fprintf ppf "@[%a@]@." Pprintast.structure tree';
    close_out oc
      
end

let strip = Solution.strip
