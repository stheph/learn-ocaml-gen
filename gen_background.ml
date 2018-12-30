(* Generate the meta.json, prelude.ml, and prepare.ml *)

open Ast_mapper
open Asttypes
open Parsetree

(* Exceptions *)

(* Here we store the structure for each file *)
let (meta : structure ref) = ref []
let (prelude : structure ref) = ref []
let (prepare : structure ref) = ref []

(* We alson want to get rid of the structures with extensions *)
(* So we have an easier time piping this into solution/template/test  *)
let rec structure_mapper mapper structure =
  begin match structure with
  | [] -> []
  | hd::tl ->
     begin match hd with
     | { pstr_desc = Pstr_extension (( ext , PStr ([str_item])), _) ; _} ->
        begin match ext with
        (* In every case with an extension, we just drop the head *)
        | { txt = "meta" ; _} ->
           meta := str_item::(!meta);
           structure_mapper mapper tl
        | { txt = "prelude" ; _} -> 
           prelude := str_item::(!prelude);
           structure_mapper mapper tl
        | { txt = "prepare" ; _} -> 
           prepare := str_item::(!prepare);
           structure_mapper mapper tl
        | _ -> hd :: (structure_mapper mapper tl)
        end
     | _ -> hd::tl
     end
  end

let background_mapper =
  {
    default_mapper with
    structure = structure_mapper
  }

let background tree =
  background_mapper.structure background_mapper tree

(* Leaves background info so type checking will succeed *)
let rec leave_structure_mapper mapper structure =
  begin match structure with
  | [] -> []
  | hd::tl ->
     begin match hd with
     | { pstr_desc = Pstr_extension (( ext , PStr ([str_item])), _) ; _} ->
        begin match ext with
        (* In every case with an extension, we just drop the head *)
        | { txt = "meta" ; _} ->
           meta := str_item::(!meta);
           leave_structure_mapper mapper tl
        | { txt = "prelude" ; _} -> 
           prelude := str_item::(!prelude);
           str_item :: leave_structure_mapper mapper tl
        | { txt = "prepare" ; _} -> 
           prepare := str_item::(!prepare);
           str_item :: leave_structure_mapper mapper tl
        | _ -> hd :: (leave_structure_mapper mapper tl)
        end
     | _ -> hd::tl
     end
  end

let leave_background_mapper =
  {
    default_mapper with
    structure = structure_mapper
  }

let leave_background tree =
  leave_background_mapper.structure leave_background_mapper tree
