(* Generates the solution file by stripping extensions from input *)

open Ast_mapper
open Asttypes
open Parsetree

let expr_mapper mapper expr =
  begin match expr with
  | [%expr [%erase [%e? e]]] ->
     e
  | expr' -> default_mapper.expr mapper expr'
  end
    
let solution_mapper = {
    default_mapper with
    expr = expr_mapper
  }

let solution tree =
  solution_mapper.structure solution_mapper tree
