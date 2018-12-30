(* Generates the template file by stripping removing parts set for erasure *)
(* This is basically solution, but we replace the expression with text  *)
(* instead of unwrapping the expression in the extension *)

open Ast_mapper
open Asttypes
open Parsetree

let expr_mapper mapper expr =
  begin match expr with
  | { pexp_desc = Pexp_extension ({ txt = "erase" ; _}, _) ; pexp_loc = loc ; _ } ->
     [%expr "YOUR CODE HERE"]
  | expr' -> default_mapper.expr mapper expr'
  end
    
let template_mapper = {
    default_mapper with
    expr = expr_mapper
  }

let template tree =
  template_mapper.structure template_mapper tree
