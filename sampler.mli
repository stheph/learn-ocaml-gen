exception No_choice
exception Not_implemented of string

(* TODO Do we need to expose choose and sampler? *)
val choose : 'a list -> 'a

val samplers : string list ref

module Untyped : sig

  open Parsetree

  val run : structure -> unit
                           
end
                   
module Typed : sig

  open Ast_helper
  open Types

  val sampler_name : type_expr -> string
  val get_sampler : type_expr -> type_expr list -> type_expr * Parsetree.expression
                                    
end

val sampler_functions : unit -> Parsetree.structure_item
