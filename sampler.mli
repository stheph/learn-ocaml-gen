module Untyped : sig

  open Parsetree

  val run : structure -> structure_item
                           
end
                   
module Typed : sig

  open Ast_helper
  open Types

  val get_sampler : type_expr -> type_expr list -> type_expr * Parsetree.expression
                                    
end

