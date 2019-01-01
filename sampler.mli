module Untyped : sig

  open Ast_helper
  open Ast_iterator
  open Asttypes
  open Longident

  open Parsetree

  exception Not_implemented of string
  
  val run : structure -> structure_item
                           
end

module Typed : sig

  open Ast_helper
  open Types

  val sampler_name : type_expr -> string
  val get_sampler : type_expr -> type_expr list -> type_expr * Parsetree.expression
                                    
end
