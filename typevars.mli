module Untyped : sig
  open Parsetree

  val instantiate_var : (core_type_desc * core_type) list
                        -> constructor_declaration list
                        -> constructor_declaration list
end

module Typed : sig
  open Typedtree
  open Types
  
  val instantiate_var : (int * type_expr) list -> type_expr -> type_expr

  val collect_vars : type_expr -> int list
end
