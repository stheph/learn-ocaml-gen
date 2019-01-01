open Ast_mapper
open Asttypes
open Parsetree

exception Unknown_extension of string

(* Containers for what we write to these files *)
val meta : structure ref
val prelude : structure ref
val prepare : structure ref
                        
(* Performs the ast mapping *)
val run : structure -> structure
val strip : structure -> structure
