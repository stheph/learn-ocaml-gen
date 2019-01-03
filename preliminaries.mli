open Parsetree

module Meta : sig

  type meta_data
  val run : structure -> structure
  val strip : structure -> structure
  val out_file : string -> unit

end

module Prelude : sig 

  val run : structure -> structure
  val strip : structure -> structure
  val out_file : string -> unit

end

module Prepare : sig

  val run : structure -> structure
  val strip : structure -> structure
  val out_file : string -> unit
                           
end

val run : structure -> structure
val strip : structure -> structure
