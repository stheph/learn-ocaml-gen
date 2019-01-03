open Parsetree

module Template : sig
  val out_file : structure -> string -> unit
end

module Solution : sig
  val exercises : string list ref
  val strip : structure -> structure
  val out_file : structure -> string -> unit
end

val strip : structure -> structure
