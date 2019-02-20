type s = int
(* and z = s *)
type t = int list
type u = int * int
type v = int -> int
type ('a, 'b) w = ('a * 'b) list

type 'a tree = Tree of 'a tree * 'a tree | Leaf of 'a
