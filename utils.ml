open Parsetree

exception Conversion_failure of string
       
let rec list_of_ast conv expr =
  begin match expr with
  | [%expr []] -> []
  | [%expr [%e? hd] :: [%e? tl]] ->
     conv hd :: list_of_ast conv tl
  | _ -> raise (Conversion_failure "expr is not a list")
  end

(* Note that these conversion functions DO NOT handle meta data *)
(* ex. the second argument of Pconst_integer                    *)
(*     which represents a suffix like l (int32) or L (int64)    *)
let rec conv_int expr =
  begin match expr.pexp_desc with
  | Pexp_constant (Pconst_integer (n, _)) ->
     begin
       try
         int_of_string n
       with
         _ -> raise (Conversion_failure "expr is not an int")
     end
  | _ -> raise (Conversion_failure "expr is not an int")
  end

let rec conv_char expr =
  begin match expr.pexp_desc with
  | Pexp_constant (Pconst_char c) -> c
  | _ -> raise (Conversion_failure "expr is not a char")
  end

let rec conv_string expr =
  begin match expr.pexp_desc with
  | Pexp_constant (Pconst_string (str, _)) -> str
  | _ -> raise (Conversion_failure "expr is not a string")
  end

let rec conv_float expr =
  begin match expr.pexp_desc with
  | Pexp_constant (Pconst_float (f, _)) ->
     begin
       try
         float_of_string f
       with
         _ -> raise (Conversion_failure "expr is not a float")
     end
  | _ -> raise (Conversion_failure "expr is not a float")
  end

(* fst and snd are conversion functions to be applied to fst p and snd p *)
let rec conv_pair fst snd expr =
  begin match expr with
  | [%expr ([%e? p1], [%e? p2])] -> (fst p1, snd p2)
  | _ -> raise (Conversion_failure "expr is not a pair")
  end


let int_list_of_ast = list_of_ast conv_int

let char_list_of_ast = list_of_ast conv_char
                                     
let string_list_of_ast = list_of_ast conv_string
                                     
let float_list_of_ast = list_of_ast conv_float
                           
let string_pair_list_of_ast = list_of_ast (conv_pair conv_string conv_string)
