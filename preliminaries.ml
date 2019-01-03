(* Contains AST mappers for meta.json, prelude.ml, and prepare.ml *)
(* Also has an iterator which converts AST structures into json for meta.json *)
(* -- incredibly unsafely *)

open Ast_iterator
open Ast_mapper
open Asttypes
open Parsetree

exception Not_implemented of string * string
       
let rec general_structure_mapper ext acc strip mapper structure =
  (* print_string (Printf.sprintf "Running %s (%s) mapper\n" ext (string_of_bool strip));  *)
  begin match structure with
  | [] -> []
  | hd::tl ->
     begin match hd with
     | { pstr_desc =
           Pstr_extension
             (({ txt = ext' ; _ }, PStr structure'), _) ; _ } when ext = ext' ->
        (* print_string (Printf.sprintf "Found %s match\n" ext);  *)
        begin match structure' with
        | [] -> []
        | [x] ->
           if strip then
             begin
               default_mapper.structure_item mapper x :: general_structure_mapper ext acc strip mapper tl
             end
           else
             begin
             (* begin match x.pstr_desc with
              * | Pstr_value _ -> *)
                (* Preserve order *)
                acc := !acc @ [x];
                general_structure_mapper ext acc strip mapper tl
             (* | _ -> raise
              *          (Not_implemented
              *             ("structure_mapper", "Unexpected structure_item_desc in payload"))
              * end *)
             end
        | l -> raise
                 (Not_implemented
                    ("structure_mapper", "multiple structures in payload")) 
        end
     (* | { pstr_desc =
      *       Pstr_extension
      *         (({ txt = ext' ; _ }, PStr structure'), _) ; _ } ->
      *    print_string (Printf.sprintf "Running %s mapper : Found %s\n" ext ext');
      *      default_mapper.structure_item mapper hd :: general_structure_mapper ext acc strip mapper tl *)
     | x -> default_mapper.structure_item mapper hd :: general_structure_mapper ext acc strip mapper tl
     end
  end

let general_mapper ext out =
  {
    default_mapper with
    structure = (general_structure_mapper ext out false) 
  }

let general_stripper ext out =
  {
    default_mapper with
    structure = (general_structure_mapper ext out true) 
  }
       
module Meta = struct
  let (out : structure ref) = ref []

  type meta_data =
    {
      learnocaml_version : int;
      kind : string option;
      stars : int option;
      title : string option;
      identifier : int;
      authors : string list option;
      focus : string list option;
      requirements : string list option;
      forward_exercises : string list option;
      backward_exercises : string list option;
      max_score : int option
    } [@@deriving yojson]

  let default =
    {
      learnocaml_version = 2;
      kind = Some "exercise";
      stars = Some 3;
      title = Some "Exercise";
      identifier = 0;
      authors = Some [];
      focus = Some [];
      requirements = Some [];
      forward_exercises = Some [];
      backward_exercises = Some [];
      max_score = Some 100;
    }

  (* AST Mappers *)
  (* Collect and remove meta extensions and payloads *)
  let mapper = general_mapper "meta" out
  let run tree =
    mapper.structure mapper tree

  (* Strip "meta" extension from tree *)
  (* We use this to allow us to send the ast to the type checker *)
  let stripper = general_stripper "meta" out
  let strip tree =
    stripper.structure stripper tree

  (* TODO Hide after debugging?  *)
  (* let out_tree () = !out *)

  (* Printing out meta data as a json file: meta.json *)
  let (out_meta : meta_data ref) = ref default

  (* Turns an ast branch that represents a list into an ocaml list *)
  (* Should only really be used for lists of string literals for meta data *)
  (* but I've opted to write it in a more general form *)
  exception List_conversion_failure

  type might_be =
    | Int : int -> might_be
    | String : string -> might_be
    | Char : char ->  might_be
    | Float : float -> might_be
              
  let rec list_of_ast expr =
    let l = list_of_ast' expr in
    (* Check if all the elements are of the same type *)
    (* Should definitely *not* be using exceptions this way *)
    if List.for_all
         (fun x ->
           try let Pconst_integer _ = x in true
           with Match_failure _ -> false)
         l
    then
      List.map (fun x -> let Pconst_integer (value,_) = x in Int (int_of_string value)) l
    else if List.for_all
         (fun x ->
           try let Pconst_string _ = x in true
           with Match_failure _ -> false)
         l
    then
      List.map (fun x -> let Pconst_string (value,_) = x in String value) l
    else if List.for_all
         (fun x ->
           try let Pconst_float _ = x in true
           with Match_failure _ -> false)
         l
    then
      List.map (fun x -> let Pconst_float (value,_) = x in Float (float_of_string value)) l
    else if List.for_all
         (fun x ->
           try let Pconst_char _ = x in true
           with Match_failure _ -> false)
         l
    then
      List.map (fun x -> let Pconst_char value = x in Char value) l
    else
      raise List_conversion_failure
            
  and list_of_ast' expr =
    begin match expr.pexp_desc with
    (* nil *)
    | Pexp_construct ({ txt = Lident "[]" ; _ } , None) -> []
    (* cons *)
    | Pexp_construct ({ txt = Lident "::" ; _ } ,
                      Some { pexp_desc =
                               Pexp_tuple [{ pexp_desc = Pexp_constant left ; _ };right] ;  _ } ) ->
       left :: list_of_ast' right
    | _ -> raise List_conversion_failure
    end

  let string_list_of_ast l =
    List.map (fun (String x) -> x) l
     
  exception Not_metadata of string
  exception Bad_value_for_metadata of string

  let value_binding_iterator iterator value_binding =
    begin match value_binding.pvb_pat with
    | { ppat_desc = Ppat_var { txt = "learnocaml_version" } ; _ } ->
       begin
         try
           let { pexp_desc = Pexp_constant (Pconst_integer (value, None)) ; _ }
             = value_binding.pvb_expr
           in
           out_meta := {!out_meta with learnocaml_version = int_of_string value }
         with Match_failure _ -> raise (Bad_value_for_metadata "learnocaml_version")
       end
    | { ppat_desc = Ppat_var { txt = "kind" } ; _ } ->
       begin
         try
           let { pexp_desc = Pexp_constant (Pconst_string (value, None)) ; _ }
             = value_binding.pvb_expr
           in
           out_meta := {!out_meta with kind = Some value }
         with Match_failure _ -> raise (Bad_value_for_metadata "kind")
       end
    | { ppat_desc = Ppat_var { txt = "stars" } ; _ } ->
       begin
         try
           let { pexp_desc = Pexp_constant (Pconst_integer (value, None)) ; _ }
             = value_binding.pvb_expr
           in
           out_meta := {!out_meta with stars = Some (int_of_string value) }
         with Match_failure _ -> raise (Bad_value_for_metadata "stars")
       end
    | { ppat_desc = Ppat_var { txt = "title" } ; _ } ->
       begin
         try
           let { pexp_desc = Pexp_constant (Pconst_string (value, None)) ; _ }
             = value_binding.pvb_expr
           in
           out_meta := {!out_meta with title = Some value }
         with Match_failure _ -> raise (Bad_value_for_metadata "title")
       end
    | { ppat_desc = Ppat_var { txt = "identifier" } ; _ } ->
       begin
         try
           let { pexp_desc = Pexp_constant (Pconst_integer (value, None)) ; _ }
             = value_binding.pvb_expr
           in
           out_meta := {!out_meta with identifier = int_of_string value }
         with Match_failure _ -> raise (Bad_value_for_metadata "identifier")
       end
    | { ppat_desc = Ppat_var { txt = "authors" } ; _ } ->
       begin
         try
           let expr = value_binding.pvb_expr in
           let list = string_list_of_ast @@ list_of_ast expr in
           out_meta := {!out_meta with authors = Some list }
         with Match_failure _ -> raise (Bad_value_for_metadata "authors")
       end
    | { ppat_desc = Ppat_var { txt = "focus" } ; _ } ->
       begin
         try
           let expr = value_binding.pvb_expr in
           let list = string_list_of_ast @@ list_of_ast expr in
           out_meta := {!out_meta with focus = Some list }
         with Match_failure _ -> raise (Bad_value_for_metadata "focus")
       end
    | { ppat_desc = Ppat_var { txt = "requirements" } ; _ } ->
       begin
         try
           let expr = value_binding.pvb_expr in
           let list = string_list_of_ast @@ list_of_ast expr in
           out_meta := {!out_meta with requirements = Some list }
         with Match_failure _ -> raise (Bad_value_for_metadata "requirements")
       end
    | { ppat_desc = Ppat_var { txt = "foward_exercises" } ; _ } ->
       begin
         try
           let expr = value_binding.pvb_expr in
           let list = string_list_of_ast @@ list_of_ast expr in
           out_meta := {!out_meta with forward_exercises = Some list }
         with Match_failure _ -> raise (Bad_value_for_metadata "forward_exercises")
       end
    | { ppat_desc = Ppat_var { txt = "backward_exercises" } ; _ } ->
       begin
         try
           let expr = value_binding.pvb_expr in
           let list = string_list_of_ast @@ list_of_ast expr in
           out_meta := {!out_meta with backward_exercises = Some list }
         with Match_failure _ -> raise (Bad_value_for_metadata "backward_exercises")
       end
    | { ppat_desc = Ppat_var { txt = "max_score" } ; _ } ->
       begin
         try
           let { pexp_desc = Pexp_constant (Pconst_integer (value, None)) ; _ }
             = value_binding.pvb_expr
           in
           out_meta := {!out_meta with max_score = Some (int_of_string value) }
         with Match_failure _ -> raise (Bad_value_for_metadata "max_score")
       end
    | { ppat_desc = Ppat_var { txt = label } ; _ } ->
       raise (Not_metadata label)
    end

  let meta_collection_iterator =
    {
      default_iterator with
      value_binding = value_binding_iterator
    }
                            
  let out_file dir =
    let structure = !out in
    meta_collection_iterator.structure meta_collection_iterator structure;
    let oc = open_out (dir ^ Filename.dir_sep ^ "meta.json") in
    Yojson.Safe.pretty_to_channel oc (meta_data_to_yojson !out_meta);
    close_out oc
    
end

module Prelude = struct
  let (out : structure ref) = ref []

  let mapper = general_mapper "prelude" out
  let run tree =
    mapper.structure mapper tree

  let stripper = general_stripper "prelude" out
  let strip tree =
    stripper.structure stripper tree

  let out_file dir =
    let oc = open_out (dir ^ Filename.dir_sep ^ "prelude.ml") in
    let ppf = Format.formatter_of_out_channel oc in
    Format.fprintf ppf "@[%a@]@." Pprintast.structure !out;
    close_out oc

end

module Prepare = struct
  let (out : structure ref) = ref []

  let mapper = general_mapper "prepare" out
  let run tree =
    mapper.structure mapper tree

  let stripper = general_stripper "prepare" out
  let strip tree =
    stripper.structure stripper tree

  let out_file dir =
    let oc = open_out (dir ^ Filename.dir_sep ^ "prepare.ml") in
    let ppf = Format.formatter_of_out_channel oc in
    Format.fprintf ppf "@[%a@]@." Pprintast.structure !out;
    close_out oc

end

let run tree = Prepare.run @@ Prelude.run @@ Meta.run tree
                                                      
(* Strips the AST of all the nodes that are contained in the preliminaries *)
(* This is so the template and solution mappers work only on the functions in the exercise *)
let strip tree = Prepare.strip @@ Prelude.strip @@ Meta.strip tree 
