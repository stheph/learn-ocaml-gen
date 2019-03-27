(* Contains AST mappers for meta.json, prelude.ml, and prepare.ml *)
(* Also has an iterator which converts AST structures into json for meta.json *)
(* -- incredibly unsafely *)

open Ast_iterator
open Ast_mapper
open Asttypes
open Parsetree

exception Payload_multiple_structures of Location.t
exception Not_metadata of Location.t * string
exception Bad_value_for_metadata of Location.t * string
exception Metadata_conversion_failure

let loc = Location.none

let () =
  Printexc.register_printer
    (fun x ->
      begin match x with
      | Payload_multiple_structures loc ->
         Some (Format.asprintf "@.%aDid not expect multiple structures in extension payload" Location.print loc)
      | Not_metadata (loc, key) ->
         Some (Format.asprintf "@.%a\"%s\" is not a valid metadata key" Location.print loc key)
      | Bad_value_for_metadata (loc, key) ->
         Some (Format.asprintf "@.%aBad value for metadata key \"%s\"" Location.print loc key)
      | Metadata_conversion_failure ->
         Some (Format.asprintf "@.An unknown error has caused metadata conversion to fail")
      | _ -> None
      end
    )
            
let rec general_structure_mapper ext acc strip mapper structure =
  begin match structure with
  | [] -> []
  | hd::tl ->
     begin match hd with
     | { pstr_desc =
           Pstr_extension
             (({ txt = ext' ; _ }, PStr structure'), _) ; pstr_loc } when ext = ext' ->
        begin match structure' with
        | [] -> []
        | [x] ->
           if strip then
             begin
               default_mapper.structure_item mapper x :: general_structure_mapper ext acc strip mapper tl
             end
           else
             begin
                (* Preserve order *)
                acc := !acc @ [x];
                general_structure_mapper ext acc strip mapper tl
             end
        | l -> raise
                 (Payload_multiple_structures pstr_loc)
        end
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
      learnocaml_version : string;
      kind : string option;
      stars : int option;
      title : string option;
      identifier : string;
      authors : (string * string) list option;
      focus : string list option;
      requirements : string list option;
      forward_exercises : string list option;
      backward_exercises : string list option;
      max_score : int option
    } [@@deriving yojson]

  let default =
    {
      learnocaml_version = "2";
      kind = Some "exercise";
      stars = Some 3;
      title = Some "Exercise";
      identifier = "0";
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

  (* Printing out meta data as a json file: meta.json *)
  let (out_meta : meta_data ref) = ref default

  let value_binding_iterator iterator value_binding =
    begin match value_binding.pvb_pat with
    | { ppat_desc = Ppat_var { txt = "learnocaml_version" } ; ppat_loc ; _ } ->
       begin
         match value_binding.pvb_expr with
         | { pexp_desc = Pexp_constant (Pconst_integer (value, None)) ; _ } ->
            out_meta := {!out_meta with learnocaml_version = value }
         | _ -> raise (Bad_value_for_metadata (ppat_loc, "learnocaml_version"))
       end
    | { ppat_desc = Ppat_var { txt = "kind" } ; ppat_loc ; _ } ->
       begin
         match value_binding.pvb_expr with
         | { pexp_desc = Pexp_constant (Pconst_string (value, None)) ; _ } ->
            out_meta := {!out_meta with kind = Some value }
         | _ -> raise (Bad_value_for_metadata (ppat_loc, "kind"))
       end
    | { ppat_desc = Ppat_var { txt = "stars" } ; ppat_loc ; _ } ->
       begin
         match value_binding.pvb_expr with
         | { pexp_desc = Pexp_constant (Pconst_integer (value, None)) ; _ } ->
            out_meta := {!out_meta with stars = Some (int_of_string value) }
         | _ -> raise (Bad_value_for_metadata (ppat_loc, "stars"))
       end
    | { ppat_desc = Ppat_var { txt = "title" } ; ppat_loc ; _ } ->
       begin
         match value_binding.pvb_expr with
         | { pexp_desc = Pexp_constant (Pconst_string (value, None)) ; _ } ->
            out_meta := {!out_meta with title = Some value }
         | _ -> raise (Bad_value_for_metadata (ppat_loc, "title"))
       end
    | { ppat_desc = Ppat_var { txt = "identifier" } ; ppat_loc ; _ } ->
       begin
         match value_binding.pvb_expr with
         | { pexp_desc = Pexp_constant (Pconst_integer (value, None)) ; _ } ->
            out_meta := {!out_meta with identifier = value }
         | _ -> raise (Bad_value_for_metadata (ppat_loc, "identifier"))
       end
    (* | { ppat_desc = Ppat_var { txt = "author" } ; _ } ->
     *    begin
     *      try
     *        let expr = value_binding.pvb_expr in
     *        let tup = string_tuple_of_ast expr in
     *        out_meta := {!out_meta with author = Some tup }
     *                      (\* TODO Error handling that reports errors in authors list *\)
     *      with Match_failure _ -> raise (Bad_value_for_metadata "author")
     *    end *)
    | { ppat_desc = Ppat_var { txt = "authors" } ; ppat_loc ; _ } ->
       begin
         try
           let expr = value_binding.pvb_expr in
           let list = Utils.string_pair_list_of_ast expr in
           out_meta := {!out_meta with authors = Some list }
                         (* TODO Error handling that reports errors in authors list *)
         with Match_failure _ -> raise (Bad_value_for_metadata (ppat_loc, "authors"))
       end
    | { ppat_desc = Ppat_var { txt = "focus" } ; ppat_loc ; _ } ->
       begin
         try
           let expr = value_binding.pvb_expr in
           let list = Utils.string_list_of_ast expr in
           out_meta := {!out_meta with focus = Some list }
         with Match_failure _ -> raise (Bad_value_for_metadata (ppat_loc, "focus"))
       end
    | { ppat_desc = Ppat_var { txt = "requirements" } ; ppat_loc ; _ } ->
       begin
         try
           let expr = value_binding.pvb_expr in
           let list = Utils.string_list_of_ast expr in
           out_meta := {!out_meta with requirements = Some list }
         with Match_failure _ -> raise (Bad_value_for_metadata (ppat_loc, "requirements"))
       end
    | { ppat_desc = Ppat_var { txt = "foward_exercises" } ; ppat_loc ; _ } ->
       begin
         try
           let expr = value_binding.pvb_expr in
           let list = Utils.string_list_of_ast expr in
           out_meta := {!out_meta with forward_exercises = Some list }
         with Match_failure _ -> raise (Bad_value_for_metadata (ppat_loc, "forward_exercises"))
       end
    | { ppat_desc = Ppat_var { txt = "backward_exercises" } ; ppat_loc ; _ } ->
       begin
         try
           let expr = value_binding.pvb_expr in
           let list = Utils.string_list_of_ast expr in
           out_meta := {!out_meta with backward_exercises = Some list }
         with Match_failure _ -> raise (Bad_value_for_metadata (ppat_loc, "backward_exercises"))
       end
    | { ppat_desc = Ppat_var { txt = "max_score" } ; ppat_loc ; _ } ->
       begin
         match value_binding.pvb_expr with
         | { pexp_desc = Pexp_constant (Pconst_integer (value, None)) ; _ } ->
            out_meta := {!out_meta with max_score = Some (int_of_string value) }
         | _ -> raise (Bad_value_for_metadata (ppat_loc, "max_score"))
       end
    | { ppat_desc = Ppat_var { txt = label } ; ppat_loc ; _ } ->
       raise (Not_metadata (ppat_loc, label))
    | _ -> raise Metadata_conversion_failure
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
  let _ = out := ([%stri exception Not_implemented]) :: !out

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
