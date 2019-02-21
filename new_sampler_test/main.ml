open Parsetree
open Ast_iterator

(* We collect all the type declarations here *)
let (type_decls : type_declaration list ref) = ref []

let type_declaration_iterator _iterator type_decl =
  type_decls := !type_decls @ [type_decl]

let type_decl_iterator =
  {
    default_iterator with
    type_declaration = type_declaration_iterator
  }

let () =
  try
    let src_file = "examples.ml" in
    let parse_tree = Pparse.parse_implementation
                       Format.std_formatter
                       ~tool_name:"dune"
                       src_file
    in
    type_decl_iterator.structure type_decl_iterator parse_tree;
    let oc = open_out ("output.ml") in
    let ppf = Format.formatter_of_out_channel oc in
    Format.fprintf ppf "@[%a@]@." Pprintast.structure [Sampler.Untyped.generate_samplers !type_decls];
    close_out oc
  with Syntaxerr.Error _ as err ->
    Location.report_exception (Format.err_formatter) err
