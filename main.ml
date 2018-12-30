(* Main function, of course *)

module TestIter = TypedtreeIter.MakeIterator(TypedtreeIter.DefaultIteratorArgument)

let () =
  let _ = Compmisc.init_path false in
  let env = Compmisc.initial_env () in
  
  let src_file = Array.get Sys.argv 1 in
  (* src_file -> parse_tree : File -> AST *)
  let parse_tree =
    Pparse.parse_implementation
      Format.std_formatter
      ~tool_name:"ocamlbuild"
      src_file
  in
  (* parse_tree -> typed_tree : AST -> Typed AST *)
  let parse_tree = Gen_background.background parse_tree in
  let _temp_tree = Gen_template.template parse_tree in
  let _sol_tree = Gen_solution.solution parse_tree in

  (* We do the transformations on the original tree to strip extensions *)
  (* but we leave the background info for typing *)
  let parse_tree_clean = Gen_background.leave_background parse_tree in
  let typing_tree = Gen_solution.solution parse_tree_clean in
  let (typed_tree,_,_) = Typemod.type_structure env typing_tree Location.none in
  (* Format.fprintf (Format.std_formatter) "@[%a@]@." (Pprintast.structure) temp_tree;
   * Format.fprintf (Format.std_formatter) "@[%a@]@." (Pprintast.structure) sol_tree; *)
  Gen_test.test parse_tree_clean typed_tree;
  ()
