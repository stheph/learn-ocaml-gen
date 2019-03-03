let () =
  try
    let _ = Compmisc.init_path false in
    let env = Compmisc.initial_env () in

    let src_file = Array.get Sys.argv 1 in
    let dot_index = String.rindex src_file '.' in
    let dir = String.sub src_file 0 dot_index in
    let parse_tree =
      Pparse.parse_implementation
        Format.std_formatter
        ~tool_name:"dune"
        src_file
    in
    let typing_tree = Preliminaries.strip parse_tree in
    let typing_tree = Exercise.strip typing_tree in
    let (typed_tree,_,_) =
      Typemod.type_structure env typing_tree Location.none
    in
    let _ =
      if not (Sys.file_exists dir) then
        Unix.mkdir dir 0o777
    in
    let oc = open_out (dir ^ Filename.dir_sep ^ "descr.html") in
    let _ = close_out oc in
    let prelim_tree = Preliminaries.run parse_tree in
    Preliminaries.Meta.out_file dir;
    Preliminaries.Prelude.out_file dir;
    Preliminaries.Prepare.out_file dir;
    Exercise.Template.out_file prelim_tree dir;
    Exercise.Solution.out_file prelim_tree dir;
    Test.out_file typing_tree typed_tree dir;
  with _ as exc ->
      Location.report_exception (Format.err_formatter) exc
