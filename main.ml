let () =
  let _ = Compmisc.init_path false in
  let env = Compmisc.initial_env () in

  let src_file = Array.get Sys.argv 1 in
  let dot_index = String.rindex src_file '.' in
  let dir = String.sub src_file 0 dot_index in
  try
    let parse_tree =
      Pparse.parse_implementation
        Format.std_formatter
        ~tool_name:"dune"
        src_file
    in
    let typing_tree = Preliminaries.strip parse_tree in
    let typing_tree = Exercise.strip typing_tree in
  (* Format.fprintf (Format.std_formatter) "@[%a@]@." Pprintast.structure typing_tree; *)
    let (typed_tree,_,_) =
      Typemod.type_structure env typing_tree Location.none
    in
    let _ =
      if not (Sys.file_exists dir) then
        Unix.mkdir dir 0o777
    in
    let _ =
      if not (Sys.file_exists @@ dir ^ "/pdf") then
        Unix.mkdir (dir ^ "/pdf") 0o777
    in
    let descr_string = "<object data=\"/static/" ^ dir ^ "/pdf/descr.pdf\" width=\"100%\"></object>" in
    let oc = open_out (dir ^ Filename.dir_sep ^ "descr.html") in
    let ppf = Format.formatter_of_out_channel oc in
    let _ = Format.fprintf ppf "@[%s@]@." (descr_string) in
    let _ = close_out oc in
    (* <object data="resume.pdf" type="application/pdf" width="100%" height="800px"> 
     *)
    (* let pre_ex_tree = Preliminaries.run parse_tree in *)
    (* * let pre_ex_tree = Preliminaries.Prelude.run parse_tree in *\) *)
    (* Format.fprintf (Format.std_formatter) "@[%a@]@." Pprintast.structure pre_ex_tree; *)
    let prelim_tree = Preliminaries.run parse_tree in
    (* Format.fprintf (Format.std_formatter) "@[%a@]@." Pprintast.structure prelim_tree; *)
    Preliminaries.Meta.out_file dir;
    Preliminaries.Prelude.out_file dir;
    Preliminaries.Prepare.out_file dir;
    Exercise.Template.out_file prelim_tree dir;
    Exercise.Solution.out_file prelim_tree dir;
    Test.out_file typing_tree typed_tree dir;
  with _ as exc ->
      Location.report_exception (Format.err_formatter) exc
