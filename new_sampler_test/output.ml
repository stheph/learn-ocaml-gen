let rec sample_s () = sample_int ()

and sample_t () = sample_list sample_int ()

and sample_u () = ((sample_int ()), (sample_int ()))

and sample_v () =
  raise (Function_sampler "Please provide functions of type int -> int")

and sample_w sample_a sample_b () =
  sample_list (fun ()  -> ((sample_a ()), (sample_b ()))) ()

and sample_tree sample_a () =
  let cnstr_Leaf = Leaf ((sample_a ()))  in
  let cnstr_Tree =
    Tree ((sample_tree sample_a ()), (sample_tree sample_a ()))  in
  (sample_alternatively [cnstr_Tree; cnstr_Leaf]) ()

