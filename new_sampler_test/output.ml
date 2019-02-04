let rec sample_t () = sample_list sample_int ()

and sample_s sample_a sample_b () = ((sample_'a ()), (sample_'b ()))

and sample_u () = sample_s sample_int sample_float ()

and sample_v () = sample_float ()

