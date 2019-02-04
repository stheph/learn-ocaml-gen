# learn-ocaml-gen

A better readme in progress

A project to generate all files necessary for a learn-ocaml exercise

Using Ocaml 4.05, with the following packages from OPAM:

dune
ocaml-compiler-libs
ppx_deriving
ppx_deriving_yojson
ppx_lib
yojson

In the project folder, run
  dune exec --profile release ./main.exe <path-to-file>
to generate an exercise folder for the annotated solution file located at <path-to-file>

