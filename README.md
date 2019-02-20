# learn-ocaml-gen

A better readme in progress

A project to generate all files necessary for a learn-ocaml exercise

Using Ocaml 4.05, with the following packages from OPAM:

* dune
* ocaml-compiler-libs
* ppx_deriving
* ppx_deriving_yojson
* ppx_lib
* yojson

In the project folder, run
`dune exec --profile release ./main.exe <path-to-file>`
to generate an exercise folder for the annotated solution file located at `<path-to-file>`

# Usage

File generation is based on annotations made via ppx extensions.

### `meta.json`
Place a declaration of the form `let%meta <id> = ...` in order to set values in the metadata file. The following identifiers are allowed:

* learnocaml_version : string
** "1" or "2" ("2" is default)
* kind : string option
** Whether the exercise is an "exercise", "problem", or "project" 
* stars : int option
** Represents difficulty from 1 (Easiest) to 5 (Hardest)
* title : string option;
** The title of the exercise
* identifier : string;
** A unique identifier to differentiate the exercise from others
* authors : (string * string) list option;
** The name and email address of the authors of the exercise
* focus : string list option;
** Skills used for the exercise
* requirements : string list option;
** Skills that should be known to complete the exercise
* forward_exercises : string list option;
** Exercises to be done upon successful completion of this exercise
* backward_exercises : string list option;
** Exercise that should be done if the student is having difficulties with this exercise
* max_score : int option
** The maximum score available for the exercise

A complete description of the `meta.json` file can be found in the [learn-ocaml repository](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/exercises_format.md)

Example:
```OCaml
let%meta kind = "Exercise"
let%meta stars = 5
let%meta authors = ["A. Person","your@email.here"]
```

Will set the `kind` field to "Exercise", `stars` to 5, and `authors` to a list with "A. Person" with email "your@email.here" 
as the only element

### `prelude.ml` and `prepare.ml`
Place a declaration of the form `let%prelude ... = ...` and `type%prelude ... = ...` or `let%prepare ... = ...` and `type%prepare ... = ...` to place the declaration in `prelude.ml` or `prepare.ml` respectively.

Example:
```OCaml
type%prelude 'a list = Nil | Cons of 'a * 'a list
let%prelude rec length l =
  begin match l with
  | [] -> 0
  | Cons (hd, tl) -> 1 + length tl
  end
```
Will add the declaration of type `'a list` and function `length` to `prelude.ml`

### `template.ml` and `solution.ml`
Use the extension `%erase` in a function body to mark it for inclusion in `template.ml` and `solution.ml`

Example:
```OCaml
let rec map f l =
  [%erase
    begin match l with
    | [] -> []
    | hd :: tl -> (f hd) :: (map f tl)
    end
  ]
```
Will add
```OCaml
let rec map f l = raise Not_implemented
```
to `template.ml`
and 
```OCaml
let rec map f l =
  begin match l with
  | [] -> []
  | hd :: tl -> (f hd) :: (map f tl)
  end
```
to `solution.ml`
