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
  * "1" or "2" ("2" is default)
* kind : string option
  * Whether the exercise is an "exercise", "problem", or "project" 
* stars : int option
  * Represents difficulty from 1 (Easiest) to 5 (Hardest)
* title : string option;
  * The title of the exercise
* identifier : string;
  * A unique identifier to differentiate the exercise from others
* authors : (string * string) list option;
  * The name and email address of the authors of the exercise
* focus : string list option;
  * Skills used for the exercise
* requirements : string list option;
  * Skills that should be known to complete the exercise
* forward_exercises : string list option;
  * Exercises to be done upon successful completion of this exercise
* backward_exercises : string list option;
  * Exercise that should be done if the student is having difficulties with this exercise
* max_score : int option
  * The maximum score available for the exercise

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

## Examples of User Tweaks

### `Function_sampler` exception
Attempting to generate a sampler for a function type will raise a `Function_sampler` exception, which also contains information on the kinds of functions it expects. Let's take `map : ('a -> 'b) -> 'a list -> 'b list` for example (but has been instantiated to `map : (string -> bool) -> string list -> bool list`)

```OCaml
let exercise_map =
  Section
    ([Text "Function:"; Code "map"],
      (test_function_2_against_solution
         [%ty : (string -> bool) -> string list -> bool list] "map" ~gen:10
         ~sampler:(fun ()  ->
                     (((fun ()  ->
                          raise
                            (Function_sampler
                               "Please provide functions of type string -> bool"))
                         ()), (sample_list (sample_string ) ()))) []))
```
We must replace the sampler argument with a sampler for the appropriate functions, which we must write ourselves, so now we need a list of functions which we can use for our test case. In the following example, I've only written one, which takes a string and compares its length to zero, but it demonstrates the general idea of what one should do
```OCaml
 let exercise_map =
  Section
    ([Text "Function:"; Code "map"],
      (test_function_2_against_solution
         [%ty : (string -> bool) -> string list -> bool list] "map" ~gen:10
         ~sampler:
         (fun () -> ((sample_alternatively 
                      [(fun () -> (fun x -> String.length x = 0))]) () , 
                     (sample_list sample_string ())))
         []))
```
Samplers are of type `unit -> 'a`, so we must wrap the function in another function which takes unit, and then to allow for multiple functions to be used for testing, we pass it in a list to `sample_alternatively`, which will randomly choose one of the functions for testing.

### A Potential Error: `'a rose_tree`
A rose tree is a tree in which has an unbounded number of branches per node, in this case, defined by a list
```OCaml
type 'a rose_tree = Tree of 'a * 'a rose_tree list
```
As you can see, this is a recursive type, however, it doesn't contain a terminal constructor (one which doesn't refer back to itself), so we run into problems with automatic generation of a sampler for types like this.
```OCaml
let rec sample_rose_tree sample_a ?(size= 10)  () =
  let cnstr_Tree () =
    Tree ((sample_a ()), ((sample_list (sample_rose_tree sample_a)) ()))  in
  if size = 0
  then (sample_alternatively []) ()
  else (sample_alternatively [cnstr_Tree]) () 
```
In the if statement, when size is zero, there's list passed to `sample_alternatively` is empty, which may pose a problem, but we can simply write the terminal case ourselves, a rose tree with an empty list.
```OCaml
let rec sample_rose_tree sample_a ?(size= 10)  () =
  let cnstr_Tree () =
    Tree ((sample_a ()), ((sample_list (sample_rose_tree sample_a)) ()))  in
  if size = 0
  then (sample_alternatively [(fun () -> Tree (sample_a (), []))]) ()
  else (sample_alternatively [cnstr_Tree]) () 
```
