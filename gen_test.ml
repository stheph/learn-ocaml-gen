(* open Ast_iterator *)
(* open Ast_mapper *)
open Asttypes
open Parsetree
open Longident
open Types

exception Not_implemented
exception Not_a_Tconstr_in_get_samplers
            
let loc = Location.none
              
(* Let's start with some  *)
let (output_file : structure ref) = ref []

(* We need these in test.ml as per learn-ocaml instructions *)
let opens =
  [
    [%stri open Test_lib] ;
    [%stri open Report]
  ]

let exns =
  [
    [%stri exception No_choice]
  ]

(* Need to include this function to randomly select an element in a list *)
(* init the random number generator first *)
let choose_fn =
  [
    [%stri let () = Random.self_init () ];
    [%stri
     let choose l =
       if List.length l = 0
       then raise No_choice
       else
         let
           choice = Random.int (List.length l)
         in
         List.nth l choice
    ];
  ]

(* let rec args_to_samplers ( {argtypes = argtypes ; _} : Typediter.function_info) =
 *   (\* Convert all the argtypes to strings *\)
 *   (\* Make a map between type_expr ids and type strings *\)
 *   (\* Gonna take a shortcut and just use what OCaml has for us in Printtyp *\)
 *   let base_types = [ "int" ; "float" ; "bool" ; "char" ; "string" ] in
 *   let id_map =
 *     ref (List.map
 *       (fun x -> (x.id, (Format.asprintf) "%a" Typediter.print_type x))
 *       argtypes) in
 *   let type_maps = List.map (fun x -> (x.id, x)) argtypes in
 *   let has_type_vars type_expr =
 *     begin match type_expr with
 *     | { desc = Tconstr (_, ts, _) ; _} ->
 *        not @@ ((List.length ts) = 0)
 *     | _ -> false
 *     end
 *   in
 *   let get_type_vars type_expr =
 *     begin match type_expr with
 *     | { desc = Tconstr (_, ts, _) ; _ } ->
 *        List.map get_type_vars' ts
 *     | _ -> raise Not_implemented
 *     end
 *   and get_type_vars' type_expr =
 *     begin match type_expr with
 *     | { id = id ; _ } -> id
 *     | _ -> raise Not_implemented
 *     end
 *   in
 *   let type_maps_vars = List.filter (fun (_, x) -> has_type_vars x) type_maps in
 *   let type_maps_no_vars = List.filter (fun (_, x) -> not @@ has_type_vars x) type_maps in
 *   let create_instances (id, _) =
 *     List.map (fun x -> [(id, x)]) base_types
 *   in
 *   let instances = List.map create_instances type_maps_vars in 
 *   let instances = Make_sampler.cross_all instances in
 *     
 *   List.iter (fun (_, y) -> print_string @@ y ^ "\n") !id_map;
 *   () *)

(* and args_to_samplers_vars
 * and args_to_samplers_no_vars *)

(* let put k v m =
 *   if not @@ List.mem_assoc k !m then
 *     m := (k,v) :: !m
 *     
 * let rec get_samplers ({ argtypes = argtypes } : Typediter.function_info) =
 *   let id_map = List.map (fun x -> (x.id, x.desc)) argtypes in
 *   begin match id_map with
 *   | [] -> []
 *   | hd::tl ->
 *      begin match hd with
 *      | (id, Tconstr (p, ts, _)) ->
 *         List.iter (fun x -> print_string @@ x ^ "\n") (string_of_type_expr_list ts); []
 *      | _ -> raise Not_a_Tconstr_in_get_samplers
 *      end
 *   end
 *     
 * and string_of_type_expr expr =
 *   begin match expr with
 *   | Tvar _ ->  [ "int" ; "float" ; "bool" ; "char" ; "string" ]
 *   | Tconstr (p, ts, _) ->
 *      let path_string  = Path.name p in
 *      begin match ts with
 *      | [] -> [[path_string]]
 *      | x -> [string_of_type_expr_list x]
 *       end
 *   end
 *     
 * and string_of_type_expr_list ts =
 *   begin match ts with
 *   | [] -> []
 *   | hd::tl ->
 *      let hd' = string_of_type_expr hd.desc in
 *      let tl' = string_of_type_expr_list tl in
 *      List.map (fun x -> List.map (fun y -> x ^ "_" ^ y) hd') tl'
 *   end *)
  
(* and get_samplers_without_vars path =
 *   let typ = Path.name path in
 *   [ "sampler_" ^ typ ] *)

let rec string_of type_expr str =
  begin match type_expr.desc with
  | Tvar _ -> str
  | Tconstr (p, ts, _) ->
     let last_one = Path.name p in
     
     
  end

let rec merge_string l str =
  begin match l with
  | [] ->
  | hd::tl ->
     [hd ]
    
let rec get_sampler argtypes =
  
    
let test parse_tree typed_tree =
  output_file := !output_file @ opens;
  output_file := !output_file @ exns;
  output_file := !output_file @ choose_fn;
  output_file := !output_file @ [Make_sampler.make parse_tree];
  Format.fprintf (Format.std_formatter) "@[%a@]@." (Pprintast.structure) !output_file;
  let fn_map = Typediter.iter typed_tree in
  (* Typediter.FnMap.iter (fun x y -> args_to_samplers y) fn_map *)
  ()
