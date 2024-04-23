open Base
open Md_to_html

(* 1. get input file path from env args *)
(* 2. read input file *)
(* 3. parse a file *)
(* 4. write output to a file *)

type pathes = { input_path : string; _output_path : string }

let get_pathes args =
  if Array.length args < 3 then Error "Not enough arguments"
  else Ok { input_path = Array.get args 1; _output_path = Array.get args 2 }

let () =
  let args = Sys.get_argv () in
  let pathes = get_pathes args in
  let pathes =
    match pathes with
    | Ok v -> v
    | Error e ->
        Stdlib.Format.eprintf "%s\n" e;
        Stdlib.exit 1
  in
  let file = Read_input.read_input pathes.input_path in
  let file =
    match file with
    | Ok f -> f
    | Error e ->
        Stdlib.Format.eprintf "%s\n" e;
        Stdlib.exit 1
  in

  let _stream = Tokenizer.input_to_stream file in
  (* List.iter ~f:(fun x -> Stdlib.Format.printf "%s, " x) stream*)
  ()
