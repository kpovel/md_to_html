open Base

type token =
  | H1 of string
  | H2 of string
  | H3 of string
  | H4 of string
  | H5 of string
  | H6 of string
  | P of string
  | Quote of string
  | NumberedList of string list
  | UnorderedList of string list
  | Br
  | Separator

type task = SolvedTask of string | UnsolvedTask of string

(* Link *)
(* Bold *)
(* Italic *)
(* Strikethrough *)
(* Quoting code *)
(* Table *)
(* Code *)
(* Task *)
(* Nested Lists *)

let print_token_list tokens =
  List.iter tokens ~f:(fun x ->
      match x with
      | H1 str -> Stdlib.Format.printf "H1: %s\n" str
      | H2 str -> Stdlib.Format.printf "H2: %s\n" str
      | H3 str -> Stdlib.Format.printf "H3: %s\n" str
      | H4 str -> Stdlib.Format.printf "H4: %s\n" str
      | H5 str -> Stdlib.Format.printf "H5: %s\n" str
      | H6 str -> Stdlib.Format.printf "H6: %s\n" str
      | P str -> Stdlib.Format.printf "P: %s\n" str
      | Quote str -> Stdlib.Format.printf "Quote: %s\n" str
      | NumberedList list ->
          Stdlib.Format.printf "Ul: ";
          List.iter list ~f:(fun x -> Stdlib.Format.printf "Li: %s," x);
          Stdlib.Format.printf "\n"
      | UnorderedList list ->
          Stdlib.Format.printf "Ol: ";
          List.iter list ~f:(fun x -> Stdlib.Format.printf "Li: %s," x);
          Stdlib.Format.printf "\n"
      | Br -> Stdlib.Format.printf "Br\n"
      | Separator -> Stdlib.Format.printf "Separator\n")

let input_to_stream input =
  let splited = String.split input ~on:'\n' in

  let tokens =
    List.map splited ~f:(fun line ->
        let trimmed_line = Stdlib.String.trim line in
        let split_line = String.split trimmed_line ~on:' ' in
        match split_line with
        | "#" :: l -> H1 (String.concat ~sep:" " l)
        | "##" :: l -> H2 (String.concat ~sep:" " l)
        | "###" :: l -> H3 (String.concat ~sep:" " l)
        | "####" :: l -> H4 (String.concat ~sep:" " l)
        | "#####" :: l -> H5 (String.concat ~sep:" " l)
        | "######" :: l -> H6 (String.concat ~sep:" " l)
        | ">" :: l -> Quote (String.concat ~sep:" " l)
        | "-" :: l -> UnorderedList [ String.concat ~sep:" " l ]
        | "---" :: [] -> Separator
        | "" :: [] -> Br
        | prefix :: rest as all -> (
            let last_char =
              String.sub prefix ~pos:(String.length prefix - 1) ~len:1
            in
            if not (String.equal last_char ".") then
              P (String.concat ~sep:" " all)
            else
              let item =
                Int.of_string_opt
                  (String.sub prefix ~pos:0 ~len:(String.length prefix - 1))
              in
              match item with
              | Some _ -> NumberedList [ String.concat ~sep:" " rest ]
              | None -> P (String.concat ~sep:" " all))
        | l -> P (String.concat ~sep:" " l))
  in
  let unduplicated =
    List.fold tokens ~init:[] ~f:(fun acc x ->
        match acc with
        | prev :: acc -> (
            match (prev, x) with
            | UnorderedList prev, UnorderedList items ->
                UnorderedList (prev @ items) :: acc
            | NumberedList prev, NumberedList items ->
                NumberedList (prev @ items) :: acc
            | P prev, P items -> P (prev ^ " " ^ items) :: acc
            | prev, curr -> curr :: prev :: acc)
        | acc -> x :: acc)
  in

  print_token_list (List.rev unduplicated);

  List.rev unduplicated
