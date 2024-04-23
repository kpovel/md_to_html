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
  | NumberedList of int * string
  | UnorderedList of string
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

let input_to_stream input =
  let splited = String.split input ~on:'\n' in

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
      | "-" :: l -> UnorderedList (String.concat ~sep:" " l)
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
            | Some n -> NumberedList (n, String.concat ~sep:" " rest)
            | None -> P (String.concat ~sep:" " all))
      | l -> P (String.concat ~sep:" " l))

(* let stream_to_html _stream = ()*)
