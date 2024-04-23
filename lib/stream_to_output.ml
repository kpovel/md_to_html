open Base
open Tokenizer

let stream_to_html stream =
  let html_stream =
    List.map stream ~f:(fun token ->
        match token with
        | H1 str -> Stdlib.Format.sprintf "<h1>%s</h1>" str
        | H2 str -> Stdlib.Format.sprintf "<h2>%s</h2>" str
        | H3 str -> Stdlib.Format.sprintf "<h3>%s</h3>" str
        | H4 str -> Stdlib.Format.sprintf "<h4>%s</h4>" str
        | H5 str -> Stdlib.Format.sprintf "<h5>%s</h5>" str
        | H6 str -> Stdlib.Format.sprintf "<h6>%s</h6>" str
        | P str -> Stdlib.Format.sprintf "<p>%s</p>" str
        | Quote str -> Stdlib.Format.sprintf "<blockquote>%s</blockquote>" str
        | NumberedList (_pos, str) -> Stdlib.Format.sprintf "<li>%s</li>" str
        | UnorderedList str -> Stdlib.Format.sprintf "<li>%s</li>" str
        | Br -> "<br />"
        | Separator -> "<hr />")
  in
  String.concat ~sep:"\n" html_stream
