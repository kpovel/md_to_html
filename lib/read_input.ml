open Base

let read_input path =
  let ch = Stdio.In_channel.create path in
  let len = Int64.to_int (Stdio.In_channel.length ch) in
  match len with
  | Some len ->
      let s = Stdlib.really_input_string ch len in
      Stdio.In_channel.close ch;
      Ok s
  | None -> Error "Cannot convert int64 to int"
