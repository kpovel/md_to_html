let write_output ~path:path ~html:html =
  let oc = open_out path in
  Stdlib.Printf.fprintf oc "%s\n" html;
  close_out oc
