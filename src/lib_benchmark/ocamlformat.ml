open Unix

exception Invalid_process_status of process_status

let run options s =
  let ic, oc =
    Unix.open_process_args
      "ocamlformat"
      (Array.of_list ("ocamlformat" :: "-" :: options))
  in
  try
    Fun.protect
      (fun () ->
        output_string oc s ;
        flush oc ;
        close_out oc ;
        let buf = Buffer.create 1024 in
        let bytes = Bytes.create 1024 in
        let rec loop () =
          match input ic bytes 0 1024 with
          | 0 -> Buffer.contents buf
          | n ->
              Buffer.add_subbytes buf bytes 0 n ;
              loop ()
        in
        Ok (loop ()))
      ~finally:(fun () ->
        match Unix.close_process (ic, oc) with
        | Unix.WEXITED 0 -> ()
        | e -> raise (Invalid_process_status e))
  with exn -> Error exn

let impl = run ["--impl"]

let intf = run ["--intf"]
