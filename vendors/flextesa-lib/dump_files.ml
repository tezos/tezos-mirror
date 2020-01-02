open Internal_pervasives
module IFmt = More_fmt

type t = {mutable trees: (string * string * (string * string) list) list}

let make () = {trees= []}

let write state ~name ~path files =
  let t = state#dump_files in
  Running_processes.run_successful_cmdf state "mkdir -p %s"
    (Caml.Filename.quote path)
  >>= fun _ ->
  List_sequential.iter files ~f:(fun (p, content) ->
      System.write_file state (path // p) ~content)
  >>= fun () ->
  (* Dbg.e EF.(wf "Adding %s" name) ; *)
  t.trees <- (name, path, files) :: t.trees ;
  return ()

let pp ppf t =
  (* Dbg.e EF.(wf "pp -ing %d" (List.length t.trees)) ; *)
  IFmt.(
    vertical_box ~indent:2 ppf (fun ppf ->
        List.iter t.trees ~f:(fun (msg, path, files) ->
            cut ppf () ;
            pf ppf "%s setup at `%s`:" msg path ;
            List.iter files ~f:(fun (p, c) ->
                cut ppf () ;
                pf ppf "* `./%s` → `%s`" p c))))
