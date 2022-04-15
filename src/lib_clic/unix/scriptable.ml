open Error_monad

type output_format = Rows of {separator : string; escape : [`No | `OCaml]}

let rows separator escape = Rows {separator; escape}

let tsv = rows "\t" `No

let csv = rows "," `OCaml

let clic_arg () =
  let open Clic in
  arg
    ~doc:"Make the output script-friendly. Possible values are 'TSV' and 'CSV'."
    ~long:"for-script"
    ~placeholder:"FORMAT"
    (parameter (fun _ spec ->
         let open Lwt_result_syntax in
         match String.lowercase_ascii spec with
         | "tsv" -> return tsv
         | "csv" -> return csv
         | other ->
             failwith
               "Cannot recognize format %S, please try 'TSV' or 'CSV'"
               other))

let fprintf_lwt chan fmt =
  let open Lwt_syntax in
  Format.kasprintf
    (fun s ->
      protect (fun () ->
          let* () = Lwt_io.write chan s in
          return_ok_unit))
    fmt

let output ?(channel = Lwt_io.stdout) how_option ~for_human ~for_script =
  let open Lwt_result_syntax in
  match how_option with
  | None -> for_human ()
  | Some (Rows {separator; escape}) ->
      let open Format in
      let* () =
        List.iter_es
          (fun row ->
            fprintf_lwt
              channel
              "%a@."
              (pp_print_list
                 ~pp_sep:(fun fmt () -> pp_print_string fmt separator)
                 (fun fmt cell ->
                   match escape with
                   | `OCaml -> fprintf fmt "%S" cell
                   | `No -> pp_print_string fmt cell))
              row)
          (for_script ())
      in
      protect (fun () ->
          let*! () = Lwt_io.flush channel in
          return_unit)

let output_for_human how_option for_human =
  output how_option ~for_human ~for_script:(fun () -> [])

let output_row ?channel how_option ~for_human ~for_script =
  output ?channel how_option ~for_human ~for_script:(fun () -> [for_script ()])
