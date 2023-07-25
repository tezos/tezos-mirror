let is_ocaml_5 =
  let major_version = List.hd @@ String.split_on_char '.' Sys.ocaml_version in
  let major_version = int_of_string major_version in
  major_version = 5

let file_mapping =
  let t = Hashtbl.create 17 in
  let rec loop path =
    List.iter
      (fun name ->
        let full = Filename.concat path name in
        if Sys.is_directory full then loop full else Hashtbl.add t name full)
      (Array.to_list (Sys.readdir path))
  in
  let test_vectors = ["test_vectors"; "test/test_vectors"] in
  List.iter (fun f -> if Sys.file_exists f then loop f) test_vectors ;
  t

let open_file filename =
  let name =
    if Sys.file_exists filename then filename
    else
      try Hashtbl.find file_mapping filename
      with _ ->
        failwith
          (Printf.sprintf
             "Cannot open %S, the file doesn't exists in test_vectors"
             filename)
  in
  let ic = open_in_bin name in
  ic

let read_file filename =
  let lines = ref [] in
  let chan = open_file filename in
  try
    while true do
      lines := input_line chan :: !lines
    done ;
    !lines
  with End_of_file ->
    close_in chan ;
    List.rev !lines

let generate_random_byte () = char_of_int (Random.int 256)

let generate_random_bytes size =
  Bytes.init size (fun _ -> generate_random_byte ())

let rec repeat n f () =
  if n > 0 then (
    f () ;
    repeat (n - 1) f ())

let project_root =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some x -> x
  | None -> (
      match Sys.getenv_opt "PWD" with
      | Some x -> x
      | None ->
          (* For some reason, under [dune runtest], [PWD] and
             [getcwd] have different values. [getcwd] is in
             [_build/default], and [PWD] is where [dune runtest] was
             executed, which is closer to what we want. *)
          Sys.getcwd ())

let path_test_vectors name =
  project_root // Filename.dirname __FILE__ // "test_vectors" // name
