let spawn_command ?(path = Constant.tezos_codec) ?hooks arguments =
  Process.spawn path ?hooks arguments

let spawn_encode ?path ?hooks ~name json =
  spawn_command ?path ?hooks ["encode"; name; "from"; JSON.encode_u json]

let encode ?path ?hooks ~name json =
  let open Lwt.Infix in
  spawn_encode ?path ?hooks ~name json
  |> Process.check_and_read_stdout >|= String.trim

let spawn_decode ?path ?hooks ~name binary =
  spawn_command ?path ?hooks ["decode"; name; "from"; binary]

let decode ?path ?hooks ~name binary =
  let* json =
    String.trim binary
    |> spawn_decode ?path ?hooks ~name
    |> Process.check_and_read_stdout
  in
  return (JSON.parse ~origin:("tezos-codec encode " ^ name) json)

let spawn_dump_encodings ?path () = spawn_command ?path ["dump"; "encodings"]

let dump_encodings ?path () =
  let* json = spawn_dump_encodings ?path () |> Process.check_and_read_stdout in
  return (JSON.parse ~origin:"tezos-codec dump encodings" json)
