let spawn_command ?(path = Constant.tezos_codec) arguments =
  Process.spawn path arguments

let spawn_encode ?path ~name json =
  spawn_command ?path ["encode"; name; "from"; JSON.encode_u json]

let encode ?path ~name json =
  spawn_encode ?path ~name json |> Process.check_and_read_stdout

let spawn_decode ?path ~name binary =
  spawn_command ?path ["decode"; name; "from"; binary]

let decode ?path ~name binary =
  let* json =
    spawn_decode ?path ~name binary |> Process.check_and_read_stdout
  in
  return (JSON.parse ~origin:("tezos-codec encode " ^ name) json)

let spawn_dump_encodings ?path () = spawn_command ?path ["dump"; "encodings"]

let dump_encodings ?path () =
  let* json = spawn_dump_encodings ?path () |> Process.check_and_read_stdout in
  return (JSON.parse ~origin:"tezos-codec dump encodings" json)
