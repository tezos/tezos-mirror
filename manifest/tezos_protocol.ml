(* This module decodes TEZOS_PROTOCOL files. *)

type t = {hash : string; modules : string list; expected_env_version : int}

let of_string_exn ?filename s =
  let die msg =
    let error_prefix =
      match filename with
      | None -> "Error"
      | Some fn -> Printf.sprintf "Error in %s" fn
    in
    failwith (Printf.sprintf "%s: %s" error_prefix msg)
  in
  let string = function `String x -> x | _ -> die "Expected string" in
  let float = function `Float x -> x | #JSON_AST.t -> die "Expected float" in
  let listed f = function
    | `A x -> List.map f x
    | #JSON_AST.t -> die "Expected list"
  in
  let optional f t = Option.map f t in
  let required f t =
    match optional f t with Some x -> x | None -> die "Required"
  in
  match JSON_parser.json JSON_lexer.token (Lexing.from_string s) with
  | `O assoc ->
      let lookup key f = f (List.assoc_opt key assoc) in
      let expected_env_version =
        lookup "expected_env_version" (optional float)
        |> Option.map Int.of_float |> Option.value ~default:0
      in
      let hash = lookup "hash" (required string) in
      let modules = lookup "modules" (required (listed string)) in
      {hash; modules; expected_env_version}
  | #JSON_AST.t -> die "Expected object"

let of_file_exn filename =
  let contents =
    let ch = open_in filename in
    let buffer = Buffer.create 512 in
    Fun.protect
      ~finally:(fun () -> close_in ch)
      (fun () ->
        let bytes = Bytes.create 512 in
        let rec loop () =
          let len = input ch bytes 0 512 in
          if len > 0 then (
            Buffer.add_subbytes buffer bytes 0 len ;
            loop ())
        in
        loop ()) ;
    Buffer.contents buffer
  in
  of_string_exn ~filename contents
