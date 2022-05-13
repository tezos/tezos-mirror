(* This module decodes TEZOS_PROTOCOL files. *)

type t = {
  hash : string;
  modules : string list;
  expected_env_version : int option;
}

let of_string_exn ?filename s =
  let filename_prefix =
    match filename with None -> "" | Some fn -> Printf.sprintf "%s: " fn
  in
  let string = function `String x -> Ok x | _ -> Error ["Expected string"] in
  let float = function `Float x -> Ok x | _ -> Error ["Expected float"] in
  let all_result l =
    List.fold_right
      (fun x acc ->
        match (x, acc) with
        | (Error x, Error xs) -> Error (x @ xs)
        | (Ok _, Error xs) -> Error xs
        | (Error x, Ok _) -> Error x
        | (Ok x, Ok xs) -> Ok (x :: xs))
      l
      (Ok [])
  in
  let listed f = function
    | `A x -> List.map f x |> all_result
    | _ -> Error ["Expected list"]
  in
  let optional f = function
    | None -> Ok None
    | Some x -> f x |> Result.map (fun x -> Some x)
  in
  let required f = function None -> Error ["Required"] | Some x -> f x in
  match JSON_parser.json JSON_lexer.token (Lexing.from_string s) with
  | `O assoc ->
      let lookup key type_ =
        assoc
        |> List.find_map (fun (k, v) ->
               if String.equal k key then Some v else None)
        |> type_
        |> function
        | Ok x -> x
        | Error es ->
            failwith
              (Printf.sprintf
                 "%sError(s) on %s:\n%s"
                 filename_prefix
                 key
                 (String.concat "\n" (List.map (fun x -> " - " ^ x) es)))
      in

      let hash = lookup "hash" (required string) in
      let modules = lookup "modules" (required (listed string)) in
      let expected_env_version =
        lookup "expected_env_version" (optional float)
        |> Option.map Float.to_int
      in
      {hash; modules; expected_env_version}
  | _ -> failwith (Printf.sprintf "%sExpected object" filename_prefix)

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
