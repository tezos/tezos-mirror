(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]

let pp_json : json Crowbar.printer =
 fun fmt json -> Format.fprintf fmt "%s" (Data_encoding.Json.to_string json)

let ascii_letter =
  let open Crowbar in
  map [choose [range ~min:65 26; range ~min:97 26]] Char.chr

let names =
  let open Crowbar in
  map [list1 ascii_letter] (fun ls -> String.of_seq @@ List.to_seq ls)

let string =
  let open Crowbar in
  map [uint8; ascii_letter] String.make

let longstring =
  let open Crowbar in
  with_printer (fun fmt s ->
      if String.length s < 8 then Format.fprintf fmt "\"%s\"" s
      else Format.fprintf fmt "%c(%04d)" s.[0] (String.length s))
  @@ map [range 4096; ascii_letter] String.make

let ezjson : json Crowbar.gen =
  (* We don't generate random floats/strings because it doesn't roundtrip on
     even the standard printer. *)
  (* We generate long strings to test chunking. *)
  let open Crowbar in
  fix (fun json ->
      let field = map [names; json] (fun k v -> (k, v)) in
      choose
        [
          map
            [list field]
            (fun kvs ->
              let has_dup, _ =
                List.fold_left
                  (fun (has, seen) (k, _) ->
                    let has = has || List.exists (( = ) k) seen in
                    if has then (has, []) else (has, k :: seen))
                  (false, [])
                  kvs
              in
              if has_dup then bad_test () else `O kvs);
          map [bool] (fun b -> `Bool b);
          map [list json] (fun vs -> `A vs);
          const `Null;
          map [string] (fun s -> `String s);
          map [longstring] (fun s -> `String s);
          map [int32] (fun i -> `Float (Int32.to_float i))
          (* map [float] (fun f -> `Float f); *);
        ])

let ezjson = Crowbar.with_printer pp_json ezjson

let large_ezjson =
  (* We use a smaller size for jsoo. It speeds up the CI and avoids
     some stackoverfow issues in nodejs. *)
  let factor =
    match Sys.backend_type with
    | Other "js_of_ocaml" -> 4
    | Native | Bytecode | Other _ -> 16
  in
  (* special generator for testing large values *)
  let open Crowbar in
  with_printer pp_json
  @@ map [ezjson; ezjson; ezjson] (fun j1 j2 j3 ->
         `A
           (List.init factor (fun _ ->
                `A
                  (List.init factor (fun _ ->
                       `O
                         [
                           ("j1", j1);
                           ("j2", `A [j2; `Bool true]);
                           ("this", `Null);
                           ("j3", j3);
                         ])))))

let jsonm_lexeme_seq =
  Crowbar.map [ezjson] Json_encoding.jsonm_lexeme_seq_of_ezjson

let () =
  let open Crowbar in
  add_test ~name:"gen" [jsonm_lexeme_seq] (fun j ->
      ignore j ;
      check true)

let () =
  let open Crowbar in
  add_test ~name:"small_string_serialisation" [jsonm_lexeme_seq] (fun j ->
      let s =
        Data_encoding.Json.small_string_seq_of_jsonm_lexeme_seq ~newline:false j
      in
      Seq.iter ignore s)

let () =
  let open Crowbar in
  add_test
    ~name:"small_string_serialisation-deserialisation"
    [ezjson]
    (fun ezj ->
      let j = Json_encoding.jsonm_lexeme_seq_of_ezjson ezj in
      let s =
        Data_encoding.Json.small_string_seq_of_jsonm_lexeme_seq ~newline:false j
      in
      let s = Seq.fold_left ( ^ ) "" s in
      match Data_encoding.Json.from_string s with
      | Error e -> fail e
      | Ok j -> check_eq ~pp:pp_json ezj j)

let () =
  let open Crowbar in
  add_test ~name:"serialisation" [jsonm_lexeme_seq] (fun j ->
      let s =
        Data_encoding.Json.string_seq_of_jsonm_lexeme_seq
          ~newline:false
          ~chunk_size_hint:512
          j
      in
      Seq.iter ignore s)

let () =
  let open Crowbar in
  add_test ~name:"serialisation(16)-deserialisation" [ezjson] (fun ezj ->
      let j = Json_encoding.jsonm_lexeme_seq_of_ezjson ezj in
      let s =
        Data_encoding.Json.string_seq_of_jsonm_lexeme_seq
          ~newline:false
          ~chunk_size_hint:16
          j
      in
      let s = Seq.fold_left ( ^ ) "" s in
      match Data_encoding.Json.from_string s with
      | Error e -> fail e
      | Ok j -> check_eq ~pp:pp_json ezj j)

let () =
  let open Crowbar in
  add_test ~name:"serialisation(1024)-deserialisation" [ezjson] (fun ezj ->
      let j = Json_encoding.jsonm_lexeme_seq_of_ezjson ezj in
      let s =
        Data_encoding.Json.string_seq_of_jsonm_lexeme_seq
          ~newline:false
          ~chunk_size_hint:1024
          j
      in
      let s = Seq.fold_left ( ^ ) "" s in
      match Data_encoding.Json.from_string s with
      | Error e -> fail e
      | Ok j -> check_eq ~pp:pp_json ezj j)

let () =
  let open Crowbar in
  add_test ~name:"blit-instructions(32)" [ezjson] (fun ezj ->
      let j = Json_encoding.jsonm_lexeme_seq_of_ezjson ezj in
      let buffer = Bytes.create 32 in
      let s =
        Data_encoding.Json.blit_instructions_seq_of_jsonm_lexeme_seq
          ~newline:false
          ~buffer
          j
      in
      let s =
        let b = Buffer.create 8 in
        Seq.iter (fun (s, o, l) -> Buffer.add_subbytes b s o l) s ;
        Buffer.contents b
      in
      match Data_encoding.Json.from_string s with
      | Error e -> fail e
      | Ok j -> check_eq ~pp:pp_json ezj j)

let () =
  let open Crowbar in
  add_test ~name:"blit-instructions(1024)" [ezjson] (fun ezj ->
      let j = Json_encoding.jsonm_lexeme_seq_of_ezjson ezj in
      let buffer = Bytes.create 1024 in
      let s =
        Data_encoding.Json.blit_instructions_seq_of_jsonm_lexeme_seq
          ~newline:false
          ~buffer
          j
      in
      let s =
        let b = Buffer.create 8 in
        Seq.iter (fun (s, o, l) -> Buffer.add_subbytes b s o l) s ;
        Buffer.contents b
      in
      match Data_encoding.Json.from_string s with
      | Error e -> fail e
      | Ok j -> check_eq ~pp:pp_json ezj j)

let () =
  let open Crowbar in
  add_test ~name:"large value, blit instructions" [large_ezjson] (fun ezj ->
      let j = Json_encoding.jsonm_lexeme_seq_of_ezjson ezj in
      let buffer = Bytes.create 128 in
      let s =
        Data_encoding.Json.blit_instructions_seq_of_jsonm_lexeme_seq
          ~newline:false
          ~buffer
          j
      in
      let s =
        let b = Buffer.create 128 in
        Seq.iter (fun (s, o, l) -> Buffer.add_subbytes b s o l) s ;
        Buffer.contents b
      in
      match Data_encoding.Json.from_string s with
      | Error e -> fail e
      | Ok j -> check_eq ~pp:pp_json ezj j)
