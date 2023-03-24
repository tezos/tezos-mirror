(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Lwt_result_syntax

let fake_hash level round delegate kind =
  Operation_hash.hash_string
    [
      level;
      Int32.to_string (Option.value round ~default:0l);
      Tezos_crypto.Signature.Public_key_hash.to_string delegate;
      (if kind = Consensus_ops.Endorsement then "" else "P");
    ]

let to_received_ops ctx endpoint auth level data =
  (*fake operation hash*)
  let received_ops =
    List.map
      (fun Data.Delegate_operations.
             {delegate; delegate_alias = _; endorsing_power = _; operations} ->
        ( delegate,
          List.flatten
            (List.map
               (fun Data.Delegate_operations.
                      {kind; round; mempool_inclusion; block_inclusion = _} ->
                 List.map
                   (fun Data.Delegate_operations.
                          {source = _; reception_time; errors} ->
                     let fake_hash = fake_hash level round delegate kind in
                     Consensus_ops.
                       {
                         op = {kind; round; hash = fake_hash};
                         errors;
                         reception_time;
                       })
                   mempool_inclusion)
               operations) ))
      data.Data.delegate_operations
  in
  let body =
    `String
      (Ezjsonm.value_to_string
         (Data_encoding.Json.construct
            Consensus_ops.delegate_ops_encoding
            received_ops))
  in
  let headers =
    Cohttp.Header.init_with "content-type" "application/json; charset=UTF-8"
  in
  let headers = Cohttp.Header.add_authorization headers (`Basic auth) in
  let*! resp, out =
    Cohttp_lwt_unix.Client.post
      ~ctx
      ~body
      ~headers
      (Uri.with_path endpoint (Uri.path endpoint ^ "/" ^ level ^ "/mempool"))
  in
  let*! out = Cohttp_lwt.Body.to_string out in
  Lwt_io.printlf
    "%s: %s: %s"
    level
    (Cohttp.Code.string_of_status resp.status)
    out

let block_map_append x e m =
  let out = Option.value (Block_hash.Map.find x m) ~default:[] in
  Block_hash.Map.add x (e :: out) m

let included_ops_map level data =
  List.fold_left
    (fun acc
         Data.Delegate_operations.
           {delegate; delegate_alias = _; endorsing_power; operations} ->
      List.fold_left
        (fun acc
             Data.Delegate_operations.
               {kind; round; mempool_inclusion = _; block_inclusion} ->
          List.fold_left
            (fun acc block ->
              let fake_hash = fake_hash level round delegate kind in
              block_map_append
                block
                Consensus_ops.
                  {
                    op = {kind; round; hash = fake_hash};
                    delegate;
                    power = endorsing_power;
                  }
                acc)
            acc
            block_inclusion)
        acc
        operations)
    Block_hash.Map.empty
    data.Data.delegate_operations

let to_blocks ctx endpoint auth level pred_ops_map ops_map data =
  let bodies =
    List.map
      (fun (Data.Block.{hash; _} as block) ->
        let v =
          ( block,
            ( Option.value (Block_hash.Map.find hash pred_ops_map) ~default:[],
              Option.value (Block_hash.Map.find hash ops_map) ~default:[] ) )
        in
        `String
          (Ezjsonm.value_to_string
             (Data_encoding.Json.construct Data.block_data_encoding v)))
      data.Data.blocks
  in
  let headers =
    Cohttp.Header.init_with "content-type" "application/json; charset=UTF-8"
  in
  let headers = Cohttp.Header.add_authorization headers (`Basic auth) in
  Lwt.join
    (List.map
       (fun body ->
         let*! resp, out =
           Cohttp_lwt_unix.Client.post
             ~ctx
             ~body
             ~headers
             (Uri.with_path
                endpoint
                (Uri.path endpoint ^ "/" ^ level ^ "/block"))
         in
         let*! out = Cohttp_lwt.Body.to_string out in
         Lwt_io.printlf
           "%s: %s: %s"
           level
           (Cohttp.Code.string_of_status resp.status)
           out)
       bodies)

let poor_man_lexical_sort x y =
  if String.length x = String.length y then String.compare x y
  else Int.compare (String.length x) (String.length y)

let main source password endpoint prefix =
  let*! ctx =
    match X509.Authenticator.of_string "none" with
    | Error _ -> Conduit_lwt_unix.init ()
    | Ok f ->
        let tls_authenticator = f (fun () -> Some (Time.System.now ())) in
        Conduit_lwt_unix.init ~tls_authenticator ()
  in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  let subdirs = Sys.readdir prefix in
  let () = Array.sort poor_man_lexical_sort subdirs in
  let*! _ =
    Array.fold_left
      (fun acc subdir ->
        let subdir = Filename.concat prefix subdir in
        if Sys.is_directory subdir then
          let filenames = Sys.readdir subdir in
          let () = Array.sort poor_man_lexical_sort filenames in
          Array.fold_left
            (fun acc filename ->
              if Filename.check_suffix filename "json" then
                let level = Filename.chop_extension filename in
                let filename = Filename.concat subdir filename in
                let*! previous_block_ops_map = acc in
                let*! data =
                  let* json = Lwt_utils_unix.Json.read_file filename in
                  try return (Data_encoding.Json.destruct Data.encoding json)
                  with exn -> Lwt.return (Error_monad.error_with_exn exn)
                in
                match data with
                | Ok data ->
                    let out = included_ops_map level data in
                    if not data.Data.unaccurate then
                      let*! _ =
                        to_received_ops
                          ctx
                          endpoint
                          (source, password)
                          level
                          data
                      in
                      let*! _ =
                        to_blocks
                          ctx
                          endpoint
                          (source, password)
                          level
                          previous_block_ops_map
                          out
                          data
                      in
                      Lwt.return out
                    else Lwt.return out
                | Error err ->
                    let*! () =
                      Lwt_io.printl
                        (Format.asprintf
                           "@[File %s does not parse :@ @[%a@]@]"
                           filename
                           Error_monad.pp_print_trace
                           err)
                    in
                    acc
              else acc)
            acc
            filenames
        else acc)
      (Lwt.return Block_hash.Map.empty)
      subdirs
  in
  return_unit
