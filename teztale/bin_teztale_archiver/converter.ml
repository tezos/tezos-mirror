(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)
open Lwt_result_syntax

let fake_hash level round delegate kind =
  Operation_hash.hash_string
    [
      level;
      Int32.to_string (Option.value round ~default:0l);
      Tezos_crypto.Signature.Public_key_hash.to_string delegate;
      (if kind = Consensus_ops.Attestation then "" else "P");
    ]

let to_received_ops ctx endpoint auth level data =
  (*fake operation hash*)
  let received_ops =
    List.map
      (fun Data.Delegate_operations.
             {delegate; first_slot; attesting_power; operations} ->
        ( Consensus_ops.{address = delegate; first_slot; power = attesting_power},
          List.flatten
            (List.map
               (fun Data.Delegate_operations.
                      {
                        hash;
                        kind;
                        round;
                        mempool_inclusion;
                        block_inclusion = _;
                      } ->
                 List.map
                   (fun Data.Delegate_operations.
                          {source = _; reception_time; errors} ->
                     let maybe_faked_hash =
                       if Operation_hash.equal hash Operation_hash.zero then
                         fake_hash level round delegate kind
                       else hash
                     in
                     Consensus_ops.
                       {
                         op = {kind; round; hash = maybe_faked_hash};
                         errors;
                         reception_time;
                       })
                   mempool_inclusion)
               operations) ))
      data.Json_archiver.delegate_operations
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
           {delegate; first_slot = _; attesting_power; operations} ->
      List.fold_left
        (fun acc
             Data.Delegate_operations.
               {hash; kind; round; mempool_inclusion = _; block_inclusion} ->
          List.fold_left
            (fun acc block ->
              let maybe_faked_hash =
                if Operation_hash.equal hash Operation_hash.zero then
                  fake_hash level round delegate kind
                else hash
              in
              block_map_append
                block
                Consensus_ops.
                  {
                    op = {kind; round; hash = maybe_faked_hash};
                    delegate;
                    power = attesting_power;
                  }
                acc)
            acc
            block_inclusion)
        acc
        operations)
    Block_hash.Map.empty
    data.Json_archiver.delegate_operations

let to_blocks ctx endpoint auth level pred_ops_map ops_map data =
  let bodies =
    List.map
      (fun (Data.Block.{hash; _} as block) ->
        let v =
          ( block,
            data.Json_archiver.cycle_info,
            ( Option.value (Block_hash.Map.find hash pred_ops_map) ~default:[],
              Option.value (Block_hash.Map.find hash ops_map) ~default:[] ),
            data.Json_archiver.baking_rights )
        in
        `String
          (Ezjsonm.value_to_string
             (Data_encoding.Json.construct
                Data.Archiver.raw_block_data_encoding
                v)))
      data.Json_archiver.blocks
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
                  try
                    return
                      (Data_encoding.Json.destruct
                         Json_archiver.level_file_content_encoding
                         json)
                  with exn -> Lwt.return (Error_monad.error_with_exn exn)
                in
                match data with
                | Ok data ->
                    let out = included_ops_map level data in
                    if not data.Json_archiver.unaccurate then
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
