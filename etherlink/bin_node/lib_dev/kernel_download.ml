(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type preimages = Contents of bytes | Hashes of string list

let preimages_encoding =
  Data_encoding.(
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"content"
          (Tag 0)
          bytes
          (function Contents _payload -> assert false | _ -> None)
          (fun payload -> Contents payload);
        case
          ~title:"hashes"
          (Tag 1)
          (list Tezos_raw_protocol_alpha.Sc_rollup_reveal_hash.encoding)
          (function Hashes _hashes -> assert false | _ -> None)
          (fun hashes ->
            Hashes
              (List.map
                 Tezos_raw_protocol_alpha.Sc_rollup_reveal_hash.to_hex
                 hashes));
      ])

let download ~preimages_endpoint ~preimages ~(root_hash : Hex.t) =
  let open Lwt_result_syntax in
  (* value found in `commands.ml`, should we make it configurable? *)
  let num_retries = 3 in
  let rec go retrieved_hashes =
    let open Lwt_result_syntax in
    match retrieved_hashes with
    | [] -> return_unit
    | hash :: hashes -> (
        let*! preimage =
          Commands.reveal_preimage
            ~preimages_endpoint
            ~preimages
            num_retries
            hash
        in
        match
          Data_encoding.Binary.of_string_exn preimages_encoding preimage
        with
        | Hashes page_hashes ->
            (go [@tailcall])
              (List.rev_append (List.map (fun s -> `Hex s) page_hashes) hashes)
        | Contents _content -> (go [@tailcall]) hashes)
  in
  let* () = go [root_hash] in
  let*! () = Events.predownload_kernel root_hash in
  return_unit
