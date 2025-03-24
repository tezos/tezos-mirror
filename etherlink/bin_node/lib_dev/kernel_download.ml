(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Reveal_hash = Tezos_raw_protocol_alpha.Sc_rollup_reveal_hash

type error += Invalid_preimage_for_hash of Hex.t * string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_invalid_preimage"
    ~title:"Preimage has not the expected hash"
    ~description:
      "The EVM node could not apply a blueprint on top of its local EVM state."
    ~pp:(fun ppf (hash, _preimage) ->
      Format.fprintf
        ppf
        "The preimage received for %s doesn't return the same hash"
        hash)
    Data_encoding.(obj2 (req "expected_hash" string) (req "preimage" string))
    (function
      | Invalid_preimage_for_hash (`Hex hash, preimage) -> Some (hash, preimage)
      | _ -> None)
    (fun (hash, preimage) -> Invalid_preimage_for_hash (`Hex hash, preimage))

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
          (list Reveal_hash.encoding)
          (function Hashes _hashes -> assert false | _ -> None)
          (fun hashes -> Hashes (List.map Reveal_hash.to_hex hashes));
      ])

let check_preimage (`Hex hash) preimage =
  let computed_hash =
    Reveal_hash.hash_string ~scheme:Reveal_hash.Blake2B [preimage]
    |> Reveal_hash.to_hex
  in
  hash = computed_hash

let delete_preimage preimages (`Hex hash) =
  Lwt_unix.unlink (Filename.concat preimages hash)

let rec reveal_and_check ~preimages_endpoint ~preimages ~num_download_retries
    hash =
  let open Lwt_result_syntax in
  let*! preimage =
    Octez_smart_rollup_wasm_debugger_lib.Commands.reveal_preimage
      ~preimages_endpoint
      ~preimages
      0
      (* Retrying makes sense only when the preimage is fed through stdin, this
         wouldn't make sense in our case. *)
      hash
  in
  if not (check_preimage hash preimage) then
    if num_download_retries <= 0 then
      tzfail (Invalid_preimage_for_hash (hash, preimage))
    else
      let*! () = delete_preimage preimages hash in
      reveal_and_check
        ~preimages_endpoint
        ~preimages
        ~num_download_retries:(pred num_download_retries)
        hash
  else return preimage

let download ~preimages_endpoint ~preimages ~(root_hash : Hex.t)
    ?(num_download_retries = 1) () =
  let open Lwt_result_syntax in
  let rec go retrieved_hashes =
    match retrieved_hashes with
    | [] -> return_unit
    | hash :: hashes -> (
        let* preimage =
          reveal_and_check
            ~preimages_endpoint
            ~preimages
            ~num_download_retries
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
