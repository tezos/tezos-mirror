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
        let* () =
          if not (check_preimage hash preimage) then
            tzfail (Invalid_preimage_for_hash (hash, preimage))
          else return_unit
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
