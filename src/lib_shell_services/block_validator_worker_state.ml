(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module Request = struct
  type validation_view = {
    chain_id : Chain_id.t;
    block : Block_hash.t;
    peer : P2p_peer.Id.t option;
  }

  let validation_view_encoding =
    let open Data_encoding in
    conv
      (fun {block; chain_id; peer} -> (block, chain_id, peer))
      (fun (block, chain_id, peer) -> {block; chain_id; peer})
      (obj3
         (req "block" Block_hash.encoding)
         (req "chain_id" Chain_id.encoding)
         (opt "peer" P2p_peer.Id.encoding))

  type preapplication_view = {chain_id : Chain_id.t; level : int32}

  let preapplication_view_encoding =
    let open Data_encoding in
    conv
      (fun {chain_id; level} -> (chain_id, level))
      (fun (chain_id, level) -> {chain_id; level})
      (obj2 (req "chain_id" Chain_id.encoding) (req "level" int32))

  type view =
    | Validation of validation_view
    | Preapplication of preapplication_view

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"validation"
          (Tag 0)
          validation_view_encoding
          (function
            | Validation {block; chain_id; peer} -> Some {block; chain_id; peer}
            | _ -> None)
          (fun {block; chain_id; peer} -> Validation {block; chain_id; peer});
        case
          ~title:"preapplication"
          (Tag 1)
          preapplication_view_encoding
          (function
            | Preapplication {chain_id; level} -> Some {chain_id; level}
            | _ -> None)
          (fun {chain_id; level} -> Preapplication {chain_id; level});
      ]

  let pp ppf = function
    | Validation {chain_id; block; peer} -> (
        Format.fprintf
          ppf
          "Validation of %a (chain: %a)"
          Block_hash.pp
          block
          Chain_id.pp_short
          chain_id ;
        match peer with
        | None -> ()
        | Some peer ->
            Format.fprintf ppf "from peer %a" P2p_peer.Id.pp_short peer)
    | Preapplication {chain_id; level} ->
        Format.fprintf
          ppf
          "Pre-application at level %ld (chain: %a)"
          level
          Chain_id.pp_short
          chain_id
end
