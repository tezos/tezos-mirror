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

open Protocol_client_context
open Protocol.Alpha_context

type highwatermark = {round : Round.t; level : int32}

let highwatermark_encoding : highwatermark Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {round; level} -> (round, level))
    (fun (round, level) -> {round; level})
    (obj2
       (req "round" Protocol.Alpha_context.Round.encoding)
       (req "level" int32))

let pp_highwatermark fmt {round; level} =
  Format.fprintf fmt "level: %ld, round: %a" level Round.pp round

type error += Block_previously_baked of highwatermark

type error += Block_previously_preattested of highwatermark

type error += Block_previously_attested of highwatermark

let () =
  register_error_kind
    `Permanent
    ~id:"highwatermarks.block_previously_baked"
    ~title:"Block previously baked"
    ~description:"Trying to bake a block at a level previously baked"
    ~pp:(fun ppf highwatermark ->
      Format.fprintf
        ppf
        "A block with a higher watermark than the current one (%a) was \
         previously baked."
        pp_highwatermark
        highwatermark)
    highwatermark_encoding
    (function
      | Block_previously_baked highwatermark -> Some highwatermark | _ -> None)
    (fun highwatermark -> Block_previously_baked highwatermark) ;
  register_error_kind
    `Permanent
    ~id:"highwatermarks.block_previously_preattested"
    ~title:"Block previously preattested"
    ~description:"Trying to preattest a block at a level previously preattested"
    ~pp:(fun ppf highwatermark ->
      Format.fprintf
        ppf
        "A preattestation with a higher watermark than the current one (%a) \
         was already produced."
        pp_highwatermark
        highwatermark)
    highwatermark_encoding
    (function
      | Block_previously_preattested highwatermark -> Some highwatermark
      | _ -> None)
    (fun highwatermark -> Block_previously_preattested highwatermark) ;
  register_error_kind
    `Permanent
    ~id:"highwatermarks.block_previously_attested"
    ~title:"Block previously attested"
    ~description:"Trying to attest a block at a level previously attested"
    ~pp:(fun ppf highwatermark ->
      Format.fprintf
        ppf
        "An attestation with a higher watermark than the current one (%a) was \
         already produced."
        pp_highwatermark
        highwatermark)
    highwatermark_encoding
    (function
      | Block_previously_attested highwatermark -> Some highwatermark
      | _ -> None)
    (fun highwatermark -> Block_previously_attested highwatermark)

module DelegateMap = Map.Make (struct
  type t = Signature.Public_key_hash.t

  let compare = Signature.Public_key_hash.compare
end)

let highwatermark_delegate_map_encoding =
  let open Data_encoding in
  conv
    DelegateMap.bindings
    DelegateMap.(
      fun l -> List.fold_left (fun map (k, v) -> add k v map) empty l)
    (list
       (obj2
          (req "delegate" Signature.Public_key_hash.encoding)
          (req "highwatermark" highwatermark_encoding)))

type highwatermarks = {
  blocks : highwatermark DelegateMap.t;
  preattestations : highwatermark DelegateMap.t;
  attestations : highwatermark DelegateMap.t;
}

type t = highwatermarks

let encoding =
  let open Data_encoding in
  conv
    (fun {blocks; preattestations; attestations} ->
      (blocks, preattestations, attestations))
    (fun (blocks, preattestations, attestations) ->
      {blocks; preattestations; attestations})
    (obj3
       (req "blocks" highwatermark_delegate_map_encoding)
       (req "preattestations" highwatermark_delegate_map_encoding)
       (req "attestations" highwatermark_delegate_map_encoding))

let empty =
  {
    blocks = DelegateMap.empty;
    preattestations = DelegateMap.empty;
    attestations = DelegateMap.empty;
  }

(* We do not lock these functions. The caller will be already locked. *)
let load (cctxt : #Protocol_client_context.full) location : t tzresult Lwt.t =
  protect (fun () ->
      cctxt#load (Baking_files.filename location) encoding ~default:empty)

let save_highwatermarks (cctxt : #Protocol_client_context.full) filename
    highwatermarks : unit tzresult Lwt.t =
  protect (fun () ->
      (* TODO: improve the backend so we don't write partial informations *)
      cctxt#write filename highwatermarks encoding)

let may_sign highwatermarks ~delegate ~level ~round =
  match DelegateMap.find delegate highwatermarks with
  | None -> true
  | Some highwatermark ->
      if Compare.Int32.(highwatermark.level < level) then true
      else if Compare.Int32.(highwatermark.level = level) then
        Round.(highwatermark.round < round)
      else false

let may_sign_block cctxt (location : [`Highwatermarks] Baking_files.location)
    ~delegate ~level ~round =
  let open Lwt_result_syntax in
  let* all_highwatermarks = load cctxt location in
  return @@ may_sign all_highwatermarks.blocks ~delegate ~level ~round

let may_sign_preattestation all_highwatermarks ~delegate ~level ~round =
  may_sign all_highwatermarks.preattestations ~delegate ~level ~round

let may_sign_attestation all_highwatermarks ~delegate ~level ~round =
  may_sign all_highwatermarks.attestations ~delegate ~level ~round

let record map ~delegate ~new_level ~new_round =
  DelegateMap.update
    delegate
    (function
      | None -> Some {level = new_level; round = new_round}
      | Some ({level; round} as prev) ->
          if Compare.Int32.(new_level > level) then
            Some {level = new_level; round = new_round}
          else if Compare.Int32.(new_level = level) then
            if Round.(new_round > round) then
              Some {level = new_level; round = new_round}
            else Some prev
          else Some prev)
    map

let record_block (cctxt : #Protocol_client_context.full) location ~delegate
    ~level ~round =
  let open Lwt_result_syntax in
  let filename = Baking_files.filename location in
  let* highwatermarks = load cctxt location in
  let new_blocks =
    record highwatermarks.blocks ~delegate ~new_level:level ~new_round:round
  in
  save_highwatermarks cctxt filename {highwatermarks with blocks = new_blocks}

let record_preattestation (cctxt : #Protocol_client_context.full) location
    ~delegate ~level ~round =
  let open Lwt_result_syntax in
  let filename = Baking_files.filename location in
  let* highwatermarks = load cctxt location in
  let new_preattestations =
    record
      highwatermarks.preattestations
      ~delegate
      ~new_level:level
      ~new_round:round
  in
  save_highwatermarks
    cctxt
    filename
    {highwatermarks with preattestations = new_preattestations}

let record_attestation (cctxt : #Protocol_client_context.full) location
    ~delegate ~level ~round =
  let open Lwt_result_syntax in
  let filename = Baking_files.filename location in
  let* highwatermarks = load cctxt location in
  let new_attestations =
    record
      highwatermarks.attestations
      ~delegate
      ~new_level:level
      ~new_round:round
  in
  save_highwatermarks
    cctxt
    filename
    {highwatermarks with attestations = new_attestations}

let record_all_preattestations all_highwatermarks cctxt location ~delegates
    ~level ~round =
  let new_preattestations =
    List.fold_left
      (fun map delegate ->
        record map ~delegate ~new_level:level ~new_round:round)
      all_highwatermarks.preattestations
      delegates
  in
  let filename = Baking_files.filename location in
  save_highwatermarks
    cctxt
    filename
    {all_highwatermarks with preattestations = new_preattestations}

let record_all_attestations all_highwatermarks cctxt location ~delegates ~level
    ~round =
  let new_attestations =
    List.fold_left
      (fun map delegate ->
        record map ~delegate ~new_level:level ~new_round:round)
      all_highwatermarks.attestations
      delegates
  in
  let filename = Baking_files.filename location in
  save_highwatermarks
    cctxt
    filename
    {all_highwatermarks with attestations = new_attestations}
