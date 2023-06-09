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

open Protocol
open Alpha_context

module Plugin = struct
  module Proto = Registerer.Registered

  type block_info = Protocol_client_context.Alpha_block_services.block_info

  let parametric_constants chain block ctxt =
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    Protocol.Constants_services.parametric cpctxt (chain, block)

  let get_constants chain block ctxt =
    let open Lwt_result_syntax in
    let* parametric = parametric_constants chain block ctxt in
    let {
      Constants.Parametric.feature_enable;
      number_of_slots;
      attestation_lag;
      availability_threshold;
      cryptobox_parameters;
    } =
      parametric.dal
    in
    return
      {
        Dal_plugin.feature_enable;
        number_of_slots;
        attestation_lag;
        attestation_threshold = availability_threshold;
        cryptobox_parameters;
        blocks_per_epoch =
          (* This is a protocol constant in future protocol. *)
          32l;
      }

  let block_info ?chain ?block ~metadata ctxt =
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    Protocol_client_context.Alpha_block_services.info
      cpctxt
      ?chain
      ?block
      ~metadata
      ()

  (* Turn the given value of type {!Protocol.Apply_operation_result.operation_result}
     into a value of type {!Dal_plugin.operation_application_result}. *)
  let status_of_result = function
    | Protocol.Apply_operation_result.Applied _ -> Dal_plugin.Succeeded
    | _ -> Dal_plugin.Failed

  let get_published_slot_headers (block : block_info) =
    let open Lwt_result_syntax in
    let open Protocol.Alpha_context in
    let apply_internal acc ~source:_ _op _res = acc in
    let apply (type kind) acc ~source:_ (op : kind manager_operation)
        (result : (kind, _, _) Protocol.Apply_operation_result.operation_result)
        =
      match op with
      | Dal_publish_slot_header slot_header ->
          (slot_header.header, status_of_result result) :: acc
      | _ -> acc
    in
    Layer1_services.(
      process_manager_operations [] block.operations {apply; apply_internal})
    |> List.map_es (fun (slot, status) ->
           return
             Dal_plugin.
               ( {
                   published_level =
                     Raw_level.to_int32 slot.Dal.Slot.Header.id.published_level;
                   slot_index =
                     Dal.Slot_index.to_int slot.Dal.Slot.Header.id.index;
                   commitment = slot.Dal.Slot.Header.commitment;
                 },
                 status ))

  let check_is_in_range slot_index =
    let zero = Dal.Slot_index.(to_int zero) in
    let max_value = 255 in
    if Compare.Int.(slot_index >= zero && slot_index <= max_value) then
      Result_syntax.return_unit
    else Stdlib.failwith "Invalid_slot_index"

  let slots_range ~lower ~upper =
    let open Result_syntax in
    let* () = check_is_in_range lower in
    let* () = check_is_in_range upper in
    return Misc.(lower --> upper)

  let get_committee _ctxt ~level:_ =
    Stdlib.failwith "get_committee not supported for this protocol version."

  let attested_slot_headers _hash (_block : block_info) ~number_of_slots:_ =
    let open Result_syntax in
    (* DAL Will not be activated in Mumbai. *)
    return []

  module RPC = RPC
end

let () = Dal_plugin.register (module Plugin)
