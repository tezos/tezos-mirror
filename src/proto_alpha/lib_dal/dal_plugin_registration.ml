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
    let cpctxt = new Protocol_client_context.wrap_full ctxt in
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
      blocks_per_epoch;
    } =
      parametric.dal
    in
    return
      {
        Dal_plugin.feature_enable;
        number_of_slots;
        attestation_lag;
        availability_threshold;
        cryptobox_parameters;
        blocks_per_epoch;
      }

  let block_info ?chain ?block ~metadata ctxt =
    let cpctxt = new Protocol_client_context.wrap_full ctxt in
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

  let slot_headers_attestation hash (block : block_info) ~number_of_slots =
    let open Lwt_result_syntax in
    let*? metadata =
      Option.to_result
        block.metadata
        ~none:(TzTrace.make @@ Layer1_services.Cannot_read_block_metadata hash)
    in
    let confirmed_slots =
      Option.value
        ~default:Dal.Attestation.empty
        metadata.protocol_data.dal_attestation
    in
    let*? all_slots =
      Dal.Slot_index.slots_range ~lower:0 ~upper:(number_of_slots - 1)
      |> Environment.wrap_tzresult
    in
    let attested, unattested =
      List.partition (Dal.Attestation.is_attested confirmed_slots) all_slots
    in
    return
      ( `Attested (Dal.Slot_index.to_int_list attested),
        `Unattested (Dal.Slot_index.to_int_list unattested) )

  module RPC = RPC
end

let () = Dal_plugin.register (module Plugin)
