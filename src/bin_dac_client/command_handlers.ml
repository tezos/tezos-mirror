(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/5339
   rename PUT v1/preimage into v1/payload, and change name
   of function accordingly. *)
let send_preimage cctxt payload =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5627
     Currently we have only one major DAC API version ([V0]). For this reason,
     client binary can always default to it. This should be revisited once we
     add another major version. *)
  Dac_node_client.V0.Coordinator.post_preimage cctxt ~payload

let number_of_witnesses certificate =
  let witnesses = Certificate_repr.get_witnesses certificate in
  let rec find_next witnesses num_witnesses =
    if Z.(equal witnesses zero) then num_witnesses
    else
      let new_witnesses = Z.(to_int @@ logand witnesses one) in
      find_next (Z.shift_right witnesses 1) (num_witnesses + new_witnesses)
  in
  find_next witnesses 0

(* TODO: https://gitlab.com/tezos/tezos/-/issues/5550
   Close connection after timeouts on server side to avoid the client
   hanging forever. *)
let wait_for_certificate cctxt root_hash threshold =
  let open Lwt_result_syntax in
  let* stream, _stopper =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5627
       Currently we have only one major DAC API version ([V0]). For this reason,
       client binary can always default to it. This should be revisited once we
       add another major version. *)
    Dac_node_client.V0.monitor_certificate cctxt ~root_hash
  in
  let rec go best_certificate_opt best_witnesses =
    let*! certificate_opt = Lwt_stream.get stream in
    match certificate_opt with
    | None -> return best_certificate_opt
    | Some certificate ->
        let num_witnesses = number_of_witnesses certificate in
        if num_witnesses >= threshold then return @@ Some certificate
        else if num_witnesses < best_witnesses then
          go best_certificate_opt best_witnesses
        else go certificate_opt num_witnesses
  in
  let* certificate_opt = go None 0 in
  match certificate_opt with
  | Some certificate ->
      let+ serialize_certificate_opt =
        Dac_node_client.V0.get_serialized_certificate
          cctxt
          ~root_page_hash:(Certificate_repr.get_root_hash certificate)
      in
      Option.map Hex.of_string serialize_certificate_opt
  | None -> return None

let get_certificate cctxt root_page_hash =
  let open Lwt_result_syntax in
  let+ certificate_opt =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5627
       Currently we have only one major DAC API version ([V0]). For this reason,
       client binary can always default to it. This should be revisited once we
       add another major version. *)
    Dac_node_client.V0.get_serialized_certificate cctxt ~root_page_hash
  in
  Option.map Hex.of_string certificate_opt
