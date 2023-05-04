(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech  <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold  <contact@marigold.dev>                       *)
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

module Map = Map.Make (struct
  type t = Dac_plugin.hash

  let compare = Dac_plugin.raw_compare
end)

(* A [Certificate_streamer.t] is a mutable map that associates to root hashes
   values of type [Certificate_repr.V0.t Data_streamer.t]. Such data streamers are
   used to notify clients of updates of DAC certificates. *)
type t = Certificate_repr.V0.t Data_streamer.t Map.t ref

let init () = ref Map.empty

let create_if_none certificate_streamers root_hash =
  certificate_streamers :=
    Map.update
      root_hash
      (function
        | None -> Some (Data_streamer.init ()) | Some stream -> Some stream)
      !certificate_streamers ;
  !certificate_streamers |> Map.find root_hash
  |> Option.value_f ~default:(fun _ -> assert false)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/5550
   Close certificate streamer after timeout to avoid clients hanging
   indefinitely. *)
let handle_subscribe dac_plugin certificate_streamers raw_root_hash =
  let open Result_syntax in
  let* root_hash = Dac_plugin.raw_to_hash dac_plugin raw_root_hash in
  let certificate_streamer_for_root_hash =
    create_if_none certificate_streamers root_hash
  in
  return @@ Data_streamer.handle_subscribe certificate_streamer_for_root_hash

let push dac_plugin certificate_streamers raw_root_hash certificate =
  let open Result_syntax in
  let* root_hash = Dac_plugin.raw_to_hash dac_plugin raw_root_hash in
  let certificate_streamer_for_root_hash =
    create_if_none certificate_streamers root_hash
  in
  return @@ Data_streamer.publish certificate_streamer_for_root_hash certificate

let close dac_plugin certificate_streamers raw_root_hash =
  let open Result_syntax in
  let* root_hash = Dac_plugin.raw_to_hash dac_plugin raw_root_hash in
  let certificate_streamer_for_root_hash =
    Map.find root_hash !certificate_streamers
  in
  match certificate_streamer_for_root_hash with
  | Some streamer ->
      Data_streamer.close streamer ;
      certificate_streamers := Map.remove root_hash !certificate_streamers ;
      return true
  | None -> return false
