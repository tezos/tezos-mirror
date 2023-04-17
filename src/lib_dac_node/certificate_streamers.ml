(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech  <contact@trili.tech>                       *)
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

type t = Certificate_repr.t Data_streamer.t Map.t ref

let init () = ref Map.empty

let create_if_none t root_hash =
  t :=
    Map.update
      root_hash
      (function
        | None -> Some (Data_streamer.init ()) | Some stream -> Some stream)
      !t ;
  !t |> Map.find root_hash |> Option.value_f ~default:(fun _ -> assert false)

let handle_subscribe t root_hash =
  let certificate_streamer = create_if_none t root_hash in
  Data_streamer.handle_subscribe certificate_streamer

let push (t : t) root_hash certificate =
  let certificate_streamer = create_if_none t root_hash in
  (* TODO: insert issue here. Add logic for closing stream if certificate has
     100% of signatures. *)
  Data_streamer.publish certificate_streamer certificate
