(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

type repr = Z.t

type t = Timestamp_tag of repr [@@ocaml.unboxed]

let compare (Timestamp_tag x) (Timestamp_tag y) = Z.compare x y

let of_int64 i = Timestamp_tag (Z.of_int64 i)

let of_string x =
  match Time_repr.of_notation x with
  | None -> Option.catch (fun () -> Timestamp_tag (Z.of_string x))
  | Some time -> Some (of_int64 (Time_repr.to_seconds time))

let to_notation (Timestamp_tag x) =
  Option.catch (fun () ->
      Time_repr.to_notation (Time.of_seconds (Z.to_int64 x)))

let to_num_str (Timestamp_tag x) = Z.to_string x

let to_string x = match to_notation x with None -> to_num_str x | Some s -> s

let diff (Timestamp_tag x) (Timestamp_tag y) = Script_int.of_zint @@ Z.sub x y

let sub_delta (Timestamp_tag t) delta =
  Timestamp_tag (Z.sub t (Script_int.to_zint delta))

let add_delta (Timestamp_tag t) delta =
  Timestamp_tag (Z.add t (Script_int.to_zint delta))

let to_zint (Timestamp_tag x) = x

let of_zint x = Timestamp_tag x

let encoding = Data_encoding.(conv to_zint of_zint z)

let now ctxt =
  let open Alpha_context in
  let first_delay = Period.to_seconds (Constants.minimal_block_delay ctxt) in
  let current_timestamp = Timestamp.predecessor ctxt in
  Time.add current_timestamp first_delay |> Timestamp.to_seconds |> of_int64
