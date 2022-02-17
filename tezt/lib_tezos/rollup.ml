(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Tx_rollup = struct
  type state = {
    burn_per_byte : int;
    inbox_ema : int;
    last_inbox_level : int option;
  }

  type inbox = {cumulated_size : int; contents : string list}

  let get_state ?hooks ~rollup client : state Lwt.t =
    let* json = RPC.Tx_rollup.get_state ?hooks ~rollup client in
    let burn_per_byte = JSON.(json |-> "burn_per_byte" |> as_int) in
    let inbox_ema = JSON.(json |-> "inbox_ema" |> as_int) in
    let last_inbox_level =
      JSON.(json |-> "last_inbox_level" |> as_opt |> Option.map as_int)
    in
    return {burn_per_byte; inbox_ema; last_inbox_level}

  let get_inbox ?hooks ~rollup client =
    let* json = RPC.Tx_rollup.get_inbox ?hooks ~rollup client in
    let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
    let contents =
      JSON.(json |-> "contents" |> as_list |> List.map as_string)
    in
    return {cumulated_size; contents}

  let get_commitments ?hooks ?block ?offset ~rollup client =
    RPC.Tx_rollup.get_commitments ?hooks ?block ?offset ~rollup client

  module Check = struct
    let state : state Check.typ =
      let open Check in
      convert
        (fun {burn_per_byte; inbox_ema; last_inbox_level} ->
          (burn_per_byte, inbox_ema, last_inbox_level))
        (tuple3 int int (option int))

    let inbox : inbox Check.typ =
      let open Check in
      convert
        (fun {cumulated_size; contents} -> (cumulated_size, contents))
        (tuple2 int (list string))
  end
end
