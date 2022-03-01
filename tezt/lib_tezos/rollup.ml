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
    oldest_inbox_level : int option;
    head_level : (int * int) option;
    burn_per_byte : int;
    inbox_ema : int;
  }

  type inbox = {cumulated_size : int; contents : string list; hash : string}

  let get_state ?hooks ~rollup client =
    let parse json =
      let oldest_inbox_level =
        JSON.(json |-> "oldest_inbox_level" |> as_opt |> Option.map as_int)
      in
      let head_level =
        JSON.(
          json |-> "head_level" |> as_opt
          |> Option.map @@ fun x ->
             as_list x |> function
             | [x; y] -> (as_int x, as_int y)
             | _ -> raise (Invalid_argument "Rollup.get_state.parse"))
      in
      let burn_per_byte = JSON.(json |-> "burn_per_byte" |> as_int) in
      let inbox_ema = JSON.(json |-> "inbox_ema" |> as_int) in
      {oldest_inbox_level; head_level; burn_per_byte; inbox_ema}
    in
    let runnable = RPC.Tx_rollup.get_state ?hooks ~rollup client in
    Process.runnable_map parse runnable

  let get_inbox ?hooks ~rollup ~level client =
    let parse json =
      let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
      let contents =
        JSON.(json |-> "contents" |> as_list |> List.map as_string)
      in
      let hash = JSON.(json |-> "hash" |> as_string) in
      {cumulated_size; contents; hash}
    in
    let runnable = RPC.Tx_rollup.get_inbox ?hooks ~rollup ~level client in
    Process.runnable_map parse runnable

  let get_commitment ?hooks ?block ~rollup ~level client =
    RPC.Tx_rollup.get_commitment ?hooks ?block ~rollup ~level client

  let get_pending_bonded_commitments ?hooks ?block ~rollup ~pkh client =
    RPC.Tx_rollup.get_pending_bonded_commitments
      ?hooks
      ?block
      ~rollup
      ~pkh
      client

  module Check = struct
    let state : state Check.typ =
      let open Check in
      convert
        (fun {head_level; oldest_inbox_level; burn_per_byte; inbox_ema} ->
          (head_level, oldest_inbox_level, burn_per_byte, inbox_ema))
        (tuple4 (option (tuple2 int int)) (option int) int int)

    let inbox : inbox Check.typ =
      let open Check in
      convert
        (fun {cumulated_size; contents; hash} ->
          (cumulated_size, contents, hash))
        (tuple3 int (list string) string)
  end

  module Parameters = struct
    type t = {finality_period : int; withdraw_period : int}

    let default = {finality_period = 60_000; withdraw_period = 60_000}

    let parameter_file ?(parameters = default) protocol =
      let args =
        [(["tx_rollup_enable"], Some "true")]
        @ [
            ( ["tx_rollup_finality_period"],
              Some (string_of_int parameters.finality_period) );
          ]
        @ [
            ( ["tx_rollup_withdraw_period"],
              Some (string_of_int parameters.withdraw_period) );
          ]
      in
      Protocol.write_parameter_file ~base:(Either.right (protocol, None)) args
  end
end
