(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(*                               utils                                       *)

let get_state tx_rollup client =
  let* json = RPC.Tx_rollup.get_state ~tx_rollup client in
  JSON.(json |-> "state" |> as_opt |> Option.map (fun _ -> ())) |> Lwt.return

(*                               test                                        *)

let test_simple_use_case =
  let open Tezt_tezos in
  Protocol.register_test ~__FILE__ ~title:"Simple use case" ~tags:["rollup"]
  @@ fun protocol ->
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.right (protocol, None))
      [(["tx_rollup_enable"], Some "true")]
  in
  let* (_node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let* tx_rollup =
    Client.originate_tx_rollup
      ~burn_cap:Tez.(of_int 9999999)
      ~storage_limit:60_000
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for client in
  let* state = get_state tx_rollup client in
  match state with
  | Some _ -> unit
  | None ->
      Test.fail
        "The tx rollups was not correctly originated and no state exists for \
         %s."
        tx_rollup

let register ~protocols = test_simple_use_case ~protocols
