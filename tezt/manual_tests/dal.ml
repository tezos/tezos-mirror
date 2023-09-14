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

(* Testing
   -------
   Component:    DAL
   Invocation:   dune exec tezt/manual_tests/main.exe -- --file dal.ml --test-arg output-file=<file>
   Subject:      Test getting informaton about the DAL distribution.
*)

module Dal = Dal_common

let dal_distribution =
  Protocol.register_test
    ~__FILE__
    ~title:"Get the DAL distribution"
    ~tags:["dal"; "distribution"]
    ~supports:Protocol.(From_protocol 15)
  @@ fun protocol ->
  let _data_dir =
    Cli.get ~default:None (fun data_dir -> Some (Some data_dir)) "data-dir"
  in
  let levels =
    Cli.get ~default:10 (fun levels -> int_of_string_opt levels) "levels"
  in
  let output_file =
    match
      Cli.get
        ~default:None
        (fun output_file -> Some (Some output_file))
        "output-file"
    with
    | None ->
        Test.fail "Specify an output file with --test-arg output-file=<file>"
    | Some output_file -> output_file
  in
  let* parameter_file = Dal.Parameters.parameter_file protocol in
  let* node, client =
    Client.init_with_protocol ~parameter_file ~protocol `Client ()
  in
  let* () = Client.bake_for_and_wait client in
  let* number_of_shards =
    let* constants =
      Client.RPC.call client (RPC.get_chain_block_context_constants ())
    in
    JSON.(constants |-> "dal_parametric" |-> "number_of_shards" |> as_int)
    |> return
  in
  let results =
    Array.init levels (fun _ ->
        Array.make number_of_shards Constant.bootstrap1.public_key_hash)
  in
  let* current_level =
    Client.RPC.call client (RPC.get_chain_block_helper_current_level ())
  in
  let rec iter offset =
    let level = current_level.level + offset in
    if offset < 0 then unit
    else
      let* json =
        Node.RPC.call node @@ RPC.get_chain_block_context_dal_shards ~level ()
      in
      List.iter
        (fun json ->
          let pkh = JSON.(json |=> 0 |> as_string) in
          let initial_slot = JSON.(json |=> 1 |=> 0 |> as_int) in
          let power = JSON.(json |=> 1 |=> 1 |> as_int) in
          for slot = initial_slot to initial_slot + power - 1 do
            let line = Array.get results offset in
            Array.set line slot pkh
          done)
        (JSON.as_list json) ;
      iter (offset - 1)
  in
  let* () = iter (levels - 1) in
  with_open_out output_file (fun oc ->
      output_string oc "levels, " ;
      for slot = 0 to number_of_shards - 1 do
        output_string oc (Format.asprintf "pkh for slot %d, " slot)
      done ;
      output_string oc "\n" ;
      for i = 0 to levels - 1 do
        output_string oc (Format.asprintf "level %d, " i) ;
        for slot = 0 to number_of_shards - 1 do
          let pkh = results.(i).(slot) in
          output_string oc pkh ;
          output_string oc ", "
        done ;
        output_string oc "\n"
      done) ;
  unit

let register protocols = dal_distribution protocols
