(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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

(** Tests originating a large contract
    with no constants. This serves for
    comparison with the case where we originate a
    much larger contract using constants. *)
let test_large_flat_contract =
  Protocol.register_test
    ~__FILE__
    ~title:"Originate a large, flat contract"
    ~tags:["global_constant"]
  @@ fun protocol ->
  let* _, client = Client.init_with_protocol ~protocol `Client () in
  let* _ =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 9999999)
      client
      ["mini_scenarios"; "large_flat_contract"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  return ()

(* To ensure a billion-laughs style attack is not possible,
   we construct a giant contract and try to originate it,
   expecting the size limit to reject it.

   We achieve this by registering a large initial value,
   then a constant with 10 of these values, then a constant
   with 10 of the first constant. We could quickly get to
   extremely large values, but the current size limit is set
   very low and only needs two layers. *)
let test_billion_laughs_contract =
  Protocol.register_test
    ~__FILE__
    ~title:"Global constants billion laughs attack"
    ~tags:["billion_laughs"; "global_constant"]
  @@ fun protocol ->
  let* _, client = Client.init_with_protocol ~protocol `Client () in
  let repeat_n_times n str start finish =
    start ^ (List.init n (fun _ -> str) |> String.concat " ") ^ finish
  in
  let value = repeat_n_times 250 "UNIT; DROP; " "{" "}" in
  let* hash =
    Client.register_global_constant
      ~src:"bootstrap1"
      ~value
      ?burn_cap:(Some (Tez.of_int 100))
      client
  in
  let* () = Client.bake_for_and_wait client in
  let value =
    repeat_n_times 10 (Format.sprintf "constant \"%s\";" hash) "{" "}"
  in
  let* hash =
    Client.register_global_constant
      ~src:"bootstrap1"
      ~value
      ?burn_cap:(Some (Tez.of_int 100))
      client
  in
  let* () = Client.bake_for_and_wait client in
  let value =
    repeat_n_times 10 (Format.sprintf "constant \"%s\";" hash) "{" "}"
  in
  let proc =
    Client.spawn_register_global_constant
      ~src:"bootstrap1"
      ~value
      ?burn_cap:(Some (Tez.of_int 100))
      client
  in
  let* _ =
    Process.check_error ~msg:(rex "larger than the expression size limit") proc
  in
  let* _ = Client.bake_for_and_wait client in
  (* Same test but for a contract

     Note the contract is ill-typed, having a extra CDR command.
     We expect the size error to occur before and prevent the type error. *)
  let prg =
    Format.sprintf
      "parameter unit; storage unit; code {%s; CDR; CDR; NIL operation; PAIR}"
      value
  in
  let proc =
    Client.spawn_originate_contract
      ~alias:"too_big"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 9999999)
      client
  in
  let* _ =
    Process.check_error ~msg:(rex "larger than the expression size limit") proc
  in
  Client.bake_for_and_wait client

let test_entrypoint_expansion =
  Protocol.register_test
    ~__FILE__
    ~title:"Global constants are expanded on entrypoints RPC"
    ~tags:["global_constant"; "rpc"]
  @@ fun protocol ->
  let* _, client = Client.init_with_protocol ~protocol `Client () in
  (* Register the expression *)
  let* _ =
    Client.register_global_constant
      ~src:"bootstrap1"
      ~value:"unit"
      ?burn_cap:(Some (Tez.of_int 100))
      client
  in
  let* () = Client.bake_for_and_wait client in
  (* Register a contract that uses the expression. *)
  let* _alias, contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"0x0054048627dedcf810b65da3ac50dfd068427615d7"
      ~burn_cap:Tez.(of_int 9999999)
      client
      ["mini_scenarios"; "constant_entrypoints"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  (* Get the entrypoints. *)
  let* result =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_entrypoints ~id:contract ()
  in
  let open JSON in
  let entrypoints =
    result |-> "entrypoints" |> as_object |> List.map fst
    |> List.sort String.compare
  in
  if entrypoints = ["default"; "do"] then return ()
  else Test.fail "Expected to find two entrypoints: 'do' and 'default'"

let register ~protocols =
  test_large_flat_contract protocols ;
  test_billion_laughs_contract protocols ;
  test_entrypoint_expansion protocols
