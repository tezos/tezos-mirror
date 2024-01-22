(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe  <gb.fefe@protonmail.com>                    *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Demo protocols counter and noops
   Invocation:   dune exec tezt/tests/main.exe -- --file demo_protocols.ml
   Subject:      Minimal tests of demo protocols counter and noops
*)

let check_protocol ?protocol_hash ~__LOC__ client expected =
  let* {protocol; _} =
    Client.RPC.call ?protocol_hash client @@ RPC.get_chain_block_metadata ()
  in
  Check.(
    (protocol = expected)
      string
      ~__LOC__
      ~error_msg:"Expected head protocol to be %R, got %L") ;
  unit

let check_level ?protocol_hash ~__LOC__ client expected =
  let* header =
    Client.RPC.call ?protocol_hash client @@ RPC.get_chain_block_header ()
  in
  let level = JSON.(header |-> "level" |> as_int) in
  Check.(
    (level = expected)
      int
      ~__LOC__
      ~error_msg:"Expected head level to be %R, got %L") ;
  unit

let check_known_protocols ~__LOC__ client expected_known_protocol =
  let* protocols = Client.Admin.list_protocols client in
  Check.(
    list_mem
      string
      expected_known_protocol
      protocols
      ~__LOC__
      ~error_msg:
        "Expected to find protocol hash %L to be in the list of protocols") ;
  unit

let check_understood_protocols ~__LOC__ client expected_understood_protocol =
  let* protocols = Client.list_understood_protocols client in
  Check.(
    list_mem
      string
      (String.sub expected_understood_protocol 0 12)
      protocols
      ~__LOC__
      ~error_msg:
        "Expected to find protocol hash %L to be in the list of understood \
         protocols") ;
  unit

module Demo_counter = struct
  let check_a ?__LOC__ client expected =
    let* a = Demo_client.get_a client in
    Check.((a = expected) ?__LOC__ int)
      ~error_msg:"expected amount for a = %R, got %L " ;
    unit

  let check_b ?__LOC__ client expected =
    let* b = Demo_client.get_b client in
    Check.((b = expected) ?__LOC__ int)
      ~error_msg:"expected amount for b = %R, got %L" ;
    unit

  let register () =
    Test.register ~__FILE__ ~title:"demo_counter" ~tags:["demo_counter"]
    @@ fun () ->
    let* node = Node.init [Synchronisation_threshold 0] in
    let* client = Client.init ~endpoint:(Node node) () in
    let demo_counter_hash = Protocol.demo_counter_hash in
    let* () = check_known_protocols ~__LOC__ client demo_counter_hash in
    let* () = check_understood_protocols ~__LOC__ client demo_counter_hash in
    let* () = check_protocol ~__LOC__ client Protocol.protocol_zero_hash in
    let* () = Demo_client.activate client in
    let* () = check_level ~__LOC__ client 1 in
    let* () = check_protocol ~__LOC__ client Protocol.genesis_hash in
    let* () = Demo_client.bake client ~msg:"This is block 2" in
    let* () = check_level ~__LOC__ client 2 in
    let* () = check_a ~__LOC__ client 0 in
    let* () = check_b ~__LOC__ client 0 in
    let* () = Demo_client.increment_a client in
    let* mempool = Mempool.get_mempool client in
    Check.(
      (List.length mempool.validated = 1)
        int
        ~__LOC__
        ~error_msg:"Expected %R validated operations, got %L") ;
    let* () = Demo_client.bake client in
    let* () = check_a ~__LOC__ client 1 in
    let* () = check_b ~__LOC__ client 0 in
    let* () = Demo_client.increment_a client in
    let* () = Demo_client.bake client in
    let* () = check_a ~__LOC__ client 2 in
    let* () = check_b ~__LOC__ client 0 in
    let* () = Demo_client.increment_b client in
    let* () = Demo_client.bake client in
    let* () = check_a ~__LOC__ client 2 in
    let* () = check_b ~__LOC__ client 1 in
    let* () = Demo_client.transfer client 2 in
    let* () = Demo_client.bake client in
    let* () = check_a ~__LOC__ client 0 in
    let* () = check_b ~__LOC__ client 3 in
    let* () = Demo_client.transfer client ~-1 in
    let* () = Demo_client.bake client in
    let* () = check_a ~__LOC__ client 1 in
    let* () = check_b ~__LOC__ client 2 in
    return ()
end

module Demo_noops = struct
  let forge_block_header_data (protocol_data : JSON.u) =
    match protocol_data with
    | `O [("block_header_data", `String block_header_data)] ->
        let tag = "0000" in
        let padded_hex_len =
          Printf.sprintf "%04x" (String.length block_header_data)
        in
        let block_header_data_hex =
          Hex.show (Hex.of_string block_header_data)
        in
        tag ^ padded_hex_len ^ block_header_data_hex
    | _ ->
        Test.fail
          "[forge_block_header_data] unexpected block header: %s"
          (JSON.encode_u protocol_data)

  let register () =
    Test.register ~__FILE__ ~title:"demo_noops" ~tags:["demo_noops"]
    @@ fun () ->
    let* node = Node.init [Synchronisation_threshold 0] in
    let* client = Client.init ~endpoint:(Node node) () in
    let protocol_hash = Protocol.demo_noops_hash in
    let* () = check_known_protocols ~__LOC__ client protocol_hash in
    let* () = check_protocol ~__LOC__ client Protocol.protocol_zero_hash in
    let* () =
      let parameter_file = Temp.file "demo-noops-parameters.json" in
      Base.write_file ~contents:"{}" parameter_file ;
      Client.activate_protocol_and_wait ~protocol_hash ~parameter_file client
    in
    let* () =
      check_level ~protocol_hash:Protocol.genesis_hash ~__LOC__ client 1
    in
    let* () =
      check_protocol
        ~protocol_hash:Protocol.genesis_hash
        ~__LOC__
        client
        Protocol.genesis_hash
    in
    Log.info "Forge and bake a block" ;
    let* () =
      let genesis_rpc_call =
        Client.RPC.call ~protocol_hash:Protocol.genesis_hash client
      in
      let message = "Hello world" in
      let data =
        `O
          [
            ( "protocol_data",
              `O
                [
                  ("protocol", `String protocol_hash);
                  ("block_header_data", `String message);
                ] );
            ("operations", `A []);
          ]
      in
      let* block =
        genesis_rpc_call
        @@ RPC.post_chain_block_helpers_preapply_block ~data:(Data data) ()
      in
      let protocol_data = `O [("block_header_data", `String message)] in
      let encoded = forge_block_header_data protocol_data in
      let shell_header =
        JSON.(
          block |-> "shell_header"
          |> put
               ( "protocol_data",
                 JSON.annotate ~origin:"shell_header" @@ `String encoded )
          |> unannotate)
      in
      let* encoded =
        genesis_rpc_call
        @@ RPC.post_chain_block_helpers_forge_block_header
             ~data:(Data shell_header)
             ()
      in
      let inject =
        `O
          [
            ("data", JSON.(encoded |-> "block" |> unannotate));
            ("operations", `A []);
          ]
      in
      let* (_ : JSON.t) =
        genesis_rpc_call @@ RPC.post_injection_block ~data:(Data inject)
      in
      unit
    in
    let* () =
      check_level ~protocol_hash:Protocol.genesis_hash ~__LOC__ client 2
    in
    return ()
end

let register () =
  Demo_counter.register () ;
  Demo_noops.register ()
