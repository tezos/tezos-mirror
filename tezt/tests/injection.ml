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
   Component:    Injection and activation
   Invocation:   dune exec tezt/tests/main.exe -- --file injection.ml
   Subject:      This tests the compilation, injection and activation of a test
                 protocol in a node and the propagation of that protocol to a
                 network.
*)

let protocol_path = "src/bin_client/test/proto_test_injection"

let is_static_binary execname =
  let* output = Process.run_and_read_stdout "file" [execname] in
  (* Example of line returned by "file": *)
  (* octez-node: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), *)
  (* statically linked, no section header *)
  return (output =~ rex "statically linked")

let test_injection_and_activation () : unit =
  Test.register
    ~__FILE__
    ~title:"protocol injection and activation"
    ~tags:
      [
        Tag.layer1;
        "protocol";
        "injection";
        "activation";
        "network";
        "not_static";
      ]
    ~uses:[Constant.octez_protocol_compiler]
  @@ fun () ->
  Log.info "Check protocol compiler and protocol availability" ;
  Check.file_exists ~__LOC__ protocol_path ;
  Check.file_exists ~__LOC__ (Uses.path Constant.octez_protocol_compiler) ;
  let* compiler_is_static =
    is_static_binary (Uses.path Constant.octez_protocol_compiler)
  in
  Check.is_false
    ~__LOC__
    compiler_is_static
    ~error_msg:"Test cannot run when protocol compiler is statically compiled" ;

  Log.info "Compute hash of protocol %s" protocol_path ;
  let* protocol_hash =
    Protocol_compiler.compile ~hash_only:true protocol_path
  in
  Check.(
    (String.length protocol_hash = 51)
      int
      ~__LOC__
      ~error_msg:"Expected length of protocol hash to be %R, got %L") ;

  Log.info "Start network of 3 sandboxed nodes running protocol genesis" ;
  let nodes = Cluster.create 3 [] in
  let node1 = List.hd nodes in
  Cluster.clique nodes ;
  let* () = Cluster.start ~wait_connections:true nodes in
  let* client1 = Client.init ~endpoint:(Node node1) () in

  Log.info "Inject protocol %s with hash %s" protocol_path protocol_hash ;
  let* () =
    let* injected_proto_hash =
      Client.Admin.inject_protocol ~protocol_path client1
    in
    Check.(
      (injected_proto_hash = protocol_hash)
        string
        ~__LOC__
        ~error_msg:
          "Expected injected protocol hash %L to equal compiled protocol hash \
           %R") ;
    unit
  in

  Log.info "Check environment version" ;
  let expected_env_version =
    sf
      "V%d"
      JSON.(
        parse_file (protocol_path ^ "/TEZOS_PROTOCOL")
        |-> "expected_env_version" |> as_int)
  in
  let* env_version = Client.Admin.protocol_environment client1 protocol_hash in
  Check.(
    (expected_env_version = env_version)
      string
      ~__LOC__
      ~error_msg:"Expected environment version %L of injected protocol, got %R") ;

  Log.info "Activate injected protocol %s" protocol_hash ;
  let parameter_file = Temp.file (protocol_hash ^ "-parameters.json") in
  Base.write_file parameter_file ~contents:"{}" ;
  let* () =
    Client.activate_protocol_and_wait
      ~parameter_file
      ~protocol_hash
      ~fitness:1
      ~key:"activator"
      ~timestamp:Client.Now
      client1
  in

  let* activation_block_level = Node.get_level node1 in
  Log.info
    "Wait for activation block propagation at level %d"
    activation_block_level ;
  let* () =
    Lwt_list.iter_s
      (fun node ->
        let* (_ : int) = Node.wait_for_level node activation_block_level in
        let* (metadata : RPC.block_metadata) =
          Client.RPC.call ~protocol_hash:Protocol.genesis_hash client1
          @@ RPC.get_chain_block_metadata ()
        in
        Log.info
          "Check that next_protocol for node %s at level %d is %s"
          (Node.name node)
          activation_block_level
          protocol_hash ;
        Check.(
          (metadata.next_protocol = protocol_hash)
            string
            ~__LOC__
            ~error_msg:
              "Expected next block to activate protocol %R, instead found %L") ;
        unit)
      nodes
  in
  unit

let protocol_hash_zero = "PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i"

let test_activation () : unit =
  Test.register
    ~__FILE__
    ~title:"protocol activation (protocol already linked to the node)"
    ~tags:["protocol"; "activation"]
  @@ fun () ->
  let protocol_hash = Protocol.demo_noops_hash in
  Log.info "Check that %s is known" protocol_hash ;
  let* _, client = Client.init_with_node `Client () in
  let* protocols = Client.Admin.list_protocols client in
  Check.(
    list_mem
      string
      ~__LOC__
      Protocol.demo_noops_hash
      protocols
      ~error_msg:"Expected %R to be in the set of known protocols %L") ;

  Log.info "Check that first protocol has the zeroth protocol hash" ;
  let* metadata =
    Client.RPC.call ~protocol_hash:Protocol.genesis_hash client
    @@ RPC.get_chain_block_metadata ()
  in
  Check.(
    (metadata.protocol = protocol_hash_zero)
      string
      ~__LOC__
      ~error_msg:"Expected first block to have hash %R, instead found %L") ;

  Log.info "Activate protocol demo noops" ;
  let parameter_file = Temp.file (protocol_hash ^ "-parameters.json") in
  Base.write_file parameter_file ~contents:"{}" ;
  let* () =
    Client.activate_protocol_and_wait
      ~parameter_file
      ~protocol_hash
      ~fitness:1
      ~key:"activator"
      ~timestamp:Client.Now
      client
  in

  Log.info "Check that protocol of activation block is genesis" ;
  let* metadata =
    Client.RPC.call ~protocol_hash:Protocol.genesis_hash client
    @@ RPC.get_chain_block_metadata ()
  in
  Check.(
    (metadata.protocol = Protocol.genesis_hash)
      string
      ~__LOC__
      ~error_msg:
        "Expected activation block to run protocol %R, instead found %L") ;

  unit

let register_protocol_independent () =
  test_injection_and_activation () ;
  test_activation ()
