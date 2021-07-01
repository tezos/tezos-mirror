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

(* Testing
   -------
   Component: Node
   Invocation: dune exec tezt/tests/main.exe -- --file qcheck_rpc.ml
   Subject: Property testing the RPC server
  *)

(* Generate random data for RPC calls *)
module Gen = struct
  (* The number of random inputs we generate for each RPC. *)
  let num_rand = 50

  let strings () : string list =
    let open QCheck.Gen in
    generate ~n:num_rand string_readable

  let bootstrap_json : Ezjsonm.t list =
    let with_bool (b : bool) : Ezjsonm.t =
      Ezjsonm.dict [("bootstrapped", Ezjsonm.bool b)]
    in
    [with_bool true; with_bool false]
end

module RPC = struct
  (* Call an rpc, allowing for failure without calling [Tezt.Test.fail].
     We need this because we are testing the resilience of the node; the
     test fails iff the node given random-input RPC calls (which will almost
     always fail) crashes.
  *)
  let call_rpc ?data meth path client :
      (JSON.t, Unix.process_status) result Lwt.t =
    let proc = Client.spawn_rpc ?data meth path client in
    let* exit_code = Process.wait proc in
    match exit_code with
    | WEXITED 0 ->
        let* output = Lwt_io.read @@ Process.stdout proc in
        let origin = Client.string_of_path path ^ " response" in
        return @@ Ok (JSON.parse ~origin output)
    | _ -> return @@ Error exit_code

  (* Check if the node attached to a client is still alive.

     We assume that if the vanilla node (not proxy) is still
     alive it will return something for "rpc get /version". *)
  let check_node_alive client log : bool Lwt.t =
    let* result = call_rpc Client.GET ["version"] client in
    match result with
    | Ok _ -> return true
    | _ ->
        log () ;
        Tezt.Test.fail "Node crashed"

  (* Property test an rpc with random input.

     Here, [log_and_rpc] is of type
       ['a -> Client.t ->
        ('a -> (), (JSON.t, Unix.process_status ) result Lwt.t)]
     for some concrete ['a]. All tests are of this type.
  *)
  let test_rpc protocol rand_input log_and_rpc : unit Lwt.t =
    let* node = Node.init [] in
    let* client = Client.init ~endpoint:(Node node) () in
    let* () = Client.activate_protocol ~protocol client in
    let single_call a =
      let open Lwt in
      let (log, lwt_rpc_result) = log_and_rpc a client in
      let* _ = lwt_rpc_result in
      check_node_alive client (fun () -> log a) >>= fun _ -> unit
    in
    Lwt_list.iter_s single_call rand_input

  let get_constants block_id client =
    let path = ["chains"; "main"; "blocks"; block_id; "context"; "constants"] in
    let log block_id = Log.info "get_constants:block_id=%s" block_id in
    (log, call_rpc Client.GET path client)

  let get_context_raw block_id client =
    let path =
      ["chains"; "main"; "blocks"; block_id; "context"; "raw"; "bytes"]
    in
    let log_get_context_raw block_id =
      Log.info "get_context:block_id=%s" block_id
    in
    (log_get_context_raw, call_rpc Client.GET path client)

  let get_protocol_data block_id client =
    let path =
      ["chains"; "main"; "blocks"; block_id; "header"; "protocol_data"]
    in
    let log block_id = Log.info "get_protocol_data:block_id=%s" block_id in
    (log, call_rpc Client.GET path client)

  let patch_boostrapped data client =
    let data = Ezjsonm.value data in
    let log data =
      let get_flag ezjsonm =
        Ezjsonm.value ezjsonm |> Ezjsonm.get_dict |> List.hd
        |> fun (_, ezjsonmbool) -> Ezjsonm.get_bool ezjsonmbool
      in
      Log.info "failed_set_bootstrapped:%b" (get_flag data)
    in
    (log, call_rpc ~data Client.PATCH ["chains"; "main"] client)
end

let fuzz_basic_rpcs =
  Protocol.register_test
    ~__FILE__
    ~title:"fuzz_rpcs"
    ~tags:["node"; "fuzz"; "rpcs"]
  @@ fun protocol ->
  (* Fuzz a few basic RPCs to see if we can crash the node. *)
  let* () =
    let block_ids = Gen.strings () in
    RPC.test_rpc protocol block_ids RPC.get_constants
  in
  let* () =
    let block_ids = Gen.strings () in
    RPC.test_rpc protocol block_ids RPC.get_context_raw
  in
  let* () =
    let block_ids = Gen.strings () in
    RPC.test_rpc protocol block_ids RPC.get_protocol_data
  in
  let* () =
    let json_data = Gen.bootstrap_json in
    RPC.test_rpc protocol json_data RPC.patch_boostrapped
  in
  unit

let register ~protocols = fuzz_basic_rpcs ~protocols
