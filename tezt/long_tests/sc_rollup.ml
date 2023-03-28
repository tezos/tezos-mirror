(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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
   Component:    Smart Contract Optimistic Rollups
   Invocation:   dune exec tezt/long_tests/main.exe -- --file sc_rollup.ml
*)

open Base

let hooks = Tezos_regression.hooks

(*

   Helpers
   =======

*)

let hex_encode (input : string) : string =
  match Hex.of_string input with `Hex s -> s

(* [read_kernel filename] reads binary encoded WebAssembly module (e.g. `foo.wasm`)
   and returns a hex-encoded Wasm PVM boot sector, suitable for passing to
   [originate_sc_rollup] or [with_fresh_rollup].

   See also [wasm_incomplete_kernel_boot_sector].

   Note that this uses [Tezos_scoru_wasm.Gather_floppies.Complete_kernel], so
   the kernel must fit into a single Tezos operation.
*)
let read_kernel name : string =
  let open Tezt.Base in
  let kernel_file =
    project_root // Filename.dirname __FILE__
    // "../../src/proto_alpha/lib_protocol/test/integration/wasm_kernel"
    // (name ^ ".wasm")
  in
  hex_encode (read_file kernel_file)

type sc_rollup_constants = {
  origination_size : int;
  challenge_window_in_blocks : int;
  max_number_of_messages_per_commitment_period : int;
  stake_amount : Tez.t;
  commitment_period_in_blocks : int;
  max_lookahead_in_blocks : int32;
  max_active_outbox_levels : int32;
  max_outbox_messages_per_level : int;
  number_of_sections_in_dissection : int;
  timeout_period_in_blocks : int;
}

(* List of scoru errors messages used in tests below. *)

let make_parameter name = function
  | None -> []
  | Some value -> [([name], `Int value)]

let regression_test ~executors ~__FILE__ ?(tags = []) title f =
  let tags = "sc_rollup" :: tags in
  Protocol.register_long_test ~executors ~__FILE__ ~title ~tags f

let setup ?commitment_period ?challenge_window ?timeout f ~protocol =
  let parameters =
    make_parameter "smart_rollup_commitment_period_in_blocks" commitment_period
    @ make_parameter "smart_rollup_challenge_window_in_blocks" challenge_window
    @ make_parameter "smart_rollup_timeout_period_in_blocks" timeout
    @ [(["smart_rollup_enable"], `Bool true)]
  in
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base parameters in
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ~nodes_args ()
  in
  let operator = Constant.bootstrap1.alias in
  f node client operator

let send_message client msg =
  let* () =
    Client.Sc_rollup.send_message
      ~hooks
      ~src:Constant.bootstrap2.alias
      ~msg
      client
  in
  Client.bake_for_and_wait client

let originate_sc_rollup ?(hooks = hooks) ?(burn_cap = Tez.(of_int 9999999))
    ?(src = "bootstrap1") ?(kind = "arith") ?(parameters_ty = "string")
    ~boot_sector client =
  let* sc_rollup =
    Client.Sc_rollup.(
      originate ~hooks ~burn_cap ~src ~kind ~parameters_ty ~boot_sector client)
  in
  let* () = Client.bake_for_and_wait client in
  return sc_rollup

let with_fresh_rollup ~protocol ?kind ~boot_sector f tezos_node tezos_client
    operator =
  let* sc_rollup =
    originate_sc_rollup ?kind ~boot_sector ~src:operator tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~protocol
      Operator
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~default_operator:operator
  in
  let* configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node sc_rollup
  in
  f sc_rollup sc_rollup_node configuration_filename

(** This helper injects an SC rollup origination via octez-client. Then it
    bakes to include the origination in a block. It returns the address of the
    originated rollup *)
let originate_sc_rollup ?(hooks = hooks) ?(burn_cap = Tez.(of_int 9999999))
    ?(src = "bootstrap1") ?(kind = "arith") ?(parameters_ty = "string")
    ~boot_sector client =
  let* sc_rollup =
    Client.Sc_rollup.(
      originate ~hooks ~burn_cap ~src ~kind ~parameters_ty ~boot_sector client)
  in
  let* () = Client.bake_for_and_wait client in
  return sc_rollup

let test_rollup_node_advances_pvm_state protocols ~test_name ~boot_sector
    ~internal ~kind =
  let go ~protocol ~internal client sc_rollup sc_rollup_node =
    let* genesis_info =
      RPC.Client.call ~hooks client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
           sc_rollup
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in

    let* () = Sc_rollup_node.run sc_rollup_node [] in
    let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in

    let* level =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node init_level
    in
    Check.(level = init_level)
      Check.int
      ~error_msg:"Current level has moved past origination level (%L = %R)" ;
    let* level, forwarder =
      if not internal then return (level, None)
      else
        (* Originate forwarder contract to send internal messages to rollup *)
        let* contract_id =
          Client.originate_contract
            ~alias:"rollup_deposit"
            ~amount:Tez.zero
            ~src:Constant.bootstrap1.alias
            ~prg:"file:./tezt/tests/contracts/proto_alpha/sc_rollup_forward.tz"
            ~init:"Unit"
            ~burn_cap:Tez.(of_int 1)
            client
        in
        let* () = Client.bake_for_and_wait client in
        Log.info
          "The forwarder %s contract was successfully originated"
          contract_id ;
        return (level + 1, Some contract_id)
    in
    (* Called with monotonically increasing [i] *)
    let test_message i =
      let*! prev_state_hash =
        Sc_rollup_client.state_hash ~hooks sc_rollup_client
      in
      let*! prev_ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
      let message = sf "%d %d + value" i ((i + 2) * 2) in
      let* () =
        match forwarder with
        | None ->
            (* External message *)
            send_message client (sf "[%S]" message)
        | Some forwarder ->
            (* Internal message through forwarder *)
            let* () =
              Client.transfer
                client
                ~amount:Tez.zero
                ~giver:Constant.bootstrap1.alias
                ~receiver:forwarder
                ~arg:(sf "Pair %S %S" message sc_rollup)
            in
            Client.bake_for_and_wait client
      in
      let* _ = Sc_rollup_node.wait_for_level sc_rollup_node (level + i) in

      (* specific per kind PVM checks *)
      let* () =
        match kind with
        | "arith" ->
            let*! encoded_value =
              Sc_rollup_client.state_value
                ~hooks
                sc_rollup_client
                ~key:"vars/value"
            in
            let value =
              match Data_encoding.(Binary.of_bytes int31) @@ encoded_value with
              | Error error ->
                  failwith
                    (Format.asprintf
                       "The arithmetic PVM has an unexpected state: %a"
                       Data_encoding.Binary.pp_read_error
                       error)
              | Ok x -> x
            in
            Check.(
              (value = i + ((i + 2) * 2))
                int
                ~error_msg:"Invalid value in rollup state (%L <> %R)") ;
            return ()
        | "wasm_2_0_0" ->
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/3729

                Add an appropriate check for various test kernels

                computation.wasm               - Gets into eval state
                no_parse_random.wasm           - Stuck state due to parse error
                no_parse_bad_fingerprint.wasm  - Stuck state due to parse error
            *)
            return ()
        | _otherwise -> raise (Invalid_argument kind)
      in

      let*! state_hash = Sc_rollup_client.state_hash ~hooks sc_rollup_client in
      Check.(state_hash <> prev_state_hash)
        Check.string
        ~error_msg:"State hash has not changed (%L <> %R)" ;

      let*! ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
      Check.(ticks >= prev_ticks)
        Check.int
        ~error_msg:"Tick counter did not advance (%L >= %R)" ;

      Lwt.return_unit
    in
    let* () = Lwt_list.iter_s test_message (range 1 10) in

    Lwt.return_unit
  in

  if not internal then
    regression_test
      ~__FILE__
      ~tags:["sc_rollup"; "run"; "node"; kind]
      test_name
      (fun protocol ->
        setup ~protocol @@ fun node client ->
        with_fresh_rollup
          ~protocol
          ~kind
          ~boot_sector
          (fun sc_rollup_address sc_rollup_node _filename ->
            go ~protocol ~internal:false client sc_rollup_address sc_rollup_node)
          node
          client)
      protocols
  else
    regression_test
      ~__FILE__
      ~tags:["sc_rollup"; "run"; "node"; "internal"; kind]
      test_name
      (fun protocol ->
        setup ~protocol @@ fun node client ->
        with_fresh_rollup
          ~protocol
          ~kind
          ~boot_sector
          (fun sc_rollup_address sc_rollup_node _filename ->
            go ~protocol ~internal:true client sc_rollup_address sc_rollup_node)
          node
          client)
      protocols

let test_rollup_node_run_with_kernel protocols ~executors ~kind ~kernel_name
    ~internal =
  test_rollup_node_advances_pvm_state
    protocols
    ~executors
    ~test_name:(Format.asprintf "%s - runs with kernel - %s" kind kernel_name)
    ~boot_sector:(read_kernel kernel_name)
    ~internal
    ~kind

let register ~executors ~protocols =
  test_rollup_node_run_with_kernel
    protocols
    ~timeout:(Minutes 300)
    ~executors
    ~kind:"wasm_2_0_0"
    ~kernel_name:"computation"
    ~internal:false
