(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Prevalidator
    Invocation:   dune exec src/lib_shell/test/main.exe \
                  -- --file test_prevalidator.ml
    Subject:      Unit tests for {!Prevalidator}
*)

let register_test ~title ~additional_tags =
  Test.register
    ~__FILE__
    ~title:("Shell: Prevalidator: " ^ title)
    ~tags:(["mempool"; "prevalidator"] @ additional_tags)

type op_data = High | Medium | Low

(** Same as [Mock_all_unit] except that [operation_data] is [High | Medium |
    Low] and that the mempool accept every operation. *)
module Mock_protocol :
  Tezos_protocol_environment.PROTOCOL
    with type operation_data = op_data
     and type operation_receipt = unit
     and type validation_state = unit
     and type application_state = unit = struct
  include
    Tezos_protocol_environment.Internal_for_tests.Environment_protocol_T_test
    .Mock_all_unit

  type operation_data = op_data

  type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
  }

  let max_operation_data_length = 33

  let operation_data_encoding : operation_data Data_encoding.t =
    let open Data_encoding in
    union
      [
        case
          ~title:"high"
          (Tag 0)
          (constant "high")
          (function High -> Some () | _ -> None)
          (fun () -> High);
        case
          ~title:"medium"
          (Tag 1)
          (constant "medium")
          (function Medium -> Some () | _ -> None)
          (fun () -> Medium);
        case
          ~title:"low"
          (Tag 2)
          (constant "low")
          (function Low -> Some () | _ -> None)
          (fun () -> Low);
      ]

  let operation_data_encoding_with_legacy_attestation_name =
    operation_data_encoding

  let operation_data_and_receipt_encoding =
    Data_encoding.conv fst (fun n -> (n, ())) operation_data_encoding

  let operation_data_and_receipt_encoding_with_legacy_attestation_name =
    operation_data_and_receipt_encoding

  let acceptable_pass _ = assert false

  let compare_operations (oph1, {shell = _; protocol_data = n1})
      (oph2, {shell = _; protocol_data = n2}) =
    Compare.or_else
      (match (n1, n2) with
      | High, High | Medium, Medium | Low, Low -> 0
      | High, (Medium | Low) | Medium, Low -> 1
      | Medium, High | Low, (High | Medium) -> -1)
      (fun () -> Operation_hash.compare oph1 oph2)

  let validate_operation ?check_signature:_ = assert false

  let apply_operation _ = assert false

  module Mempool = struct
    include Mempool

    let init _ _ ~head_hash:_ ~head:_ ~cache:_ = Lwt_result.return ((), ())

    type conflict_handler =
      existing_operation:Tezos_crypto.Hashed.Operation_hash.t * operation ->
      new_operation:Tezos_crypto.Hashed.Operation_hash.t * operation ->
      [`Keep | `Replace]

    let add_operation ?check_signature:_ ?conflict_handler:_ _ _ _ =
      Lwt_result_syntax.return ((), Added)

    let merge ?conflict_handler:_ () () = assert false

    let operations () = assert false
  end
end

module Wrap_protocol (Proto : Tezos_protocol_environment.PROTOCOL) :
  Protocol_plugin.T
    with type operation_data = Proto.operation_data
     and type operation = Proto.operation
     and type Mempool.t = Proto.Mempool.t = Protocol_plugin.No_plugin (struct
  let hash = Protocol_hash.zero

  include Proto

  let complete_b58prefix _ = assert false
end)

(** Same as [No_plugin] except that [pre_filter] returns a priority that match
    the [protocol_data]. *)
module Plugin = struct
  include Wrap_protocol (Mock_protocol)

  module Plugin = struct
    include Plugin

    let pre_filter _ _ {shell = _; protocol_data} =
      Lwt.return
      @@ `Passed_prefilter
           (match protocol_data with
           | High -> `High
           | Medium -> `Medium
           | Low -> `Low [])
  end
end

(* Helper to create a mocked ddb *)
let create_ddb () =
  Lwt_utils_unix.with_tempdir "tezos_test_" (fun test_dir ->
      let open Lwt_result_syntax in
      let*! store = Shell_test_helpers.init_chain test_dir in
      let* p2p =
        Shell_test_helpers.init_mock_p2p Distributed_db_version.Name.zero
      in
      let db = Distributed_db.create store p2p in
      let chain_store = Store.(main_chain_store store) in
      let callback =
        P2p_reader.
          {
            notify_branch = (fun _ _ -> ());
            notify_head = (fun _ _ _ _ -> ());
            disconnection = (fun _ -> ());
          }
      in
      return (Distributed_db.activate db chain_store callback))

let create_tools (advertised : Mempool.t ref) chain_db =
  let open Lwt_result_syntax in
  let advertise_current_head ~(mempool : Mempool.t) _ =
    advertised :=
      Mempool.
        {
          known_valid =
            Operation_hash.Set.union !advertised.known_valid mempool.known_valid;
          pending = Operation_hash.Set.union !advertised.pending mempool.pending;
        }
  in
  let chain_tools = Prevalidator.Internal_for_tests.mk_chain_tools chain_db in
  let fetch ?peer:_ ?timeout:_ _oph = assert false in
  let read_block bh =
    let chain_store = Distributed_db.chain_store chain_db in
    Store.Block.read_block chain_store bh
  in
  let send_get_current_head ?peer:_ () = assert false in
  let set_mempool ~head:_ _mempool = return_unit in
  Prevalidator.Internal_for_tests.Tools.
    {
      advertise_current_head;
      chain_tools;
      fetch;
      read_block;
      send_get_current_head;
      set_mempool;
    }

let rec wait_for_empty_request_queue prevalidator =
  let open Lwt_syntax in
  match Prevalidator.pending_requests prevalidator with
  | [] -> return_unit
  | _ ->
      let* () = Lwt_unix.sleep 0.1 in
      wait_for_empty_request_queue prevalidator

let test_advertisement () =
  let open Lwt_result_syntax in
  let*! r =
    let prevalidator_limits = Shell_limits.default_prevalidator_limits in
    let* chain_db = create_ddb () in
    let advertised = ref Mempool.empty in
    let tools = create_tools advertised chain_db in
    let* prevalidator =
      Prevalidator.Internal_for_tests.create
        tools
        prevalidator_limits
        (module Plugin)
        chain_db
    in
    let branch =
      Block_hash.of_b58check_exn
        "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"
    in
    let forge_and_inject_operation kind =
      let proto =
        Data_encoding.Binary.to_bytes_exn
          Mock_protocol.operation_data_encoding
          kind
      in
      let op = Operation.{shell = {branch}; proto} in
      Prevalidator.inject_operation prevalidator ~force:false op
    in
    Format.printf "Inject High priority operation@." ;
    let* () = forge_and_inject_operation High in
    Format.printf "Inject Medium priority operation@." ;
    let* () = forge_and_inject_operation Medium in
    Format.printf "Inject Low priority operation@." ;
    let* () = forge_and_inject_operation Low in
    (* Injection of operation always set the priority of operation to high. *)
    Format.printf "Advertising injected operations classified as known_valid@." ;
    let*! () = Prevalidator.Internal_for_tests.advertise_mempool prevalidator in
    let*! () = wait_for_empty_request_queue prevalidator in
    assert (Operation_hash.Set.cardinal !advertised.pending = 0) ;
    assert (Operation_hash.Set.cardinal !advertised.known_valid = 3) ;
    advertised := Mempool.empty ;
    Format.eprintf
      "Flushing the mempool, pre_filter is called so that the operation have \
       their correct priority@." ;
    let* () =
      Prevalidator.flush
        prevalidator
        Chain_validator_worker_state.Head_increment
        branch
        (Block_hash.Set.singleton branch)
        Operation_hash.Set.empty
    in
    let*! () = wait_for_empty_request_queue prevalidator in
    Format.printf
      "Advertising only reclassified operations with high priority as \
       known_valid@." ;
    let*! () = Prevalidator.Internal_for_tests.advertise_mempool prevalidator in
    let*! () = wait_for_empty_request_queue prevalidator in
    assert (Operation_hash.Set.cardinal !advertised.pending = 0) ;
    assert (Operation_hash.Set.cardinal !advertised.known_valid = 1) ;
    return_unit
  in
  match r with Error errs -> Test.fail "%a" pp_print_trace errs | Ok _ -> unit

let () =
  let register_test ~title ~additional_tags test =
    register_test ~title ~additional_tags @@ fun () -> test ()
  in
  register_test
    ~title:"Advertisement"
    ~additional_tags:["prevalidator"; "advertisement"]
    test_advertisement
