(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Tezlink_imports

(** We add to Imported_protocol the mocked protocol data used in headers *)
module Imported_protocol = struct
  include Imported_protocol

  let contents : Block_header_repr.contents =
    {
      payload_hash = Block_payload_hash.zero;
      payload_round = Round_repr.zero;
      seed_nonce_hash = None;
      proof_of_work_nonce =
        Bytes.make Constants_repr.proof_of_work_nonce_size '\000';
      per_block_votes =
        {
          liquidity_baking_vote = Per_block_vote_pass;
          adaptive_issuance_vote = Per_block_vote_pass;
        };
    }

  let signature : Alpha_context.signature =
    Unknown (Bytes.make Tezos_crypto.Signature.Ed25519.size '\000')

  let mock_protocol_data : Block_header_repr.protocol_data =
    {contents; signature}
end

module Block_services = struct
  include
    Tezos_shell_services.Block_services.Make
      (Imported_protocol)
      (Imported_protocol)

  let mock_block_header_data : Proto.block_header_data tzresult =
    Tezos_types.convert_using_serialization
      ~name:"block_header_data"
      ~dst:Proto.block_header_data_encoding
      ~src:Imported_protocol.Block_header_repr.protocol_data_encoding
      Imported_protocol.mock_protocol_data
end

module Zero_block_services = struct
  include
    Tezos_shell_services.Block_services.Make (Zero_protocol) (Genesis_protocol)

  let mock_block_header_data : Proto.block_header_data tzresult =
    Tezos_types.convert_using_serialization
      ~name:"block_header_data"
      ~dst:Proto.block_header_data_encoding
      ~src:Data_encoding.empty
      ()
end

module Genesis_block_services = struct
  include
    Tezos_shell_services.Block_services.Make
      (Genesis_protocol)
      (Imported_protocol)

  let mock_block_header_data : Proto.block_header_data tzresult =
    Result_syntax.return
      Genesis_protocol.
        {
          command =
            Genesis_protocol.Data.Command.Activate
              {
                protocol = Imported_protocol.hash;
                fitness = [];
                protocol_parameters = Bytes.empty;
              };
          signature = Signature.V0.zero;
        }
end

type level = Tezos_types.level

(* Provides mock values necessary for constructing L1 types that contain fields
    that are either irrelevant to the L2 or are not yet supported. *)
module Mock = struct
  let proto_level = 1

  let validation_passes = 4

  (* TODO #7866
     When blocks are populated, this mock value will be unnecessary,
     being replaced by actual data. *)
  let operations_hash =
    Operation_list_list_hash.of_bytes_exn (Bytes.make 32 '\000')

  let fitness =
    [
      Bytes.make 4 '\255';
      Bytes.make 4 '\255';
      Bytes.make 4 '\255';
      Bytes.make 4 '\255';
      Bytes.make 4 '\255';
    ]

  (* TODO #7866
     When blocks are populated, this mock value will be unnecessary,
     being replaced by actual data. *)
  let context = Context_hash.of_bytes_exn (Bytes.make 32 '\255')

  module Operation_metadata = struct
    open Imported_protocol.Apply_results
    open Alpha_context

    type error += Unsupported_operation_kind of string

    let () =
      register_error_kind
        `Permanent
        ~id:"evm_node.dev.tezlink.unsupported_operation_kind"
        ~title:"Unsupported operation kind"
        ~description:
          "In a RPC call, an operation of unsupported kind was given."
        ~pp:(fun ppf s -> Format.fprintf ppf "Unsupported operation kind: %s" s)
        Data_encoding.(obj1 (req "message" string))
        (function
          | Unsupported_operation_kind message -> Some message | _ -> None)
        (fun message -> Unsupported_operation_kind message)

    let consumed_gas = Gas.Arith.zero

    let manager_op_result (type kind) (contents : kind manager_operation) :
        kind successful_manager_operation_result tzresult =
      let open Result_syntax in
      match contents with
      | Reveal _ -> return (Reveal_result {consumed_gas})
      | Transaction _ ->
          return
            (Transaction_result
               (Transaction_to_contract_result
                  {
                    storage = None;
                    lazy_storage_diff = None;
                    balance_updates = [];
                    ticket_receipt = [];
                    originated_contracts = [];
                    consumed_gas;
                    storage_size = Z.zero;
                    paid_storage_size_diff = Z.zero;
                    allocated_destination_contract = true;
                  }))
      | _ ->
          tzfail
            (Unsupported_operation_kind
               "only supported kinds are 'reveal' and 'transaction'")

    let contents_result (type kind) (contents : kind contents) :
        kind contents_result tzresult =
      let open Result_syntax in
      match contents with
      | Manager_operation {operation; _} ->
          let* result = manager_op_result operation in
          return
            (Manager_operation_result
               {
                 balance_updates = [];
                 internal_operation_results = [];
                 operation_result = Applied result;
               })
      | _ ->
          tzfail
            (Unsupported_operation_kind "only manager operations are supported")

    let rec contents_list_result :
        type kind. kind contents_list -> kind contents_result_list tzresult =
     fun contents ->
      let open Result_syntax in
      match contents with
      | Single contents ->
          let* result = contents_result contents in
          return (Single_result result)
      | Cons (contents, contents_list) ->
          let* result = contents_result contents in
          let* result_list = contents_list_result contents_list in
          return (Cons_result (result, result_list))

    let operation_metadata (Operation_data op) =
      let open Result_syntax in
      let* contents = contents_list_result op.contents in
      return (Operation_metadata {contents})
  end

  (* When indexing Tezlink, Tzkt requires Liquidity baking contracts. *)
  module Liquidity_baking = struct
    let cpmm_address = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"

    let lqt_address = "KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

    let lq_fallback_token = "KT1VqarPDicMFn1ejmQqqshUkUXTCTXwmkCN"

    let cpmm_script : Alpha_context.Script.t =
      let cpmm_storage =
        let open Imported_protocol in
        Script_repr.lazy_expr
          (Imported_env.Micheline.strip_locations
             (Prim
                ( 0,
                  Michelson_v1_primitives.D_Pair,
                  [
                    Int (1, Z.one);
                    Int (2, Z.of_int 100);
                    Int (3, Z.of_int 100);
                    String (4, lq_fallback_token);
                    String (5, lqt_address);
                  ],
                  [] )))
      in
      {
        code =
          Imported_protocol.(Script_repr.lazy_expr Liquidity_baking_cpmm.script);
        storage = cpmm_storage;
      }

    let token_script : Alpha_context.Script.t =
      let open Imported_protocol in
      let lqt_storage =
        Script_repr.lazy_expr
          (Imported_env.Micheline.strip_locations
             (Prim
                ( 0,
                  Michelson_v1_primitives.D_Pair,
                  [
                    Int (1, Z.of_int 2);
                    Int (2, Z.of_int 3);
                    String (3, cpmm_address);
                    Int (4, Z.of_int 100);
                  ],
                  [] )))
      in
      {
        code =
          Imported_protocol.(Script_repr.lazy_expr Liquidity_baking_lqt.script);
        storage = lqt_storage;
      }

    let fallback_script : Alpha_context.Script.t =
      let open Imported_protocol in
      let fallback_token_storage =
        Script_repr.lazy_expr
          (Imported_env.Micheline.strip_locations
             (Prim
                ( 0,
                  Michelson_v1_primitives.D_Pair,
                  [
                    Int (1, Z.of_int 0);
                    Int (2, Z.of_int 1);
                    String (3, "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx");
                    Int (4, Z.of_int 10_000);
                  ],
                  [] )))
      in
      {
        code =
          Imported_protocol.(Script_repr.lazy_expr Liquidity_baking_lqt.script);
        storage = fallback_token_storage;
      }
  end
end

(* Module importing, amending, and converting, protocol types. Those types
   might be difficult to actually build, so we define conversion function from
   local types to protocol types. *)
module Protocol_types = struct
  module Alpha_context = Imported_protocol.Alpha_context
  module Raw_level = Alpha_context.Raw_level

  module Cycle = struct
    include Alpha_context.Cycle

    (* This function is copied from [cycle_repr.ml] because it is not exposed
       in [alpha_context.mli]. *)
    let of_int32_exn i =
      if Compare.Int32.(i >= 0l) then add root (Int32.to_int i)
      else invalid_arg "Cycle_repr.of_int32_exn"
  end

  let tezlink_to_tezos_chain_id ~l2_chain_id _chain =
    let (L2_types.Chain_id l2_chain_id) = l2_chain_id in
    let bytes = Bytes.make 4 '\000' in
    l2_chain_id |> Z.to_int32 |> Bytes.set_int32_be bytes 0 ;
    Chain_id.of_bytes bytes

  let ethereum_to_tezos_block_hash hash =
    hash |> Ethereum_types.block_hash_to_bytes |> Block_hash.of_string

  module type HEADER = sig
    module Block_services : BLOCK_SERVICES

    val tezlink_block_to_shell_header :
      L2_types.Tezos_block.t -> Block_header.shell_header tzresult

    val tezlink_block_to_block_header :
      l2_chain_id:L2_types.chain_id ->
      L2_types.Tezos_block.t * 'b ->
      Block_services.block_header tzresult

    val tezlink_block_to_block_info :
      l2_chain_id:L2_types.chain_id ->
      'a * L2_types.Tezos_block.t * 'b ->
      ('a * Block_services.block_info) tzresult
  end

  module Make_block_header (Block_services : BLOCK_SERVICES) :
    HEADER with module Block_services = Block_services = struct
    module Block_services = Block_services

    let tezlink_block_to_shell_header (block : L2_types.Tezos_block.t) :
        Block_header.shell_header tzresult =
      let open Result_syntax in
      let open Mock in
      let* predecessor = ethereum_to_tezos_block_hash block.parent_hash in
      return
        Block_header.
          {
            level = block.level;
            proto_level;
            predecessor;
            timestamp = block.timestamp;
            validation_passes;
            operations_hash;
            fitness;
            context;
          }

    let tezlink_block_to_block_header ~l2_chain_id
        ((block : L2_types.Tezos_block.t), chain) :
        Block_services.block_header tzresult =
      let open Result_syntax in
      let* chain_id = tezlink_to_tezos_chain_id ~l2_chain_id chain in
      let* protocol_data = Block_services.mock_block_header_data in
      let* hash = ethereum_to_tezos_block_hash block.hash in
      let* shell = tezlink_block_to_shell_header block in
      let block_header : Block_services.block_header =
        {chain_id; hash; shell; protocol_data}
      in
      return block_header

    let tezlink_block_to_raw_block_header block =
      let open Result_syntax in
      let* protocol_data = Block_services.mock_block_header_data in
      let* shell = tezlink_block_to_shell_header block in
      let raw_block_header : Block_services.raw_block_header =
        {shell; protocol_data}
      in
      return raw_block_header

    let tezlink_block_to_block_info ~l2_chain_id (version, block, chain) =
      let open Result_syntax in
      let* chain_id = tezlink_to_tezos_chain_id ~l2_chain_id chain in
      let* hash =
        ethereum_to_tezos_block_hash block.L2_types.Tezos_block.hash
      in
      let* header = tezlink_block_to_raw_block_header block in
      let block_info : Block_services.block_info =
        {chain_id; hash; header; metadata = None; operations = []}
      in
      return (version, block_info)
  end

  module Level = struct
    type t = Alpha_context.Level.t

    let encoding = Alpha_context.Level.encoding

    (** The sole purpose of this encoding is to reflect as closely as possible
        the encoding of Alpha_context.Level.t, so it can be used to convert to
        that type, with a serialization pass. This is necessary only because
        there isn't a simple way to build that type. *)
    let conversion_encoding =
      let open Data_encoding in
      conv
        (fun ({level; cycle; cycle_position} : level) ->
          ( Raw_level.of_int32_exn level,
            Int32.pred level,
            Cycle.of_int32_exn cycle,
            cycle_position,
            false ))
        (fun ( level,
               _level_position,
               cycle,
               cycle_position,
               _expected_commitment ) ->
          let level = Raw_level.to_int32 level in
          let cycle = Cycle.to_int32 cycle in
          {level; cycle; cycle_position})
        (obj5
           (req "level" Raw_level.encoding)
           (req "level_position" int32)
           (req "cycle" Cycle.encoding)
           (req "cycle_position" int32)
           (req "expected_commitment" bool))

    let convert : level -> t tzresult =
      Tezos_types.convert_using_serialization
        ~name:"level"
        ~dst:encoding
        ~src:conversion_encoding
  end

  module Counter = struct
    type t = Alpha_context.Manager_counter.t

    let encoding = Alpha_context.Manager_counter.encoding_for_RPCs

    let of_z : Z.t -> t tzresult =
      Tezos_types.convert_using_serialization
        ~name:"counter"
        ~dst:encoding
        ~src:Data_encoding.z
  end

  (* When indexing Tezlink, Tzkt requires Liquidity baking contracts.
     Instead of really storing it, in the durable_storage, we mock the
     rpc contract as this is the only RPC needed by tzkt for
     Liquidity baking. *)
  let mocked_script (contract : Alpha_context.Contract.t) =
    let address = Alpha_context.Contract.to_b58check contract in
    if address = Mock.Liquidity_baking.cpmm_address then
      Some Mock.Liquidity_baking.cpmm_script
    else if address = Mock.Liquidity_baking.lqt_address then
      Some Mock.Liquidity_baking.token_script
    else if address = Mock.Liquidity_baking.lq_fallback_token then
      Some Mock.Liquidity_baking.fallback_script
    else None

  module Contract = struct
    let make_info (contract, contract_balance, counter_z) =
      let open Result_syntax in
      let open Imported_protocol_plugin.Contract_services in
      let* counter = Counter.of_z counter_z in
      let script = mocked_script contract in
      return
        {
          balance = contract_balance;
          delegate = None;
          counter = Some counter;
          script;
        }
  end
end

module Current_block_header = Protocol_types.Make_block_header (Block_services)
module Zero_block_header = Protocol_types.Make_block_header (Zero_block_services)
module Genesis_block_header =
  Protocol_types.Make_block_header (Genesis_block_services)

(** [wrap conversion service_implementation] changes the output type
    of [service_implementation] using [conversion]. *)
let wrap conv impl p q i =
  let open Lwt_result_syntax in
  let* res = impl p q i in
  let*? result = conv res in
  return result

(** [import_service s] makes it possible to substitute new [prefix] and [param]
    types in the signature of the service [s]. *)
let import_service s = Tezos_rpc.Service.subst0 s

type block = Tezos_shell_services.Block_services.block

type chain = Tezos_shell_services.Chain_services.chain

type tezlink_rpc_context = {block : block; chain : chain}

(** Builds a [tezlink_rpc_context] from paths parameters. *)
let make_env (chain : chain) (block : block) : tezlink_rpc_context Lwt.t =
  Lwt.return {block; chain}

module Tezlink_version = struct
  type version = Tezos_version.Octez_node_version.t = {
    version : Tezos_version.Version.t;
    network_version : Tezos_version.Network_version.t;
    commit_info : commit_info option;
  }

  and commit_info = Tezos_version.Octez_node_version.commit_info = {
    commit_hash : string;
    commit_date : string;
  }

  let mock =
    Tezos_version.
      {
        version = Tezos_version_parser.default;
        network_version = Network_version.Internal_for_tests.mock ();
        commit_info = Some {commit_hash = ""; commit_date = ""};
      }
end

module Tezlink_protocols = struct
  module Shell_impl = Tezos_shell_services.Block_services

  type protocols = Shell_impl.protocols

  let current =
    Shell_impl.
      {
        current_protocol = Imported_protocol.hash;
        next_protocol = Imported_protocol.hash;
      }
end

(* Copied from src/proto_alpha/lib_protocol/constants_services.ml. *)
(* TODO: #7875
   Import from the protocol once it is exposed instead of copying it here. *)
module Constants_services = struct
  let custom_root : tezlink_rpc_context Tezos_rpc.Path.context =
    Tezos_rpc.Path.(open_root / "context" / "constants")

  let all =
    let open Tezos_rpc in
    Service.get_service
      ~description:"All constants"
      ~query:Query.empty
      ~output:Alpha_context.Constants.encoding
      custom_root
end

(* Copied from src/proto_alpha/lib_plugin/adaptive_issuance_services.ml. *)
(* TODO: #7875
   It's exposed in proto_alpha, but not in the plugin of Rio. Import when we
   move to a protocol that exposes it. *)
module Adaptive_issuance_services = struct
  module Cycle = Protocol_types.Cycle
  module Tez = Tezos_types.Tez

  type expected_rewards = {
    cycle : Cycle.t;
    baking_reward_fixed_portion : Tez.t;
    baking_reward_bonus_per_slot : Tez.t;
    attesting_reward_per_slot : Tez.t;
    dal_attesting_reward_per_shard : Tez.t;
    seed_nonce_revelation_tip : Tez.t;
    vdf_revelation_tip : Tez.t;
  }

  let dummy_reward i =
    {
      cycle = i;
      baking_reward_fixed_portion = Tez.one;
      baking_reward_bonus_per_slot = Tez.one;
      attesting_reward_per_slot = Tez.one;
      dal_attesting_reward_per_shard = Tez.one;
      seed_nonce_revelation_tip = Tez.one;
      vdf_revelation_tip = Tez.one;
    }

  let consensus_rights_delay =
    Tezlink_constants.all_constants.parametric.consensus_rights_delay

  let dummy_rewards current_cycle =
    List.init ~when_negative_length:[] (consensus_rights_delay + 1) (fun i ->
        dummy_reward Cycle.(add (of_int32_exn current_cycle) i))

  let expected_rewards_encoding : expected_rewards Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {
             cycle;
             baking_reward_fixed_portion;
             baking_reward_bonus_per_slot;
             attesting_reward_per_slot;
             dal_attesting_reward_per_shard;
             seed_nonce_revelation_tip;
             vdf_revelation_tip;
           } ->
        ( cycle,
          baking_reward_fixed_portion,
          baking_reward_bonus_per_slot,
          attesting_reward_per_slot,
          seed_nonce_revelation_tip,
          vdf_revelation_tip,
          dal_attesting_reward_per_shard ))
      (fun ( cycle,
             baking_reward_fixed_portion,
             baking_reward_bonus_per_slot,
             attesting_reward_per_slot,
             seed_nonce_revelation_tip,
             vdf_revelation_tip,
             dal_attesting_reward_per_shard ) ->
        {
          cycle;
          baking_reward_fixed_portion;
          baking_reward_bonus_per_slot;
          attesting_reward_per_slot;
          dal_attesting_reward_per_shard;
          seed_nonce_revelation_tip;
          vdf_revelation_tip;
        })
      (obj7
         (req "cycle" Cycle.encoding)
         (req "baking_reward_fixed_portion" Tez.encoding)
         (req "baking_reward_bonus_per_slot" Tez.encoding)
         (req "attesting_reward_per_slot" Tez.encoding)
         (req "seed_nonce_revelation_tip" Tez.encoding)
         (req "vdf_revelation_tip" Tez.encoding)
         (req "dal_attesting_reward_per_shard" Tez.encoding))

  let expected_issuance_path =
    let open Tezos_rpc in
    (Path.(open_root / "context" / "issuance" / "expected_issuance")
      : tezlink_rpc_context Path.context)

  let expected_issuance =
    let open Tezos_rpc in
    Service.get_service
      ~description:
        "Returns the expected issued tez for the provided block and the next \
         'consensus_rights_delay' cycles (in mutez)"
      ~query:Query.empty
      ~output:(Data_encoding.list expected_rewards_encoding)
      expected_issuance_path
end

(* This is where we import service declarations from the protocol. *)
module Protocol_plugin_services = Imported_protocol_plugin.RPC.S

type level_query = Protocol_plugin_services.level_query = {offset : int32}

let current_level :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      level_query,
      unit,
      Protocol_types.Level.t )
    Tezos_rpc.Service.t =
  import_service Protocol_plugin_services.current_level

let version :
    ( [`GET],
      unit,
      unit,
      unit,
      unit,
      Tezlink_version.version )
    Tezos_rpc.Service.t =
  Tezos_shell_services.Version_services.S.version

let protocols :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      unit,
      unit,
      Tezlink_protocols.protocols )
    Tezos_rpc.Service.t =
  import_service Tezos_shell_services.Shell_services.Blocks.S.protocols

(* Queries will be ignored for now. *)
let contract_info :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      Imported_protocol_plugin.Contract_services.S.normalize_types_query,
      unit,
      Imported_protocol_plugin.Contract_services.info )
    Tezos_rpc.Service.t =
  Tezos_rpc.Service.subst1 Imported_protocol_plugin.Contract_services.S.info

let balance :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      unit,
      unit,
      Tezos_types.Tez.t )
    Tezos_rpc.Service.t =
  Tezos_rpc.Service.subst1 Imported_protocol_plugin.Contract_services.S.balance

let manager_key :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      unit,
      unit,
      Protocol_types.Alpha_context.public_key option )
    Tezos_rpc.Service.t =
  let open Tezos_rpc in
  Service.subst1 Imported_protocol_plugin.Contract_services.S.manager_key

let counter :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      unit,
      unit,
      Protocol_types.Counter.t )
    Tezos_rpc.Service.t =
  let open Tezos_rpc in
  Service.subst1 Imported_protocol_plugin.Contract_services.S.counter

let constants :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      unit,
      unit,
      Protocol_types.Alpha_context.Constants.t )
    Tezos_rpc.Service.t =
  import_service Constants_services.all

let hash :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      unit,
      unit,
      Block_hash.t )
    Tezos_rpc.Service.t =
  import_service Block_services.S.hash

let chain_id :
    ([`GET], chain, chain, unit, unit, Chain_id.t) Tezos_rpc.Service.t =
  import_service Tezos_shell_services.Chain_services.S.chain_id

let header :
    ( [`GET],
      tezlink_rpc_context,
      tezlink_rpc_context,
      unit,
      unit,
      Block_services.block_header )
    Tezos_rpc.Service.t =
  import_service Block_services.S.header

let bootstrapped :
    ( [`GET],
      unit,
      unit,
      unit,
      unit,
      Block_hash.t * Time.Protocol.t )
    Tezos_rpc.Service.t =
  import_service Tezos_shell_services.Monitor_services.S.bootstrapped

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7965 *)
(* We need a proper implementation *)
let simulate_operation :
    ( [`POST],
      tezlink_rpc_context,
      tezlink_rpc_context,
      < successor_level : bool
      ; version : Imported_protocol_plugin.RPC.version option >,
      int32 option * Alpha_context.packed_operation * Chain_id.t * int,
      Alpha_context.packed_protocol_data * Imported_protocol.operation_receipt
    )
    Tezos_rpc.Service.t =
  import_service Imported_protocol_plugin.RPC.Scripts.S.simulate_operation

let monitor_heads :
    ( [`GET],
      unit,
      unit * chain,
      < protocols : Protocol_hash.t list
      ; next_protocols : Protocol_hash.t list >,
      unit,
      Block_hash.t * Block_header.t )
    Tezos_rpc.Service.t =
  Tezos_shell_services.Monitor_services.S.heads

let get_storage_normalized :
    ( [`POST],
      tezlink_rpc_context,
      tezlink_rpc_context * Tezos_types.Contract.t,
      unit,
      Imported_protocol.Script_ir_unparser.unparsing_mode,
      Alpha_context.Script.expr option )
    Tezos_rpc.Service.t =
  Tezos_rpc.Service.subst1
    Imported_protocol_plugin.RPC.Contract.S.get_storage_normalized
