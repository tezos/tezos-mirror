(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
open Alpha_context
open Protocol_client_context
module Events = Delegate_events.Baking_forge

module Operations_source = struct
  type t =
    | Local of {filename : string}
    | Remote of {uri : Uri.t; http_headers : (string * string) list option}

  let encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 1)
          ~title:"Local"
          (obj2 (req "filename" string) (req "kind" (constant "Local")))
          (function Local {filename} -> Some (filename, ()) | _ -> None)
          (fun (filename, ()) -> Local {filename});
        case
          (Tag 2)
          ~title:"Remote"
          (obj3
             (req "uri" string)
             (opt "http_headers" (list (tup2 string string)))
             (req "kind" (constant "Remote")))
          (function
            | Remote {uri; http_headers} ->
                Some (Uri.to_string uri, http_headers, ())
            | _ -> None)
          (fun (uri_str, http_headers, ()) ->
            Remote {uri = Uri.of_string uri_str; http_headers});
      ]

  type error +=
    | Failed_mempool_fetch of {
        path : string;
        reason : string;
        details : Data_encoding.json option;
      }

  let () =
    register_error_kind
      `Permanent
      ~id:(Format.sprintf "baker-%s.failed_to_get_mempool" Protocol.name)
      ~title:"Failed to get mempool"
      ~description:"Failed to retrieve the mempool from the given file or uri."
      ~pp:(fun fmt (path, reason, details) ->
        Format.fprintf
          fmt
          "@[<v 2>Failed to retrieve the mempool from %s:@ @[%s%a@]@]"
          path
          reason
          (fun fmt -> function
            | None -> ()
            | Some json -> Format.fprintf fmt ": %a" Data_encoding.Json.pp json)
          details)
      Data_encoding.(
        obj3
          (req "path" string)
          (req "reason" string)
          (opt "details" Data_encoding.json))
      (function
        | Failed_mempool_fetch {path; reason; details} ->
            Some (path, reason, details)
        | _ -> None)
      (fun (path, reason, details) ->
        Failed_mempool_fetch {path; reason; details})

  let operations_encoding =
    Data_encoding.(list (dynamic_size Alpha_context.Operation.encoding))

  let retrieve mempool =
    match mempool with
    | None -> Lwt.return_none
    | Some mempool -> (
        let fail reason details =
          let path =
            match mempool with
            | Local {filename} -> filename
            | Remote {uri; _} -> Uri.to_string uri
          in
          fail (Failed_mempool_fetch {path; reason; details})
        in
        let decode_mempool json =
          protect
            ~on_error:(fun _ ->
              fail "cannot decode the received JSON into mempool" (Some json))
            (fun () ->
              return (Data_encoding.Json.destruct operations_encoding json))
        in
        match mempool with
        | Local {filename} ->
            if Sys.file_exists filename then
              Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file filename
              >>= function
              | Error _ ->
                  Events.(emit invalid_json_file filename) >>= fun () ->
                  Lwt.return_none
              | Ok json -> (
                  decode_mempool json >>= function
                  | Ok mempool -> Lwt.return_some mempool
                  | Error _ -> assert false)
            else
              Events.(emit no_mempool_found_in_file filename) >>= fun () ->
              Lwt.return_none
        | Remote {uri; http_headers} -> (
            ( ((with_timeout
                  (Systime_os.sleep (Time.System.Span.of_seconds_exn 5.))
                  (fun _ ->
                    Tezos_rpc_http_client_unix.RPC_client_unix
                    .generic_media_type_call
                      ~accept:[Media_type.json]
                      ?headers:http_headers
                      `GET
                      uri)
                >>=? function
                | `Json json -> return json
                | _ -> fail "json not returned" None)
               >>=? function
               | `Ok json -> return json
               | `Unauthorized json -> fail "unauthorized request" json
               | `Gone json -> fail "gone" json
               | `Error json -> fail "error" json
               | `Not_found json -> fail "not found" json
               | `Forbidden json -> fail "forbidden" json
               | `Conflict json -> fail "conflict" json)
            >>=? fun json -> decode_mempool json )
            >>= function
            | Ok mempool -> Lwt.return_some mempool
            | Error errs ->
                Events.(emit cannot_fetch_mempool errs) >>= fun () ->
                Lwt.return_none))
end

(* The index of the different components of the protocol's validation passes *)
(* TODO: ideally, we would like this to be more abstract and possibly part of
   the protocol, while retaining the generality of lists *)
(* Hypothesis : we suppose [List.length Protocol.Main.validation_passes = 4] *)
let endorsements_index = 0

let votes_index = 1

let anonymous_index = 2

let managers_index = 3

let default_max_priority = 64

let default_minimal_fees =
  match Tez.of_mutez 100L with None -> assert false | Some t -> t

let default_minimal_nanotez_per_gas_unit = Q.of_int 100

let default_minimal_nanotez_per_byte = Q.of_int 1000

let default_retry_counter = 5

type slot =
  Time.Protocol.t * (Client_baking_blocks.block_info * int * public_key_hash)

type state = {
  context_path : string;
  mutable index : Context.index;
  (* Nonces file location *)
  nonces_location : [`Nonce] Client_baking_files.location;
  (* see [get_delegates] below to find delegates when the list is empty *)
  delegates : public_key_hash list;
  (* lazy-initialisation with retry-on-error *)
  constants : Constants.t;
  (* Minimal operation fee required to include an operation in a block *)
  minimal_fees : Tez.t;
  (* Minimal operation fee per gas required to include an operation in a block *)
  minimal_nanotez_per_gas_unit : Q.t;
  (* Minimal operation fee per byte required to include an operation in a block *)
  minimal_nanotez_per_byte : Q.t;
  (* truly mutable *)
  mutable best_slot : slot option;
  mutable retry_counter : int;
  extra_operations : Operations_source.t option;
}

let create_state ?(minimal_fees = default_minimal_fees)
    ?(minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit)
    ?(minimal_nanotez_per_byte = default_minimal_nanotez_per_byte)
    ?(retry_counter = default_retry_counter) ?extra_operations context_path
    index nonces_location delegates constants =
  {
    context_path;
    index;
    nonces_location;
    delegates;
    constants;
    minimal_fees;
    minimal_nanotez_per_gas_unit;
    minimal_nanotez_per_byte;
    best_slot = None;
    retry_counter;
    extra_operations;
  }

let get_delegates cctxt state =
  match state.delegates with
  | [] ->
      Client_keys.get_keys cctxt >>=? fun keys ->
      return (List.map (fun (_, pkh, _, _) -> pkh) keys)
  | _ -> return state.delegates

let generate_seed_nonce () =
  match Nonce.of_bytes (Rand.generate Constants.nonce_length) with
  | Error _errs -> assert false
  | Ok nonce -> nonce

let forge_block_header (cctxt : #Protocol_client_context.full) ~chain block
    delegate_sk shell priority seed_nonce_hash ~liquidity_baking_escape_vote =
  Client_baking_pow.mine cctxt chain block shell (fun proof_of_work_nonce ->
      {
        Block_header.priority;
        seed_nonce_hash;
        proof_of_work_nonce;
        liquidity_baking_escape_vote;
      })
  >>=? fun contents ->
  let unsigned_header =
    Data_encoding.Binary.to_bytes_exn
      Alpha_context.Block_header.unsigned_encoding
      (shell, contents)
  in
  Shell_services.Chain.chain_id cctxt ~chain () >>=? fun chain_id ->
  Client_keys.append
    cctxt
    delegate_sk
    ~watermark:(Block_header chain_id)
    unsigned_header

let forge_faked_protocol_data ~priority ~seed_nonce_hash
    ~liquidity_baking_escape_vote =
  Alpha_context.Block_header.
    {
      contents =
        {
          priority;
          seed_nonce_hash;
          proof_of_work_nonce = Client_baking_pow.empty_proof_of_work_nonce;
          liquidity_baking_escape_vote;
        };
      signature = Signature.zero;
    }

let assert_valid_operations_hash shell_header operations =
  let operations_hash =
    Operation_list_list_hash.compute
      (List.map
         Operation_list_hash.compute
         (List.map (List.map Tezos_base.Operation.hash) operations))
  in
  fail_unless
    (Operation_list_list_hash.equal
       operations_hash
       shell_header.Tezos_base.Block_header.operations_hash)
    (error_of_fmt "Client_baking_forge.inject_block: inconsistent header.")

let inject_block cctxt ?(force = false) ?seed_nonce_hash ~chain ~shell_header
    ~priority ~delegate_pkh ~delegate_sk ~level operations
    ~liquidity_baking_escape_vote =
  assert_valid_operations_hash shell_header operations >>=? fun () ->
  let block = `Hash (shell_header.Tezos_base.Block_header.predecessor, 0) in
  forge_block_header
    cctxt
    ~chain
    block
    delegate_sk
    shell_header
    priority
    seed_nonce_hash
    ~liquidity_baking_escape_vote
  >>=? fun signed_header ->
  (* Record baked blocks to prevent double baking  *)
  let open Client_baking_highwatermarks in
  cctxt#with_lock (fun () ->
      Client_baking_files.resolve_location cctxt ~chain `Block
      >>=? fun block_location ->
      may_inject_block cctxt block_location ~delegate:delegate_pkh level
      >>=? function
      | true ->
          record_block cctxt block_location ~delegate:delegate_pkh level
          >>=? fun () -> return_true
      | false ->
          Events.(emit double_bake_near_miss) level >>= fun () -> return force)
  >>=? function
  | false -> fail (Level_previously_baked level)
  | true ->
      Shell_services.Injection.block
        cctxt
        ~force
        ~chain
        signed_header
        operations
      >>=? fun block_hash ->
      Events.(emit inject_baked_block) (block_hash, signed_header, operations)
      >>= fun () -> return block_hash

type error += Failed_to_preapply of Tezos_base.Operation.t * error list

let () =
  register_error_kind
    `Permanent
    ~id:"Client_baking_forge.failed_to_preapply"
    ~title:"Fail to preapply an operation"
    ~description:""
    ~pp:(fun ppf (op, trace) ->
      let h = Tezos_base.Operation.hash op in
      Format.fprintf
        ppf
        "@[Failed to preapply %a:@ @[<v 4>%a@]@]"
        Operation_hash.pp_short
        h
        pp_print_trace
        trace)
    Data_encoding.(
      obj2
        (req "operation" (dynamic_size Tezos_base.Operation.encoding))
        (req "error" RPC_error.encoding))
    (function Failed_to_preapply (hash, err) -> Some (hash, err) | _ -> None)
    (fun (hash, err) -> Failed_to_preapply (hash, err))

type manager_content = {
  total_fee : Tez.t;
  total_gas : Fixed_point_repr.integral_tag Gas.Arith.t;
  source : public_key_hash;
  counter : counter;
}

module PrioritizedOperation : sig
  type t = private High of packed_operation | Low of packed_operation

  (** prioritize operations coming from an external source (file, uri, ...)*)
  val extern : packed_operation -> t

  (** prioritize operations coming from a node *)
  val node : packed_operation -> t

  (** [packed t] is [t.operation]*)
  val packed : t -> packed_operation

  val compare_priority : t -> t -> int
end = struct
  (* Higher priority operations will be included first *)
  type t = High of packed_operation | Low of packed_operation

  let extern op = High op

  let node op = Low op

  let packed = function High op | Low op -> op

  let compare_priority t1 t2 =
    match (t1, t2) with
    | (High _, Low _) -> 1
    | (Low _, High _) -> -1
    | (Low _, Low _) | (High _, High _) -> 0
end

let get_manager_content op =
  let {protocol_data = Operation_data {contents; _}; _} =
    PrioritizedOperation.packed op
  in
  let open Operation in
  let l = to_list (Contents_list contents) in
  List.fold_left_e
    (fun ((first_source, first_counter, total_fee, total_gas) as acc) ->
       function
      | Contents (Manager_operation {source; counter; fee; gas_limit; _}) ->
          (Environment.wrap_tzresult @@ Tez.(total_fee +? fee))
          >>? fun total_fee ->
          (* There is only one unique source per packed transaction *)
          let first_source = Option.value ~default:source first_source in
          (* We only care about the first counter *)
          let first_counter = Option.value ~default:counter first_counter in
          ok
            ( Some first_source,
              Some first_counter,
              total_fee,
              Gas.Arith.add total_gas gas_limit )
      | _ -> ok acc)
    (None, None, Tez.zero, Gas.Arith.zero)
    l
  |> function
  | Ok (Some source, Some counter, total_fee, total_gas) ->
      Some {total_fee; total_gas; source; counter}
  | _ -> None

(* Sort operation considering potential gas and storage usage.
   Weight = fee / (max ( (size/size_total), (gas/gas_total))) *)
let sort_manager_operations ~max_size ~hard_gas_limit_per_block ~minimal_fees
    ~minimal_nanotez_per_gas_unit ~minimal_nanotez_per_byte
    (operations : PrioritizedOperation.t list) =
  let compute_weight op (fee, gas) =
    let size = Data_encoding.Binary.length Operation.encoding op in
    let size_f = Q.of_int size in
    let gas_f = Q.of_bigint (Gas.Arith.integral_to_z gas) in
    let fee_f = Q.of_int64 (Tez.to_mutez fee) in
    let size_ratio = Q.(size_f / Q.of_int max_size) in
    let gas_ratio =
      Q.(gas_f / Q.of_bigint (Gas.Arith.integral_to_z hard_gas_limit_per_block))
    in
    (size, gas, Q.(fee_f / max size_ratio gas_ratio))
  in
  List.filter_map
    (fun op ->
      match get_manager_content op with
      | None -> None
      | Some {total_fee; total_gas; source; counter} ->
          if Tez.(total_fee < minimal_fees) then None
          else
            let (size, gas, weight_ratio) =
              compute_weight
                (PrioritizedOperation.packed op)
                (total_fee, total_gas)
            in
            let fees_in_nanotez =
              Q.mul (Q.of_int64 (Tez.to_mutez total_fee)) (Q.of_int 1000)
            in
            let enough_fees_for_gas =
              let minimal_fees_in_nanotez =
                Q.mul
                  minimal_nanotez_per_gas_unit
                  (Q.of_bigint @@ Gas.Arith.integral_to_z gas)
              in
              Q.compare minimal_fees_in_nanotez fees_in_nanotez <= 0
            in
            let enough_fees_for_size =
              let minimal_fees_in_nanotez =
                Q.mul minimal_nanotez_per_byte (Q.of_int size)
              in
              Q.compare minimal_fees_in_nanotez fees_in_nanotez <= 0
            in
            if enough_fees_for_size && enough_fees_for_gas then
              Some (op, weight_ratio, source, counter)
            else None)
    operations
  |> fun operations ->
  (* We order the operations by their weights except if they belong
     to the same manager, if they do, we order them by their
     counter. *)
  let compare (op, weight_ratio, source, counter)
      (op', weight_ratio', source', counter') =
    (* Be careful with the [compare]s *)
    if Signature.Public_key_hash.equal source source' then
      (* we want the smallest counter first *)
      Z.compare counter counter'
    else
      (* Prioritize according to tags first, then weight *)
      match PrioritizedOperation.compare_priority op op' with
      | 0 ->
          (* We want the biggest weight first *)
          Q.compare weight_ratio' weight_ratio
      | n -> n
  in
  List.sort compare operations |> List.map (fun (op, _, _, _) -> op)

let retain_operations_up_to_quota operations quota =
  let {Tezos_protocol_environment.max_op; max_size} = quota in
  let operations =
    match max_op with Some n -> List.sub operations n | None -> operations
  in
  let exception Full of packed_operation list in
  let operations =
    try
      List.fold_left
        (fun (ops, size) op ->
          let operation_size =
            Data_encoding.Binary.length Alpha_context.Operation.encoding op
          in
          let new_size = size + operation_size in
          if new_size > max_size then raise (Full ops) else (op :: ops, new_size))
        ([], 0)
        operations
      |> fst
    with Full ops -> ops
  in
  List.rev operations

let trim_manager_operations ~max_size ~hard_gas_limit_per_block
    manager_operations =
  let manager_operations =
    List.filter_map
      (fun op ->
        match get_manager_content op with
        | Some {total_gas; _} ->
            let size =
              Data_encoding.Binary.length
                Operation.encoding
                (PrioritizedOperation.packed op)
            in
            Some (op, (size, total_gas))
        | None -> None)
      manager_operations
  in
  List.fold_left
    (fun (total_size, total_gas, (good_ops, bad_ops)) (op, (size, gas)) ->
      let new_size = total_size + size in
      let new_gas = Gas.Arith.(add total_gas gas) in
      if new_size > max_size || Gas.Arith.(new_gas > hard_gas_limit_per_block)
      then (total_size, total_gas, (good_ops, op :: bad_ops))
      else (new_size, new_gas, (op :: good_ops, bad_ops)))
    (0, Gas.Arith.zero, ([], []))
    manager_operations
  |> fun (_, _, (good_ops, bad_ops)) ->
  (* We keep the overflowing operations, it may be used for client-side validation *)
  (List.rev good_ops, List.rev bad_ops)

(* We classify operations, sort managers operation by interest and add bad ones at the end *)
(* Hypothesis : we suppose that the received manager operations have a valid gas_limit *)

(** [classify_operations] classify the operation in 4 lists indexed as such :
    - 0 -> Endorsements
    - 1 -> Votes and proposals
    - 2 -> Anonymous operations
    - 3 -> High-priority manager operations.
    Returns two list :
    - A desired set of operations to be included
    - Potentially overflowing operations *)
let classify_operations (cctxt : #Protocol_client_context.full) ~chain ~block
    ~hard_gas_limit_per_block ~minimal_fees ~minimal_nanotez_per_gas_unit
    ~minimal_nanotez_per_byte (ops : PrioritizedOperation.t list) =
  Alpha_block_services.live_blocks cctxt ~chain ~block ()
  >>=? fun live_blocks ->
  let t =
    (* Remove operations that are too old *)
    let ops =
      List.filter
        (fun pop ->
          let {shell = {branch; _}; _} = PrioritizedOperation.packed pop in
          Block_hash.Set.mem branch live_blocks)
        ops
    in
    let validation_passes_len = List.length Main.validation_passes in
    let t = Array.make validation_passes_len [] in
    List.iter
      (fun (op : PrioritizedOperation.t) ->
        List.iter
          (fun pass -> t.(pass) <- op :: t.(pass))
          (Main.acceptable_passes (PrioritizedOperation.packed op)))
      ops ;
    Array.map List.rev t
  in
  let overflowing_manager_operations =
    (* Retrieve the optimist maximum paying manager operations *)
    let manager_operations = t.(managers_index) in
    let {Environment.Updater.max_size; _} =
      WithExceptions.Option.get ~loc:__LOC__
      @@ List.nth Main.validation_passes managers_index
    in
    let ordered_operations =
      sort_manager_operations
        ~max_size
        ~hard_gas_limit_per_block
        ~minimal_fees
        ~minimal_nanotez_per_gas_unit
        ~minimal_nanotez_per_byte
        manager_operations
    in
    (* Greedy heuristic *)
    let (desired_manager_operations, overflowing_manager_operations) =
      trim_manager_operations
        ~max_size
        ~hard_gas_limit_per_block
        ordered_operations
    in
    t.(managers_index) <- desired_manager_operations ;
    ok overflowing_manager_operations
  in

  Lwt.return
    ( overflowing_manager_operations >>? fun overflowing_manager_operations ->
      ok
        ( Array.to_list t |> List.map (List.map PrioritizedOperation.packed),
          overflowing_manager_operations ) )

let forge (op : Operation.packed) : Operation.raw =
  {
    shell = op.shell;
    proto =
      Data_encoding.Binary.to_bytes_exn
        Alpha_context.Operation.protocol_data_encoding
        op.protocol_data;
  }

let ops_of_mempool (ops : Alpha_block_services.Mempool.t) =
  (* We only retain the applied, unprocessed and delayed operations *)
  List.rev
    (Operation_hash.Map.fold
       (fun _ op acc -> PrioritizedOperation.node op :: acc)
       ops.unprocessed
    @@ Operation_hash.Map.fold
         (fun _ (op, _) acc -> PrioritizedOperation.node op :: acc)
         ops.branch_delayed
    @@ List.rev_map (fun (_, op) -> PrioritizedOperation.node op) ops.applied)

let get_operations cctxt ~ignore_node_mempool chain mempool =
  Operations_source.retrieve mempool >>= fun mempool_ops_opt ->
  let mempool_ops =
    match mempool_ops_opt with
    | None -> []
    | Some ops -> List.map PrioritizedOperation.extern ops
  in
  if ignore_node_mempool then return mempool_ops
  else
    Alpha_block_services.Mempool.pending_operations cctxt ~chain ()
    >>=? fun mpool ->
    let ops = ops_of_mempool mpool in
    return (mempool_ops @ ops)

let all_ops_valid (results : error Preapply_result.t list) =
  let open Operation_hash.Map in
  List.for_all
    (fun (result : error Preapply_result.t) ->
      is_empty result.refused
      && is_empty result.branch_refused
      && is_empty result.branch_delayed)
    results

let decode_priority cctxt chain block ~priority ~endorsing_power =
  match priority with
  | `Set priority ->
      Alpha_services.Delegate.Minimal_valid_time.get
        cctxt
        (chain, block)
        priority
        endorsing_power
      >>=? fun minimal_timestamp -> return (priority, minimal_timestamp)
  | `Auto (src_pkh, max_priority) -> (
      Plugin.RPC.current_level cctxt ~offset:1l (chain, block)
      >>=? fun {level; _} ->
      Plugin.RPC.Baking_rights.get
        cctxt
        ?max_priority
        ~levels:[level]
        ~delegates:[src_pkh]
        (chain, block)
      >>=? fun possibilities ->
      match
        List.find
          (fun p -> p.Plugin.RPC.Baking_rights.level = level)
          possibilities
      with
      | Some {Plugin.RPC.Baking_rights.priority = prio; _} ->
          Alpha_services.Delegate.Minimal_valid_time.get
            cctxt
            (chain, block)
            prio
            endorsing_power
          >>=? fun minimal_timestamp -> return (prio, minimal_timestamp)
      | None -> failwith "No slot found at level %a" Raw_level.pp level)

let unopt_timestamp ?(force = false) timestamp minimal_timestamp =
  let timestamp =
    match timestamp with
    | None -> minimal_timestamp
    | Some timestamp -> timestamp
  in
  if (not force) && timestamp < minimal_timestamp then
    failwith
      "Proposed timestamp %a is earlier than minimal timestamp %a"
      Time.Protocol.pp_hum
      timestamp
      Time.Protocol.pp_hum
      minimal_timestamp
  else return timestamp

let merge_preapps (old : error Preapply_result.t)
    (neu : error Preapply_result.t) =
  let merge _ a b = (* merge ops *) Option.either b a in
  let merge = (* merge op maps *) Operation_hash.Map.merge merge in
  (* merge preapplies *)
  {
    Preapply_result.applied = [];
    outdated = merge old.outdated neu.outdated;
    refused = merge old.refused neu.refused;
    branch_refused = merge old.branch_refused neu.branch_refused;
    branch_delayed = merge old.branch_delayed neu.branch_delayed;
  }

let error_of_op (result : error Preapply_result.t) op =
  let op = forge op in
  let h = Tezos_base.Operation.hash op in
  match Operation_hash.Map.find h result.refused with
  | Some (_, trace) -> Some (Failed_to_preapply (op, trace))
  | None -> (
      match Operation_hash.Map.find h result.branch_refused with
      | Some (_, trace) -> Some (Failed_to_preapply (op, trace))
      | None -> (
          match Operation_hash.Map.find h result.branch_delayed with
          | Some (_, trace) -> Some (Failed_to_preapply (op, trace))
          | None -> None))

let compute_endorsement_powers cctxt constants ~chain ~block =
  Plugin.RPC.Endorsing_rights.get
    cctxt
    ~levels:[block.Client_baking_blocks.level]
    (chain, `Hash (block.hash, 0))
  >>=? fun endorsing_rights ->
  let slots_arr = Array.make constants.Constants.endorsers_per_block 0 in
  (* Populate the array *)
  List.iter
    (fun {Plugin.RPC.Endorsing_rights.slots; _} ->
      let endorsing_power = List.length slots in
      List.iter (fun slot -> slots_arr.(slot) <- endorsing_power) slots)
    endorsing_rights ;
  return slots_arr

let compute_endorsing_power endorsement_powers operations =
  List.fold_left
    (fun sum op ->
      match op with
      | {
       Alpha_context.protocol_data =
         Operation_data {contents = Single (Endorsement_with_slot {slot; _}); _};
       _;
      } -> (
          try
            let endorsement_power = endorsement_powers.(slot) in
            sum + endorsement_power
          with _ -> sum)
      | _ -> sum)
    0
    operations

let compute_minimal_valid_time constants ~priority ~endorsing_power
    ~predecessor_timestamp =
  Environment.wrap_tzresult
    (Baking.minimal_valid_time
       constants
       ~priority
       ~endorsing_power
       ~predecessor_timestamp)

let filter_and_apply_operations cctxt state endorsements_map ~chain ~block
    block_info ~priority ?protocol_data
    ((operations : packed_operation list list), _overflowing_operations) =
  (* Retrieve the minimal valid time for when the block can be baked with 0 endorsements *)
  Delegate_services.Minimal_valid_time.get cctxt (chain, block) priority 0
  >>=? fun min_valid_timestamp ->
  let open Client_baking_simulator in
  Events.(emit baking_local_validation_start)
    block_info.Client_baking_blocks.hash
  >>= fun () ->
  let quota : Environment.Updater.quota list = Main.validation_passes in
  let endorsements =
    retain_operations_up_to_quota
      (WithExceptions.Option.get
         ~loc:__LOC__
         (List.nth operations endorsements_index))
      (WithExceptions.Option.get ~loc:__LOC__
      @@ List.nth quota endorsements_index)
  in
  let votes =
    retain_operations_up_to_quota
      (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth operations votes_index)
      (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth quota votes_index)
  in
  let anonymous =
    retain_operations_up_to_quota
      (WithExceptions.Option.get ~loc:__LOC__
      @@ List.nth operations anonymous_index)
      (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth quota anonymous_index)
  in
  let managers =
    (* Managers are already trimmed *)
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth operations managers_index
  in
  (begin_construction
     ~timestamp:min_valid_timestamp
     ?protocol_data
     state.index
     block_info
   >>= function
   | Ok inc -> return inc
   | Error errs ->
       Events.(emit context_fetch_error) errs >>= fun () ->
       Events.(emit reopen_context) () >>= fun () ->
       Client_baking_simulator.load_context ~context_path:state.context_path
       >>= fun index ->
       begin_construction
         ~timestamp:min_valid_timestamp
         ?protocol_data
         index
         block_info
       >>=? fun inc ->
       state.index <- index ;
       return inc)
  >>=? fun initial_inc ->
  let validate_operation inc op =
    protect (fun () -> add_operation inc op) >>= function
    | Error errs ->
        Events.(emit baking_rejected_invalid_operation)
          (Operation.hash_packed op, errs)
        >>= fun () -> Lwt.return_none
    | Ok (resulting_state, receipt) -> (
        try
          (* Check that the metadata are serializable/deserializable *)
          let _ =
            Data_encoding.Binary.(
              of_bytes_exn
                Protocol.operation_receipt_encoding
                (to_bytes_exn Protocol.operation_receipt_encoding receipt))
          in
          Lwt.return_some resulting_state
        with exn ->
          let errs =
            [Validation_errors.Cannot_serialize_operation_metadata; Exn exn]
          in
          Events.(emit baking_rejected_invalid_operation)
            (Operation.hash_packed op, errs)
          >>= fun () -> Lwt.return_none)
  in
  let filter_valid_operations inc ops =
    List.fold_left_s
      (fun (inc, acc) op ->
        validate_operation inc op >>= function
        | None -> Lwt.return (inc, acc)
        | Some inc' -> Lwt.return (inc', op :: acc))
      (inc, [])
      ops
    >>= fun (inc, ops) -> Lwt.return (inc, List.rev ops)
  in
  (* Apply operations and filter the invalid ones *)
  filter_valid_operations initial_inc endorsements
  >>= fun (inc, endorsements) ->
  filter_valid_operations inc votes >>= fun (inc, votes) ->
  filter_valid_operations inc anonymous >>= fun (manager_inc, anonymous) ->
  filter_valid_operations manager_inc managers >>= fun (inc, managers) ->
  finalize_construction inc >>=? fun _ ->
  let operations = [endorsements; votes; anonymous; managers] in
  (* Construct a context with the valid operations and a correct timestamp *)
  let current_endorsing_power =
    compute_endorsing_power endorsements_map endorsements
  in
  compute_minimal_valid_time
    state.constants.parametric
    ~priority
    ~endorsing_power:current_endorsing_power
    ~predecessor_timestamp:block_info.timestamp
  >>?= fun expected_validity ->
  (* Finally, we construct a block with the minimal possible timestamp
     given the endorsing power *)
  begin_construction
    ~timestamp:expected_validity
    ?protocol_data
    state.index
    block_info
  >>=? fun inc ->
  List.fold_left_es
    (fun inc op -> add_operation inc op >>=? fun (inc, _receipt) -> return inc)
    inc
    (List.flatten operations)
  >>=? fun final_inc ->
  let operations_hash : Operation_list_list_hash.t =
    Operation_list_list_hash.compute
      (List.map
         (fun sl ->
           Operation_list_hash.compute (List.map Operation.hash_packed sl))
         operations)
  in
  let validation_passes = List.length Main.validation_passes in
  let final_inc =
    {
      final_inc with
      header =
        {
          final_inc.header with
          operations_hash;
          validation_passes;
          level = Int32.succ final_inc.header.level;
        };
    }
  in
  finalize_construction final_inc >>=? fun (validation_result, metadata) ->
  return
    (final_inc, (validation_result, metadata), operations, expected_validity)

(* Build the block header : mimics node prevalidation *)
let finalize_block_header shell_header ~timestamp validation_result
    predecessor_block_metadata_hash predecessor_ops_metadata_hash =
  let {Tezos_protocol_environment.context; fitness; message; _} =
    validation_result
  in
  let header =
    Tezos_base.Block_header.
      {shell_header with fitness; context = Context_hash.zero}
  in
  let context = Shell_context.unwrap_disk_context context in
  (match predecessor_block_metadata_hash with
  | Some predecessor_block_metadata_hash ->
      Context.add_predecessor_block_metadata_hash
        context
        predecessor_block_metadata_hash
  | None -> Lwt.return context)
  >>= fun context ->
  (match predecessor_ops_metadata_hash with
  | Some predecessor_ops_metadata_hash ->
      Context.add_predecessor_ops_metadata_hash
        context
        predecessor_ops_metadata_hash
  | None -> Lwt.return context)
  >>= fun context ->
  let context = Context.hash ~time:timestamp ?message context in
  return {header with context}

let forge_block cctxt ?force ?(best_effort = true) ?(sort = best_effort)
    ?(minimal_fees = default_minimal_fees)
    ?(minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit)
    ?(minimal_nanotez_per_byte = default_minimal_nanotez_per_byte) ?timestamp
    ?(ignore_node_mempool = false) ?extra_operations ?context_path
    ?seed_nonce_hash ~liquidity_baking_escape_vote ~chain ~priority
    ~delegate_pkh ~delegate_sk block =
  Alpha_services.Constants.all cctxt (chain, block) >>=? fun constants ->
  (* making the arguments usable *)
  get_operations cctxt ~ignore_node_mempool chain extra_operations
  >>=? fun operations_arg ->
  Client_baking_blocks.info cctxt ~chain block >>=? fun block_info ->
  compute_endorsement_powers cctxt constants.parametric ~chain ~block:block_info
  >>=? fun endorsement_powers ->
  let untagged_operations =
    List.map PrioritizedOperation.packed operations_arg
  in
  let endorsing_power =
    compute_endorsing_power endorsement_powers untagged_operations
  in
  decode_priority cctxt chain block ~priority ~endorsing_power
  >>=? fun (priority, minimal_timestamp) ->
  unopt_timestamp ?force timestamp minimal_timestamp >>=? fun timestamp ->
  (* get basic building blocks *)
  let protocol_data =
    forge_faked_protocol_data
      ~priority
      ~seed_nonce_hash
      ~liquidity_baking_escape_vote
  in
  classify_operations
    cctxt
    ~chain
    ~hard_gas_limit_per_block:constants.parametric.hard_gas_limit_per_block
    ~block
    ~minimal_fees
    ~minimal_nanotez_per_gas_unit
    ~minimal_nanotez_per_byte
    operations_arg
  >>=? fun (operations, overflowing_ops) ->
  (* Ensure that we retain operations up to the quota *)
  let quota : Environment.Updater.quota list = Main.validation_passes in
  let endorsements =
    List.sub
      (WithExceptions.Option.get ~loc:__LOC__
      @@ List.nth operations endorsements_index)
      constants.parametric.endorsers_per_block
  in
  let votes =
    retain_operations_up_to_quota
      (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth operations votes_index)
      (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth quota votes_index)
  in
  let anonymous =
    retain_operations_up_to_quota
      (WithExceptions.Option.get ~loc:__LOC__
      @@ List.nth operations anonymous_index)
      (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth quota anonymous_index)
  in
  (* Size/Gas check already occurred in classify operations *)
  let managers =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth operations managers_index
  in
  let operations = [endorsements; votes; anonymous; managers] in
  (match context_path with
  | None ->
      Alpha_block_services.Helpers.Preapply.block
        cctxt
        ~chain
        ~block
        ~timestamp
        ~sort
        ~protocol_data
        operations
      >>=? fun (shell_header, result) ->
      let operations =
        List.map (fun l -> List.map snd l.Preapply_result.applied) result
      in
      (* everything went well (or we don't care about errors): GO! *)
      if best_effort || all_ops_valid result then
        return (shell_header, operations)
        (* some errors (and we care about them) *)
      else
        let result =
          List.fold_left merge_preapps Preapply_result.empty result
        in
        Lwt.return_error
        @@ List.filter_map (error_of_op result) untagged_operations
  | Some context_path ->
      assert sort ;
      assert best_effort ;
      Context.init ~readonly:true context_path >>= fun index ->
      Client_baking_files.resolve_location cctxt ~chain `Nonce
      >>=? fun nonces_location ->
      let state =
        {
          context_path;
          index;
          nonces_location;
          constants;
          delegates = [];
          best_slot = None;
          minimal_fees = default_minimal_fees;
          minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit;
          minimal_nanotez_per_byte = default_minimal_nanotez_per_byte;
          retry_counter = default_retry_counter;
          extra_operations;
        }
      in
      compute_endorsement_powers
        cctxt
        constants.parametric
        ~chain
        ~block:block_info
      >>=? fun endorsement_powers ->
      filter_and_apply_operations
        cctxt
        state
        endorsement_powers
        ~chain
        ~block
        ~priority
        ~protocol_data
        block_info
        (operations, overflowing_ops)
      >>=? fun ( final_context,
                 (validation_result, _),
                 operations,
                 min_valid_timestamp ) ->
      let current_protocol = block_info.next_protocol in
      let context =
        Shell_context.unwrap_disk_context validation_result.context
      in
      Context.get_protocol context >>= fun next_protocol ->
      if Protocol_hash.equal current_protocol next_protocol then
        finalize_block_header
          final_context.header
          ~timestamp:min_valid_timestamp
          validation_result
          block_info.predecessor_block_metadata_hash
          block_info.predecessor_operations_metadata_hash
        >>= function
        | Error _ as errs -> Lwt.return errs
        | Ok shell_header ->
            return (shell_header, List.map (List.map forge) operations)
      else
        Events.(emit shell_prevalidation_new_protocol) () >>= fun () ->
        Alpha_block_services.Helpers.Preapply.block
          cctxt
          ~chain
          ~block
          ~timestamp:min_valid_timestamp
          ~sort
          ~protocol_data
          operations
        >>=? fun (shell_header, _result) ->
        return (shell_header, List.map (List.map forge) operations))
  >>=? fun (shell_header, operations) ->
  (* Now for some logging *)
  let total_op_count = List.length operations_arg in
  let valid_op_count = List.length (List.concat operations) in
  let time = Time.System.of_protocol_exn timestamp in
  Events.(emit found_valid_operations)
    (valid_op_count, total_op_count - valid_op_count, time, shell_header.fitness)
  >>= fun () ->
  (match Raw_level.of_int32 shell_header.level with
  | Ok level -> return level
  | Error errs ->
      let errs = Environment.wrap_tztrace errs in
      Events.(emit block_conversion_failed) errs >>= fun () ->
      Lwt.return_error errs)
  >>=? fun level ->
  inject_block
    cctxt
    ?force
    ~chain
    ~shell_header
    ~priority
    ?seed_nonce_hash
    ~delegate_pkh
    ~delegate_sk
    ~level
    operations
    ~liquidity_baking_escape_vote
  >>= function
  | Ok hash -> return hash
  | Error errs as error ->
      Events.(emit block_injection_failed) (List.concat operations, errs)
      >>= fun () -> Lwt.return error

let shell_prevalidation (cctxt : #Protocol_client_context.full) ~chain ~block
    ~timestamp seed_nonce_hash operations
    ((_, (bi, priority, delegate)) as _slot) =
  let liquidity_baking_escape_vote = false in
  let protocol_data =
    forge_faked_protocol_data
      ~priority
      ~seed_nonce_hash
      ~liquidity_baking_escape_vote
  in
  Alpha_block_services.Helpers.Preapply.block
    cctxt
    ~chain
    ~block
    ~timestamp
    ~sort:true
    ~protocol_data
    operations
  >>= function
  | Error errs ->
      Events.(emit built_invalid_block_error) errs >>= fun () -> return_none
  | Ok (shell_header, operations) ->
      let raw_ops =
        List.map (fun l -> List.map snd l.Preapply_result.applied) operations
      in
      return_some
        (bi, priority, shell_header, raw_ops, delegate, seed_nonce_hash)

let extract_op_and_filter_outdated_endorsements expected_level ops =
  List.filter_map
    (function
      | ( ( _,
            ({
               Alpha_context.protocol_data =
                 Operation_data
                   {
                     contents =
                       Single
                         (Endorsement_with_slot
                           {
                             endorsement =
                               {
                                 protocol_data =
                                   {
                                     contents = Single (Endorsement {level; _});
                                     _;
                                   };
                                 _;
                               };
                             _;
                           });
                     _;
                   };
               _;
             } as op) ),
          _ ) ->
          if Raw_level.equal expected_level level then Some op else None
      | ((_, op), _) -> Some op)
    ops

(** [fetch_operations] retrieve the operations present in the
    mempool. If no endorsements are present in the initial set, it
    waits until it's able to build a valid block. *)
let fetch_operations (cctxt : #Protocol_client_context.full) ~chain state
    endorsement_powers (_, (head, priority, _delegate)) =
  Alpha_block_services.Mempool.monitor_operations
    cctxt
    ~chain
    ~applied:true
    ~branch_delayed:false
    ~refused:false
    ~branch_refused:false
    ()
  >>=? fun (operation_stream, _stop) ->
  let notify_endorsement_arrival operations =
    List.iter_s
      (function
        | {
            Alpha_context.protocol_data =
              Operation_data
                {contents = Single (Endorsement_with_slot {slot; _}); _};
            _;
          } -> (
            try
              let endorsing_power = endorsement_powers.(slot) in
              Events.(emit endorsement_received (slot, endorsing_power))
            with _ -> Lwt.return_unit)
        | _ -> Lwt.return_unit)
      operations
  in
  (* Hypothesis : the first call to the stream returns instantly, even if the mempool is empty. *)
  Lwt_stream.get operation_stream >>= function
  | None ->
      (* New head received : aborting block construction *)
      return_none
  | Some current_mempool ->
      let operations =
        ref
          (extract_op_and_filter_outdated_endorsements
             head.Client_baking_blocks.level
             current_mempool)
      in
      notify_endorsement_arrival !operations >>= fun () ->
      let current_endorsing_power =
        ref (compute_endorsing_power endorsement_powers !operations)
      in
      let previous_endorsing_power = ref 0 in
      let previous_expected_validity_time = ref None in
      (* Actively request our peers' for missing operations *)
      Shell_services.Mempool.request_operations cctxt ~chain () >>=? fun () ->
      let compute_timeout () =
        let compute_minimal_valid_time () =
          compute_minimal_valid_time
            state.constants.parametric
            ~priority
            ~endorsing_power:!current_endorsing_power
            ~predecessor_timestamp:head.timestamp
          >>?= fun expected_validity ->
          Events.(
            emit
              expected_validity_time
              (expected_validity, !current_endorsing_power))
          >>= fun () -> return expected_validity
        in
        (match !previous_expected_validity_time with
        | None -> compute_minimal_valid_time ()
        | Some _
          when Compare.Int.(
                 !current_endorsing_power > !previous_endorsing_power) ->
            compute_minimal_valid_time ()
        | Some previous_expected_validity_time ->
            return previous_expected_validity_time)
        >>=? fun expected_validity ->
        previous_expected_validity_time := Some expected_validity ;
        match Client_baking_scheduling.sleep_until expected_validity with
        | None -> return_unit
        | Some timeout -> timeout >>= fun () -> return_unit
      in
      let last_get_event = ref None in
      let get_event () =
        match !last_get_event with
        | None ->
            let t = Lwt_stream.get operation_stream in
            last_get_event := Some t ;
            t
        | Some t -> t
      in
      let rec loop () =
        Lwt.choose
          [
            (compute_timeout () >|= fun _ -> `Timeout);
            (get_event () >|= fun e -> `Event e);
          ]
        >>= function
        | `Event (Some op_list) ->
            last_get_event := None ;
            let op_list =
              extract_op_and_filter_outdated_endorsements head.level op_list
            in
            notify_endorsement_arrival op_list >>= fun () ->
            let added_endorsing_power =
              compute_endorsing_power endorsement_powers op_list
            in
            current_endorsing_power :=
              added_endorsing_power + !current_endorsing_power ;
            operations := op_list @ !operations ;
            loop ()
        | `Timeout ->
            (* Retrieve the remaining operations present in the stream
               before block construction *)
            let remaining_operations =
              extract_op_and_filter_outdated_endorsements
                head.level
                (List.flatten (Lwt_stream.get_available operation_stream))
            in
            operations := remaining_operations @ !operations ;
            compute_minimal_valid_time
              state.constants.parametric
              ~priority
              ~endorsing_power:!current_endorsing_power
              ~predecessor_timestamp:head.timestamp
            >>?= fun expected_validity ->
            return_some (!operations, expected_validity)
        | `Event None ->
            (* Got new head while waiting:
               - not enough endorsements received ;
               - late at baking *)
            return_none
      in
      loop ()

(** Given a delegate baking slot [build_block] constructs a full block
    with consistent operations that went through the client-side
    validation *)
let build_block cctxt ~user_activated_upgrades state seed_nonce_hash
    ((slot_timestamp, (block_info, priority, delegate)) as slot)
    ~liquidity_baking_escape_vote =
  let chain = `Hash block_info.Client_baking_blocks.chain_id in
  let block = `Hash (block_info.hash, 0) in
  Plugin.RPC.current_level cctxt ~offset:1l (chain, block)
  >>=? fun next_level ->
  let seed_nonce_hash =
    if next_level.expected_commitment then Some seed_nonce_hash else None
  in
  Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
  let time = Time.System.of_protocol_exn slot_timestamp in
  Events.(emit try_baking) (block_info.hash, priority, name, time) >>= fun () ->
  compute_endorsement_powers
    cctxt
    state.constants.parametric
    ~chain
    ~block:block_info
  >>=? fun endorsement_powers ->
  (* if --ignore-node-mempool was implemented for the baker, this is
     approximately where it would step in *)
  fetch_operations cctxt ~chain state endorsement_powers slot >>=? function
  | None -> Events.(emit new_head_received) () >>= fun () -> return_none
  | Some (operations, timestamp) -> (
      Operations_source.retrieve state.extra_operations
      >>= fun external_mempool_ops_opt ->
      (* prepend external mempool operations *)
      let operations =
        let mops = List.map PrioritizedOperation.node operations in
        match external_mempool_ops_opt with
        | None -> mops
        | Some ops -> List.map PrioritizedOperation.extern ops @ mops
      in
      classify_operations
        cctxt
        ~chain
        ~hard_gas_limit_per_block:
          state.constants.parametric.hard_gas_limit_per_block
        ~minimal_fees:state.minimal_fees
        ~minimal_nanotez_per_gas_unit:state.minimal_nanotez_per_gas_unit
        ~minimal_nanotez_per_byte:state.minimal_nanotez_per_byte
        ~block
        operations
      >>=? fun (operations, overflowing_ops) ->
      let next_version =
        match
          Tezos_base.Block_header.get_forced_protocol_upgrade
            ~user_activated_upgrades
            ~level:(Raw_level.to_int32 next_level.level)
        with
        | None -> block_info.next_protocol
        | Some hash -> hash
      in
      if Protocol_hash.(Protocol.hash <> next_version) then
        (* Let the shell validate this *)
        shell_prevalidation
          cctxt
          ~chain
          ~block
          ~timestamp
          seed_nonce_hash
          operations
          slot
      else
        let protocol_data =
          forge_faked_protocol_data
            ~priority
            ~seed_nonce_hash
            ~liquidity_baking_escape_vote
        in
        filter_and_apply_operations
          cctxt
          state
          endorsement_powers
          ~chain
          ~block
          ~priority
          ~protocol_data
          block_info
          (operations, overflowing_ops)
        >>= function
        | Error errs ->
            Events.(emit client_side_validation_error) errs >>= fun () ->
            Events.(emit shell_prevalidation_notice) () >>= fun () ->
            shell_prevalidation
              cctxt
              ~chain
              ~block
              ~timestamp
              seed_nonce_hash
              operations
              slot
        | Ok (final_context, (validation_result, _), operations, valid_timestamp)
          ->
            (if
             Time.System.(Systime_os.now () < of_protocol_exn valid_timestamp)
            then
             Events.(emit waiting_before_injection)
               (Systime_os.now (), Time.System.of_protocol_exn valid_timestamp)
             >>= fun () ->
             match Client_baking_scheduling.sleep_until valid_timestamp with
             | None -> Lwt.return_unit
             | Some timeout -> timeout
            else Lwt.return_unit)
            >>= fun () ->
            Events.(emit try_forging)
              ( block_info.hash,
                priority,
                name,
                Time.System.of_protocol_exn timestamp )
            >>= fun () ->
            let current_protocol = block_info.next_protocol in
            let context =
              Shell_context.unwrap_disk_context validation_result.context
            in
            Context.get_protocol context >>= fun next_protocol ->
            if Protocol_hash.equal current_protocol next_protocol then
              finalize_block_header
                final_context.header
                ~timestamp:valid_timestamp
                validation_result
                block_info.predecessor_block_metadata_hash
                block_info.predecessor_operations_metadata_hash
              >>= function
              | Error _ as errs -> Lwt.return errs
              | Ok shell_header ->
                  let raw_ops = List.map (List.map forge) operations in
                  return_some
                    ( block_info,
                      priority,
                      shell_header,
                      raw_ops,
                      delegate,
                      seed_nonce_hash )
            else
              Events.(emit shell_prevalidation_new_protocol) () >>= fun () ->
              shell_prevalidation
                cctxt
                ~chain
                ~block
                ~timestamp
                seed_nonce_hash
                operations
                slot)

type per_block_votes = {liquidity_baking_escape_vote : bool option}

let per_block_votes_encoding =
  let open Data_encoding in
  def "per_block_votes.alpha"
  @@ conv
       (fun {liquidity_baking_escape_vote} -> liquidity_baking_escape_vote)
       (fun liquidity_baking_escape_vote -> {liquidity_baking_escape_vote})
       (obj1 (opt "liquidity_baking_escape_vote" Data_encoding.bool))

type error += Block_vote_file_not_found of string

type error += Block_vote_file_invalid of string

type error += Block_vote_file_wrong_content of string

type error += Block_vote_file_missing_liquidity_baking_escape_vote of string

let () =
  register_error_kind
    `Permanent
    ~id:"Client_baking_forge.block_vote_file_not_found"
    ~title:
      "The provided block vote file path does not point to an existing file."
    ~description:
      "A block vote file path was provided on the command line but the path \
       does not point to an existing file."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file path \"%s\" does not point to an \
         existing file.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_not_found file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_not_found file_path) ;
  register_error_kind
    `Permanent
    ~id:"Client_baking_forge.block_vote_file_invalid"
    ~title:
      "The provided block vote file path does not point to a valid JSON file."
    ~description:
      "A block vote file path was provided on the command line but the path \
       does not point to a valid JSON file."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file path \"%s\" does not point to a valid \
         JSON file. The file exists but its content is not valid JSON.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function Block_vote_file_invalid file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_invalid file_path) ;
  register_error_kind
    `Permanent
    ~id:"Client_baking_forge.block_vote_file_wrong_content"
    ~title:"The content of the provided block vote file is unexpected."
    ~description:
      "The block vote file is valid JSON but its content is not the expected \
       one."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file \"%s\" is a valid JSON file but its \
         content is unexpected. Expecting a JSON file containing either \
         '{\"liquidity_baking_escape_vote\": true}' or \
         '{\"liquidity_baking_escape_vote\": false}'.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_wrong_content file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_wrong_content file_path) ;
  register_error_kind
    `Permanent
    ~id:
      "Client_baking_forge.block_vote_file_missing_liquidity_baking_escape_vote"
    ~title:
      "In the provided block vote file, no entry for liquidity baking escape \
       vote was found"
    ~description:
      "In the provided block vote file, no entry for liquidity baking escape \
       vote was found."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[In the provided block vote file \"%s\", the \
         \"liquidity_baking_escape_vote\" boolean field is missing. Expecting \
         a JSON file containing either '{\"liquidity_baking_escape_vote\": \
         true}' or '{\"liquidity_baking_escape_vote\": false}'.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_missing_liquidity_baking_escape_vote file_path ->
          Some file_path
      | _ -> None)
    (fun file_path ->
      Block_vote_file_missing_liquidity_baking_escape_vote file_path)

let traced_option_to_result ~error =
  Option.fold ~some:ok ~none:(Error_monad.error error)

let check_file_exists file =
  if Sys.file_exists file then Result.return_unit
  else error (Block_vote_file_not_found file)

let read_liquidity_baking_escape_vote ~per_block_vote_file =
  Events.(emit reading_per_block) per_block_vote_file >>= fun () ->
  check_file_exists per_block_vote_file >>?= fun () ->
  trace (Block_vote_file_invalid per_block_vote_file)
  @@ Lwt_utils_unix.Json.read_file per_block_vote_file
  >>=? fun votes_json ->
  Events.(emit per_block_vote_file_notice) "found" >>= fun () ->
  trace (Block_vote_file_wrong_content per_block_vote_file)
  @@ Error_monad.protect (fun () ->
         return
         @@ Data_encoding.Json.destruct per_block_votes_encoding votes_json)
  >>=? fun votes ->
  Events.(emit per_block_vote_file_notice) "JSON decoded" >>= fun () ->
  traced_option_to_result
    ~error:
      (Block_vote_file_missing_liquidity_baking_escape_vote per_block_vote_file)
    votes.liquidity_baking_escape_vote
  >>?= fun liquidity_baking_escape_vote ->
  Events.(emit reading_liquidity_baking) () >>= fun () ->
  Events.(emit liquidity_baking_escape_vote) liquidity_baking_escape_vote
  >>= fun () -> return liquidity_baking_escape_vote

let read_liquidity_baking_escape_vote_no_fail ~per_block_vote_file =
  read_liquidity_baking_escape_vote ~per_block_vote_file >>= function
  | Ok vote -> Lwt.return vote
  | Error errs ->
      Events.(emit per_block_vote_file_fail) errs >>= fun () -> Lwt.return false

(** [bake cctxt state] create a single block when woken up to do
    so. All the necessary information is available in the
    [state.best_slot]. *)
let bake ?per_block_vote_file (cctxt : #Protocol_client_context.full)
    ~user_activated_upgrades ~chain state =
  (match state.best_slot with
  | None -> assert false (* unreachable *)
  | Some slot -> return slot)
  >>=? fun slot ->
  let seed_nonce = generate_seed_nonce () in
  let seed_nonce_hash = Nonce.hash seed_nonce in
  Option.fold
    ~none:(Lwt.return false)
    ~some:(fun per_block_vote_file ->
      read_liquidity_baking_escape_vote_no_fail ~per_block_vote_file)
    per_block_vote_file
  >>= fun liquidity_baking_escape_vote ->
  build_block
    cctxt
    ~user_activated_upgrades
    state
    seed_nonce_hash
    slot
    ~liquidity_baking_escape_vote
  >>=? function
  | Some (head, priority, shell_header, operations, delegate, seed_nonce_hash)
    -> (
      let level = Raw_level.succ head.level in
      Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
      Events.(emit start_injecting_block)
        ( priority,
          shell_header.fitness,
          name,
          shell_header.predecessor,
          delegate )
      >>= fun () ->
      Client_keys.get_key cctxt delegate >>=? fun (_, _, delegate_sk) ->
      inject_block
        cctxt
        ~chain
        ~force:false
        ~shell_header
        ~priority
        ?seed_nonce_hash
        ~delegate_pkh:delegate
        ~delegate_sk
        ~level
        operations
        ~liquidity_baking_escape_vote
      >>= function
      | Error errs ->
          Events.(emit block_injection_failed) (List.concat operations, errs)
          >>= fun () -> return_unit
      | Ok block_hash ->
          Events.(emit injected_block)
            ( block_hash,
              name,
              shell_header.predecessor,
              level,
              priority,
              shell_header.fitness,
              List.concat operations )
          >>= fun () ->
          (if seed_nonce_hash <> None then
           cctxt#with_lock (fun () ->
               let open Client_baking_nonces in
               load cctxt state.nonces_location >>=? fun nonces ->
               let nonces = add nonces block_hash seed_nonce in
               save cctxt state.nonces_location nonces)
           |> trace (error_of_exn (Failure "Error while recording nonce"))
          else return_unit)
          >>=? fun () -> return_unit)
  | None -> return_unit

(** [get_baking_slots] calls the node via RPC to retrieve the potential
    slots for the given delegates within a given range of priority *)
let get_baking_slots cctxt ?(max_priority = default_max_priority) new_head
    delegates =
  let chain = `Hash new_head.Client_baking_blocks.chain_id in
  let block = `Hash (new_head.hash, 0) in
  let level = Raw_level.succ new_head.level in
  Plugin.RPC.Baking_rights.get
    cctxt
    ~max_priority
    ~levels:[level]
    ~delegates
    (chain, block)
  >>= function
  | Error errs ->
      Events.(emit baking_slot_fetch_errors) errs >>= fun () -> Lwt.return_nil
  | Ok [] -> Lwt.return_nil
  | Ok slots ->
      let slots =
        List.filter_map
          (function
            | {Plugin.RPC.Baking_rights.timestamp = None; _} -> None
            | {timestamp = Some timestamp; priority; delegate; _} ->
                Some (timestamp, (new_head, priority, delegate)))
          slots
      in
      Lwt.return slots

(** [compute_best_slot_on_current_level] retrieves, among the given
    delegates, the highest priority slot for the current level. Then,
    it registers this slot in the state so the timeout knows when to
    wake up. *)
let compute_best_slot_on_current_level ?max_priority
    (cctxt : #Protocol_client_context.full) state new_head =
  get_delegates cctxt state >>=? fun delegates ->
  let level = Raw_level.succ new_head.Client_baking_blocks.level in
  get_baking_slots cctxt ?max_priority new_head delegates >>= function
  | [] ->
      let max_priority =
        Option.value ~default:default_max_priority max_priority
      in
      Events.(emit no_slot_found) (level, max_priority) >>= fun () ->
      return_none
      (* No slot found *)
  | h :: t ->
      (* One or more slot found, fetching the best (lowest) priority.
         We do not suppose that the received slots are sorted. *)
      let ((timestamp, (_, priority, delegate)) as best_slot) =
        List.fold_left
          (fun ((_, (_, priority, _)) as acc) ((_, (_, priority', _)) as slot) ->
            if priority < priority' then acc else slot)
          h
          t
      in
      Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
      let time = Time.System.of_protocol_exn timestamp in
      Events.(emit have_baking_slot)
        (level, priority, time, name, new_head.hash, delegate)
      >>= fun () ->
      (* Found at least a slot *)
      return_some best_slot

(** [reveal_potential_nonces] reveal registered nonces *)
let reveal_potential_nonces (cctxt : #Client_context.full) constants ~chain
    ~block =
  cctxt#with_lock (fun () ->
      Client_baking_files.resolve_location cctxt ~chain `Nonce
      >>=? fun nonces_location ->
      Client_baking_nonces.load cctxt nonces_location >>= function
      | Error err -> Events.(emit read_nonce_fail) err >>= fun () -> return_unit
      | Ok nonces -> (
          Client_baking_nonces.get_unrevealed_nonces
            cctxt
            nonces_location
            nonces
          >>= function
          | Error err ->
              Events.(emit nonce_retrieval_fail) err >>= fun () -> return_unit
          | Ok [] -> return_unit
          | Ok nonces_to_reveal -> (
              Client_baking_revelation.inject_seed_nonce_revelation
                cctxt
                ~chain
                ~block
                nonces_to_reveal
              >>= function
              | Error err ->
                  Events.(emit nonce_injection_fail) err >>= fun () ->
                  return_unit
              | Ok () ->
                  (* If some nonces are to be revealed it means:
                     - We entered a new cycle and we can clear old nonces ;
                     - A revelation was not included yet in the cycle beginning.
                     So, it is safe to only filter outdated_nonces there *)
                  Client_baking_nonces.filter_outdated_nonces
                    cctxt
                    ~constants
                    nonces_location
                    nonces
                  >>=? fun live_nonces ->
                  Client_baking_nonces.save cctxt nonces_location live_nonces
                  >>=? fun () -> return_unit)))

(** [create] starts the main loop of the baker. The loop monitors new blocks and
    starts individual baking operations when baking-slots are available to any of
    the [delegates] *)
let create (cctxt : #Protocol_client_context.full) ~user_activated_upgrades
    ?minimal_fees ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte
    ?max_priority ?per_block_vote_file ?extra_operations ~chain ~context_path
    delegates block_stream =
  let state_maker bi =
    Alpha_services.Constants.all cctxt (chain, `Head 0) >>=? fun constants ->
    Client_baking_simulator.load_context ~context_path >>= fun index ->
    Client_baking_simulator.check_context_consistency
      index
      bi.Client_baking_blocks.context
    >>=? fun () ->
    Client_baking_files.resolve_location cctxt ~chain `Nonce
    >>=? fun nonces_location ->
    let state =
      create_state
        ?minimal_fees
        ?minimal_nanotez_per_gas_unit
        ?minimal_nanotez_per_byte
        ?extra_operations
        context_path
        index
        nonces_location
        delegates
        constants
    in
    return state
  in
  let event_k cctxt state new_head =
    reveal_potential_nonces
      cctxt
      state.constants
      ~chain
      ~block:(`Hash (new_head.Client_baking_blocks.hash, 0))
    >>= fun _ignore_nonce_err ->
    compute_best_slot_on_current_level ?max_priority cctxt state new_head
    >>=? fun slot ->
    state.best_slot <- slot ;
    return_unit
  in
  let compute_timeout state =
    match state.best_slot with
    | None ->
        (* No slot, just wait for new blocks which will give more info *)
        Lwt_utils.never_ending ()
    | Some (timestamp, _) -> (
        match Client_baking_scheduling.sleep_until timestamp with
        | None -> Lwt.return_unit
        | Some timeout -> timeout)
  in
  let timeout_k cctxt state () =
    bake ?per_block_vote_file cctxt ~user_activated_upgrades ~chain state
    >>= function
    | Error err ->
        if state.retry_counter = 0 then (
          (* Stop the timeout and wait for the next block *)
          state.best_slot <- None ;
          state.retry_counter <- default_retry_counter ;
          Lwt.return (Error err))
        else
          Events.(emit retrying_on_error) err >>= fun () ->
          state.retry_counter <- pred state.retry_counter ;
          return_unit
    | Ok () ->
        (* Stop the timeout and wait for the next block *)
        state.best_slot <- None ;
        state.retry_counter <- default_retry_counter ;
        return_unit
  in
  let finalizer state = Context.close state.index in
  Option.fold
    ~none:return_unit
    ~some:(fun per_block_vote_file ->
      read_liquidity_baking_escape_vote ~per_block_vote_file
      >>=? fun liquidity_baking_escape_vote ->
      (if liquidity_baking_escape_vote then
       Events.(emit liquidity_baking_escape) ()
      else Events.(emit liquidity_baking_continue) ())
      >>= fun () -> return_unit)
    per_block_vote_file
  >>=? fun () ->
  Client_baking_scheduling.main
    ~name:"baker"
    ~cctxt
    ~stream:block_stream
    ~state_maker
    ~pre_loop:event_k
    ~compute_timeout
    ~timeout_k
    ~event_k
    ~finalizer

module Internal_for_tests = struct
  module PrioritizedOperation = PrioritizedOperation

  let get_manager_content op =
    match get_manager_content op with
    | Some {source; counter; _} -> Some (source, counter)
    | None -> None

  let sort_manager_operations = sort_manager_operations
end
