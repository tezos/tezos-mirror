(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** The assumed number of blocks between operation-creation time and
    the actual time when the operation is included in a block. *)
let default_operation_inclusion_latency = 3

type Environment.Error_monad.error += Cannot_parse_operation (* `Branch *)

type Environment.Error_monad.error += Cannot_serialize_log

type Environment.Error_monad.error += Cannot_retrieve_predecessor_level

let () =
  Environment.Error_monad.register_error_kind
    `Branch
    ~id:"operation.cannot_parse"
    ~title:"Cannot parse operation"
    ~description:"The operation is ill-formed or for another protocol version"
    ~pp:(fun ppf () -> Format.fprintf ppf "The operation cannot be parsed")
    Data_encoding.unit
    (function Cannot_parse_operation -> Some () | _ -> None)
    (fun () -> Cannot_parse_operation) ;
  (* Cannot serialize log *)
  Environment.Error_monad.register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_log"
    ~title:"Not enough gas to serialize execution trace"
    ~description:
      "Execution trace with stacks was to big to be serialized with the \
       provided gas"
    Data_encoding.empty
    (function Cannot_serialize_log -> Some () | _ -> None)
    (fun () -> Cannot_serialize_log) ;
  Environment.Error_monad.register_error_kind
    `Temporary
    ~id:"cannot_retrieve_predecessor_level"
    ~title:"Cannot retrieve predecessor level"
    ~description:"Cannot retrieve predecessor level."
    Data_encoding.empty
    (function Cannot_retrieve_predecessor_level -> Some () | _ -> None)
    (fun () -> Cannot_retrieve_predecessor_level)

module Mempool = struct
  type error_classification =
    [ `Branch_delayed of tztrace
    | `Branch_refused of tztrace
    | `Refused of tztrace
    | `Outdated of tztrace ]

  type nanotez = Q.t

  let nanotez_enc : nanotez Data_encoding.t =
    let open Data_encoding in
    def
      "nanotez"
      ~title:"A thousandth of a mutez"
      ~description:"One thousand nanotez make a mutez (1 tez = 1e9 nanotez)"
      (conv
         (fun q -> (q.Q.num, q.Q.den))
         (fun (num, den) -> {Q.num; den})
         (tup2 z z))

  let manager_op_replacement_factor_enc : Q.t Data_encoding.t =
    let open Data_encoding in
    def
      "manager operation replacement factor"
      ~title:"A manager operation's replacement factor"
      ~description:
        "The fee and fee/gas ratio of an operation to replace another"
      (conv
         (fun q -> (q.Q.num, q.Q.den))
         (fun (num, den) -> {Q.num; den})
         (tup2 z z))

  type config = {
    minimal_fees : Tez.t;
    minimal_nanotez_per_gas_unit : nanotez;
    minimal_nanotez_per_byte : nanotez;
    allow_script_failure : bool;
        (** If [true], this makes [post_filter_manager] unconditionally return
            [`Passed_postfilter filter_state], no matter the operation's
            success. *)
    clock_drift : Period.t option;
    replace_by_fee_factor : Q.t;
        (** This field determines the amount of additional fees (given as a
            factor of the declared fees) a manager should add to an operation
            in order to (eventually) replace an existing (prechecked) one
            in the mempool. Note that other criteria, such as the gas ratio,
            are also taken into account to decide whether to accept the
            replacement or not. *)
    max_prechecked_manager_operations : int;
        (** Maximal number of prechecked operations to keep. The mempool only
            keeps the [max_prechecked_manager_operations] operations with the
            highest fee/gas and fee/size ratios. *)
  }

  let default_minimal_fees =
    match Tez.of_mutez 100L with None -> assert false | Some t -> t

  let default_minimal_nanotez_per_gas_unit = Q.of_int 100

  let default_minimal_nanotez_per_byte = Q.of_int 1000

  let quota = Main.validation_passes

  let managers_index = 3 (* in Main.validation_passes *)

  let managers_quota = Stdlib.List.nth quota managers_index

  (* If the drift is not specified, it will be the duration of round zero.
     It allows only to spam with one future round.

     /!\ Warning /!\ : current plugin implementation implies that this drift
     cumulates with the accepted  drift regarding the current head's timestamp.
  *)
  let default_config =
    {
      minimal_fees = default_minimal_fees;
      minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte = default_minimal_nanotez_per_byte;
      allow_script_failure = true;
      clock_drift = None;
      replace_by_fee_factor =
        Q.make (Z.of_int 105) (Z.of_int 100)
        (* Default value of [replace_by_fee_factor] is set to 5% *);
      max_prechecked_manager_operations = 5_000;
    }

  let config_encoding : config Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {
             minimal_fees;
             minimal_nanotez_per_gas_unit;
             minimal_nanotez_per_byte;
             allow_script_failure;
             clock_drift;
             replace_by_fee_factor;
             max_prechecked_manager_operations;
           } ->
        ( minimal_fees,
          minimal_nanotez_per_gas_unit,
          minimal_nanotez_per_byte,
          allow_script_failure,
          clock_drift,
          replace_by_fee_factor,
          max_prechecked_manager_operations ))
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             allow_script_failure,
             clock_drift,
             replace_by_fee_factor,
             max_prechecked_manager_operations ) ->
        {
          minimal_fees;
          minimal_nanotez_per_gas_unit;
          minimal_nanotez_per_byte;
          allow_script_failure;
          clock_drift;
          replace_by_fee_factor;
          max_prechecked_manager_operations;
        })
      (obj7
         (dft "minimal_fees" Tez.encoding default_config.minimal_fees)
         (dft
            "minimal_nanotez_per_gas_unit"
            nanotez_enc
            default_config.minimal_nanotez_per_gas_unit)
         (dft
            "minimal_nanotez_per_byte"
            nanotez_enc
            default_config.minimal_nanotez_per_byte)
         (dft "allow_script_failure" bool default_config.allow_script_failure)
         (opt "clock_drift" Period.encoding)
         (dft
            "replace_by_fee_factor"
            manager_op_replacement_factor_enc
            default_config.replace_by_fee_factor)
         (dft
            "max_prechecked_manager_operations"
            int31
            default_config.max_prechecked_manager_operations))

  (* For each Prechecked manager operation (batched or not), we associate the
     following information to its source:
     - the operation's hash, needed in case the operation is replaced
       afterwards,
     - the total fee and gas_limit, needed to compare operations of the same
       manager to decide which one has more fees w.r.t. announced gas limit
       (modulo replace_by_fee_factor)
  *)
  type manager_op_info = {
    operation_hash : Operation_hash.t;
    gas_limit : Gas.Arith.fp;
    fee : Tez.t;
    weight : Q.t;
  }

  type manager_op_weight = {operation_hash : Operation_hash.t; weight : Q.t}

  let op_weight_of_info (info : manager_op_info) : manager_op_weight =
    {operation_hash = info.operation_hash; weight = info.weight}

  module ManagerOpWeightSet = Set.Make (struct
    type t = manager_op_weight

    (* Sort by weight *)
    let compare op1 op2 =
      let c = Q.compare op1.weight op2.weight in
      if c <> 0 then c
      else Operation_hash.compare op1.operation_hash op2.operation_hash
  end)

  type state = {
    grandparent_level_start : Alpha_context.Timestamp.t option;
    round_zero_duration : Period.t option;
    op_prechecked_managers : manager_op_info Signature.Public_key_hash.Map.t;
        (** All managers that are the source of manager operations
            prechecked in the mempool. Each manager in the map is associated to
            a record of type [manager_op_info] (See for record details above).
            Each manager in the map should be accessible
            with an operation hash in [operation_hash_to_manager]. *)
    operation_hash_to_manager :
      Signature.Public_key_hash.t Operation_hash.Map.t;
        (** Map of operation hash to manager used to remove a manager from
            [op_prechecked_managers] with an operation hash. Each manager in the
            map should also be in [op_prechecked_managers]. *)
    prechecked_operations_count : int;
        (** Number of prechecked manager operations.
            Invariants:
            - [Operation_hash.Map.cardinal operation_hash_to_manager =
               prechecked_operations_count]
            - [prechecked_operations_count <= max_prechecked_manager_operations] *)
    ops_prechecked : ManagerOpWeightSet.t;
    min_prechecked_op_weight : manager_op_weight option;
        (** The prechecked operation in [op_prechecked_managers], if any, with
            the minimal weight.
            Invariant:
            - [min_prechecked_op_weight = min { x | x \in ops_prechecked }] *)
  }

  let empty : state =
    {
      grandparent_level_start = None;
      round_zero_duration = None;
      op_prechecked_managers = Signature.Public_key_hash.Map.empty;
      operation_hash_to_manager = Operation_hash.Map.empty;
      prechecked_operations_count = 0;
      ops_prechecked = ManagerOpWeightSet.empty;
      min_prechecked_op_weight = None;
    }

  let init config ?(validation_state : validation_state option) ~predecessor ()
      =
    ignore config ;
    (match validation_state with
    | None -> return empty
    | Some {ctxt; _} ->
        let {
          Tezos_base.Block_header.fitness = predecessor_fitness;
          timestamp = predecessor_timestamp;
          _;
        } =
          predecessor.Tezos_base.Block_header.shell
        in
        Alpha_context.Fitness.predecessor_round_from_raw predecessor_fitness
        >>?= fun grandparent_round ->
        Alpha_context.Fitness.round_from_raw predecessor_fitness
        >>?= fun predecessor_round ->
        Alpha_context.(
          let round_durations = Constants.round_durations ctxt in
          let round_zero_duration =
            Round.round_duration round_durations Round.zero
          in
          Round.level_offset_of_round
            round_durations
            ~round:Round.(succ grandparent_round)
          >>?= fun proposal_level_offset ->
          Round.level_offset_of_round round_durations ~round:predecessor_round
          >>?= fun proposal_round_offset ->
          Period.(add proposal_level_offset proposal_round_offset)
          >>?= fun proposal_offset ->
          return
            {
              empty with
              grandparent_level_start =
                Some Timestamp.(predecessor_timestamp - proposal_offset);
              round_zero_duration = Some round_zero_duration;
            }))
    >|= Environment.wrap_tzresult

  let manager_prio p = `Low p

  let consensus_prio = `High

  let other_prio = `Medium

  let on_flush config filter_state ?(validation_state : validation_state option)
      ~predecessor () =
    ignore filter_state ;
    init config ?validation_state ~predecessor ()

  let remove ~(filter_state : state) oph =
    let removed_oph_source = ref None in
    let operation_hash_to_manager =
      Operation_hash.Map.update
        oph
        (function
          | None -> None
          | Some source ->
              removed_oph_source := Some source ;
              None)
        filter_state.operation_hash_to_manager
    in
    match !removed_oph_source with
    | None ->
        (* Not present anywhere in the filter state, because of invariants.
           @see {!state} *)
        filter_state
    | Some source ->
        let prechecked_operations_count =
          filter_state.prechecked_operations_count - 1
        in
        let removed_op = ref None in
        let op_prechecked_managers =
          Signature.Public_key_hash.Map.update
            source
            (function
              | None -> None
              | Some op ->
                  removed_op := Some op ;
                  None)
            filter_state.op_prechecked_managers
        in
        let ops_prechecked =
          match !removed_op with
          | None -> filter_state.ops_prechecked
          | Some op ->
              ManagerOpWeightSet.remove
                (op_weight_of_info op)
                filter_state.ops_prechecked
        in
        let min_prechecked_op_weight =
          match filter_state.min_prechecked_op_weight with
          | None -> None
          | Some op ->
              if Operation_hash.equal op.operation_hash oph then
                ManagerOpWeightSet.min_elt ops_prechecked
              else Some op
        in
        {
          filter_state with
          op_prechecked_managers;
          operation_hash_to_manager;
          ops_prechecked;
          prechecked_operations_count;
          min_prechecked_op_weight;
        }

  let get_manager_operation_gas_and_fee contents =
    let open Operation in
    let l = to_list (Contents_list contents) in
    List.fold_left
      (fun acc -> function
        | Contents (Manager_operation {fee; gas_limit; _}) -> (
            match acc with
            | Error _ as e -> e
            | Ok (total_fee, total_gas) -> (
                match Tez.(total_fee +? fee) with
                | Ok total_fee ->
                    Ok (total_fee, Gas.Arith.add total_gas gas_limit)
                | Error _ as e -> e))
        | _ -> acc)
      (Ok (Tez.zero, Gas.Arith.zero))
      l

  type Environment.Error_monad.error += Fees_too_low

  let () =
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"prefilter.fees_too_low"
      ~title:"Operation fees are too low"
      ~description:"Operation fees are too low"
      ~pp:(fun ppf () -> Format.fprintf ppf "Operation fees are too low")
      Data_encoding.unit
      (function Fees_too_low -> Some () | _ -> None)
      (fun () -> Fees_too_low)

  type Environment.Error_monad.error +=
    | Manager_restriction of {oph : Operation_hash.t; fee : Tez.t}

  let () =
    Environment.Error_monad.register_error_kind
      `Temporary
      ~id:"prefilter.manager_restriction"
      ~title:"Only one manager operation per manager per block allowed"
      ~description:"Only one manager operation per manager per block allowed"
      ~pp:(fun ppf (oph, fee) ->
        Format.fprintf
          ppf
          "Only one manager operation per manager per block allowed (found %a \
           with %atez fee. You may want to use --replace to provide adequate \
           fee and replace it)."
          Operation_hash.pp
          oph
          Tez.pp
          fee)
      Data_encoding.(
        obj2
          (req "operation_hash" Operation_hash.encoding)
          (req "operation_fee" Tez.encoding))
      (function Manager_restriction {oph; fee} -> Some (oph, fee) | _ -> None)
      (fun (oph, fee) -> Manager_restriction {oph; fee})

  type Environment.Error_monad.error +=
    | Manager_operation_replaced of {
        old_hash : Operation_hash.t;
        new_hash : Operation_hash.t;
      }

  let () =
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"plugin.manager_operation_replaced"
      ~title:"Manager operation replaced"
      ~description:"The manager operation has been replaced"
      ~pp:(fun ppf (old_hash, new_hash) ->
        Format.fprintf
          ppf
          "The manager operation %a has been replaced with %a"
          Operation_hash.pp
          old_hash
          Operation_hash.pp
          new_hash)
      (Data_encoding.obj2
         (Data_encoding.req "old_hash" Operation_hash.encoding)
         (Data_encoding.req "new_hash" Operation_hash.encoding))
      (function
        | Manager_operation_replaced {old_hash; new_hash} ->
            Some (old_hash, new_hash)
        | _ -> None)
      (fun (old_hash, new_hash) ->
        Manager_operation_replaced {old_hash; new_hash})

  type Environment.Error_monad.error += Fees_too_low_for_mempool of Tez.t

  let () =
    Environment.Error_monad.register_error_kind
      `Temporary
      ~id:"prefilter.fees_too_low_for_mempool"
      ~title:"Operation fees are too low to be considered in full mempool"
      ~description:"Operation fees are too low to be considered in full mempool"
      ~pp:(fun ppf required_fees ->
        Format.fprintf
          ppf
          "The mempool is full, the number of prechecked manager operations \
           has reached the limit max_prechecked_manager_operations set by the \
           filter. Increase operation fees to at least %atz for the operation \
           to be considered and propagated by THIS node. Note that the \
           operations with the minimum fees in the mempool risk being removed \
           if better ones are received."
          Tez.pp
          required_fees)
      Data_encoding.(obj1 (req "required_fees" Tez.encoding))
      (function
        | Fees_too_low_for_mempool required_fees -> Some required_fees
        | _ -> None)
      (fun required_fees -> Fees_too_low_for_mempool required_fees)

  type Environment.Error_monad.error += Removed_fees_too_low_for_mempool

  let () =
    Environment.Error_monad.register_error_kind
      `Temporary
      ~id:"plugin.removed_fees_too_low_for_mempool"
      ~title:"Operation removed because fees are too low for full mempool"
      ~description:"Operation removed because fees are too low for full mempool"
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "The mempool is full, the number of prechecked manager operations \
           has reached the limit max_prechecked_manager_operations set by the \
           filter. Operation was removed because another operation with a \
           better fees/gas-size ratio was received and accepted by the \
           mempool.")
      Data_encoding.unit
      (function Removed_fees_too_low_for_mempool -> Some () | _ -> None)
      (fun () -> Removed_fees_too_low_for_mempool)

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2238
     Write unit tests for the feature 'replace-by-fee' and for other changes
     introduced by other MRs in the plugin. *)
  (* In order to decide if the new operation can replace an old one from the
     same manager, we check if its fees (resp. fees/gas ratio) are greater than
     (or equal to) the old operations's fees (resp. fees/gas ratio), bumped by
     the factor [config.replace_by_fee_factor].
  *)
  let better_fees_and_ratio =
    let bump config q = Q.mul q config.replace_by_fee_factor in
    fun config old_gas old_fee new_gas new_fee ->
      let old_fee = Tez.to_mutez old_fee |> Z.of_int64 |> Q.of_bigint in
      let old_gas = Gas.Arith.integral_to_z old_gas |> Q.of_bigint in
      let new_fee = Tez.to_mutez new_fee |> Z.of_int64 |> Q.of_bigint in
      let new_gas = Gas.Arith.integral_to_z new_gas |> Q.of_bigint in
      let old_ratio = Q.div old_fee old_gas in
      let new_ratio = Q.div new_fee new_gas in
      Q.compare new_ratio (bump config old_ratio) >= 0
      && Q.compare new_fee (bump config old_fee) >= 0

  let check_manager_restriction config filter_state source ~fee ~gas_limit =
    match
      Signature.Public_key_hash.Map.find
        source
        filter_state.op_prechecked_managers
    with
    | None -> `Fresh
    | Some
        {
          operation_hash = old_hash;
          gas_limit = old_gas;
          fee = old_fee;
          weight = _;
        } ->
        (* Manager already seen: one manager per block limitation triggered.
           Can replace old operation if new operation's fees are better *)
        if
          better_fees_and_ratio
            config
            (Gas.Arith.floor old_gas)
            old_fee
            gas_limit
            fee
        then `Replace old_hash
        else
          `Fail
            (`Branch_delayed
              [
                Environment.wrap_tzerror
                  (Manager_restriction {oph = old_hash; fee = old_fee});
              ])

  let size_of_operation op =
    (WithExceptions.Option.get ~loc:__LOC__
    @@ Data_encoding.Binary.fixed_length
         Tezos_base.Operation.shell_header_encoding)
    + Data_encoding.Binary.length Operation.protocol_data_encoding op

  (** Returns the weight and resources consumption of an operation. The weight
      corresponds to the one implemented by the baker, to decide which operations
      to put in a block first (the code is largely duplicated).
      @see {!Tezos_baking_alpha.Operation_selection.weight_manager} *)
  let weight_and_resources_manager_operation ~validation_state ?size ~fee ~gas
      op =
    let hard_gas_limit_per_block =
      Constants.hard_gas_limit_per_block validation_state.ctxt
    in
    let max_size = managers_quota.max_size in
    let size = match size with None -> size_of_operation op | Some s -> s in
    let size_f = Q.of_int size in
    let gas_f = Q.of_bigint (Gas.Arith.integral_to_z gas) in
    let fee_f = Q.of_int64 (Tez.to_mutez fee) in
    let size_ratio = Q.(size_f / Q.of_int max_size) in
    let gas_ratio =
      Q.(gas_f / Q.of_bigint (Gas.Arith.integral_to_z hard_gas_limit_per_block))
    in
    let resources = Q.max size_ratio gas_ratio in
    (Q.(fee_f / resources), resources)

  (** Returns the weight of an operation, i.e. the fees w.r.t the gas and size
      consumption in the block. *)
  let weight_manager_operation ~validation_state ?size ~fee ~gas op =
    let (weight, _resources) =
      weight_and_resources_manager_operation
        ~validation_state
        ?size
        ~fee
        ~gas
        op
    in
    weight

  (** Return fee for an operation that consumes [op_resources] for its weight to
      be strictly greater than [min_weight]. *)
  let required_fee_manager_operation_weight ~op_resources ~min_weight =
    let req_mutez_q = Q.((min_weight * op_resources) + Q.one) in
    Tez.of_mutez_exn @@ Q.to_int64 req_mutez_q

  (** Check if an operation as a weight (fees w.r.t gas and size) large enough to
      be prechecked and return said weight. In the case where the prechecked
      mempool is full, return an error if the weight is too small, or return the
      operation to be replaced otherwise. *)
  let check_minimal_weight ?validation_state config filter_state ~fee ~gas_limit
      op =
    match validation_state with
    | None -> `Weight_ok (`No_replace, [])
    | Some validation_state -> (
        let (weight, op_resources) =
          weight_and_resources_manager_operation
            ~validation_state
            ~fee
            ~gas:gas_limit
            op
        in
        if
          filter_state.prechecked_operations_count
          < config.max_prechecked_manager_operations
        then
          (* The precheck mempool is not full yet *)
          `Weight_ok (`No_replace, [weight])
        else
          match filter_state.min_prechecked_op_weight with
          | None ->
              (* The precheck mempool is empty *)
              `Weight_ok (`No_replace, [weight])
          | Some {weight = min_weight; operation_hash = min_oph} ->
              if Q.(weight > min_weight) then
                (* The operation has a weight greater than the minimal
                   prechecked operation, replace the latest with the new one *)
                `Weight_ok (`Replace min_oph, [weight])
              else
                (* Otherwise fail and give indication as to what to fee should
                   be for the operation to be prechecked *)
                let required_fee =
                  required_fee_manager_operation_weight
                    ~op_resources
                    ~min_weight
                in
                `Fail
                  (`Branch_delayed
                    [
                      Environment.wrap_tzerror
                        (Fees_too_low_for_mempool required_fee);
                    ]))

  let pre_filter_manager :
      type t.
      config ->
      state ->
      validation_state_before:validation_state option ->
      public_key_hash ->
      Operation.packed_protocol_data ->
      t Kind.manager contents_list ->
      [ `Passed_prefilter of Q.t list
      | `Branch_refused of tztrace
      | `Branch_delayed of tztrace
      | `Refused of tztrace
      | `Outdated of tztrace ] =
   fun config filter_state ~validation_state_before source packed_op op ->
    let size = size_of_operation packed_op in
    let check_gas_and_fee fee gas_limit =
      let fees_in_nanotez =
        Q.mul (Q.of_int64 (Tez.to_mutez fee)) (Q.of_int 1000)
      in
      let minimal_fees_in_nanotez =
        Q.mul (Q.of_int64 (Tez.to_mutez config.minimal_fees)) (Q.of_int 1000)
      in
      let minimal_fees_for_gas_in_nanotez =
        Q.mul
          config.minimal_nanotez_per_gas_unit
          (Q.of_bigint @@ Gas.Arith.integral_to_z gas_limit)
      in
      let minimal_fees_for_size_in_nanotez =
        Q.mul config.minimal_nanotez_per_byte (Q.of_int size)
      in
      if
        Q.compare
          fees_in_nanotez
          (Q.add
             minimal_fees_in_nanotez
             (Q.add
                minimal_fees_for_gas_in_nanotez
                minimal_fees_for_size_in_nanotez))
        >= 0
      then `Fees_ok
      else `Refused [Environment.wrap_tzerror Fees_too_low]
    in
    match get_manager_operation_gas_and_fee op with
    | Error err -> `Refused (Environment.wrap_tztrace err)
    | Ok (fee, gas_limit) -> (
        match
          check_manager_restriction config filter_state source ~fee ~gas_limit
        with
        | `Fail errs -> errs
        | `Fresh | `Replace _ -> (
            match check_gas_and_fee fee gas_limit with
            | `Refused _ as err -> err
            | `Fees_ok -> (
                match
                  check_minimal_weight
                    ?validation_state:validation_state_before
                    config
                    filter_state
                    ~fee
                    ~gas_limit
                    packed_op
                with
                | `Fail errs -> errs
                | `Weight_ok (_, weight) -> `Passed_prefilter weight)))

  type Environment.Error_monad.error += Outdated_endorsement

  let () =
    Environment.Error_monad.register_error_kind
      `Temporary
      ~id:"prefilter.outdated_endorsement"
      ~title:"Endorsement is outdated"
      ~description:"Endorsement is outdated"
      ~pp:(fun ppf () -> Format.fprintf ppf "Endorsement is outdated")
      Data_encoding.unit
      (function Outdated_endorsement -> Some () | _ -> None)
      (fun () -> Outdated_endorsement)

  type Environment.Error_monad.error += Wrong_operation

  let () =
    Environment.Error_monad.register_error_kind
      `Temporary
      ~id:"prefilter.wrong_operation"
      ~title:"Wrong operation"
      ~description:"Failing_noop and old endorsement format are not accepted."
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "Failing_noop and old endorsement format are not accepted")
      Data_encoding.unit
      (function Wrong_operation -> Some () | _ -> None)
      (fun () -> Wrong_operation)

  type Environment.Error_monad.error += Consensus_operation_in_far_future

  let () =
    Environment.Error_monad.register_error_kind
      `Branch
      ~id:"prefilter.Consensus_operation_in_far_future"
      ~title:"Consensus operation in far future"
      ~description:"Consensus operation too far in the future are not accepted."
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "Consensus operation too far in the future are not accepted.")
      Data_encoding.unit
      (function Consensus_operation_in_far_future -> Some () | _ -> None)
      (fun () -> Consensus_operation_in_far_future)

  (** {2} consensus operation filtering.

     In Tenderbake, we increased a lot the number of consensus
      operations, therefore it seems necessary to be able to filter consensus
     operations that could be produced by a Byzantine baker mis-using
     its right to produce operations in future rounds or levels.

      We consider the situation where the head is at level [h_l],
     round [h_r], and with timestamp [h_ts], with the predecessor of the head
     being at round [hp_r].
      We receive at a time [now] a consensus operation for level [op_l] and
     round [op_r].

       A consensus operation is considered too far in the future, and therefore filtered,
      if the earliest possible starting time of its round is greater than the
      current time plus a safety margin of [config.clock_drift].

      To consider potential level 2 reorgs, we first compute the expected
      timestamp of round zero at previous level [hp0_ts],

      All ops at level p_l and round r' such that time(r') is greater than (now + drift) are
     deemed too far in the future:

                  h_r                          op_ts    now+drift     (h_l,r')
     hp0_ts h_0   h_l                            |        |              |
        +----+-----+---------+-------------------+--+-----+--------------+-----------
             |     |         |                   |  |     |              |
             |    h_ts     h_r end time          | now    |        earliest expected
             |     |                             |        |        time of round r'
             |<----op_r rounds duration -------->|        |
                   |
                   |<--------------- operations kept ---->|<-rejected----------...
                   |
                   |<-----------operations considered by the filter -----------...

    For an operation on a proposal at the next level, we consider the minimum
    starting time of the operation's round, obtained by assuming that the proposal
    at the next level was built on top of a proposal at round 0 for the current
    level, itself based on a proposal at round 0 of previous level.
    Operations on proposal with higher levels are treated similarly.

    All ops at the next level and round r' such that timestamp(r') > now+drift
    are deemed too far in the future.

                r=0     r=1   h_r      now     now+drift   (h_l+1,r')
   hp0_ts h_0   h_l           h_l       |          |          |
      +----+---- |-------+----+---------+----------+----------+----------
           |     |       |    |                               |
           |     t0      |   h_ts                      earliest expected
           |     |       |    |                         time of round r'
           |<--- |    earliest|                               |
                 |  next level|                               |
                 |       |<---------------------------------->|
                                  round_offset(r')

  *)

  (** At a given level a consensus operation is acceptable if its earliest
      expected timestamp, [op_earliest_ts] is below the current clock with an
      accepted drift for the clock given by a configuration.  *)
  let acceptable ~drift ~op_earliest_ts ~now_timestamp =
    Timestamp.(
      now_timestamp +? drift >|? fun now_drifted ->
      op_earliest_ts <= now_drifted)

  (** Check that an operation with the given [op_round], at level [op_level]
      is likely to be correct, meaning it could have been produced before
      now (+ the safety margin from configuration).

      Given an operation at level greater or equal than/to the current level, we
      compute the expected timestamp of the operation's round. If the operation
      is at a greater level, we assume that it is based on the proposal at round
      zero of the current level.

      All operations whose (level, round) is lower than or equal to the current
      head are deemed valid.
      Note that in case where their is a high drift in the computer clock, they
      might not have been considered valid by comparing their expected timestamp
      to the clock.

      This is a stricter than necessary filter as it will reject operations that
      could be valid in the current timeframe if the proposal they endorse is
      built over a predecessor of the current proposal that would be of lower
      round than the current one.

      What can we do that would be smarter: get current head's predecessor round
      and timestamp to compute the timestamp t0 of a predecessor that would have
      been proposed at round 0.

      Timestamp of round at current level for an alternative head that would be
      based on such proposal would be computed based on t0.
      For level higher than current head, compute the round's earliest timestamp
      if all proposal passed at round 0 starting from t0.
  *)
  let acceptable_op ~config ~round_durations ~round_zero_duration
      ~proposal_level ~proposal_round ~proposal_timestamp
      ~(proposal_predecessor_level_start : Timestamp.t) ~op_level ~op_round
      ~now_timestamp =
    if
      Raw_level.(succ op_level < proposal_level)
      || (op_level = proposal_level && op_round <= proposal_round)
    then
      (* Past and current round operations are not in the future *)
      (* This case could be handled directly in `pre_filter_far_future_consensus_ops`
         for a (slightly) better performance. *)
      Ok true
    else
      (* If, by some tolerance on local clock drift, the timestamp of the
         current head is itself in the future, we use this time instead of
         now_timestamp *)
      let now_timestamp = Timestamp.(max now_timestamp proposal_timestamp) in
      (* Computing when the current level started. *)
      let drift =
        Option.value ~default:round_zero_duration config.clock_drift
      in
      (* We compute the earliest timestamp possible [op_earliest_ts] for the
         operation's (level,round), as if all proposals were accepted at round 0
         since the previous level. *)
      (* Invariant: [op_level + 1 >= proposal_level] *)
      let level_offset = Raw_level.(diff (succ op_level) proposal_level) in
      Period.mult level_offset round_zero_duration >>? fun time_shift ->
      Timestamp.(proposal_predecessor_level_start +? time_shift)
      >>? fun earliest_op_level_start ->
      (* computing the operations's round start from it's earliest
         possible level start *)
      Round.timestamp_of_another_round_same_level
        round_durations
        ~current_round:Round.zero
        ~current_timestamp:earliest_op_level_start
        ~considered_round:op_round
      >>? fun op_earliest_ts ->
      (* We finally check that the expected time of the operation is
         acceptable *)
      acceptable ~drift ~op_earliest_ts ~now_timestamp

  let pre_filter_far_future_consensus_ops config
      ~filter_state:({grandparent_level_start; round_zero_duration; _} : state)
      ?validation_state_before
      ({level = op_level; round = op_round; _} : consensus_content) : bool Lwt.t
      =
    match
      (grandparent_level_start, validation_state_before, round_zero_duration)
    with
    | (None, _, _) | (_, None, _) | (_, _, None) -> Lwt.return_true
    | ( Some grandparent_level_start,
        Some validation_state_before,
        Some round_zero_duration ) -> (
        let ctxt : t = validation_state_before.ctxt in
        match validation_state_before.mode with
        | Application _ | Partial_application _ | Full_construction _ ->
            assert false
        (* Prefilter is always applied in mempool mode aka Partial_construction *)
        | Partial_construction {predecessor_round = proposal_round; _} -> (
            (let proposal_timestamp =
               Alpha_context.Timestamp.predecessor ctxt
             in
             let now_timestamp = Systime_os.now () |> Time.System.to_protocol in
             let Level.{level; _} = Alpha_context.Level.current ctxt in
             let proposal_level =
               match Raw_level.pred level with
               | None ->
                   (* mempool level is set to the successor of the
                      current head *)
                   assert false
               | Some proposal_level -> proposal_level
             in
             let round_durations =
               Alpha_context.Constants.round_durations ctxt
             in
             Lwt.return
             @@ acceptable_op
                  ~config
                  ~round_durations
                  ~round_zero_duration
                  ~proposal_level
                  ~proposal_round
                  ~proposal_timestamp
                  ~proposal_predecessor_level_start:grandparent_level_start
                  ~op_level
                  ~op_round
                  ~now_timestamp)
            >>= function
            | Ok b -> Lwt.return b
            | _ -> Lwt.return_false))

  (** A quasi infinite amount of "valid" (pre)endorsements could be
      sent by a committee member, one for each possible round number.

      This filter rejects (pre)endorsements that refer to a round
      that could not have been reached within the time span between
      the last head's timestamp and the current local clock.

      We add [config.clock_drift] time as a safety margin.
  *)
  let pre_filter config ~(filter_state : state) ?validation_state_before
      ({shell = _; protocol_data = Operation_data {contents; _} as op} :
        Main.operation) =
    let prefilter_manager_op source manager_op =
      Lwt.return
      @@
      match
        pre_filter_manager
          config
          filter_state
          ~validation_state_before
          source
          op
          manager_op
      with
      | `Passed_prefilter prio -> `Passed_prefilter (manager_prio prio)
      | (`Branch_refused _ | `Branch_delayed _ | `Refused _ | `Outdated _) as
        err ->
          err
    in
    match contents with
    | Single (Failing_noop _) ->
        Lwt.return (`Refused [Environment.wrap_tzerror Wrong_operation])
    | Single (Preendorsement consensus_content)
    | Single (Endorsement consensus_content) ->
        pre_filter_far_future_consensus_ops
          ~filter_state
          config
          ?validation_state_before
          consensus_content
        >>= fun keep ->
        if keep then Lwt.return @@ `Passed_prefilter consensus_prio
        else
          Lwt.return
            (`Branch_refused
              [Environment.wrap_tzerror Consensus_operation_in_far_future])
    | Single (Seed_nonce_revelation _)
    | Single (Double_preendorsement_evidence _)
    | Single (Double_endorsement_evidence _)
    | Single (Double_baking_evidence _)
    | Single (Activate_account _)
    | Single (Proposals _)
    | Single (Ballot _) ->
        Lwt.return @@ `Passed_prefilter other_prio
    | Single (Manager_operation {source; _}) as op ->
        prefilter_manager_op source op
    | Cons (Manager_operation {source; _}, _) as op ->
        prefilter_manager_op source op

  let precheck_manager :
      type t.
      config ->
      state ->
      validation_state ->
      Operation_hash.t ->
      Tezos_base.Operation.shell_header ->
      t Kind.manager protocol_data ->
      nb_successful_prechecks:int ->
      fee:Tez.t ->
      gas_limit:Gas.Arith.fp ->
      public_key_hash ->
      [> `Prechecked_manager of
         [`No_replace | `Replace of Operation_hash.t * error_classification]
      | error_classification ]
      Lwt.t =
   fun config
       filter_state
       validation_state
       oph
       shell
       ({contents; _} as protocol_data : t Kind.manager protocol_data)
       ~nb_successful_prechecks
       ~fee
       ~gas_limit
       source ->
    let precheck_manager_and_check_signature ~on_success =
      ( Main.precheck_manager validation_state contents >>=? fun () ->
        let (raw_operation : t Kind.manager operation) =
          Alpha_context.{shell; protocol_data}
        in
        if Compare.Int.(nb_successful_prechecks > 0) then
          (* Signature succesfully checked at least once. *)
          return_unit
        else
          (* Signature probably never checked. *)
          Main.check_manager_signature validation_state contents raw_operation
      )
      >|= function
      | Ok () -> on_success
      | Error err -> (
          let err = Environment.wrap_tztrace err in
          match classify_trace err with
          | Branch -> `Branch_refused err
          | Permanent -> `Refused err
          | Temporary -> `Branch_delayed err
          | Outdated -> `Outdated err)
    in
    let gas_limit = Gas.Arith.floor gas_limit in
    match
      check_manager_restriction config filter_state source ~fee ~gas_limit
    with
    | `Fail err -> Lwt.return err
    | `Replace old_oph ->
        let err =
          Environment.wrap_tzerror
          @@ Manager_operation_replaced {old_hash = old_oph; new_hash = oph}
        in
        precheck_manager_and_check_signature
          ~on_success:
            (`Prechecked_manager (`Replace (old_oph, `Outdated [err])))
    | `Fresh -> (
        match
          check_minimal_weight
            ~validation_state
            config
            filter_state
            ~fee
            ~gas_limit
            (Operation_data protocol_data)
        with
        | `Fail err -> Lwt.return err
        | `Weight_ok (replacement, _weight) ->
            let on_success =
              match replacement with
              | `No_replace -> `Prechecked_manager `No_replace
              | `Replace oph ->
                  (* The operation with the lowest fees ratio, is reclassified as
                     branch_delayed. *)
                  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2347 The
                     branch_delayed ring is bounded to 1000, so we may loose
                     operations. We can probably do better. *)
                  `Prechecked_manager
                    (`Replace
                      ( oph,
                        `Branch_delayed
                          [
                            Environment.wrap_tzerror
                              Removed_fees_too_low_for_mempool;
                          ] ))
            in
            precheck_manager_and_check_signature ~on_success)

  let add_manager_restriction filter_state oph info source replacement =
    let filter_state =
      match replacement with
      | `No_replace -> filter_state
      | `Replace (oph, _class) -> remove ~filter_state oph
    in
    let prechecked_operations_count =
      if Operation_hash.Map.mem oph filter_state.operation_hash_to_manager then
        filter_state.prechecked_operations_count
      else filter_state.prechecked_operations_count + 1
    in
    let op_weight = op_weight_of_info info in
    let min_prechecked_op_weight =
      match filter_state.min_prechecked_op_weight with
      | Some mini when Q.(mini.weight < info.weight) -> Some mini
      | Some _ | None -> Some op_weight
    in
    {
      filter_state with
      op_prechecked_managers =
        (* Manager not seen yet, record it for next ops *)
        Signature.Public_key_hash.Map.add
          source
          info
          filter_state.op_prechecked_managers;
      operation_hash_to_manager =
        Operation_hash.Map.add oph source filter_state.operation_hash_to_manager
        (* Record which manager is used for the operation hash. *);
      ops_prechecked =
        ManagerOpWeightSet.add op_weight filter_state.ops_prechecked;
      prechecked_operations_count;
      min_prechecked_op_weight;
    }

  let precheck :
      config ->
      filter_state:state ->
      validation_state:validation_state ->
      Operation_hash.t ->
      Main.operation ->
      nb_successful_prechecks:int ->
      [ `Passed_precheck of
        state
        * [`No_replace | `Replace of Operation_hash.t * error_classification]
      | error_classification
      | `Undecided ]
      Lwt.t =
   fun config
       ~filter_state
       ~validation_state
       oph
       {shell = shell_header; protocol_data = Operation_data protocol_data}
       ~nb_successful_prechecks ->
    let precheck_manager protocol_data source op =
      match get_manager_operation_gas_and_fee op with
      | Error err -> Lwt.return (`Refused (Environment.wrap_tztrace err))
      | Ok (fee, gas_limit) -> (
          let weight =
            weight_manager_operation
              ~validation_state
              ~fee
              ~gas:gas_limit
              (Operation_data protocol_data)
          in
          let gas_limit = Gas.Arith.fp gas_limit in
          let info = {operation_hash = oph; gas_limit; fee; weight} in
          precheck_manager
            config
            filter_state
            validation_state
            oph
            shell_header
            protocol_data
            source
            ~nb_successful_prechecks
            ~fee
            ~gas_limit
          >|= function
          | `Prechecked_manager replacement ->
              let filter_state =
                add_manager_restriction filter_state oph info source replacement
              in
              `Passed_precheck (filter_state, replacement)
          | (`Refused _ | `Branch_delayed _ | `Branch_refused _ | `Outdated _)
            as errs ->
              errs)
    in
    match protocol_data.contents with
    | Single (Manager_operation {source; _}) as op ->
        precheck_manager protocol_data source op
    | Cons (Manager_operation {source; _}, _) as op ->
        precheck_manager protocol_data source op
    | Single _ -> Lwt.return `Undecided

  open Apply_results

  type Environment.Error_monad.error += Skipped_operation

  let () =
    Environment.Error_monad.register_error_kind
      `Temporary
      ~id:"postfilter.skipped_operation"
      ~title:"The operation has been skipped by the protocol"
      ~description:"The operation has been skipped by the protocol"
      ~pp:(fun ppf () ->
        Format.fprintf ppf "The operation has been skipped by the protocol")
      Data_encoding.unit
      (function Skipped_operation -> Some () | _ -> None)
      (fun () -> Skipped_operation)

  type Environment.Error_monad.error += Backtracked_operation

  let () =
    Environment.Error_monad.register_error_kind
      `Temporary
      ~id:"postfilter.backtracked_operation"
      ~title:"The operation has been backtracked by the protocol"
      ~description:"The operation has been backtracked by the protocol"
      ~pp:(fun ppf () ->
        Format.fprintf ppf "The operation has been backtracked by the protocol")
      Data_encoding.unit
      (function Backtracked_operation -> Some () | _ -> None)
      (fun () -> Backtracked_operation)

  let rec post_filter_manager :
      type t.
      Alpha_context.t ->
      state ->
      t Kind.manager contents_result_list ->
      config ->
      [`Passed_postfilter of state | `Refused of tztrace] =
   fun ctxt filter_state result config ->
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/2181
       This function should be unit tested.
       The errors that can be raised if allow_script_failure is enable should
       be tested. *)
    match result with
    | Single_result (Manager_operation_result {operation_result; _}) -> (
        let check_allow_script_failure errs =
          if config.allow_script_failure then `Passed_postfilter filter_state
          else `Refused errs
        in
        match operation_result with
        | Applied _ -> `Passed_postfilter filter_state
        | Skipped _ ->
            check_allow_script_failure
              [Environment.wrap_tzerror Skipped_operation]
        | Failed (_, errors) ->
            check_allow_script_failure (Environment.wrap_tztrace errors)
        | Backtracked (_, errors) ->
            check_allow_script_failure
              (match errors with
              | Some e -> Environment.wrap_tztrace e
              | None -> [Environment.wrap_tzerror Backtracked_operation]))
    | Cons_result (Manager_operation_result res, rest) -> (
        post_filter_manager
          ctxt
          filter_state
          (Single_result (Manager_operation_result res))
          config
        |> function
        | `Passed_postfilter filter_state ->
            post_filter_manager ctxt filter_state rest config
        | `Refused _ as errs -> errs)

  let post_filter config ~(filter_state : state) ~validation_state_before:_
      ~validation_state_after:({ctxt; _} : validation_state) (_op, receipt) =
    match receipt with
    | No_operation_metadata -> assert false (* only for multipass validator *)
    | Operation_metadata {contents} -> (
        match contents with
        | Single_result (Preendorsement_result _)
        | Single_result (Endorsement_result _)
        | Single_result (Seed_nonce_revelation_result _)
        | Single_result (Double_preendorsement_evidence_result _)
        | Single_result (Double_endorsement_evidence_result _)
        | Single_result (Double_baking_evidence_result _)
        | Single_result (Activate_account_result _)
        | Single_result Proposals_result
        | Single_result Ballot_result ->
            Lwt.return (`Passed_postfilter filter_state)
        | Single_result (Manager_operation_result _) as result ->
            Lwt.return (post_filter_manager ctxt filter_state result config)
        | Cons_result (Manager_operation_result _, _) as result ->
            Lwt.return (post_filter_manager ctxt filter_state result config))
end

module View_helpers = struct
  open Tezos_micheline

  type Environment.Error_monad.error += Viewed_contract_has_no_script

  type Environment.Error_monad.error += View_callback_origination_failed

  type Environment.Error_monad.error +=
    | Illformed_view_type of Entrypoint.t * Script.expr

  type Environment.Error_monad.error +=
    | View_never_returns of Entrypoint.t * Contract.t

  type Environment.Error_monad.error +=
    | View_unexpected_return of Entrypoint.t * Contract.t

  let () =
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewedContractHasNoScript"
      ~title:"Viewed contract has no script"
      ~description:"A view was called on a contract with no script."
      ~pp:(fun ppf () ->
        Format.fprintf ppf "A view was called on a contract with no script.")
      Data_encoding.(unit)
      (function Viewed_contract_has_no_script -> Some () | _ -> None)
      (fun () -> Viewed_contract_has_no_script) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewCallbackOriginationFailed"
      ~title:"View callback origination failed"
      ~description:"View callback origination failed"
      ~pp:(fun ppf () ->
        Format.fprintf ppf "Error during origination of view callback contract.")
      Data_encoding.(unit)
      (function View_callback_origination_failed -> Some () | _ -> None)
      (fun () -> View_callback_origination_failed) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"illformedViewType"
      ~title:"An entrypoint type is incompatible with TZIP-4 view type."
      ~description:"An entrypoint type is incompatible with TZIP-4 view type."
      ~pp:(fun ppf (entrypoint, typ) ->
        Format.fprintf
          ppf
          "The view %a has type %a, it is not compatible with a TZIP-4 view \
           type."
          Entrypoint.pp
          entrypoint
          Micheline_printer.print_expr
          (Micheline_printer.printable
             (fun x -> x)
             (Michelson_v1_primitives.strings_of_prims typ)))
      Data_encoding.(
        obj2
          (req "entrypoint" Entrypoint.simple_encoding)
          (req "type" Script.expr_encoding))
      (function Illformed_view_type (etp, exp) -> Some (etp, exp) | _ -> None)
      (fun (etp, exp) -> Illformed_view_type (etp, exp)) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewNeverReturns"
      ~title:
        "A view never returned a transaction to the given callback contract"
      ~description:
        "A view never initiated a transaction to the given callback contract."
      ~pp:(fun ppf (entrypoint, callback) ->
        Format.fprintf
          ppf
          "The view %a never initiated a transaction to the given callback \
           contract %a."
          Entrypoint.pp
          entrypoint
          Contract.pp
          callback)
      Data_encoding.(
        obj2
          (req "entrypoint" Entrypoint.simple_encoding)
          (req "callback" Contract.encoding))
      (function View_never_returns (e, c) -> Some (e, c) | _ -> None)
      (fun (e, c) -> View_never_returns (e, c)) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewUnexpectedReturn"
      ~title:"A view returned an unexpected list of operations"
      ~description:
        "A view initiated a list of operations while the TZIP-4 standard \
         expects only a transaction to the given callback contract."
      ~pp:(fun ppf (entrypoint, callback) ->
        Format.fprintf
          ppf
          "The view %a initiated a list of operations while the TZIP-4 \
           standard expects only a transaction to the given callback contract \
           %a."
          Entrypoint.pp
          entrypoint
          Contract.pp
          callback)
      Data_encoding.(
        obj2
          (req "entrypoint" Entrypoint.simple_encoding)
          (req "callback" Contract.encoding))
      (function View_never_returns (e, c) -> Some (e, c) | _ -> None)
      (fun (e, c) -> View_never_returns (e, c))

  (* This script is actually never run, its usage is to ensure a
     contract that has the type `contract <ty>` is originated, which
     will be required as callback of the view. *)
  let make_viewer_script ty : Script.t =
    let loc = 0 in
    let ty = Micheline.root ty in
    let code =
      Micheline.strip_locations
      @@ Micheline.Seq
           ( loc,
             [
               Micheline.Prim (loc, Script.K_parameter, [ty], []);
               Micheline.Prim
                 ( loc,
                   Script.K_storage,
                   [Micheline.Prim (loc, Script.T_unit, [], [])],
                   [] );
               Micheline.Prim
                 ( loc,
                   Script.K_code,
                   [Micheline.Prim (loc, Script.I_FAILWITH, [], [])],
                   [] );
             ] )
    in
    let storage =
      Micheline.strip_locations (Micheline.Prim (loc, Script.D_Unit, [], []))
    in
    {code = Script.lazy_expr code; storage = Script.lazy_expr storage}

  let make_view_parameter input callback =
    let loc = 0 in
    Micheline.strip_locations
      (Micheline.Prim
         ( loc,
           Script.D_Pair,
           [
             input;
             Micheline.Bytes
               ( loc,
                 Data_encoding.Binary.to_bytes_exn Contract.encoding callback );
           ],
           [] ))

  let extract_view_output_type entrypoint ty =
    match Micheline.root ty with
    | Micheline.Prim
        ( _,
          Script.T_pair,
          [_; Micheline.Prim (_, Script.T_contract, [ty], _)],
          _ ) ->
        ok (Micheline.strip_locations ty)
    | _ -> Environment.Error_monad.error (Illformed_view_type (entrypoint, ty))

  (* 'view' entrypoints returns their value by calling a callback contract, thus
     the expected result is a unique internal transaction to this callback. *)
  let extract_parameter_from_operations entrypoint operations callback =
    let unexpected_return =
      Environment.Error_monad.error
      @@ View_unexpected_return (entrypoint, callback)
    in
    match operations with
    | [
     Internal_operation
       {operation = Transaction {destination; parameters; _}; _};
    ]
      when Destination.equal destination (Contract callback) ->
        ok parameters
    | [] ->
        Environment.Error_monad.error
          (View_never_returns (entrypoint, callback))
    | _ -> unexpected_return
end

module RPC = struct
  open Environment
  open Alpha_context
  open Environment.Error_monad

  let parse_operation (op : Operation.raw) =
    match
      Data_encoding.Binary.of_bytes_opt
        Operation.protocol_data_encoding
        op.proto
    with
    | Some protocol_data -> ok {shell = op.shell; protocol_data}
    | None -> error Cannot_parse_operation

  let path = RPC_path.(open_root / "helpers")

  module Registration = struct
    let patched_services =
      ref (RPC_directory.empty : Updater.rpc_context RPC_directory.t)

    let register0_fullctxt ~chunked s f =
      patched_services :=
        RPC_directory.register ~chunked !patched_services s (fun ctxt q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt q i)

    let register0 ~chunked s f =
      register0_fullctxt ~chunked s (fun {context; _} -> f context)

    let register0_noctxt ~chunked s f =
      patched_services :=
        RPC_directory.register ~chunked !patched_services s (fun _ q i -> f q i)

    let opt_register0_fullctxt ~chunked s f =
      patched_services :=
        RPC_directory.opt_register ~chunked !patched_services s (fun ctxt q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt q i)

    let opt_register0 ~chunked s f =
      opt_register0_fullctxt ~chunked s (fun {context; _} -> f context)

    let register1_fullctxt ~chunked s f =
      patched_services :=
        RPC_directory.register
          ~chunked
          !patched_services
          s
          (fun (ctxt, arg) q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt arg q i)

    let register1 ~chunked s f =
      register1_fullctxt ~chunked s (fun {context; _} x -> f context x)

    let register2_fullctxt ~chunked s f =
      patched_services :=
        RPC_directory.register
          ~chunked
          !patched_services
          s
          (fun ((ctxt, arg1), arg2) q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt ->
            f ctxt arg1 arg2 q i)

    let register2 ~chunked s f =
      register2_fullctxt ~chunked s (fun {context; _} a1 a2 q i ->
          f context a1 a2 q i)
  end

  let unparsing_mode_encoding =
    let open Script_ir_translator in
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          ~title:"Readable"
          (constant "Readable")
          (function
            | Readable -> Some () | Optimized | Optimized_legacy -> None)
          (fun () -> Readable);
        case
          (Tag 1)
          ~title:"Optimized"
          (constant "Optimized")
          (function
            | Optimized -> Some () | Readable | Optimized_legacy -> None)
          (fun () -> Optimized);
        case
          (Tag 2)
          ~title:"Optimized_legacy"
          (constant "Optimized_legacy")
          (function
            | Optimized_legacy -> Some () | Readable | Optimized -> None)
          (fun () -> Optimized_legacy);
      ]

  module Scripts = struct
    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "scripts")

      let run_code_input_encoding =
        merge_objs
          (obj10
             (req "script" Script.expr_encoding)
             (req "storage" Script.expr_encoding)
             (req "input" Script.expr_encoding)
             (req "amount" Tez.encoding)
             (opt "balance" Tez.encoding)
             (req "chain_id" Chain_id.encoding)
             (opt "source" Contract.encoding)
             (opt "payer" Contract.encoding)
             (opt "self" Contract.encoding)
             (dft "entrypoint" Entrypoint.simple_encoding Entrypoint.default))
          (obj4
             (opt "unparsing_mode" unparsing_mode_encoding)
             (opt "gas" Gas.Arith.z_integral_encoding)
             (opt "now" Script_timestamp.encoding)
             (opt "level" Script_int.n_encoding))

      let run_code_output_encoding =
        conv
          (fun (storage, operations, lazy_storage_diff) ->
            (storage, operations, lazy_storage_diff, lazy_storage_diff))
          (fun (storage, operations, legacy_lazy_storage_diff, lazy_storage_diff)
               ->
            let lazy_storage_diff =
              Option.either lazy_storage_diff legacy_lazy_storage_diff
            in
            (storage, operations, lazy_storage_diff))
          (obj4
             (req "storage" Script.expr_encoding)
             (req "operations" (list Operation.internal_operation_encoding))
             (opt "big_map_diff" Lazy_storage.legacy_big_map_diff_encoding)
             (opt "lazy_storage_diff" Lazy_storage.encoding))

      let trace_code_input_encoding = run_code_input_encoding

      let trace_encoding =
        def "scripted.trace" @@ list
        @@ obj3
             (req "location" Script.location_encoding)
             (req "gas" Gas.encoding)
             (req "stack" (list Script.expr_encoding))

      let trace_code_output_encoding =
        conv
          (fun (storage, operations, trace, lazy_storage_diff) ->
            (storage, operations, trace, lazy_storage_diff, lazy_storage_diff))
          (fun ( storage,
                 operations,
                 trace,
                 legacy_lazy_storage_diff,
                 lazy_storage_diff ) ->
            let lazy_storage_diff =
              Option.either lazy_storage_diff legacy_lazy_storage_diff
            in
            (storage, operations, trace, lazy_storage_diff))
          (obj5
             (req "storage" Script.expr_encoding)
             (req "operations" (list Operation.internal_operation_encoding))
             (req "trace" trace_encoding)
             (opt "big_map_diff" Lazy_storage.legacy_big_map_diff_encoding)
             (opt "lazy_storage_diff" Lazy_storage.encoding))

      let run_view_encoding =
        let open Data_encoding in
        obj10
          (req "contract" Contract.encoding)
          (req "entrypoint" Entrypoint.simple_encoding)
          (req "input" Script.expr_encoding)
          (req "chain_id" Chain_id.encoding)
          (opt "source" Contract.encoding)
          (opt "payer" Contract.encoding)
          (opt "gas" Gas.Arith.z_integral_encoding)
          (req "unparsing_mode" unparsing_mode_encoding)
          (opt "now" Script_timestamp.encoding)
          (opt "level" Script_int.n_encoding)

      let run_code =
        RPC_service.post_service
          ~description:"Run a piece of code in the current context"
          ~query:RPC_query.empty
          ~input:run_code_input_encoding
          ~output:run_code_output_encoding
          RPC_path.(path / "run_code")

      let trace_code =
        RPC_service.post_service
          ~description:
            "Run a piece of code in the current context, keeping a trace"
          ~query:RPC_query.empty
          ~input:trace_code_input_encoding
          ~output:trace_code_output_encoding
          RPC_path.(path / "trace_code")

      let run_view =
        RPC_service.post_service
          ~description:
            "Simulate a call to a view following the TZIP-4 standard. See \
             https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md#view-entrypoints."
          ~input:run_view_encoding
          ~output:(obj1 (req "data" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "run_view")

      let typecheck_code =
        RPC_service.post_service
          ~description:"Typecheck a piece of code in the current context"
          ~query:RPC_query.empty
          ~input:
            (obj4
               (req "program" Script.expr_encoding)
               (opt "gas" Gas.Arith.z_integral_encoding)
               (opt "legacy" bool)
               (opt "show_types" bool))
          ~output:
            (obj2
               (req "type_map" Script_tc_errors_registration.type_map_enc)
               (req "gas" Gas.encoding))
          RPC_path.(path / "typecheck_code")

      let script_size =
        RPC_service.post_service
          ~description:"Compute the size of a script in the current context"
          ~query:RPC_query.empty
          ~input:
            (obj4
               (req "program" Script.expr_encoding)
               (req "storage" Script.expr_encoding)
               (opt "gas" Gas.Arith.z_integral_encoding)
               (opt "legacy" bool))
          ~output:(obj1 (req "script_size" int31))
          RPC_path.(path / "script_size")

      let typecheck_data =
        RPC_service.post_service
          ~description:
            "Check that some data expression is well formed and of a given \
             type in the current context"
          ~query:RPC_query.empty
          ~input:
            (obj4
               (req "data" Script.expr_encoding)
               (req "type" Script.expr_encoding)
               (opt "gas" Gas.Arith.z_integral_encoding)
               (opt "legacy" bool))
          ~output:(obj1 (req "gas" Gas.encoding))
          RPC_path.(path / "typecheck_data")

      let pack_data =
        RPC_service.post_service
          ~description:
            "Computes the serialized version of some data expression using the \
             same algorithm as script instruction PACK"
          ~input:
            (obj3
               (req "data" Script.expr_encoding)
               (req "type" Script.expr_encoding)
               (opt "gas" Gas.Arith.z_integral_encoding))
          ~output:(obj2 (req "packed" bytes) (req "gas" Gas.encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "pack_data")

      let normalize_data =
        RPC_service.post_service
          ~description:
            "Normalizes some data expression using the requested unparsing mode"
          ~input:
            (obj4
               (req "data" Script.expr_encoding)
               (req "type" Script.expr_encoding)
               (req "unparsing_mode" unparsing_mode_encoding)
               (opt "legacy" bool))
          ~output:(obj1 (req "normalized" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "normalize_data")

      let normalize_script =
        RPC_service.post_service
          ~description:
            "Normalizes a Michelson script using the requested unparsing mode"
          ~input:
            (obj2
               (req "script" Script.expr_encoding)
               (req "unparsing_mode" unparsing_mode_encoding))
          ~output:(obj1 (req "normalized" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "normalize_script")

      let normalize_type =
        RPC_service.post_service
          ~description:
            "Normalizes some Michelson type by expanding `pair a b c` as `pair \
             a (pair b c)"
          ~input:(obj1 (req "type" Script.expr_encoding))
          ~output:(obj1 (req "normalized" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "normalize_type")

      let run_operation =
        RPC_service.post_service
          ~description:"Run an operation without signature checks"
          ~query:RPC_query.empty
          ~input:
            (obj2
               (req "operation" Operation.encoding)
               (req "chain_id" Chain_id.encoding))
          ~output:Apply_results.operation_data_and_metadata_encoding
          RPC_path.(path / "run_operation")

      let simulate_operation =
        RPC_service.post_service
          ~description:"Simulate an operation"
          ~query:RPC_query.empty
          ~input:
            (obj3
               (req "operation" Operation.encoding)
               (req "chain_id" Chain_id.encoding)
               (dft "latency" int16 default_operation_inclusion_latency))
          ~output:Apply_results.operation_data_and_metadata_encoding
          RPC_path.(path / "simulate_operation")

      let entrypoint_type =
        RPC_service.post_service
          ~description:"Return the type of the given entrypoint"
          ~query:RPC_query.empty
          ~input:
            (obj2
               (req "script" Script.expr_encoding)
               (dft "entrypoint" Entrypoint.simple_encoding Entrypoint.default))
          ~output:(obj1 (req "entrypoint_type" Script.expr_encoding))
          RPC_path.(path / "entrypoint")

      let list_entrypoints =
        RPC_service.post_service
          ~description:"Return the list of entrypoints of the given script"
          ~query:RPC_query.empty
          ~input:(obj1 (req "script" Script.expr_encoding))
          ~output:
            (obj2
               (dft
                  "unreachable"
                  (Data_encoding.list
                     (obj1
                        (req
                           "path"
                           (Data_encoding.list
                              Michelson_v1_primitives.prim_encoding))))
                  [])
               (req "entrypoints" (assoc Script.expr_encoding)))
          RPC_path.(path / "entrypoints")
    end

    module type UNPARSING_MODE = sig
      val unparsing_mode : Script_ir_translator.unparsing_mode
    end

    module Traced_interpreter (Unparsing_mode : UNPARSING_MODE) = struct
      type log_element =
        | Log :
            context
            * Script.location
            * ('a * 's)
            * ('a, 's) Script_typed_ir.stack_ty
            -> log_element

      let unparse_stack ctxt (stack, stack_ty) =
        (* We drop the gas limit as this function is only used for debugging/errors. *)
        let ctxt = Gas.set_unlimited ctxt in
        let rec unparse_stack :
            type a s.
            (a, s) Script_typed_ir.stack_ty * (a * s) ->
            Script.expr list tzresult Lwt.t = function
          | (Bot_t, (EmptyCell, EmptyCell)) -> return_nil
          | (Item_t (ty, rest_ty), (v, rest)) ->
              Script_ir_translator.unparse_data
                ctxt
                Unparsing_mode.unparsing_mode
                ty
                v
              >>=? fun (data, _ctxt) ->
              unparse_stack (rest_ty, rest) >|=? fun rest ->
              let data = Micheline.strip_locations data in
              data :: rest
        in
        unparse_stack (stack_ty, stack)

      let trace_logger () : Script_typed_ir.logger =
        let log : log_element list ref = ref [] in
        let log_interp _ ctxt loc sty stack =
          log := Log (ctxt, loc, stack, sty) :: !log
        in
        let log_entry _ _ctxt _loc _sty _stack = () in
        let log_exit _ ctxt loc sty stack =
          log := Log (ctxt, loc, stack, sty) :: !log
        in
        let log_control _ = () in
        let get_log () =
          List.map_es
            (fun (Log (ctxt, loc, stack, stack_ty)) ->
              trace Cannot_serialize_log (unparse_stack ctxt (stack, stack_ty))
              >>=? fun stack -> return (loc, Gas.level ctxt, stack))
            !log
          >>=? fun res -> return (Some (List.rev res))
        in
        {log_exit; log_entry; log_interp; get_log; log_control}

      let execute ctxt step_constants ~script ~entrypoint ~parameter =
        let open Script_interpreter in
        let logger = trace_logger () in
        execute
          ~logger
          ~cached_script:None
          ctxt
          Unparsing_mode.unparsing_mode
          step_constants
          ~script
          ~entrypoint
          ~parameter
          ~internal:true
        >>=? fun ({ctxt; storage; lazy_storage_diff; operations}, _) ->
        logger.get_log () >|=? fun trace ->
        let trace = Option.value ~default:[] trace in
        ({ctxt; storage; lazy_storage_diff; operations}, trace)
    end

    let typecheck_data :
        legacy:bool ->
        context ->
        Script.expr * Script.expr ->
        context tzresult Lwt.t =
     fun ~legacy ctxt (data, exp_ty) ->
      record_trace
        (Script_tc_errors.Ill_formed_type (None, exp_ty, 0))
        (Script_ir_translator.parse_parameter_ty
           ctxt
           ~legacy
           (Micheline.root exp_ty))
      >>?= fun (Ex_ty exp_ty, ctxt) ->
      trace_eval
        (fun () ->
          let exp_ty = Script_ir_translator.serialize_ty_for_error exp_ty in
          Script_tc_errors.Ill_typed_data (None, data, exp_ty))
        (let allow_forged =
           true
           (* Safe since we ignore the value afterwards. *)
         in
         Script_ir_translator.parse_data
           ctxt
           ~legacy
           ~allow_forged
           exp_ty
           (Micheline.root data))
      >|=? fun (_, ctxt) -> ctxt

    module Unparse_types = struct
      (* Same as the unparsing functions for types in Script_ir_translator but
         does not consume gas and never folds (pair a (pair b c)) *)

      open Script_ir_translator
      open Micheline
      open Michelson_v1_primitives
      open Script_typed_ir

      let rec unparse_comparable_ty :
          type a loc.
          loc:loc -> a comparable_ty -> (loc, Script.prim) Micheline.node =
       fun ~loc -> function
        | Unit_key _meta -> Prim (loc, T_unit, [], [])
        | Never_key _meta -> Prim (loc, T_never, [], [])
        | Int_key _meta -> Prim (loc, T_int, [], [])
        | Nat_key _meta -> Prim (loc, T_nat, [], [])
        | Signature_key _meta -> Prim (loc, T_signature, [], [])
        | String_key _meta -> Prim (loc, T_string, [], [])
        | Bytes_key _meta -> Prim (loc, T_bytes, [], [])
        | Mutez_key _meta -> Prim (loc, T_mutez, [], [])
        | Bool_key _meta -> Prim (loc, T_bool, [], [])
        | Key_hash_key _meta -> Prim (loc, T_key_hash, [], [])
        | Key_key _meta -> Prim (loc, T_key, [], [])
        | Timestamp_key _meta -> Prim (loc, T_timestamp, [], [])
        | Address_key _meta -> Prim (loc, T_address, [], [])
        | Chain_id_key _meta -> Prim (loc, T_chain_id, [], [])
        | Pair_key (l, r, _meta) ->
            let tl = unparse_comparable_ty ~loc l in
            let tr = unparse_comparable_ty ~loc r in
            Prim (loc, T_pair, [tl; tr], [])
        | Union_key (l, r, _meta) ->
            let tl = unparse_comparable_ty ~loc l in
            let tr = unparse_comparable_ty ~loc r in
            Prim (loc, T_or, [tl; tr], [])
        | Option_key (t, _meta) ->
            Prim (loc, T_option, [unparse_comparable_ty ~loc t], [])

      let unparse_memo_size ~loc memo_size =
        let z = Alpha_context.Sapling.Memo_size.unparse_to_z memo_size in
        Int (loc, z)

      let rec unparse_ty :
          type a loc. loc:loc -> a ty -> (loc, Script.prim) Micheline.node =
       fun ~loc ty ->
        let return (name, args, annot) = Prim (loc, name, args, annot) in
        match ty with
        | Unit_t _meta -> return (T_unit, [], [])
        | Int_t _meta -> return (T_int, [], [])
        | Nat_t _meta -> return (T_nat, [], [])
        | Signature_t _meta -> return (T_signature, [], [])
        | String_t _meta -> return (T_string, [], [])
        | Bytes_t _meta -> return (T_bytes, [], [])
        | Mutez_t _meta -> return (T_mutez, [], [])
        | Bool_t _meta -> return (T_bool, [], [])
        | Key_hash_t _meta -> return (T_key_hash, [], [])
        | Key_t _meta -> return (T_key, [], [])
        | Timestamp_t _meta -> return (T_timestamp, [], [])
        | Address_t _meta -> return (T_address, [], [])
        | Operation_t _meta -> return (T_operation, [], [])
        | Chain_id_t _meta -> return (T_chain_id, [], [])
        | Never_t _meta -> return (T_never, [], [])
        | Bls12_381_g1_t _meta -> return (T_bls12_381_g1, [], [])
        | Bls12_381_g2_t _meta -> return (T_bls12_381_g2, [], [])
        | Bls12_381_fr_t _meta -> return (T_bls12_381_fr, [], [])
        | Contract_t (ut, _meta) ->
            let t = unparse_ty ~loc ut in
            return (T_contract, [t], [])
        | Pair_t (utl, utr, _meta) ->
            let annot = [] in
            let tl = unparse_ty ~loc utl in
            let tr = unparse_ty ~loc utr in
            return (T_pair, [tl; tr], annot)
        | Union_t ((utl, l_field), (utr, r_field), _meta) ->
            let annot = [] in
            let utl = unparse_ty ~loc utl in
            let tl = add_field_annot l_field utl in
            let utr = unparse_ty ~loc utr in
            let tr = add_field_annot r_field utr in
            return (T_or, [tl; tr], annot)
        | Lambda_t (uta, utr, _meta) ->
            let ta = unparse_ty ~loc uta in
            let tr = unparse_ty ~loc utr in
            return (T_lambda, [ta; tr], [])
        | Option_t (ut, _meta) ->
            let annot = [] in
            let ut = unparse_ty ~loc ut in
            return (T_option, [ut], annot)
        | List_t (ut, _meta) ->
            let t = unparse_ty ~loc ut in
            return (T_list, [t], [])
        | Ticket_t (ut, _meta) ->
            let t = unparse_comparable_ty ~loc ut in
            return (T_ticket, [t], [])
        | Set_t (ut, _meta) ->
            let t = unparse_comparable_ty ~loc ut in
            return (T_set, [t], [])
        | Map_t (uta, utr, _meta) ->
            let ta = unparse_comparable_ty ~loc uta in
            let tr = unparse_ty ~loc utr in
            return (T_map, [ta; tr], [])
        | Big_map_t (uta, utr, _meta) ->
            let ta = unparse_comparable_ty ~loc uta in
            let tr = unparse_ty ~loc utr in
            return (T_big_map, [ta; tr], [])
        | Sapling_transaction_t (memo_size, _meta) ->
            return
              (T_sapling_transaction, [unparse_memo_size ~loc memo_size], [])
        | Sapling_state_t (memo_size, _meta) ->
            return (T_sapling_state, [unparse_memo_size ~loc memo_size], [])
        | Chest_t _meta -> return (T_chest, [], [])
        | Chest_key_t _meta -> return (T_chest_key, [], [])
    end

    let run_operation_service ctxt ()
        ({shell; protocol_data = Operation_data protocol_data}, chain_id) =
      (* this code is a duplicate of Apply without signature check *)
      let ret contents =
        ( Operation_data protocol_data,
          Apply_results.Operation_metadata {contents} )
      in
      let operation : _ operation = {shell; protocol_data} in
      let hash = Operation.hash {shell; protocol_data} in
      let ctxt = Origination_nonce.init ctxt hash in
      let payload_producer = Signature.Public_key_hash.zero in
      match protocol_data.contents with
      | Single (Manager_operation _) as op ->
          Apply.precheck_manager_contents_list ctxt op ~mempool_mode:true
          >>=? fun (ctxt, prechecked_contents_list) ->
          (* removed signature check here *)
          Apply.apply_manager_contents_list
            ctxt
            Optimized
            ~payload_producer
            chain_id
            prechecked_contents_list
          >|= fun (_ctxt, result) -> ok @@ ret result
      | Cons (Manager_operation _, _) as op ->
          Apply.precheck_manager_contents_list ctxt op ~mempool_mode:true
          >>=? fun (ctxt, prechecked_contents_list) ->
          (* removed signature check here *)
          Apply.apply_manager_contents_list
            ctxt
            Optimized
            ~payload_producer
            chain_id
            prechecked_contents_list
          >|= fun (_ctxt, result) -> ok @@ ret result
      | _ ->
          let predecessor_level =
            match
              Alpha_context.Level.pred ctxt (Alpha_context.Level.current ctxt)
            with
            | Some level -> level
            | None -> assert false
          in
          Alpha_context.Round.get ctxt >>=? fun predecessor_round ->
          Apply.apply_contents_list
            ctxt
            chain_id
            (Partial_construction
               {
                 predecessor_level;
                 predecessor_round;
                 grand_parent_round = Round.zero;
               })
            Optimized
            ~payload_producer
            operation
            operation.protocol_data.contents
          >|=? fun (_ctxt, result) -> ret result

    (*

       The execution of an operation depends on the state of the
       cache. In particular, gas consumption is usually impacted by
       cache hits and misses.

       Unfortunately, the state of the cache is different between the
       context at operation-creation time and the context when is
       included in a block.

       Therefore, the simulation tries to predict the state of the
       cache in a [time_in_blocks] assumed to be close to the inclusion
       time of the operation.

    *)
    let simulate_operation_service ctxt () (op, chain_id, time_in_blocks) =
      let ctxt = Cache.Admin.future_cache_expectation ctxt ~time_in_blocks in
      run_operation_service ctxt () (op, chain_id)

    let default_from_context ctxt get = function
      | None -> get ctxt
      | Some x -> return x

    (* A convenience type for return values of [ensure_contracts_exist] below. *)
    type run_code_config = {
      balance : Tez.t;
      self : Contract.t;
      payer : Contract.t;
      source : Contract.t;
    }

    (* 4_000_000 ꜩ *)
    let default_balance = Tez.of_mutez_exn 4_000_000_000_000L

    let register () =
      let originate_dummy_contract ctxt script balance =
        let ctxt = Origination_nonce.init ctxt Operation_hash.zero in
        Lwt.return (Contract.fresh_contract_from_current_nonce ctxt)
        >>=? fun (ctxt, dummy_contract) ->
        Contract.raw_originate
          ctxt
          ~prepaid_bootstrap_storage:false
          dummy_contract
          ~script:(script, None)
        >>=? fun ctxt ->
        Token.transfer
          ~origin:Simulation
          ctxt
          `Minted
          (`Contract dummy_contract)
          balance
        >>=? fun (ctxt, _) -> return (ctxt, dummy_contract)
      in
      let configure_contracts ctxt script balance ~src_opt ~pay_opt ~self_opt =
        (match self_opt with
        | None ->
            let balance = Option.value ~default:default_balance balance in
            originate_dummy_contract ctxt script balance
            >>=? fun (ctxt, addr) -> return (ctxt, addr, balance)
        | Some addr ->
            default_from_context
              ctxt
              (fun c -> Contract.get_balance c addr)
              balance
            >>=? fun bal -> return (ctxt, addr, bal))
        >>=? fun (ctxt, self, balance) ->
        let (source, payer) =
          match (src_opt, pay_opt) with
          | (None, None) -> (self, self)
          | (Some c, None) | (None, Some c) -> (c, c)
          | (Some src, Some pay) -> (src, pay)
        in
        return (ctxt, {balance; self; source; payer})
      in
      let script_entrypoint_type ctxt expr entrypoint =
        let ctxt = Gas.set_unlimited ctxt in
        let legacy = false in
        let open Script_ir_translator in
        parse_toplevel ctxt ~legacy expr
        >>=? fun ({arg_type; root_name; _}, ctxt) ->
        Lwt.return
          ( parse_parameter_ty_and_entrypoints ctxt ~legacy arg_type ~root_name
          >>? fun (Ex_parameter_ty_and_entrypoints {arg_type; root_name}, _) ->
            Gas_monad.run ctxt
            @@ Script_ir_translator.find_entrypoint
                 ~error_details:Informative
                 ~root_name
                 arg_type
                 entrypoint
            >>? fun (r, ctxt) ->
            r >>? fun (_f, Ex_ty ty) ->
            unparse_ty ~loc:() ctxt ty >|? fun (ty_node, _) ->
            Micheline.strip_locations ty_node )
      in
      Registration.register0
        ~chunked:true
        S.run_code
        (fun
          ctxt
          ()
          ( ( code,
              storage,
              parameter,
              amount,
              balance,
              chain_id,
              src_opt,
              pay_opt,
              self_opt,
              entrypoint ),
            (unparsing_mode, gas, now, level) )
        ->
          let unparsing_mode = Option.value ~default:Readable unparsing_mode in
          let storage = Script.lazy_expr storage in
          let code = Script.lazy_expr code in
          configure_contracts
            ctxt
            {storage; code}
            balance
            ~src_opt
            ~pay_opt
            ~self_opt
          >>=? fun (ctxt, {self; source; payer; balance}) ->
          let gas =
            match gas with
            | Some gas -> gas
            | None -> Constants.hard_gas_limit_per_operation ctxt
          in
          let ctxt = Gas.set_limit ctxt gas in
          let now =
            match now with None -> Script_timestamp.now ctxt | Some t -> t
          in
          let level =
            match level with
            | None ->
                (Level.current ctxt).level |> Raw_level.to_int32
                |> Script_int.of_int32 |> Script_int.abs
            | Some z -> z
          in
          let step_constants =
            let open Script_interpreter in
            {source; payer; self; amount; balance; chain_id; now; level}
          in
          Script_interpreter.execute
            ctxt
            unparsing_mode
            step_constants
            ~cached_script:None
            ~script:{storage; code}
            ~entrypoint
            ~parameter
            ~internal:true
          >|=? fun ( {
                       Script_interpreter.storage;
                       operations;
                       lazy_storage_diff;
                       _;
                     },
                     _ ) -> (storage, operations, lazy_storage_diff)) ;
      Registration.register0
        ~chunked:true
        S.trace_code
        (fun
          ctxt
          ()
          ( ( code,
              storage,
              parameter,
              amount,
              balance,
              chain_id,
              src_opt,
              pay_opt,
              self_opt,
              entrypoint ),
            (unparsing_mode, gas, now, level) )
        ->
          let unparsing_mode = Option.value ~default:Readable unparsing_mode in
          let storage = Script.lazy_expr storage in
          let code = Script.lazy_expr code in
          configure_contracts
            ctxt
            {storage; code}
            balance
            ~src_opt
            ~pay_opt
            ~self_opt
          >>=? fun (ctxt, {self; source; payer; balance}) ->
          let gas =
            match gas with
            | Some gas -> gas
            | None -> Constants.hard_gas_limit_per_operation ctxt
          in
          let ctxt = Gas.set_limit ctxt gas in
          let now =
            match now with None -> Script_timestamp.now ctxt | Some t -> t
          in
          let level =
            match level with
            | None ->
                (Level.current ctxt).level |> Raw_level.to_int32
                |> Script_int.of_int32 |> Script_int.abs
            | Some z -> z
          in
          let step_constants =
            let open Script_interpreter in
            {source; payer; self; amount; balance; chain_id; now; level}
          in
          let module Unparsing_mode = struct
            let unparsing_mode = unparsing_mode
          end in
          let module Interp = Traced_interpreter (Unparsing_mode) in
          Interp.execute
            ctxt
            step_constants
            ~script:{storage; code}
            ~entrypoint
            ~parameter
          >|=? fun ( {
                       Script_interpreter.storage;
                       operations;
                       lazy_storage_diff;
                       _;
                     },
                     trace ) -> (storage, operations, trace, lazy_storage_diff)) ;
      Registration.register0
        ~chunked:true
        S.run_view
        (fun
          ctxt
          ()
          ( contract,
            entrypoint,
            input,
            chain_id,
            source,
            payer,
            gas,
            unparsing_mode,
            now,
            level )
        ->
          Contract.get_script ctxt contract >>=? fun (ctxt, script_opt) ->
          Option.fold
            ~some:ok
            ~none:(Error_monad.error View_helpers.Viewed_contract_has_no_script)
            script_opt
          >>?= fun script ->
          Script_repr.(force_decode script.code) >>?= fun decoded_script ->
          script_entrypoint_type ctxt decoded_script entrypoint
          >>=? fun view_ty ->
          View_helpers.extract_view_output_type entrypoint view_ty
          >>?= fun ty ->
          Contract.get_balance ctxt contract >>=? fun balance ->
          Error_monad.trace View_helpers.View_callback_origination_failed
          @@ originate_dummy_contract
               ctxt
               (View_helpers.make_viewer_script ty)
               Tez.zero
          >>=? fun (ctxt, viewer_contract) ->
          let (source, payer) =
            match (source, payer) with
            | (Some source, Some payer) -> (source, payer)
            | (Some source, None) -> (source, source)
            | (None, Some payer) -> (payer, payer)
            | (None, None) -> (contract, contract)
          in
          let gas =
            Option.value
              ~default:(Constants.hard_gas_limit_per_operation ctxt)
              gas
          in
          let ctxt = Gas.set_limit ctxt gas in
          let now =
            match now with None -> Script_timestamp.now ctxt | Some t -> t
          in
          let level =
            match level with
            | None ->
                (Level.current ctxt).level |> Raw_level.to_int32
                |> Script_int.of_int32 |> Script_int.abs
            | Some z -> z
          in
          let step_constants =
            let open Script_interpreter in
            {
              source;
              payer;
              self = contract;
              amount = Tez.zero;
              balance;
              chain_id;
              now;
              level;
            }
          in
          let parameter =
            View_helpers.make_view_parameter
              (Micheline.root input)
              viewer_contract
          in
          Script_interpreter.execute
            ctxt
            unparsing_mode
            step_constants
            ~script
            ~cached_script:None
            ~entrypoint
            ~parameter
            ~internal:true
          >>=? fun ({Script_interpreter.operations; _}, (_, _)) ->
          View_helpers.extract_parameter_from_operations
            entrypoint
            operations
            viewer_contract
          >>?= fun parameter -> Lwt.return (Script_repr.force_decode parameter)) ;
      Registration.register0
        ~chunked:false
        S.typecheck_code
        (fun ctxt () (expr, maybe_gas, legacy, show_types) ->
          let legacy = Option.value ~default:false legacy in
          let show_types = Option.value ~default:true show_types in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          Script_ir_translator.typecheck_code ~legacy ~show_types ctxt expr
          >|=? fun (res, ctxt) -> (res, Gas.level ctxt)) ;
      Registration.register0
        ~chunked:false
        S.script_size
        (fun ctxt () (expr, storage, maybe_gas, legacy) ->
          let legacy = Option.value ~default:false legacy in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          let code = Script.lazy_expr expr in
          Script_ir_translator.parse_code ~legacy ctxt ~code
          >>=? fun ( Ex_code
                       {
                         code;
                         arg_type;
                         storage_type;
                         views;
                         root_name;
                         code_size;
                       },
                     ctxt ) ->
          Script_ir_translator.parse_data
            ~legacy
            ~allow_forged:true
            ctxt
            storage_type
            (Micheline.root storage)
          >>=? fun (storage, _) ->
          let script =
            Script_ir_translator.Ex_script
              {
                code;
                arg_type;
                storage_type;
                views;
                root_name;
                code_size;
                storage;
              }
          in
          let (size, cost) = Script_ir_translator.script_size script in
          Gas.consume ctxt cost >>?= fun _ctxt -> return @@ size) ;

      Registration.register0
        ~chunked:false
        S.typecheck_data
        (fun ctxt () (data, ty, maybe_gas, legacy) ->
          let legacy = Option.value ~default:false legacy in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          typecheck_data ~legacy ctxt (data, ty) >|=? fun ctxt -> Gas.level ctxt) ;
      Registration.register0
        ~chunked:true
        S.pack_data
        (fun ctxt () (expr, typ, maybe_gas) ->
          let open Script_ir_translator in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          parse_packable_ty ctxt ~legacy:true (Micheline.root typ)
          >>?= fun (Ex_ty typ, ctxt) ->
          parse_data
            ctxt
            ~legacy:true
            ~allow_forged:true
            typ
            (Micheline.root expr)
          >>=? fun (data, ctxt) ->
          Script_ir_translator.pack_data ctxt typ data >|=? fun (bytes, ctxt) ->
          (bytes, Gas.level ctxt)) ;
      Registration.register0
        ~chunked:true
        S.normalize_data
        (fun ctxt () (expr, typ, unparsing_mode, legacy) ->
          let open Script_ir_translator in
          let legacy = Option.value ~default:false legacy in
          let ctxt = Gas.set_unlimited ctxt in
          Script_ir_translator.parse_any_ty ctxt ~legacy (Micheline.root typ)
          >>?= fun (Ex_ty typ, ctxt) ->
          parse_data ctxt ~legacy ~allow_forged:true typ (Micheline.root expr)
          >>=? fun (data, ctxt) ->
          Script_ir_translator.unparse_data ctxt unparsing_mode typ data
          >|=? fun (normalized, _ctxt) -> Micheline.strip_locations normalized) ;
      Registration.register0
        ~chunked:true
        S.normalize_script
        (fun ctxt () (script, unparsing_mode) ->
          let ctxt = Gas.set_unlimited ctxt in
          Script_ir_translator.unparse_code
            ctxt
            unparsing_mode
            (Micheline.root script)
          >|=? fun (normalized, _ctxt) -> Micheline.strip_locations normalized) ;
      Registration.register0 ~chunked:true S.normalize_type (fun ctxt () typ ->
          let open Script_ir_translator in
          let ctxt = Gas.set_unlimited ctxt in
          (* Unfortunately, Script_ir_translator.parse_any_ty is not exported *)
          Script_ir_translator.parse_ty
            ctxt
            ~legacy:true
            ~allow_lazy_storage:true
            ~allow_operation:true
            ~allow_contract:true
            ~allow_ticket:true
            (Micheline.root typ)
          >>?= fun (Ex_ty typ, _ctxt) ->
          let normalized = Unparse_types.unparse_ty ~loc:() typ in
          return @@ Micheline.strip_locations normalized) ;
      Registration.register0 ~chunked:true S.run_operation run_operation_service ;
      Registration.register0
        ~chunked:true
        S.simulate_operation
        simulate_operation_service ;
      Registration.register0
        ~chunked:true
        S.entrypoint_type
        (fun ctxt () (expr, entrypoint) ->
          script_entrypoint_type ctxt expr entrypoint) ;
      Registration.register0
        ~chunked:true
        S.list_entrypoints
        (fun ctxt () expr ->
          let ctxt = Gas.set_unlimited ctxt in
          let legacy = false in
          let open Script_ir_translator in
          parse_toplevel ~legacy ctxt expr
          >>=? fun ({arg_type; root_name; _}, ctxt) ->
          Lwt.return
            ( parse_parameter_ty_and_entrypoints ctxt ~legacy arg_type ~root_name
            >>? fun (Ex_parameter_ty_and_entrypoints {arg_type; root_name}, _)
              ->
              Script_ir_translator.list_entrypoints ~root_name arg_type ctxt
              >|? fun (unreachable_entrypoint, map) ->
              ( unreachable_entrypoint,
                Entrypoint.Map.fold
                  (fun entry (_, ty) acc ->
                    (Entrypoint.to_string entry, Micheline.strip_locations ty)
                    :: acc)
                  map
                  [] ) ))

    let run_code ?unparsing_mode ?gas ?(entrypoint = Entrypoint.default)
        ?balance ~script ~storage ~input ~amount ~chain_id ~source ~payer ~self
        ~now ~level ctxt block =
      RPC_context.make_call0
        S.run_code
        ctxt
        block
        ()
        ( ( script,
            storage,
            input,
            amount,
            balance,
            chain_id,
            source,
            payer,
            self,
            entrypoint ),
          (unparsing_mode, gas, now, level) )

    let trace_code ?unparsing_mode ?gas ?(entrypoint = Entrypoint.default)
        ?balance ~script ~storage ~input ~amount ~chain_id ~source ~payer ~self
        ~now ~level ctxt block =
      RPC_context.make_call0
        S.trace_code
        ctxt
        block
        ()
        ( ( script,
            storage,
            input,
            amount,
            balance,
            chain_id,
            source,
            payer,
            self,
            entrypoint ),
          (unparsing_mode, gas, now, level) )

    let run_view ?gas ~contract ~entrypoint ~input ~chain_id ~now ~level ?source
        ?payer ~unparsing_mode ctxt block =
      RPC_context.make_call0
        S.run_view
        ctxt
        block
        ()
        ( contract,
          entrypoint,
          input,
          chain_id,
          source,
          payer,
          gas,
          unparsing_mode,
          now,
          level )

    let typecheck_code ?gas ?legacy ~script ?show_types ctxt block =
      RPC_context.make_call0
        S.typecheck_code
        ctxt
        block
        ()
        (script, gas, legacy, show_types)

    let script_size ?gas ?legacy ~script ~storage ctxt block =
      RPC_context.make_call0
        S.script_size
        ctxt
        block
        ()
        (script, storage, gas, legacy)

    let typecheck_data ?gas ?legacy ~data ~ty ctxt block =
      RPC_context.make_call0
        S.typecheck_data
        ctxt
        block
        ()
        (data, ty, gas, legacy)

    let pack_data ?gas ~data ~ty ctxt block =
      RPC_context.make_call0 S.pack_data ctxt block () (data, ty, gas)

    let normalize_data ?legacy ~data ~ty ~unparsing_mode ctxt block =
      RPC_context.make_call0
        S.normalize_data
        ctxt
        block
        ()
        (data, ty, unparsing_mode, legacy)

    let normalize_script ~script ~unparsing_mode ctxt block =
      RPC_context.make_call0
        S.normalize_script
        ctxt
        block
        ()
        (script, unparsing_mode)

    let normalize_type ~ty ctxt block =
      RPC_context.make_call0 S.normalize_type ctxt block () ty

    let run_operation ~op ~chain_id ctxt block =
      RPC_context.make_call0 S.run_operation ctxt block () (op, chain_id)

    let simulate_operation ~op ~chain_id ~latency ctxt block =
      RPC_context.make_call0
        S.simulate_operation
        ctxt
        block
        ()
        (op, chain_id, latency)

    let entrypoint_type ~script ~entrypoint ctxt block =
      RPC_context.make_call0 S.entrypoint_type ctxt block () (script, entrypoint)

    let list_entrypoints ctxt block ~script =
      RPC_context.make_call0 S.list_entrypoints ctxt block () script
  end

  module Contract = struct
    module S = struct
      let path =
        (RPC_path.(open_root / "context" / "contracts")
          : RPC_context.t RPC_path.context)

      let get_storage_normalized =
        let open Data_encoding in
        RPC_service.post_service
          ~description:
            "Access the data of the contract and normalize it using the \
             requested unparsing mode."
          ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
          ~query:RPC_query.empty
          ~output:(option Script.expr_encoding)
          RPC_path.(path /: Contract.rpc_arg / "storage" / "normalized")

      let get_script_normalized =
        let open Data_encoding in
        RPC_service.post_service
          ~description:
            "Access the script of the contract and normalize it using the \
             requested unparsing mode."
          ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
          ~query:RPC_query.empty
          ~output:(option Script.encoding)
          RPC_path.(path /: Contract.rpc_arg / "script" / "normalized")
    end

    let register () =
      (* Patched RPC: get_storage *)
      Registration.register1
        ~chunked:true
        S.get_storage_normalized
        (fun ctxt contract () unparsing_mode ->
          Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
          match script with
          | None -> return_none
          | Some script ->
              let ctxt = Gas.set_unlimited ctxt in
              let open Script_ir_translator in
              parse_script
                ctxt
                ~legacy:true
                ~allow_forged_in_storage:true
                script
              >>=? fun (Ex_script script, ctxt) ->
              unparse_script ctxt unparsing_mode script
              >>=? fun (script, ctxt) ->
              Script.force_decode_in_context
                ~consume_deserialization_gas:When_needed
                ctxt
                script.storage
              >>?= fun (storage, _ctxt) -> return_some storage) ;
      (* Patched RPC: get_script *)
      Registration.register1
        ~chunked:true
        S.get_script_normalized
        (fun ctxt contract () unparsing_mode ->
          Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
          match script with
          | None -> return_none
          | Some script ->
              let ctxt = Gas.set_unlimited ctxt in
              let open Script_ir_translator in
              parse_script
                ctxt
                ~legacy:true
                ~allow_forged_in_storage:true
                script
              >>=? fun (Ex_script script, ctxt) ->
              unparse_script ctxt unparsing_mode script
              >>=? fun (script, _ctxt) -> return_some script)

    let get_storage_normalized ctxt block ~contract ~unparsing_mode =
      RPC_context.make_call1
        S.get_storage_normalized
        ctxt
        block
        contract
        ()
        unparsing_mode

    let get_script_normalized ctxt block ~contract ~unparsing_mode =
      RPC_context.make_call1
        S.get_script_normalized
        ctxt
        block
        contract
        ()
        unparsing_mode
  end

  module Big_map = struct
    module S = struct
      let path =
        (RPC_path.(open_root / "context" / "big_maps")
          : RPC_context.t RPC_path.context)

      let big_map_get_normalized =
        let open Data_encoding in
        RPC_service.post_service
          ~description:
            "Access the value associated with a key in a big map, normalize \
             the output using the requested unparsing mode."
          ~query:RPC_query.empty
          ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
          ~output:Script.expr_encoding
          RPC_path.(
            path /: Big_map.Id.rpc_arg /: Script_expr_hash.rpc_arg
            / "normalized")
    end

    let register () =
      Registration.register2
        ~chunked:true
        S.big_map_get_normalized
        (fun ctxt id key () unparsing_mode ->
          let open Script_ir_translator in
          let ctxt = Gas.set_unlimited ctxt in
          Big_map.exists ctxt id >>=? fun (ctxt, types) ->
          match types with
          | None -> raise Not_found
          | Some (_, value_type) -> (
              parse_big_map_value_ty
                ctxt
                ~legacy:true
                (Micheline.root value_type)
              >>?= fun (Ex_ty value_type, ctxt) ->
              Big_map.get_opt ctxt id key >>=? fun (_ctxt, value) ->
              match value with
              | None -> raise Not_found
              | Some value ->
                  parse_data
                    ctxt
                    ~legacy:true
                    ~allow_forged:true
                    value_type
                    (Micheline.root value)
                  >>=? fun (value, ctxt) ->
                  unparse_data ctxt unparsing_mode value_type value
                  >|=? fun (value, _ctxt) -> Micheline.strip_locations value))

    let big_map_get_normalized ctxt block id key ~unparsing_mode =
      RPC_context.make_call2
        S.big_map_get_normalized
        ctxt
        block
        id
        key
        ()
        unparsing_mode
  end

  module Sc_rollup = struct
    open Data_encoding

    module S = struct
      let path =
        (RPC_path.(open_root / "context" / "sc_rollup")
          : RPC_context.t RPC_path.context)

      let kind =
        RPC_service.get_service
          ~description:"Kind of smart-contract rollup"
          ~query:RPC_query.empty
          ~output:(obj1 (opt "kind" Sc_rollup.Kind.encoding))
          RPC_path.(path /: Sc_rollup.Address.rpc_arg / "kind")

      let inbox =
        RPC_service.get_service
          ~description:"Inbox for a smart-contract rollup"
          ~query:RPC_query.empty
          ~output:Sc_rollup.Inbox.encoding
          RPC_path.(path /: Sc_rollup.Address.rpc_arg / "inbox")
    end

    let kind ctxt block sc_rollup_address =
      RPC_context.make_call1 S.kind ctxt block sc_rollup_address ()

    let register_inbox () =
      Registration.register1 ~chunked:true S.inbox (fun ctxt rollup () () ->
          Stdlib.Format.eprintf
            "@[Context level at RPC time at %a@]@."
            Level.pp
            (Level.current ctxt) ;
          Sc_rollup.inbox ctxt rollup >>=? fun (inbox, _ctxt) -> return inbox)

    let register_kind () =
      Registration.register1 ~chunked:true S.kind @@ fun ctxt address () () ->
      Alpha_context.Sc_rollup.kind ctxt address

    let register () =
      register_kind () ;
      register_inbox ()
  end

  module Forge = struct
    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "forge")

      let operations =
        RPC_service.post_service
          ~description:"Forge an operation"
          ~query:RPC_query.empty
          ~input:Operation.unsigned_encoding
          ~output:bytes
          RPC_path.(path / "operations")

      let empty_proof_of_work_nonce =
        Bytes.make Constants_repr.proof_of_work_nonce_size '\000'

      let protocol_data =
        RPC_service.post_service
          ~description:"Forge the protocol-specific part of a block header"
          ~query:RPC_query.empty
          ~input:
            (obj5
               (req "payload_hash" Block_payload_hash.encoding)
               (req "payload_round" Round.encoding)
               (opt "nonce_hash" Nonce_hash.encoding)
               (dft
                  "proof_of_work_nonce"
                  (Fixed.bytes Alpha_context.Constants.proof_of_work_nonce_size)
                  empty_proof_of_work_nonce)
               (dft "liquidity_baking_escape_vote" bool false))
          ~output:(obj1 (req "protocol_data" bytes))
          RPC_path.(path / "protocol_data")
    end

    let register () =
      Registration.register0_noctxt
        ~chunked:true
        S.operations
        (fun () (shell, proto) ->
          return
            (Data_encoding.Binary.to_bytes_exn
               Operation.unsigned_encoding
               (shell, proto))) ;
      Registration.register0_noctxt
        ~chunked:true
        S.protocol_data
        (fun
          ()
          ( payload_hash,
            payload_round,
            seed_nonce_hash,
            proof_of_work_nonce,
            liquidity_baking_escape_vote )
        ->
          return
            (Data_encoding.Binary.to_bytes_exn
               Block_header.contents_encoding
               {
                 payload_hash;
                 payload_round;
                 seed_nonce_hash;
                 proof_of_work_nonce;
                 liquidity_baking_escape_vote;
               }))

    module Manager = struct
      let[@coq_axiom_with_reason "cast on e"] operations ctxt block ~branch
          ~source ?sourcePubKey ~counter ~fee ~gas_limit ~storage_limit
          operations =
        Contract_services.manager_key ctxt block source >>= function
        | Error _ as e -> Lwt.return e
        | Ok revealed ->
            let ops =
              List.map
                (fun (Manager operation) ->
                  Contents
                    (Manager_operation
                       {
                         source;
                         counter;
                         operation;
                         fee;
                         gas_limit;
                         storage_limit;
                       }))
                operations
            in
            let ops =
              match (sourcePubKey, revealed) with
              | (None, _) | (_, Some _) -> ops
              | (Some pk, None) ->
                  let operation = Reveal pk in
                  Contents
                    (Manager_operation
                       {
                         source;
                         counter;
                         operation;
                         fee;
                         gas_limit;
                         storage_limit;
                       })
                  :: ops
            in
            Environment.wrap_tzresult @@ Operation.of_list ops >>?= fun ops ->
            RPC_context.make_call0 S.operations ctxt block () ({branch}, ops)

      let reveal ctxt block ~branch ~source ~sourcePubKey ~counter ~fee () =
        operations
          ctxt
          block
          ~branch
          ~source
          ~sourcePubKey
          ~counter
          ~fee
          ~gas_limit:Gas.Arith.zero
          ~storage_limit:Z.zero
          []

      let transaction ctxt block ~branch ~source ?sourcePubKey ~counter ~amount
          ~destination ?(entrypoint = Entrypoint.default) ?parameters ~gas_limit
          ~storage_limit ~fee () =
        let parameters =
          Option.fold
            ~some:Script.lazy_expr
            ~none:Script.unit_parameter
            parameters
        in
        operations
          ctxt
          block
          ~branch
          ~source
          ?sourcePubKey
          ~counter
          ~fee
          ~gas_limit
          ~storage_limit
          [Manager (Transaction {amount; parameters; destination; entrypoint})]

      let origination ctxt block ~branch ~source ?sourcePubKey ~counter ~balance
          ?delegatePubKey ~script ~gas_limit ~storage_limit ~fee () =
        operations
          ctxt
          block
          ~branch
          ~source
          ?sourcePubKey
          ~counter
          ~fee
          ~gas_limit
          ~storage_limit
          [
            Manager
              (Origination
                 {
                   delegate = delegatePubKey;
                   script;
                   credit = balance;
                   preorigination = None;
                 });
          ]

      let delegation ctxt block ~branch ~source ?sourcePubKey ~counter ~fee
          delegate =
        operations
          ctxt
          block
          ~branch
          ~source
          ?sourcePubKey
          ~counter
          ~fee
          ~gas_limit:Gas.Arith.zero
          ~storage_limit:Z.zero
          [Manager (Delegation delegate)]
    end

    let operation ctxt block ~branch operation =
      RPC_context.make_call0
        S.operations
        ctxt
        block
        ()
        ({branch}, Contents_list (Single operation))

    let endorsement ctxt b ~branch ~consensus_content () =
      operation ctxt b ~branch (Endorsement consensus_content)

    let proposals ctxt b ~branch ~source ~period ~proposals () =
      operation ctxt b ~branch (Proposals {source; period; proposals})

    let ballot ctxt b ~branch ~source ~period ~proposal ~ballot () =
      operation ctxt b ~branch (Ballot {source; period; proposal; ballot})

    let failing_noop ctxt b ~branch ~message () =
      operation ctxt b ~branch (Failing_noop message)

    let seed_nonce_revelation ctxt block ~branch ~level ~nonce () =
      operation ctxt block ~branch (Seed_nonce_revelation {level; nonce})

    let double_baking_evidence ctxt block ~branch ~bh1 ~bh2 () =
      operation ctxt block ~branch (Double_baking_evidence {bh1; bh2})

    let double_endorsement_evidence ctxt block ~branch ~op1 ~op2 () =
      operation ctxt block ~branch (Double_endorsement_evidence {op1; op2})

    let double_preendorsement_evidence ctxt block ~branch ~op1 ~op2 () =
      operation ctxt block ~branch (Double_preendorsement_evidence {op1; op2})

    let empty_proof_of_work_nonce =
      Bytes.make Constants_repr.proof_of_work_nonce_size '\000'

    let protocol_data ctxt block ?(payload_hash = Block_payload_hash.zero)
        ?(payload_round = Round.zero) ?seed_nonce_hash
        ?(proof_of_work_nonce = empty_proof_of_work_nonce)
        ~liquidity_baking_escape_vote () =
      RPC_context.make_call0
        S.protocol_data
        ctxt
        block
        ()
        ( payload_hash,
          payload_round,
          seed_nonce_hash,
          proof_of_work_nonce,
          liquidity_baking_escape_vote )
  end

  module Parse = struct
    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "parse")

      let operations =
        RPC_service.post_service
          ~description:"Parse operations"
          ~query:RPC_query.empty
          ~input:
            (obj2
               (req "operations" (list (dynamic_size Operation.raw_encoding)))
               (opt "check_signature" bool))
          ~output:(list (dynamic_size Operation.encoding))
          RPC_path.(path / "operations")

      let block =
        RPC_service.post_service
          ~description:"Parse a block"
          ~query:RPC_query.empty
          ~input:Block_header.raw_encoding
          ~output:Block_header.protocol_data_encoding
          RPC_path.(path / "block")
    end

    let parse_protocol_data protocol_data =
      match
        Data_encoding.Binary.of_bytes_opt
          Block_header.protocol_data_encoding
          protocol_data
      with
      | None -> Stdlib.failwith "Cant_parse_protocol_data"
      | Some protocol_data -> protocol_data

    let register () =
      Registration.register0
        ~chunked:true
        S.operations
        (fun _ctxt () (operations, check) ->
          List.map_es
            (fun raw ->
              parse_operation raw >>?= fun op ->
              (match check with
              | Some true -> return_unit (* FIXME *)
              (* I.check_signature ctxt *)
              (* op.protocol_data.signature op.shell op.protocol_data.contents *)
              | Some false | None -> return_unit)
              >|=? fun () -> op)
            operations) ;
      Registration.register0_noctxt ~chunked:false S.block (fun () raw_block ->
          return @@ parse_protocol_data raw_block.protocol_data)

    let operations ctxt block ?check operations =
      RPC_context.make_call0 S.operations ctxt block () (operations, check)

    let block ctxt block shell protocol_data =
      RPC_context.make_call0
        S.block
        ctxt
        block
        ()
        ({shell; protocol_data} : Block_header.raw)
  end

  (* Compute the estimated starting time of a [round] at a future
     [level], given the head's level [current_level], timestamp
     [current_timestamp], and round [current_round]. Assumes blocks at
     intermediate levels are produced at round 0. *)
  let estimated_time round_durations ~current_level ~current_round
      ~current_timestamp ~level ~round =
    if Level.(level <= current_level) then Result.return_none
    else
      Round.of_int round >>? fun round ->
      Round.timestamp_of_round
        round_durations
        ~round
        ~predecessor_timestamp:current_timestamp
        ~predecessor_round:current_round
      >>? fun round_start_at_next_level ->
      let step = Round.round_duration round_durations Round.zero in
      let diff = Level.diff level current_level in
      Period.mult (Int32.pred diff) step >>? fun delay ->
      Timestamp.(round_start_at_next_level +? delay) >>? fun timestamp ->
      Result.return_some timestamp

  let requested_levels ~default_level ctxt cycles levels =
    match (levels, cycles) with
    | ([], []) -> [default_level]
    | (levels, cycles) ->
        (* explicitly fail when requested levels or cycle are in the past...
           or too far in the future...
           TODO: https://gitlab.com/tezos/tezos/-/issues/2335
                 this old comment (from version Alpha) conflicts with
                 the specification of the RPCs that use this code.
        *)
        List.sort_uniq
          Level.compare
          (List.rev_append
             (List.rev_map (Level.from_raw ctxt) levels)
             (List.concat_map (Level.levels_in_cycle ctxt) cycles))

  module Baking_rights = struct
    type t = {
      level : Raw_level.t;
      delegate : Signature.Public_key_hash.t;
      round : int;
      timestamp : Timestamp.t option;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun {level; delegate; round; timestamp} ->
          (level, delegate, round, timestamp))
        (fun (level, delegate, round, timestamp) ->
          {level; delegate; round; timestamp})
        (obj4
           (req "level" Raw_level.encoding)
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "round" uint16)
           (opt "estimated_time" Timestamp.encoding))

    let default_max_round = 64

    module S = struct
      open Data_encoding

      let path = RPC_path.(open_root / "helpers" / "baking_rights")

      type baking_rights_query = {
        levels : Raw_level.t list;
        cycle : Cycle.t option;
        delegates : Signature.Public_key_hash.t list;
        max_round : int option;
        all : bool;
      }

      let baking_rights_query =
        let open RPC_query in
        query (fun levels cycle delegates max_round all ->
            {levels; cycle; delegates; max_round; all})
        |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
        |+ opt_field "cycle" Cycle.rpc_arg (fun t -> t.cycle)
        |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t ->
               t.delegates)
        |+ opt_field "max_round" RPC_arg.uint (fun t -> t.max_round)
        |+ flag "all" (fun t -> t.all)
        |> seal

      let baking_rights =
        RPC_service.get_service
          ~description:
            (Format.sprintf
               "Retrieves the list of delegates allowed to bake a block.\n\
                By default, it gives the best baking opportunities (in terms \
                of rounds) for bakers that have at least one opportunity below \
                the %dth round for the next block.\n\
                Parameters `level` and `cycle` can be used to specify the \
                (valid) level(s) in the past or future at which the baking \
                rights have to be returned.\n\
                Parameter `delegate` can be used to restrict the results to \
                the given delegates. If parameter `all` is set, all the baking \
                opportunities for each baker at each level are returned, \
                instead of just the first one.\n\
                Returns the list of baking opportunities up to round %d. Also \
                returns the minimal timestamps that correspond to these \
                opportunities. The timestamps are omitted for levels in the \
                past, and are only estimates for levels higher that the next \
                block's, based on the hypothesis that all predecessor blocks \
                were baked at the first round."
               default_max_round
               default_max_round)
          ~query:baking_rights_query
          ~output:(list encoding)
          path
    end

    let baking_rights_at_level ctxt max_round level =
      Baking.baking_rights ctxt level >>=? fun delegates ->
      Round.get ctxt >>=? fun current_round ->
      let current_level = Level.current ctxt in
      let current_timestamp = Timestamp.current ctxt in
      let round_durations = Alpha_context.Constants.round_durations ctxt in
      let rec loop l acc round =
        if Compare.Int.(round > max_round) then return (List.rev acc)
        else
          let (Misc.LCons (pk, next)) = l in
          let delegate = Signature.Public_key.hash pk in
          estimated_time
            round_durations
            ~current_level
            ~current_round
            ~current_timestamp
            ~level
            ~round
          >>?= fun timestamp ->
          let acc = {level = level.level; delegate; round; timestamp} :: acc in
          next () >>=? fun l -> loop l acc (round + 1)
      in
      loop delegates [] 0

    let remove_duplicated_delegates rights =
      List.rev @@ fst
      @@ List.fold_left
           (fun (acc, previous) r ->
             if
               Signature.Public_key_hash.Set.exists
                 (Signature.Public_key_hash.equal r.delegate)
                 previous
             then (acc, previous)
             else
               (r :: acc, Signature.Public_key_hash.Set.add r.delegate previous))
           ([], Signature.Public_key_hash.Set.empty)
           rights

    let register () =
      Registration.register0 ~chunked:true S.baking_rights (fun ctxt q () ->
          let cycles =
            match q.cycle with None -> [] | Some cycle -> [cycle]
          in
          let levels =
            requested_levels
              ~default_level:(Level.succ ctxt (Level.current ctxt))
              ctxt
              cycles
              q.levels
          in
          let max_round =
            match q.max_round with
            | None -> default_max_round
            | Some max_round ->
                Compare.Int.min
                  max_round
                  (Constants.consensus_committee_size ctxt)
          in
          List.map_es (baking_rights_at_level ctxt max_round) levels
          >|=? fun rights ->
          let rights =
            if q.all then List.concat rights
            else List.concat_map remove_duplicated_delegates rights
          in
          match q.delegates with
          | [] -> rights
          | _ :: _ as delegates ->
              let is_requested p =
                List.exists
                  (Signature.Public_key_hash.equal p.delegate)
                  delegates
              in
              List.filter is_requested rights)

    let get ctxt ?(levels = []) ?cycle ?(delegates = []) ?(all = false)
        ?max_round block =
      RPC_context.make_call0
        S.baking_rights
        ctxt
        block
        {levels; cycle; delegates; max_round; all}
        ()
  end

  module Endorsing_rights = struct
    type delegate_rights = {
      delegate : Signature.Public_key_hash.t;
      first_slot : Slot.t;
      endorsing_power : int;
    }

    type t = {
      level : Raw_level.t;
      delegates_rights : delegate_rights list;
      estimated_time : Time.t option;
    }

    let delegate_rights_encoding =
      let open Data_encoding in
      conv
        (fun {delegate; first_slot; endorsing_power} ->
          (delegate, first_slot, endorsing_power))
        (fun (delegate, first_slot, endorsing_power) ->
          {delegate; first_slot; endorsing_power})
        (obj3
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "first_slot" Slot.encoding)
           (req "endorsing_power" uint16))

    let encoding =
      let open Data_encoding in
      conv
        (fun {level; delegates_rights; estimated_time} ->
          (level, delegates_rights, estimated_time))
        (fun (level, delegates_rights, estimated_time) ->
          {level; delegates_rights; estimated_time})
        (obj3
           (req "level" Raw_level.encoding)
           (req "delegates" (list delegate_rights_encoding))
           (opt "estimated_time" Timestamp.encoding))

    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "endorsing_rights")

      type endorsing_rights_query = {
        levels : Raw_level.t list;
        cycle : Cycle.t option;
        delegates : Signature.Public_key_hash.t list;
      }

      let endorsing_rights_query =
        let open RPC_query in
        query (fun levels cycle delegates -> {levels; cycle; delegates})
        |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
        |+ opt_field "cycle" Cycle.rpc_arg (fun t -> t.cycle)
        |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t ->
               t.delegates)
        |> seal

      let endorsing_rights =
        RPC_service.get_service
          ~description:
            "Retrieves the delegates allowed to endorse a block.\n\
             By default, it gives the endorsing power for delegates that have \
             at least one endorsing slot for the next block.\n\
             Parameters `level` and `cycle` can be used to specify the (valid) \
             level(s) in the past or future at which the endorsing rights have \
             to be returned. Parameter `delegate` can be used to restrict the \
             results to the given delegates.\n\
             Returns the smallest endorsing slots and the endorsing power. \
             Also returns the minimal timestamp that corresponds to endorsing \
             at the given level. The timestamps are omitted for levels in the \
             past, and are only estimates for levels higher that the next \
             block's, based on the hypothesis that all predecessor blocks were \
             baked at the first round."
          ~query:endorsing_rights_query
          ~output:(list encoding)
          path
    end

    let endorsing_rights_at_level ctxt level =
      Baking.endorsing_rights_by_first_slot ctxt level
      >>=? fun (ctxt, rights) ->
      Round.get ctxt >>=? fun current_round ->
      let current_level = Level.current ctxt in
      let current_timestamp = Timestamp.current ctxt in
      let round_durations = Alpha_context.Constants.round_durations ctxt in
      estimated_time
        round_durations
        ~current_level
        ~current_round
        ~current_timestamp
        ~level
        ~round:0
      >>?= fun estimated_time ->
      let rights =
        Slot.Map.fold
          (fun first_slot (_pk, delegate, endorsing_power) acc ->
            {delegate; first_slot; endorsing_power} :: acc)
          rights
          []
      in
      return {level = level.level; delegates_rights = rights; estimated_time}

    let register () =
      Registration.register0 ~chunked:true S.endorsing_rights (fun ctxt q () ->
          let cycles =
            match q.cycle with None -> [] | Some cycle -> [cycle]
          in
          let levels =
            requested_levels
              ~default_level:(Level.current ctxt)
              ctxt
              cycles
              q.levels
          in
          List.map_es (endorsing_rights_at_level ctxt) levels
          >|=? fun rights_per_level ->
          match q.delegates with
          | [] -> rights_per_level
          | _ :: _ as delegates ->
              List.filter_map
                (fun rights_at_level ->
                  let is_requested p =
                    List.exists
                      (Signature.Public_key_hash.equal p.delegate)
                      delegates
                  in
                  match
                    List.filter is_requested rights_at_level.delegates_rights
                  with
                  | [] -> None
                  | delegates_rights ->
                      Some {rights_at_level with delegates_rights})
                rights_per_level)

    let get ctxt ?(levels = []) ?cycle ?(delegates = []) block =
      RPC_context.make_call0
        S.endorsing_rights
        ctxt
        block
        {levels; cycle; delegates}
        ()
  end

  module Validators = struct
    type t = {
      level : Raw_level.t;
      delegate : Signature.Public_key_hash.t;
      slots : Slot.t list;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun {level; delegate; slots} -> (level, delegate, slots))
        (fun (level, delegate, slots) -> {level; delegate; slots})
        (obj3
           (req "level" Raw_level.encoding)
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "slots" (list Slot.encoding)))

    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "validators")

      type validators_query = {
        levels : Raw_level.t list;
        delegates : Signature.Public_key_hash.t list;
      }

      let validators_query =
        let open RPC_query in
        query (fun levels delegates -> {levels; delegates})
        |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
        |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t ->
               t.delegates)
        |> seal

      let validators =
        RPC_service.get_service
          ~description:
            "Retrieves the delegates allowed to endorse a block.\n\
             By default, it gives the endorsing slots for delegates that have \
             at least one in the next block.\n\
             Parameter `level` can be used to specify the (valid) level(s) in \
             the past or future at which the endorsement rights have to be \
             returned. Parameter `delegate` can be used to restrict the \
             results to the given delegates.\n\
             Returns the list of endorsing slots. Also returns the minimal \
             timestamps that correspond to these slots. The timestamps are \
             omitted for levels in the past, and are only estimates for levels \
             later that the next block, based on the hypothesis that all \
             predecessor blocks were baked at the first round."
          ~query:validators_query
          ~output:(list encoding)
          path
    end

    let endorsing_slots_at_level ctxt level =
      Baking.endorsing_rights ctxt level >|=? fun (_, rights) ->
      Signature.Public_key_hash.Map.fold
        (fun delegate slots acc ->
          {level = level.level; delegate; slots} :: acc)
        rights
        []

    let register () =
      Registration.register0 ~chunked:true S.validators (fun ctxt q () ->
          let levels =
            requested_levels
              ~default_level:(Level.current ctxt)
              ctxt
              []
              q.levels
          in
          List.concat_map_es (endorsing_slots_at_level ctxt) levels
          >|=? fun rights ->
          match q.delegates with
          | [] -> rights
          | _ :: _ as delegates ->
              let is_requested p =
                List.exists
                  (Signature.Public_key_hash.equal p.delegate)
                  delegates
              in
              List.filter is_requested rights)

    let get ctxt ?(levels = []) ?(delegates = []) block =
      RPC_context.make_call0 S.validators ctxt block {levels; delegates} ()
  end

  module S = struct
    open Data_encoding

    type level_query = {offset : int32}

    let level_query : level_query RPC_query.t =
      let open RPC_query in
      query (fun offset -> {offset})
      |+ field "offset" RPC_arg.int32 0l (fun t -> t.offset)
      |> seal

    let current_level =
      RPC_service.get_service
        ~description:
          "Returns the level of the interrogated block, or the one of a block \
           located `offset` blocks after it in the chain. For instance, the \
           next block if `offset` is 1. The offset cannot be negative."
        ~query:level_query
        ~output:Level.encoding
        RPC_path.(path / "current_level")

    let levels_in_current_cycle =
      RPC_service.get_service
        ~description:"Levels of a cycle"
        ~query:level_query
        ~output:
          (obj2
             (req "first" Raw_level.encoding)
             (req "last" Raw_level.encoding))
        RPC_path.(path / "levels_in_current_cycle")

    let round =
      RPC_service.get_service
        ~description:
          "Returns the round of the interrogated block, or the one of a block \
           located `offset` blocks after in the chain (or before when \
           negative). For instance, the next block if `offset` is 1."
        ~query:RPC_query.empty
        ~output:Round.encoding
        RPC_path.(path / "round")
  end

  type Environment.Error_monad.error += Negative_level_offset

  let () =
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"negative_level_offset"
      ~title:"The specified level offset is negative"
      ~description:"The specified level offset is negative"
      ~pp:(fun ppf () ->
        Format.fprintf ppf "The specified level offset should be positive.")
      Data_encoding.unit
      (function Negative_level_offset -> Some () | _ -> None)
      (fun () -> Negative_level_offset)

  let register () =
    Scripts.register () ;
    Forge.register () ;
    Parse.register () ;
    Contract.register () ;
    Big_map.register () ;
    Baking_rights.register () ;
    Endorsing_rights.register () ;
    Validators.register () ;
    Sc_rollup.register () ;
    Registration.register0 ~chunked:false S.current_level (fun ctxt q () ->
        if q.offset < 0l then fail Negative_level_offset
        else
          Lwt.return
            (Level.from_raw_with_offset
               ctxt
               ~offset:q.offset
               (Level.current ctxt).level)) ;
    Registration.opt_register0
      ~chunked:true
      S.levels_in_current_cycle
      (fun ctxt q () ->
        let rev_levels =
          Level.levels_in_current_cycle ctxt ~offset:q.offset ()
        in
        match rev_levels with
        | [] -> return_none
        | [level] -> return (Some (level.level, level.level))
        | last :: default_first :: rest ->
            (* The [rev_levels] list is reversed, the last level is the head *)
            let first = List.last default_first rest in
            return (Some (first.level, last.level))) ;
    Registration.register0 ~chunked:false S.round (fun ctxt () () ->
        Round.get ctxt)

  let current_level ctxt ?(offset = 0l) block =
    RPC_context.make_call0 S.current_level ctxt block {offset} ()

  let levels_in_current_cycle ctxt ?(offset = 0l) block =
    RPC_context.make_call0 S.levels_in_current_cycle ctxt block {offset} ()

  let rpc_services =
    register () ;
    RPC_directory.merge rpc_services !Registration.patched_services
end
