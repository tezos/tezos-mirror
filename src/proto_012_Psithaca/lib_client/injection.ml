(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Apply_results
open Protocol_client_context

let get_branch (rpc_config : #Protocol_client_context.full) ~chain
    ~(block : Block_services.block) branch =
  (* The default branch is set to HEAD~2, because with Tenderbake the
     same transaction may be included again in another block candidate
     at the same level, so the operation branch should not point to
     the current head. It's not a good idea if it points to the head's
     predecessor as well, as the predecessor hash may still change
     because of potential reorgs (only the predecessor payload is
     finalized, not the whole block). *)
  let branch = Option.value ~default:0 branch in
  (* TODO export parameter *)
  (match block with
  | `Head 0 ->
      (* Default client's block value: we branch to head's grandfather *)
      return (`Head (2 + branch))
  | `Head n -> return (`Head (n + branch))
  | `Hash (h, n) -> return (`Hash (h, n + branch))
  | `Alias (a, n) -> return (`Alias (a, n))
  | `Genesis -> return `Genesis
  | `Level i -> return (`Level i))
  >>=? fun block ->
  Shell_services.Blocks.hash rpc_config ~chain ~block () >>=? fun hash ->
  Shell_services.Chain.chain_id rpc_config ~chain () >>=? fun chain_id ->
  return (chain_id, hash)

type 'kind preapply_result =
  Operation_hash.t * 'kind operation * 'kind operation_metadata

type 'kind result_list =
  Operation_hash.t * 'kind contents_list * 'kind contents_result_list

type 'kind result = Operation_hash.t * 'kind contents * 'kind contents_result

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
              | Ok total_fee -> Ok (total_fee, Gas.Arith.add total_gas gas_limit)
              | Error _ as e -> e))
      | _ -> acc)
    (Ok (Tez.zero, Gas.Arith.zero))
    l

type fee_parameter = {
  minimal_fees : Tez.t;
  minimal_nanotez_per_byte : Q.t;
  minimal_nanotez_per_gas_unit : Q.t;
  force_low_fee : bool;
  fee_cap : Tez.t;
  burn_cap : Tez.t;
}

let dummy_fee_parameter =
  {
    minimal_fees = Tez.zero;
    minimal_nanotez_per_byte = Q.zero;
    minimal_nanotez_per_gas_unit = Q.zero;
    force_low_fee = false;
    fee_cap = Tez.one;
    burn_cap = Tez.zero;
  }

(* Rounding up (see Z.cdiv) *)
let z_mutez_of_q_nanotez (ntz : Q.t) =
  let q_mutez = Q.div ntz (Q.of_int 1000) in
  Z.cdiv q_mutez.Q.num q_mutez.Q.den

let check_fees : type t.
    #Protocol_client_context.full ->
    fee_parameter ->
    t contents_list ->
    int ->
    unit Lwt.t =
 fun cctxt config op size ->
  match get_manager_operation_gas_and_fee op with
  | Error _ -> assert false (* FIXME *)
  | Ok (fee, gas) ->
      if Tez.compare fee config.fee_cap > 0 then
        cctxt#error
          "The proposed fee (%s%a) are higher than the configured fee cap \
           (%s%a).@\n\
          \ Use `--fee-cap %a` to emit this operation anyway."
          Client_proto_args.tez_sym
          Tez.pp
          fee
          Client_proto_args.tez_sym
          Tez.pp
          config.fee_cap
          Tez.pp
          fee
        >>= fun () -> exit 1
      else
        let fees_in_nanotez =
          Q.mul (Q.of_int64 (Tez.to_mutez fee)) (Q.of_int 1000)
        in
        let minimal_fees_in_nanotez =
          Q.mul (Q.of_int64 (Tez.to_mutez config.minimal_fees)) (Q.of_int 1000)
        in
        let minimal_fees_for_gas_in_nanotez =
          Q.mul
            config.minimal_nanotez_per_gas_unit
            (Q.of_bigint (Gas.Arith.integral_to_z gas))
        in
        let minimal_fees_for_size_in_nanotez =
          Q.mul config.minimal_nanotez_per_byte (Q.of_int size)
        in
        let estimated_fees_in_nanotez =
          Q.add
            minimal_fees_in_nanotez
            (Q.add
               minimal_fees_for_gas_in_nanotez
               minimal_fees_for_size_in_nanotez)
        in
        let estimated_fees_in_mutez =
          z_mutez_of_q_nanotez estimated_fees_in_nanotez
        in
        let estimated_fees =
          match Tez.of_mutez (Z.to_int64 estimated_fees_in_mutez) with
          | None -> assert false
          | Some fee -> fee
        in
        if
          (not config.force_low_fee)
          && Q.compare fees_in_nanotez estimated_fees_in_nanotez < 0
        then
          cctxt#error
            "The proposed fee (%s%a) are lower than the fee that baker expect \
             by default (%s%a).@\n\
            \ Use `--force-low-fee` to emit this operation anyway."
            Client_proto_args.tez_sym
            Tez.pp
            fee
            Client_proto_args.tez_sym
            Tez.pp
            estimated_fees
          >>= fun () -> exit 1
        else Lwt.return_unit

let print_for_verbose_signing ppf ~watermark ~bytes ~branch ~contents =
  let open Format in
  pp_open_vbox ppf 0 ;
  let item f =
    pp_open_hovbox ppf 4 ;
    pp_print_string ppf "  * " ;
    f ppf () ;
    pp_close_box ppf () ;
    pp_print_cut ppf ()
  in
  let hash_pp l =
    fprintf
      ppf
      "%s"
      (Tezos_crypto.Base58.raw_encode
         Tezos_crypto.Blake2B.(hash_bytes l |> to_string))
  in
  item (fun ppf () ->
      pp_print_text ppf "Branch: " ;
      Block_hash.pp ppf branch) ;
  item (fun ppf () ->
      fprintf
        ppf
        "Watermark: `%a` (0x%s)"
        Tezos_crypto.Signature.V0.pp_watermark
        watermark
        (Hex.of_bytes (Tezos_crypto.Signature.V0.bytes_of_watermark watermark)
        |> Hex.show)) ;
  item (fun ppf () ->
      pp_print_text ppf "Operation bytes: " ;
      TzString.fold_left (* We split the bytes into lines for display: *)
        (fun n c ->
          pp_print_char ppf c ;
          if
            n < 72
            (* is the email-body standard width, ideal for copy-pasting. *)
          then n + 1
          else (
            pp_print_space ppf () ;
            0))
        0
        (Hex.of_bytes bytes |> Hex.show)
      |> ignore) ;
  item (fun ppf () ->
      pp_print_text ppf "Blake 2B Hash (raw): " ;
      hash_pp [bytes]) ;
  item (fun ppf () ->
      pp_print_text
        ppf
        "Blake 2B Hash (ledger-style, with operation watermark): " ;
      hash_pp [Tezos_crypto.Signature.V0.bytes_of_watermark watermark; bytes]) ;
  let json =
    Data_encoding.Json.construct
      Operation.unsigned_encoding
      ({branch}, Contents_list contents)
  in
  item (fun ppf () ->
      pp_print_text ppf "JSON encoding: " ;
      Data_encoding.Json.pp ppf json) ;
  pp_close_box ppf ()

let preapply (type t) (cctxt : #Protocol_client_context.full) ~chain ~block
    ?(verbose_signing = false) ?fee_parameter ?branch ?src_sk
    (contents : t contents_list) =
  get_branch cctxt ~chain ~block branch >>=? fun (_chain_id, branch) ->
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({branch}, Contents_list contents)
  in
  (match src_sk with
  | None -> return_none
  | Some src_sk ->
      let watermark =
        match contents with
        (* TODO-TB sign endosrement? *)
        | _ -> Tezos_crypto.Signature.V0.Generic_operation
      in
      (if verbose_signing then
         cctxt#message
           "Pre-signature information (verbose signing):@.%t%!"
           (print_for_verbose_signing ~watermark ~bytes ~branch ~contents)
       else Lwt.return_unit)
      >>= fun () ->
      Client_keys_v0.sign cctxt ~watermark src_sk bytes >>=? fun signature ->
      return_some signature)
  >>=? fun signature ->
  let op : _ Operation.t =
    {shell = {branch}; protocol_data = {contents; signature}}
  in
  let oph = Operation.hash op in
  let size = Bytes.length bytes + Tezos_crypto.Signature.V0.size in
  (match fee_parameter with
  | Some fee_parameter -> check_fees cctxt fee_parameter contents size
  | None -> Lwt.return_unit)
  >>= fun () ->
  Protocol_client_context.Alpha_block_services.Helpers.Preapply.operations
    cctxt
    ~chain
    ~block
    [Operation.pack op]
  >>=? function
  | [(Operation_data op', Operation_metadata result)] -> (
      match
        ( Operation.equal op {shell = {branch}; protocol_data = op'},
          Apply_results.kind_equal_list contents result.contents )
      with
      | Some Operation.Eq, Some Apply_results.Eq ->
          return ((oph, op, result) : t preapply_result)
      | _ -> failwith "Unexpected result")
  | _ -> failwith "Unexpected result"

let simulate (type t) (cctxt : #Protocol_client_context.full) ~chain ~block
    ?branch ?(latency = Plugin.default_operation_inclusion_latency)
    (contents : t contents_list) =
  get_branch cctxt ~chain ~block branch >>=? fun (_chain_id, branch) ->
  let op : _ Operation.t =
    {shell = {branch}; protocol_data = {contents; signature = None}}
  in
  let oph = Operation.hash op in
  Chain_services.chain_id cctxt ~chain () >>=? fun chain_id ->
  Plugin.RPC.Scripts.simulate_operation
    cctxt
    (chain, block)
    ~op:(Operation.pack op)
    ~chain_id
    ~latency
  >>=? function
  | Operation_data op', Operation_metadata result -> (
      match
        ( Operation.equal op {shell = {branch}; protocol_data = op'},
          Apply_results.kind_equal_list contents result.contents )
      with
      | Some Operation.Eq, Some Apply_results.Eq ->
          return ((oph, op, result) : t preapply_result)
      | _ -> failwith "Unexpected result")
  | _ -> failwith "Unexpected result"

let estimated_gas_single (type kind)
    (Manager_operation_result {operation_result; internal_operation_results; _} :
      kind Kind.manager contents_result) =
  let consumed_gas (type kind) (result : kind manager_operation_result) =
    match result with
    | Applied (Transaction_result {consumed_gas; _}) -> Ok consumed_gas
    | Applied (Origination_result {consumed_gas; _}) -> Ok consumed_gas
    | Applied (Reveal_result {consumed_gas}) -> Ok consumed_gas
    | Applied (Delegation_result {consumed_gas}) -> Ok consumed_gas
    | Applied (Register_global_constant_result {consumed_gas; _}) ->
        Ok consumed_gas
    | Applied (Set_deposits_limit_result {consumed_gas}) -> Ok consumed_gas
    | Skipped _ -> assert false
    | Backtracked (_, None) ->
        Ok Gas.Arith.zero (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Error (Environment.wrap_tztrace errs)
    | Failed (_, errs) -> Error (Environment.wrap_tztrace errs)
  in
  consumed_gas operation_result >>? fun acc ->
  List.fold_left_e
    (fun acc (Internal_operation_result (_, r)) ->
      consumed_gas r >>? fun gas -> Ok (Gas.Arith.add acc gas))
    acc
    internal_operation_results

let estimated_storage_single (type kind) origination_size
    (Manager_operation_result {operation_result; internal_operation_results; _} :
      kind Kind.manager contents_result) =
  let storage_size_diff (type kind) (result : kind manager_operation_result) =
    match result with
    | Applied
        (Transaction_result
           {paid_storage_size_diff; allocated_destination_contract; _}) ->
        if allocated_destination_contract then
          Ok (Z.add paid_storage_size_diff origination_size)
        else Ok paid_storage_size_diff
    | Applied (Origination_result {paid_storage_size_diff; _}) ->
        Ok (Z.add paid_storage_size_diff origination_size)
    | Applied (Reveal_result _) -> Ok Z.zero
    | Applied (Delegation_result _) -> Ok Z.zero
    | Applied (Register_global_constant_result {size_of_constant; _}) ->
        Ok size_of_constant
    | Applied (Set_deposits_limit_result _) -> Ok Z.zero
    | Skipped _ -> assert false
    | Backtracked (_, None) ->
        Ok Z.zero (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Error (Environment.wrap_tztrace errs)
    | Failed (_, errs) -> Error (Environment.wrap_tztrace errs)
  in
  storage_size_diff operation_result >>? fun acc ->
  List.fold_left_e
    (fun acc (Internal_operation_result (_, r)) ->
      storage_size_diff r >>? fun storage -> Ok (Z.add acc storage))
    acc
    internal_operation_results

let estimated_storage origination_size res =
  let rec estimated_storage : type kind. kind contents_result_list -> _ =
    function
    | Single_result (Manager_operation_result _ as res) ->
        estimated_storage_single origination_size res
    | Single_result _ -> Ok Z.zero
    | Cons_result (res, rest) ->
        estimated_storage_single origination_size res >>? fun storage1 ->
        estimated_storage rest >>? fun storage2 -> Ok (Z.add storage1 storage2)
  in
  estimated_storage res >>? fun diff -> Ok (Z.max Z.zero diff)

let originated_contracts_single (type kind)
    (Manager_operation_result {operation_result; internal_operation_results; _} :
      kind Kind.manager contents_result) =
  let originated_contracts (type kind) (result : kind manager_operation_result)
      =
    match result with
    | Applied (Transaction_result {originated_contracts; _}) ->
        Ok originated_contracts
    | Applied (Origination_result {originated_contracts; _}) ->
        Ok originated_contracts
    | Applied (Register_global_constant_result _) -> Ok []
    | Applied (Reveal_result _) -> Ok []
    | Applied (Delegation_result _) -> Ok []
    | Applied (Set_deposits_limit_result _) -> Ok []
    | Skipped _ -> assert false
    | Backtracked (_, None) ->
        Ok [] (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Error (Environment.wrap_tztrace errs)
    | Failed (_, errs) -> Error (Environment.wrap_tztrace errs)
  in
  originated_contracts operation_result >>? fun acc ->
  let acc = List.rev acc in
  List.fold_left_e
    (fun acc (Internal_operation_result (_, r)) ->
      originated_contracts r >>? fun contracts ->
      Ok (List.rev_append contracts acc))
    acc
    internal_operation_results

let rec originated_contracts : type kind. kind contents_result_list -> _ =
  function
  | Single_result (Manager_operation_result _ as res) ->
      originated_contracts_single res >|? List.rev
  | Single_result _ -> Ok []
  | Cons_result (res, rest) ->
      originated_contracts_single res >>? fun contracts1 ->
      originated_contracts rest >>? fun contracts2 ->
      Ok (List.rev_append contracts1 contracts2)

(* When --force is used, we don't want [originated_contracts] to fail as
   it would stop the client before the injection of the operation. *)
let originated_contracts ~force results =
  match originated_contracts results with Error _ when force -> Ok [] | e -> e

let detect_script_failure : type kind. kind operation_metadata -> _ =
  let rec detect_script_failure : type kind. kind contents_result_list -> _ =
    let detect_script_failure_single (type kind)
        (Manager_operation_result
           {operation_result; internal_operation_results; _} :
          kind Kind.manager contents_result) =
      let detect_script_failure (type kind)
          (result : kind manager_operation_result) =
        match result with
        | Applied _ -> Ok ()
        | Skipped _ -> assert false
        | Backtracked (_, None) ->
            (* there must be another error for this to happen *)
            Ok ()
        | Backtracked (_, Some errs) ->
            record_trace
              (error_of_fmt "The transfer simulation failed.")
              (Error (Environment.wrap_tztrace errs))
        | Failed (_, errs) ->
            record_trace
              (error_of_fmt "The transfer simulation failed.")
              (Error (Environment.wrap_tztrace errs))
      in
      detect_script_failure operation_result >>? fun () ->
      List.iter_e
        (fun (Internal_operation_result (_, r)) -> detect_script_failure r)
        internal_operation_results
    in
    function
    | Single_result (Manager_operation_result _ as res) ->
        detect_script_failure_single res
    | Single_result _ -> Ok ()
    | Cons_result (res, rest) ->
        detect_script_failure_single res >>? fun () ->
        detect_script_failure rest
  in
  fun {contents} -> detect_script_failure contents

(* This value is used as a safety guard for gas limit. *)
let safety_guard = Gas.Arith.(integral_of_int_exn 100)

(*

   {2 High-level description of the automatic gas patching algorithm}

   When the user wants to inject a list of operations, some of which
   might have unspecified gas, fees or storage limit, the client
   performs a {e simulation} to estimate those limits and assign
   sensible values to them.

   The simulation works as follows:
   1. limits are assigned to dummy, high values to ensure that the operations
      can be simulated
      - 1.a) when a list of operations is partially specified, the algorithm
        allocates to each unspecified operation an equal portion of the
        maximum gas per block minus the gas consumed by the operations that
        do specify their limit
   2. the algorithm retrieves the effectively consumed gas and storage from the
      receipt
   3. the algorithm assigns slight overapproximations to the operation
   4. a default fee is computed and set

*)

let may_patch_limits (type kind) (cctxt : #Protocol_client_context.full)
    ~fee_parameter ~chain ~block ?branch
    (annotated_contents : kind Annotated_manager_operation.annotated_list) :
    kind Kind.manager contents_list tzresult Lwt.t =
  Tezos_client_base.Client_confirmations.wait_for_bootstrapped cctxt
  >>=? fun () ->
  Alpha_services.Constants.all cctxt (chain, block)
  >>=?
  fun {
        parametric =
          {
            hard_gas_limit_per_operation;
            hard_gas_limit_per_block;
            hard_storage_limit_per_operation;
            origination_size;
            cost_per_byte;
            _;
          };
        _;
      }
    ->
  let user_gas_limit_needs_patching user_gas_limit =
    Limit.fold user_gas_limit ~unknown:true ~known:(fun user_gas_limit ->
        Gas.Arith.(
          user_gas_limit < zero || hard_gas_limit_per_operation < user_gas_limit))
  in
  let user_storage_limit_needs_patching user_storage_limit =
    Limit.fold
      user_storage_limit
      ~unknown:true
      ~known:(fun user_storage_limit ->
        Z.Compare.(
          user_storage_limit < Z.zero
          || hard_storage_limit_per_operation < user_storage_limit))
  in
  let gas_patching_stats (Annotated_manager_operation.Manager_info c)
      need_patching gas_consumed =
    if user_gas_limit_needs_patching c.gas_limit then
      (need_patching + 1, gas_consumed)
    else
      ( need_patching,
        Gas.Arith.add
          gas_consumed
          (Limit.value ~when_unknown:Gas.Arith.zero c.gas_limit) )
  in
  let rec gas_patching_stats_list : type kind.
      kind Annotated_manager_operation.annotated_list ->
      int ->
      Saturation_repr.may_saturate Saturation_repr.t ->
      int * Saturation_repr.may_saturate Saturation_repr.t =
   fun op need_patching gas_consumed ->
    match op with
    | Single_manager minfo ->
        gas_patching_stats minfo need_patching gas_consumed
    | Cons_manager (minfo, rest) ->
        let need_patching, gas_consumed =
          gas_patching_stats minfo need_patching gas_consumed
        in
        gas_patching_stats_list rest need_patching gas_consumed
  in
  let may_need_patching_single : type kind.
      Gas.Arith.integral ->
      kind Annotated_manager_operation.t ->
      kind Annotated_manager_operation.t option =
   fun gas_limit_per_patched_op op ->
    match op with
    | Manager_info c ->
        let needs_patching =
          Limit.is_unknown c.fee
          || user_gas_limit_needs_patching c.gas_limit
          || user_storage_limit_needs_patching c.storage_limit
        in
        if not needs_patching then None
        else
          (* Set limits for simulation purposes *)
          let gas_limit =
            if user_gas_limit_needs_patching c.gas_limit then
              Limit.known gas_limit_per_patched_op
            else c.gas_limit
          in
          let storage_limit =
            if user_storage_limit_needs_patching c.storage_limit then
              Limit.known hard_storage_limit_per_operation
            else c.storage_limit
          in
          let fee = Limit.value ~when_unknown:Tez.zero c.fee in
          Some
            (Manager_info
               {c with gas_limit; storage_limit; fee = Limit.known fee})
  in
  let may_need_patching gas_limit_per_patched_op ops =
    let rec loop : type kind.
        kind Annotated_manager_operation.annotated_list ->
        kind Annotated_manager_operation.annotated_list option = function
      | Single_manager annotated_op ->
          Option.map (fun op -> Annotated_manager_operation.Single_manager op)
          @@ may_need_patching_single gas_limit_per_patched_op annotated_op
      | Cons_manager (annotated_op, rest) -> (
          let annotated_op_opt =
            may_need_patching_single gas_limit_per_patched_op annotated_op
          in
          let rest_opt = loop rest in
          match (annotated_op_opt, rest_opt) with
          | None, None -> None
          | _ ->
              let op = Option.value ~default:annotated_op annotated_op_opt in
              let rest = Option.value ~default:rest rest_opt in
              Some (Cons_manager (op, rest)))
    in
    loop ops
  in
  (*
    The recursion here handles the case where an increased fee might increase the
    size of the operation, and so require a recalculation of the gas costs.
    Rationale for termination:
    - the fee for size increases linearly with the size of the operation.
    - however, when the size of the operation increase to make space for an
      increased fee, the amount of new fee that can be added without increasing
      the size of the block again increases exponentially.
    - hence, there will eventually be a increase of size that will fit any new
      fee without having to increase the size of the operation again.
  *)
  let rec patch_fee : type kind. first:bool -> kind contents -> kind contents =
   fun ~first -> function
     | Manager_operation c as op -> (
         let size =
           if first then
             (WithExceptions.Option.get ~loc:__LOC__
             @@ Data_encoding.Binary.fixed_length
                  Tezos_base.Operation.shell_header_encoding)
             + Data_encoding.Binary.length
                 Operation.contents_encoding
                 (Contents op)
             + Tezos_crypto.Signature.V0.size
           else
             Data_encoding.Binary.length
               Operation.contents_encoding
               (Contents op)
         in
         let minimal_fees_in_nanotez =
           Q.mul
             (Q.of_int64 (Tez.to_mutez fee_parameter.minimal_fees))
             (Q.of_int 1000)
         in
         let minimal_fees_for_gas_in_nanotez =
           Q.mul
             fee_parameter.minimal_nanotez_per_gas_unit
             (Q.of_bigint @@ Gas.Arith.integral_to_z c.gas_limit)
         in
         let minimal_fees_for_size_in_nanotez =
           Q.mul fee_parameter.minimal_nanotez_per_byte (Q.of_int size)
         in
         let fees_in_nanotez =
           Q.add minimal_fees_in_nanotez
           @@ Q.add
                minimal_fees_for_gas_in_nanotez
                minimal_fees_for_size_in_nanotez
         in
         let fees_in_mutez = z_mutez_of_q_nanotez fees_in_nanotez in
         match Tez.of_mutez (Z.to_int64 fees_in_mutez) with
         | None -> assert false
         | Some fee ->
             if Tez.(fee <= c.fee) then op
             else patch_fee ~first (Manager_operation {c with fee}))
     | c -> c
  in
  let patch : type kind.
      first:bool ->
      kind Annotated_manager_operation.t * kind Kind.manager contents_result ->
      kind Kind.manager contents tzresult Lwt.t =
   fun ~first -> function
     | (Manager_info c as op), (Manager_operation_result _ as result) ->
         (if user_gas_limit_needs_patching c.gas_limit then
            Lwt.return (estimated_gas_single result) >>=? fun gas ->
            if Gas.Arith.(gas = zero) then
              cctxt#message "Estimated gas: none" >>= fun () ->
              return
                (Annotated_manager_operation.set_gas_limit
                   (Limit.known Gas.Arith.zero)
                   op)
            else
              let safety_guard =
                match c.operation with
                | Transaction {destination; _}
                  when Option.is_some (Contract.is_implicit destination) ->
                    Gas.Arith.zero
                | Reveal _ | Delegation _ | Set_deposits_limit _ ->
                    Gas.Arith.zero
                | _ -> safety_guard
              in
              cctxt#message
                "Estimated gas: %a units (will add %a for safety)"
                Gas.Arith.pp
                gas
                Gas.Arith.pp
                safety_guard
              >>= fun () ->
              let safe_gas = Gas.Arith.(add (ceil gas) safety_guard) in
              let patched_gas =
                Gas.Arith.min safe_gas hard_gas_limit_per_operation
              in
              return
                (Annotated_manager_operation.set_gas_limit
                   (Limit.known patched_gas)
                   op)
          else return op)
         >>=? fun op ->
         (if user_storage_limit_needs_patching c.storage_limit then
            Lwt.return
              (estimated_storage_single (Z.of_int origination_size) result)
            >>=? fun storage ->
            if Z.equal storage Z.zero then
              cctxt#message "Estimated storage: no bytes added" >>= fun () ->
              return
                (Annotated_manager_operation.set_storage_limit
                   (Limit.known Z.zero)
                   op)
            else
              cctxt#message
                "Estimated storage: %s bytes added (will add 20 for safety)"
                (Z.to_string storage)
              >>= fun () ->
              let storage_limit =
                Z.min
                  (Z.add storage (Z.of_int 20))
                  hard_storage_limit_per_operation
              in
              return
                (Annotated_manager_operation.set_storage_limit
                   (Limit.known storage_limit)
                   op)
          else return op)
         >>=? fun op ->
         if Limit.is_unknown c.fee then
           (* Setting a dummy fee is required for converting to manager op *)
           let op =
             Annotated_manager_operation.set_fee (Limit.known Tez.zero) op
           in
           Annotated_manager_operation.manager_from_annotated op >>?= fun cm ->
           return (patch_fee ~first cm)
         else Lwt.return (Annotated_manager_operation.manager_from_annotated op)
  in
  let rec patch_list : type kind.
      bool ->
      kind Annotated_manager_operation.annotated_list ->
      kind Kind.manager contents_result_list ->
      kind Kind.manager contents_list tzresult Lwt.t =
   fun first annotated_list result_list ->
    match (annotated_list, result_list) with
    | Single_manager annotated, Single_result res ->
        patch ~first (annotated, res) >>=? fun op -> return (Single op)
    | Cons_manager (annotated, annotated_rest), Cons_result (res, res_rest) ->
        patch ~first (annotated, res) >>=? fun op ->
        patch_list false annotated_rest res_rest >>=? fun rest ->
        return (Cons (op, rest))
    | _ -> assert false
  in
  let gas_limit_per_patched_op =
    let need_gas_patching, gas_consumed =
      gas_patching_stats_list annotated_contents 0 Gas.Arith.zero
    in
    if need_gas_patching = 0 then hard_gas_limit_per_operation
    else
      let remaining_gas = Gas.Arith.sub hard_gas_limit_per_block gas_consumed in
      let average_per_operation_gas =
        Gas.Arith.integral_exn
        @@ Z.div
             (Gas.Arith.integral_to_z remaining_gas)
             (Z.of_int need_gas_patching)
      in
      Gas.Arith.min hard_gas_limit_per_operation average_per_operation_gas
  in
  match may_need_patching gas_limit_per_patched_op annotated_contents with
  | Some annotated_for_simulation ->
      Lwt.return
        (Annotated_manager_operation.manager_list_from_annotated
           annotated_for_simulation)
      >>=? fun contents_for_simulation ->
      simulate cctxt ~chain ~block ?branch contents_for_simulation
      >>=? fun (_, _, result) ->
      (match detect_script_failure result with
      | Ok () -> return_unit
      | Error _ ->
          cctxt#message
            "@[<v 2>This simulation failed:@,%a@]"
            Operation_result.pp_operation_result
            (contents_for_simulation, result.contents)
          >>= fun () -> return_unit)
      >>=? fun () ->
      ( Lwt.return
          (estimated_storage (Z.of_int origination_size) result.contents)
      >>=? fun storage ->
        Lwt.return
          (Environment.wrap_tzresult Tez.(cost_per_byte *? Z.to_int64 storage))
        >>=? fun burn ->
        if Tez.(burn > fee_parameter.burn_cap) then
          cctxt#error
            "The operation will burn %s%a which is higher than the configured \
             burn cap (%s%a).@\n\
            \ Use `--burn-cap %a` to emit this operation."
            Client_proto_args.tez_sym
            Tez.pp
            burn
            Client_proto_args.tez_sym
            Tez.pp
            fee_parameter.burn_cap
            Tez.pp
            burn
          >>= fun () -> exit 1
        else return_unit )
      >>=? fun () -> patch_list true annotated_contents result.contents
  | None ->
      Lwt.return
        (Annotated_manager_operation.manager_list_from_annotated
           annotated_contents)

let tenderbake_finality_confirmations = 1

let tenderbake_adjust_confirmations (cctxt : #Client_context.full) = function
  | None -> Lwt.return_none
  | Some cli_confirmations ->
      if cli_confirmations > tenderbake_finality_confirmations then
        cctxt#message
          "Tenderbake needs at most %d confirmations for finality (%d given). \
           Using %d confirmations."
          tenderbake_finality_confirmations
          cli_confirmations
          tenderbake_finality_confirmations
        >>= fun () -> Lwt.return_some tenderbake_finality_confirmations
      else Lwt.return_some cli_confirmations

(* For Tenderbake we restrain the interval of confirmations to be [0,
   tenderbake_finality_confirmations]

   Any value greater than the tenderbake_finality_confirmations is treated as if it
   were tenderbake_finality_confirmations.
*)
let inject_operation_internal (type kind) cctxt ~chain ~block ?confirmations
    ?(dry_run = false) ?(simulation = false) ?(force = false) ?branch ?src_sk
    ?verbose_signing ~fee_parameter (contents : kind contents_list) =
  (if simulation then simulate cctxt ~chain ~block ?branch contents
   else
     preapply
       cctxt
       ~chain
       ~block
       ~fee_parameter
       ?verbose_signing
       ?branch
       ?src_sk
       contents)
  >>=? fun (_oph, op, result) ->
  (match detect_script_failure result with
  | Ok () -> return_unit
  | Error _ as res ->
      cctxt#message
        "@[<v 2>This simulation failed:@,%a@]"
        Operation_result.pp_operation_result
        (op.protocol_data.contents, result.contents)
      >>= fun () -> if force then return_unit else Lwt.return res)
  >>=? fun () ->
  let bytes =
    Data_encoding.Binary.to_bytes_exn Operation.encoding (Operation.pack op)
  in
  if dry_run || simulation then
    let oph = Operation_hash.hash_bytes [bytes] in
    cctxt#message
      "@[<v 0>Operation: 0x%a@,Operation hash is '%a'@]"
      Hex.pp
      (Hex.of_bytes bytes)
      Operation_hash.pp
      oph
    >>= fun () ->
    cctxt#message
      "@[<v 2>Simulation result:@,%a@]"
      Operation_result.pp_operation_result
      (op.protocol_data.contents, result.contents)
    >>= fun () -> return (oph, op.protocol_data.contents, result.contents)
  else
    Shell_services.Injection.operation cctxt ~chain bytes >>=? fun oph ->
    cctxt#message "Operation successfully injected in the node." >>= fun () ->
    cctxt#message "Operation hash is '%a'" Operation_hash.pp oph >>= fun () ->
    (* Adjust user-provided confirmations with respect to Alpha protocol finality properties *)
    tenderbake_adjust_confirmations cctxt confirmations >>= fun confirmations ->
    (match confirmations with
    | None ->
        cctxt#message
          "@[<v 0>NOT waiting for the operation to be included.@,\
           Use command@,\
          \  octez-client wait for %a to be included --confirmations %d \
           --branch %a@,\
           and/or an external block explorer to make sure that it has been \
           included.@]"
          Operation_hash.pp
          oph
          tenderbake_finality_confirmations
          Block_hash.pp
          op.shell.branch
        >>= fun () -> return result
    | Some confirmations -> (
        cctxt#message "Waiting for the operation to be included..."
        >>= fun () ->
        Client_confirmations.wait_for_operation_inclusion
          ~branch:op.shell.branch
          ~confirmations
          cctxt
          ~chain
          oph
        >>=? fun (h, i, j) ->
        Alpha_block_services.Operations.operation
          cctxt
          ~chain
          ~block:(`Hash (h, 0))
          i
          j
        >>=? fun op' ->
        match op'.receipt with
        | Empty -> failwith "Internal error: pruned metadata."
        | Too_large -> failwith "Internal error: too large metadata."
        | Receipt No_operation_metadata ->
            failwith "Internal error: unexpected receipt."
        | Receipt (Operation_metadata receipt) -> (
            match Apply_results.kind_equal_list contents receipt.contents with
            | Some Apply_results.Eq ->
                return (receipt : kind operation_metadata)
            | None -> failwith "Internal error: unexpected receipt.")))
    >>=? fun result ->
    cctxt#message
      "@[<v 2>This sequence of operations was run:@,%a@]"
      Operation_result.pp_operation_result
      (op.protocol_data.contents, result.contents)
    >>= fun () ->
    Lwt.return (originated_contracts result.contents ~force)
    >>=? fun contracts ->
    List.iter_s
      (fun c -> cctxt#message "New contract %a originated." Contract.pp c)
      contracts
    >>= fun () ->
    (match confirmations with
    | None -> Lwt.return_unit
    | Some number ->
        if number >= tenderbake_finality_confirmations then
          cctxt#message
            "The operation was included in a block %d blocks ago."
            number
        else
          cctxt#message
            "@[<v 0>The operation has only been included %d blocks ago.@,\
             We recommend to wait more.@,\
             Use command@,\
            \  octez-client wait for %a to be included --confirmations %d \
             --branch %a@,\
             and/or an external block explorer.@]"
            number
            Operation_hash.pp
            oph
            tenderbake_finality_confirmations
            Block_hash.pp
            op.shell.branch)
    >>= fun () -> return (oph, op.protocol_data.contents, result.contents)

let inject_operation (type kind) cctxt ~chain ~block ?confirmations
    ?(dry_run = false) ?(simulation = false) ?branch ?src_sk ?verbose_signing
    ~fee_parameter (contents : kind contents_list) =
  Tezos_client_base.Client_confirmations.wait_for_bootstrapped cctxt
  >>=? fun () ->
  inject_operation_internal
    cctxt
    ~chain
    ~block
    ?confirmations
    ~dry_run
    ~simulation
    ?branch
    ?src_sk
    ?verbose_signing
    ~fee_parameter
    (contents : kind contents_list)

let prepare_manager_operation ~fee ~gas_limit ~storage_limit operation =
  Annotated_manager_operation.Manager_info
    {source = None; fee; gas_limit; storage_limit; counter = None; operation}

(* [gas_limit] must correspond to
   [Michelson_v1_gas.Cost_of.manager_operation] *)
let cost_of_manager_operation = Gas.Arith.integral_of_int_exn 1_000

let reveal_error_message =
  "Requested operation requires to perform a public key revelation beforehand.\n\
   This cannot be done automatically when a custom fee or storage limit is \
   given.\n\
   If you wish to use a custom fee or storage limit, please first perform the \
   reveal operation separately using the dedicated command.\n\
   Otherwise, please do not specify custom fee or storage parameters."

let reveal_error (cctxt : #Protocol_client_context.full) =
  cctxt#error "%s" reveal_error_message

let inject_manager_operation cctxt ~chain ~block ?branch ?confirmations ?dry_run
    ?verbose_signing ?simulation ?force ~source ~src_pk ~src_sk ~fee ~gas_limit
    ~storage_limit ?counter ~fee_parameter (type kind)
    (operations : kind Annotated_manager_operation.annotated_list) :
    (Operation_hash.t
    * kind Kind.manager contents_list
    * kind Kind.manager contents_result_list)
    tzresult
    Lwt.t =
  (match counter with
  | None ->
      Alpha_services.Contract.counter cctxt (chain, block) source
      >>=? fun pcounter ->
      let counter = Z.succ pcounter in
      return counter
  | Some counter -> return counter)
  >>=? fun counter ->
  Alpha_services.Contract.manager_key cctxt (chain, block) source
  >>=? fun key ->
  (* [has_reveal] assumes that a Reveal operation only appears as the first of a batch *)
  let has_reveal : type kind.
      kind Annotated_manager_operation.annotated_list -> bool = function
    | Single_manager (Manager_info {operation = Reveal _; _}) -> true
    | Cons_manager (Manager_info {operation = Reveal _; _}, _) -> true
    | _ -> false
  in
  let apply_specified_options counter op =
    Annotated_manager_operation.set_source source op >>? fun op ->
    Annotated_manager_operation.set_counter counter op >>? fun op ->
    Annotated_manager_operation.join_fee fee op >>? fun op ->
    Annotated_manager_operation.join_gas_limit gas_limit op >>? fun op ->
    Annotated_manager_operation.join_storage_limit storage_limit op
  in
  let rec build_contents : type kind.
      Z.t ->
      kind Annotated_manager_operation.annotated_list ->
      kind Annotated_manager_operation.annotated_list tzresult =
   fun counter -> function
     | Single_manager op ->
         apply_specified_options counter op >|? fun op ->
         Annotated_manager_operation.Single_manager op
     | Cons_manager (op, rest) ->
         apply_specified_options counter op >>? fun op ->
         build_contents (Z.succ counter) rest >|? fun rest ->
         Annotated_manager_operation.Cons_manager (op, rest)
  in
  match key with
  | None when not (has_reveal operations) -> (
      (if not (Limit.is_unknown fee && Limit.is_unknown storage_limit) then
         reveal_error cctxt
       else return_unit)
      >>=? fun () ->
      let reveal =
        prepare_manager_operation
          ~fee:Limit.unknown
          ~gas_limit:(Limit.known cost_of_manager_operation)
          ~storage_limit:Limit.unknown
          (Reveal src_pk)
      in
      Annotated_manager_operation.set_source source reveal >>?= fun reveal ->
      Annotated_manager_operation.set_counter counter reveal >>?= fun reveal ->
      build_contents (Z.succ counter) operations >>?= fun rest ->
      let contents = Annotated_manager_operation.Cons_manager (reveal, rest) in
      may_patch_limits cctxt ~fee_parameter ~chain ~block ?branch contents
      >>=? fun contents ->
      inject_operation_internal
        cctxt
        ~chain
        ~block
        ?confirmations
        ?dry_run
        ?simulation
        ?force
        ~fee_parameter
        ?verbose_signing
        ?branch
        ~src_sk
        contents
      >>=? fun (oph, op, result) ->
      match pack_contents_list op result with
      | Cons_and_result (_, _, rest) ->
          let op, result = unpack_contents_list rest in
          return (oph, op, result)
      | _ -> assert false)
  | Some _ when has_reveal operations ->
      failwith "The manager key was previously revealed."
  | _ ->
      build_contents counter operations >>?= fun contents ->
      may_patch_limits cctxt ~fee_parameter ~chain ~block ?branch contents
      >>=? fun contents ->
      inject_operation_internal
        cctxt
        ~chain
        ~block
        ?confirmations
        ?dry_run
        ?verbose_signing
        ?simulation
        ?force
        ~fee_parameter
        ?branch
        ~src_sk
        contents
