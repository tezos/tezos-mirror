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
  let branch = Option.value ~default:0 branch in
  (* TODO export parameter *)
  (match block with
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

type _ annotated_manager_operation =
  | Manager_info : {
      fee : Tez.t option;
      operation : 'kind manager_operation;
      gas_limit : Gas.Arith.integral option;
      storage_limit : Z.t option;
    }
      -> 'kind annotated_manager_operation

type packed_annotated_manager_operation =
  | Annotated_manager_operation :
      'kind annotated_manager_operation
      -> packed_annotated_manager_operation

type _ annotated_manager_operation_list =
  | Single_manager :
      'kind annotated_manager_operation
      -> 'kind annotated_manager_operation_list
  | Cons_manager :
      'kind annotated_manager_operation * 'rest annotated_manager_operation_list
      -> ('kind * 'rest) annotated_manager_operation_list

type packed_annotated_manager_operation_list =
  | Manager_list :
      'kind annotated_manager_operation_list
      -> packed_annotated_manager_operation_list

let rec manager_to_list = function
  | Manager_list (Single_manager o) -> [Annotated_manager_operation o]
  | Manager_list (Cons_manager (o, os)) ->
      Annotated_manager_operation o :: manager_to_list (Manager_list os)

let rec manager_of_list = function
  | [] -> assert false
  | [Annotated_manager_operation o] -> Manager_list (Single_manager o)
  | Annotated_manager_operation o :: os ->
      let (Manager_list os) = manager_of_list os in
      Manager_list (Cons_manager (o, os))

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
  get_branch cctxt ~chain ~block branch >>=? fun (chain_id, branch) ->
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
        | Single (Endorsement _) ->
            Tezos_crypto.Signature.V0.(Endorsement chain_id)
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
    ?branch (contents : t contents_list) =
  get_branch cctxt ~chain ~block branch >>=? fun (_chain_id, branch) ->
  let op : _ Operation.t =
    {shell = {branch}; protocol_data = {contents; signature = None}}
  in
  let oph = Operation.hash op in
  Chain_services.chain_id cctxt ~chain () >>=? fun chain_id ->
  Alpha_services.Helpers.Scripts.run_operation
    cctxt
    (chain, block)
    ~op:(Operation.pack op)
    ~chain_id
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
    | Skipped _ -> assert false
    | Backtracked (_, None) ->
        Ok Gas.Arith.zero (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Environment.wrap_error (Error errs)
    | Failed (_, errs) -> Environment.wrap_error (Error errs)
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
    | Skipped _ -> assert false
    | Backtracked (_, None) ->
        Ok Z.zero (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Environment.wrap_error (Error errs)
    | Failed (_, errs) -> Environment.wrap_error (Error errs)
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
    | Applied (Reveal_result _) -> Ok []
    | Applied (Delegation_result _) -> Ok []
    | Skipped _ -> assert false
    | Backtracked (_, None) ->
        Ok [] (* there must be another error for this to happen *)
    | Backtracked (_, Some errs) -> Environment.wrap_error (Error errs)
    | Failed (_, errs) -> Environment.wrap_error (Error errs)
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
              (Environment.wrap_error (Error errs))
        | Failed (_, errs) ->
            record_trace
              (error_of_fmt "The transfer simulation failed.")
              (Environment.wrap_error (Error errs))
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

let may_patch_limits (type kind) (cctxt : #Protocol_client_context.full)
    ~fee_parameter ~chain ~block ?branch ?(compute_fee = false)
    (contents : kind contents_list) : kind contents_list tzresult Lwt.t =
  Alpha_services.Constants.all cctxt (chain, block)
  >>=?
  fun {
        parametric =
          {
            hard_gas_limit_per_operation;
            hard_storage_limit_per_operation = storage_limit;
            origination_size;
            cost_per_byte;
            _;
          };
        _;
      }
    ->
  let user_gas_limit_needs_patching user_gas_limit =
    Gas.Arith.(user_gas_limit < zero)
    || Gas.Arith.(hard_gas_limit_per_operation <= user_gas_limit)
  in
  let user_storage_limit_needs_patching user_storage_limit =
    user_storage_limit < Z.zero || storage_limit <= user_storage_limit
  in
  let may_need_patching_single : type kind.
      kind contents -> kind contents option = function
    | Manager_operation c
      when (compute_fee && c.fee = Tez.zero)
           || user_gas_limit_needs_patching c.gas_limit
           || user_storage_limit_needs_patching c.storage_limit ->
        let gas_limit =
          if user_gas_limit_needs_patching c.gas_limit then
            hard_gas_limit_per_operation
          else c.gas_limit
        in
        let storage_limit =
          if user_storage_limit_needs_patching c.storage_limit then
            storage_limit
          else c.storage_limit
        in
        Some (Manager_operation {c with gas_limit; storage_limit})
    | _ -> None
  in
  let rec may_need_patching : type kind.
      kind contents_list -> kind contents_list option = function
    | Single (Manager_operation _ as c) -> (
        match may_need_patching_single c with
        | None -> None
        | Some op -> Some (Single op))
    | Single _ -> None
    | Cons ((Manager_operation _ as c), rest) -> (
        match (may_need_patching_single c, may_need_patching rest) with
        | None, None -> None
        | Some c, None -> Some (Cons (c, rest))
        | None, Some rest -> Some (Cons (c, rest))
        | Some c, Some rest -> Some (Cons (c, rest)))
  in
  let rec patch_fee : type kind. bool -> kind contents -> kind contents =
   fun first -> function
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
             if fee <= c.fee then op
             else patch_fee first (Manager_operation {c with fee}))
     | c -> c
  in
  let patch : type kind.
      bool ->
      kind contents * kind contents_result ->
      kind contents tzresult Lwt.t =
   fun first -> function
     | Manager_operation c, (Manager_operation_result _ as result) ->
         (if user_gas_limit_needs_patching c.gas_limit then
            Lwt.return (estimated_gas_single result) >>=? fun gas ->
            if Gas.Arith.(gas = zero) then
              cctxt#message "Estimated gas: none" >>= fun () ->
              return Gas.Arith.zero
            else
              cctxt#message
                "Estimated gas: %a units (will add 100 for safety)"
                Gas.Arith.pp
                gas
              >>= fun () ->
              let gas_plus_100 =
                Gas.Arith.(add (ceil gas) (integral_of_int 100))
              in
              let patched_gas =
                Gas.Arith.min gas_plus_100 hard_gas_limit_per_operation
              in
              return patched_gas
          else return c.gas_limit)
         >>=? fun gas_limit ->
         (if c.storage_limit < Z.zero || storage_limit <= c.storage_limit then
            Lwt.return
              (estimated_storage_single (Z.of_int origination_size) result)
            >>=? fun storage ->
            if Z.equal storage Z.zero then
              cctxt#message "Estimated storage: no bytes added" >>= fun () ->
              return Z.zero
            else
              cctxt#message
                "Estimated storage: %s bytes added (will add 20 for safety)"
                (Z.to_string storage)
              >>= fun () ->
              return (Z.min (Z.add storage (Z.of_int 20)) storage_limit)
          else return c.storage_limit)
         >>=? fun storage_limit ->
         let cm = Manager_operation {c with gas_limit; storage_limit} in
         if compute_fee && c.fee = Tez.zero then return (patch_fee first cm)
         else return cm
     | c, _ -> return c
  in
  let rec patch_list : type kind.
      bool -> kind contents_and_result_list -> kind contents_list tzresult Lwt.t
      =
   fun first -> function
     | Single_and_result
         ((Manager_operation _ as op), (Manager_operation_result _ as res)) ->
         patch first (op, res) >>=? fun op -> return (Single op)
     | Single_and_result (op, _) -> return (Single op)
     | Cons_and_result
         ((Manager_operation _ as op), (Manager_operation_result _ as res), rest)
       ->
         patch first (op, res) >>=? fun op ->
         patch_list false rest >>=? fun rest -> return (Cons (op, rest))
  in
  match may_need_patching contents with
  | Some contents ->
      simulate cctxt ~chain ~block ?branch contents >>=? fun (_, _, result) ->
      (match detect_script_failure result with
      | Ok () -> return_unit
      | Error _ ->
          cctxt#message
            "@[<v 2>This simulation failed:@,%a@]"
            Operation_result.pp_operation_result
            (contents, result.contents)
          >>= fun () -> return_unit)
      >>=? fun () ->
      ( Lwt.return
          (estimated_storage (Z.of_int origination_size) result.contents)
      >>=? fun storage ->
        Lwt.return
          (Environment.wrap_error Tez.(cost_per_byte *? Z.to_int64 storage))
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
      >>=? fun () ->
      let res = pack_contents_list contents result.contents in
      patch_list true res
  | None -> return contents

let inject_operation (type kind) cctxt ~chain ~block ?confirmations
    ?(dry_run = false) ?(simulation = false) ?branch ?src_sk ?verbose_signing
    ~fee_parameter ?compute_fee (contents : kind contents_list) =
  Tezos_client_base.Client_confirmations.wait_for_bootstrapped cctxt
  >>=? fun () ->
  may_patch_limits
    cctxt
    ~chain
    ~block
    ?branch
    ~fee_parameter
    ?compute_fee
    contents
  >>=? fun contents ->
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
      >>= fun () -> Lwt.return res)
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
    (match confirmations with
    | None ->
        cctxt#message
          "@[<v 0>NOT waiting for the operation to be included.@,\
           Use command@,\
          \  octez-client wait for %a to be included --confirmations 30 \
           --branch %a@,\
           and/or an external block explorer to make sure that it has been \
           included.@]"
          Operation_hash.pp
          oph
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
    Lwt.return (originated_contracts result.contents) >>=? fun contracts ->
    List.iter_s
      (fun c -> cctxt#message "New contract %a originated." Contract.pp c)
      contracts
    >>= fun () ->
    (match confirmations with
    | None -> Lwt.return_unit
    | Some number ->
        if number >= 30 then
          cctxt#message
            "The operation was included in a block %d blocks ago."
            number
        else
          cctxt#message
            "@[<v 0>The operation has only been included %d blocks ago.@,\
             We recommend to wait more.@,\
             Use command@,\
            \  octez-client wait for %a to be included --confirmations 30 \
             --branch %a@,\
             and/or an external block explorer.@]"
            number
            Operation_hash.pp
            oph
            Block_hash.pp
            op.shell.branch)
    >>= fun () -> return (oph, op.protocol_data.contents, result.contents)

let prepare_manager_operation ?fee ?gas_limit ?storage_limit operation =
  Manager_info {fee; gas_limit; storage_limit; operation}

let inject_manager_operation cctxt ~chain ~block ?branch ?confirmations ?dry_run
    ?verbose_signing ?simulation ~source ~src_pk ~src_sk ?fee
    ?(gas_limit = Gas.Arith.integral Z.minus_one)
    ?(storage_limit = Z.of_int (-1)) ?counter ~fee_parameter (type kind)
    (operations : kind annotated_manager_operation_list) :
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
  (* [is_reveal] assumes that a Reveal operation only appears as the first of a batch *)
  let is_reveal : type kind. kind annotated_manager_operation_list -> bool =
    function
    | Single_manager (Manager_info {operation = Reveal _; _}) -> true
    | Cons_manager (Manager_info {operation = Reveal _; _}, _) -> true
    | _ -> false
  in
  let compute_fee, fee =
    match fee with None -> (true, Tez.zero) | Some fee -> (false, fee)
  in
  let contents_of_manager ~source ~fee ~counter ~gas_limit ~storage_limit
      operation =
    let (Manager_info {fee = f; gas_limit = g; storage_limit = s; operation}) =
      operation
    in
    let fee = Option.value ~default:fee f in
    let gas_limit = Option.value ~default:gas_limit g in
    let storage_limit = Option.value ~default:storage_limit s in
    Manager_operation
      {source; fee; counter; gas_limit; storage_limit; operation}
  in
  let rec build_contents : type kind.
      Z.t ->
      kind annotated_manager_operation_list ->
      kind Kind.manager contents_list =
   fun counter -> function
     | Single_manager operation ->
         Single
           (contents_of_manager
              ~source
              ~fee
              ~counter
              ~gas_limit
              ~storage_limit
              operation)
     | Cons_manager (operation, rest) ->
         Cons
           ( contents_of_manager
               ~source
               ~fee
               ~counter
               ~gas_limit
               ~storage_limit
               operation,
             build_contents (Z.succ counter) rest )
  in
  match key with
  | None when not (is_reveal operations) -> (
      let contents =
        Cons
          ( Manager_operation
              {
                source;
                fee = Tez.zero;
                counter;
                (* [gas_limit] must correspond to
                   [Michelson_v1_gas.Cost_of.manager_operation] *)
                gas_limit = Gas.Arith.integral_of_int 1_000;
                storage_limit = Z.zero;
                operation = Reveal src_pk;
              },
            build_contents (Z.succ counter) operations )
      in
      inject_operation
        cctxt
        ~chain
        ~block
        ?confirmations
        ?dry_run
        ?simulation
        ~fee_parameter
        ~compute_fee
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
  | Some _ when is_reveal operations ->
      failwith "The manager key was previously revealed."
  | _ ->
      let contents = build_contents counter operations in
      inject_operation
        cctxt
        ~chain
        ~block
        ?confirmations
        ?dry_run
        ?verbose_signing
        ?simulation
        ~compute_fee
        ~fee_parameter
        ?branch
        ~src_sk
        contents
