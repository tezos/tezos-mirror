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

module Int_set = Set.Make (Compare.Int)

(*

   Gas levels maintainance
   =======================

   The context maintains two levels of gas, one corresponds to the gas
   available for the current operation while the other is the gas
   available for the current block. Both levels are maintained
   independently: [consume_gas] only decreases the operation level,
   and block level should be updated with [consume_gas_limit_in_block].

   A layered context
   =================

   Updating the context [remaining_operation_gas] is a critical routine
   called very frequently by the operations performed by the protocol.
   On the contrary, other fields are less frequently updated.

   In a previous version of the context datatype definition, all
   the fields were represented at the toplevel. To update the remaining
   gas, we had to copy ~25 fields (that is 200 bytes).

   With the following layered representation, we only have to
   copy 2 fields (16 bytes) during [remaining_operation_gas] update.
   This has a significant impact on the Michelson runtime efficiency.

   Here are the fields on the [back] of the context:

 *)
type back = {
  context : Context.t;
  constants : Constants_repr.parametric;
  cycle_eras : Level_repr.cycle_eras;
  level : Level_repr.t;
  predecessor_timestamp : Time.t;
  timestamp : Time.t;
  fitness : Int64.t;
  deposits : Tez_repr.t Signature.Public_key_hash.Map.t;
  included_endorsements : int;
  allowed_endorsements :
    (Signature.Public_key.t * int list * bool) Signature.Public_key_hash.Map.t;
  fees : Tez_repr.t;
  rewards : Tez_repr.t;
  storage_space_to_pay : Z.t option;
  allocated_contracts : int option;
  origination_nonce : Contract_repr.origination_nonce option;
  temporary_lazy_storage_ids : Lazy_storage_kind.Temp_ids.t;
  internal_nonce : int;
  internal_nonces_used : Int_set.t;
  remaining_block_gas : Gas_limit_repr.Arith.fp;
  unlimited_operation_gas : bool;
}

(*

   The context is simply a record with two fields which
   limits the cost of updating the [remaining_operation_gas].

*)
type t = {remaining_operation_gas : Gas_limit_repr.Arith.fp; back : back}

type root = t

(*

   Context fields accessors
   ========================

   To have the context related code more robust to evolutions,
   we introduce accessors to get and to update the context
   components.

*)
let[@inline] context ctxt = ctxt.back.context

let[@inline] current_level ctxt = ctxt.back.level

let[@inline] storage_space_to_pay ctxt = ctxt.back.storage_space_to_pay

let[@inline] predecessor_timestamp ctxt = ctxt.back.predecessor_timestamp

let[@inline] current_timestamp ctxt = ctxt.back.timestamp

let[@inline] current_fitness ctxt = ctxt.back.fitness

let[@inline] cycle_eras ctxt = ctxt.back.cycle_eras

let[@inline] constants ctxt = ctxt.back.constants

let[@inline] recover ctxt = ctxt.back.context

let[@inline] fees ctxt = ctxt.back.fees

let[@inline] origination_nonce ctxt = ctxt.back.origination_nonce

let[@inline] deposits ctxt = ctxt.back.deposits

let[@inline] allowed_endorsements ctxt = ctxt.back.allowed_endorsements

let[@inline] included_endorsements ctxt = ctxt.back.included_endorsements

let[@inline] internal_nonce ctxt = ctxt.back.internal_nonce

let[@inline] internal_nonces_used ctxt = ctxt.back.internal_nonces_used

let[@inline] remaining_block_gas ctxt = ctxt.back.remaining_block_gas

let[@inline] unlimited_operation_gas ctxt = ctxt.back.unlimited_operation_gas

let[@inline] rewards ctxt = ctxt.back.rewards

let[@inline] allocated_contracts ctxt = ctxt.back.allocated_contracts

let[@inline] temporary_lazy_storage_ids ctxt =
  ctxt.back.temporary_lazy_storage_ids

let[@inline] remaining_operation_gas ctxt = ctxt.remaining_operation_gas

let[@inline] update_remaining_operation_gas ctxt remaining_operation_gas =
  {ctxt with remaining_operation_gas}

let[@inline] update_back ctxt back = {ctxt with back}

let[@inline] update_remaining_block_gas ctxt remaining_block_gas =
  update_back ctxt {ctxt.back with remaining_block_gas}

let[@inline] update_unlimited_operation_gas ctxt unlimited_operation_gas =
  update_back ctxt {ctxt.back with unlimited_operation_gas}

let[@inline] update_context ctxt context =
  update_back ctxt {ctxt.back with context}

let[@inline] update_constants ctxt constants =
  update_back ctxt {ctxt.back with constants}

let[@inline] update_fitness ctxt fitness =
  update_back ctxt {ctxt.back with fitness}

let[@inline] update_deposits ctxt deposits =
  update_back ctxt {ctxt.back with deposits}

let[@inline] update_allowed_endorsements ctxt allowed_endorsements =
  update_back ctxt {ctxt.back with allowed_endorsements}

let[@inline] update_rewards ctxt rewards =
  update_back ctxt {ctxt.back with rewards}

let[@inline] raw_update_storage_space_to_pay ctxt storage_space_to_pay =
  update_back ctxt {ctxt.back with storage_space_to_pay}

let[@inline] update_allocated_contracts ctxt allocated_contracts =
  update_back ctxt {ctxt.back with allocated_contracts}

let[@inline] update_origination_nonce ctxt origination_nonce =
  update_back ctxt {ctxt.back with origination_nonce}

let[@inline] update_internal_nonce ctxt internal_nonce =
  update_back ctxt {ctxt.back with internal_nonce}

let[@inline] update_internal_nonces_used ctxt internal_nonces_used =
  update_back ctxt {ctxt.back with internal_nonces_used}

let[@inline] update_included_endorsements ctxt included_endorsements =
  update_back ctxt {ctxt.back with included_endorsements}

let[@inline] update_fees ctxt fees = update_back ctxt {ctxt.back with fees}

let[@inline] update_temporary_lazy_storage_ids ctxt temporary_lazy_storage_ids =
  update_back ctxt {ctxt.back with temporary_lazy_storage_ids}

let record_endorsement ctxt k =
  match Signature.Public_key_hash.Map.find k (allowed_endorsements ctxt) with
  | None -> assert false
  | Some (_, _, true) -> assert false (* right already used *)
  | Some (d, s, false) ->
      let ctxt =
        update_included_endorsements
          ctxt
          (included_endorsements ctxt + List.length s)
      in
      update_allowed_endorsements
        ctxt
        (Signature.Public_key_hash.Map.add
           k
           (d, s, true)
           (allowed_endorsements ctxt))

let init_endorsements ctxt allowed_endorsements' =
  if Signature.Public_key_hash.Map.is_empty allowed_endorsements' then
    assert false (* can't initialize to empty *)
  else if Signature.Public_key_hash.Map.is_empty (allowed_endorsements ctxt)
  then update_allowed_endorsements ctxt allowed_endorsements'
  else assert false

type error += Too_many_internal_operations (* `Permanent *)

type error += Block_quota_exceeded (* `Temporary *)

type error += Operation_quota_exceeded (* `Temporary *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"too_many_internal_operations"
    ~title:"Too many internal operations"
    ~description:
      "A transaction exceeded the hard limit of internal operations it can emit"
    empty
    (function Too_many_internal_operations -> Some () | _ -> None)
    (fun () -> Too_many_internal_operations) ;
  register_error_kind
    `Temporary
    ~id:"gas_exhausted.operation"
    ~title:"Gas quota exceeded for the operation"
    ~description:
      "A script or one of its callee took more time than the operation said it \
       would"
    empty
    (function Operation_quota_exceeded -> Some () | _ -> None)
    (fun () -> Operation_quota_exceeded) ;
  register_error_kind
    `Temporary
    ~id:"gas_exhausted.block"
    ~title:"Gas quota exceeded for the block"
    ~description:
      "The sum of gas consumed by all the operations in the block exceeds the \
       hard gas limit per block"
    empty
    (function Block_quota_exceeded -> Some () | _ -> None)
    (fun () -> Block_quota_exceeded)

let fresh_internal_nonce ctxt =
  if Compare.Int.(internal_nonce ctxt >= 65_535) then
    error Too_many_internal_operations
  else
    ok
      (update_internal_nonce ctxt (internal_nonce ctxt + 1), internal_nonce ctxt)

let reset_internal_nonce ctxt =
  let ctxt = update_internal_nonce ctxt 0 in
  update_internal_nonces_used ctxt Int_set.empty

let record_internal_nonce ctxt k =
  update_internal_nonces_used ctxt (Int_set.add k (internal_nonces_used ctxt))

let internal_nonce_already_recorded ctxt k =
  Int_set.mem k (internal_nonces_used ctxt)

let set_current_fitness ctxt fitness = update_fitness ctxt fitness

let add_fees ctxt fees' = Tez_repr.(fees ctxt +? fees') >|? update_fees ctxt

let add_rewards ctxt rewards' =
  Tez_repr.(rewards ctxt +? rewards') >|? update_rewards ctxt

let add_deposit ctxt delegate deposit =
  let open Signature.Public_key_hash.Map in
  let previous =
    match find delegate (deposits ctxt) with
    | Some tz -> tz
    | None -> Tez_repr.zero
  in
  Tez_repr.(previous +? deposit) >|? fun deposit ->
  let deposits = add delegate deposit (deposits ctxt) in
  update_deposits ctxt deposits

let get_deposits = deposits

let get_rewards = rewards

let get_fees = fees

type error += Undefined_operation_nonce (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"undefined_operation_nonce"
    ~title:"Ill timed access to the origination nonce"
    ~description:
      "An origination was attempted out of the scope of a manager operation"
    empty
    (function Undefined_operation_nonce -> Some () | _ -> None)
    (fun () -> Undefined_operation_nonce)

let init_origination_nonce ctxt operation_hash =
  let origination_nonce =
    Some (Contract_repr.initial_origination_nonce operation_hash)
  in
  update_origination_nonce ctxt origination_nonce

let increment_origination_nonce ctxt =
  match origination_nonce ctxt with
  | None -> error Undefined_operation_nonce
  | Some cur_origination_nonce ->
      let origination_nonce =
        Some (Contract_repr.incr_origination_nonce cur_origination_nonce)
      in
      let ctxt = update_origination_nonce ctxt origination_nonce in
      ok (ctxt, cur_origination_nonce)

let get_origination_nonce ctxt =
  match origination_nonce ctxt with
  | None -> error Undefined_operation_nonce
  | Some origination_nonce -> ok origination_nonce

let unset_origination_nonce ctxt = update_origination_nonce ctxt None

type error += Gas_limit_too_high (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"gas_limit_too_high"
    ~title:"Gas limit out of protocol hard bounds"
    ~description:"A transaction tried to exceed the hard limit on gas"
    empty
    (function Gas_limit_too_high -> Some () | _ -> None)
    (fun () -> Gas_limit_too_high)

let gas_level ctxt =
  let open Gas_limit_repr in
  if unlimited_operation_gas ctxt then Unaccounted
  else Limited {remaining = remaining_operation_gas ctxt}

let block_gas_level = remaining_block_gas

let check_gas_limit_is_valid ctxt (remaining : 'a Gas_limit_repr.Arith.t) =
  if
    Gas_limit_repr.Arith.(
      remaining > (constants ctxt).hard_gas_limit_per_operation
      || remaining < zero)
  then error Gas_limit_too_high
  else ok_unit

let consume_gas_limit_in_block ctxt (limit : 'a Gas_limit_repr.Arith.t) =
  let open Gas_limit_repr in
  check_gas_limit_is_valid ctxt limit >>? fun () ->
  let block_gas = block_gas_level ctxt in
  let limit = Arith.fp limit in
  if Arith.(limit > block_gas) then error Block_quota_exceeded
  else
    let level = Arith.sub (block_gas_level ctxt) limit in
    let ctxt = update_remaining_block_gas ctxt level in
    Ok ctxt

let set_gas_limit ctxt (remaining : 'a Gas_limit_repr.Arith.t) =
  let open Gas_limit_repr in
  let remaining_operation_gas = Arith.fp remaining in
  let ctxt = update_unlimited_operation_gas ctxt false in
  {ctxt with remaining_operation_gas}

let set_gas_unlimited ctxt = update_unlimited_operation_gas ctxt true

let gas_exhausted_error _ctxt = error Operation_quota_exceeded

let consume_gas ctxt cost =
  match Gas_limit_repr.raw_consume (remaining_operation_gas ctxt) cost with
  | Some gas_counter -> Ok (update_remaining_operation_gas ctxt gas_counter)
  | None ->
      if unlimited_operation_gas ctxt then ok ctxt
      else error Operation_quota_exceeded

let check_enough_gas ctxt cost = consume_gas ctxt cost >>? fun _ -> ok_unit

let gas_consumed ~since ~until =
  match (gas_level since, gas_level until) with
  | (Limited {remaining = before}, Limited {remaining = after}) ->
      Gas_limit_repr.Arith.sub before after
  | (_, _) -> Gas_limit_repr.Arith.zero

let init_storage_space_to_pay ctxt =
  match storage_space_to_pay ctxt with
  | Some _ -> assert false
  | None ->
      let ctxt = raw_update_storage_space_to_pay ctxt (Some Z.zero) in
      update_allocated_contracts ctxt (Some 0)

let clear_storage_space_to_pay ctxt =
  match (storage_space_to_pay ctxt, allocated_contracts ctxt) with
  | (None, _) | (_, None) -> assert false
  | (Some storage_space_to_pay, Some allocated_contracts) ->
      let ctxt = raw_update_storage_space_to_pay ctxt None in
      let ctxt = update_allocated_contracts ctxt None in
      (ctxt, storage_space_to_pay, allocated_contracts)

let update_storage_space_to_pay ctxt n =
  match storage_space_to_pay ctxt with
  | None -> assert false
  | Some storage_space_to_pay ->
      raw_update_storage_space_to_pay ctxt (Some (Z.add n storage_space_to_pay))

let update_allocated_contracts_count ctxt =
  match allocated_contracts ctxt with
  | None -> assert false
  | Some allocated_contracts ->
      update_allocated_contracts ctxt (Some (succ allocated_contracts))

type missing_key_kind = Get | Set | Del | Copy

type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * missing_key_kind
  | Existing_key of string list
  | Corrupted_data of string list

let storage_error_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Incompatible_protocol_version"
        (obj1 (req "incompatible_protocol_version" string))
        (function Incompatible_protocol_version arg -> Some arg | _ -> None)
        (fun arg -> Incompatible_protocol_version arg);
      case
        (Tag 1)
        ~title:"Missing_key"
        (obj2
           (req "missing_key" (list string))
           (req
              "function"
              (string_enum
                 [("get", Get); ("set", Set); ("del", Del); ("copy", Copy)])))
        (function Missing_key (key, f) -> Some (key, f) | _ -> None)
        (fun (key, f) -> Missing_key (key, f));
      case
        (Tag 2)
        ~title:"Existing_key"
        (obj1 (req "existing_key" (list string)))
        (function Existing_key key -> Some key | _ -> None)
        (fun key -> Existing_key key);
      case
        (Tag 3)
        ~title:"Corrupted_data"
        (obj1 (req "corrupted_data" (list string)))
        (function Corrupted_data key -> Some key | _ -> None)
        (fun key -> Corrupted_data key);
    ]

let pp_storage_error ppf = function
  | Incompatible_protocol_version version ->
      Format.fprintf
        ppf
        "Found a context with an unexpected version '%s'."
        version
  | Missing_key (key, Get) ->
      Format.fprintf ppf "Missing key '%s'." (String.concat "/" key)
  | Missing_key (key, Set) ->
      Format.fprintf
        ppf
        "Cannot set undefined key '%s'."
        (String.concat "/" key)
  | Missing_key (key, Del) ->
      Format.fprintf
        ppf
        "Cannot delete undefined key '%s'."
        (String.concat "/" key)
  | Missing_key (key, Copy) ->
      Format.fprintf
        ppf
        "Cannot copy undefined key '%s'."
        (String.concat "/" key)
  | Existing_key key ->
      Format.fprintf
        ppf
        "Cannot initialize defined key '%s'."
        (String.concat "/" key)
  | Corrupted_data key ->
      Format.fprintf
        ppf
        "Failed to parse the data at '%s'."
        (String.concat "/" key)

type error += Storage_error of storage_error

let () =
  register_error_kind
    `Permanent
    ~id:"context.storage_error"
    ~title:"Storage error (fatal internal error)"
    ~description:
      "An error that should never happen unless something has been deleted or \
       corrupted in the database."
    ~pp:(fun ppf err ->
      Format.fprintf ppf "@[<v 2>Storage error:@ %a@]" pp_storage_error err)
    storage_error_encoding
    (function Storage_error err -> Some err | _ -> None)
    (fun err -> Storage_error err)

let storage_error err = error (Storage_error err)

(* Initialization *********************************************************)

(* This key should always be populated for every version of the
   protocol.  It's absence meaning that the context is empty. *)
let version_key = ["version"]

(* This value is set by the snapshot_alpha.sh script, don't change it. *)
let version_value = "alpha_current"

let version = "v1"

(* DEPRECATED: remove after activation of G *)
let first_level_key = [version; "first_level"]

let cycle_eras_key = [version; "cycle_eras"]

let constants_key = [version; "constants"]

let protocol_param_key = ["protocol_parameters"]

(* DEPRECATED: remove after activation of G *)
let get_first_level ctxt =
  Context.find ctxt first_level_key >|= function
  | None -> storage_error (Missing_key (first_level_key, Get))
  | Some bytes -> (
      match Data_encoding.Binary.of_bytes_opt Raw_level_repr.encoding bytes with
      | None -> storage_error (Corrupted_data first_level_key)
      | Some level -> ok level)

let get_cycle_eras ctxt =
  Context.find ctxt cycle_eras_key >|= function
  | None -> storage_error (Missing_key (cycle_eras_key, Get))
  | Some bytes -> (
      match
        Data_encoding.Binary.of_bytes_opt Level_repr.cycle_eras_encoding bytes
      with
      | None -> storage_error (Corrupted_data cycle_eras_key)
      | Some cycle_eras -> ok cycle_eras)

let set_cycle_eras ctxt cycle_eras =
  let bytes =
    Data_encoding.Binary.to_bytes_exn Level_repr.cycle_eras_encoding cycle_eras
  in
  Context.add ctxt cycle_eras_key bytes >|= ok

type error += Failed_to_parse_parameter of bytes

type error += Failed_to_decode_parameter of Data_encoding.json * string

let () =
  register_error_kind
    `Temporary
    ~id:"context.failed_to_parse_parameter"
    ~title:"Failed to parse parameter"
    ~description:"The protocol parameters are not valid JSON."
    ~pp:(fun ppf bytes ->
      Format.fprintf
        ppf
        "@[<v 2>Cannot parse the protocol parameter:@ %s@]"
        (Bytes.to_string bytes))
    Data_encoding.(obj1 (req "contents" bytes))
    (function Failed_to_parse_parameter data -> Some data | _ -> None)
    (fun data -> Failed_to_parse_parameter data) ;
  register_error_kind
    `Temporary
    ~id:"context.failed_to_decode_parameter"
    ~title:"Failed to decode parameter"
    ~description:"Unexpected JSON object."
    ~pp:(fun ppf (json, msg) ->
      Format.fprintf
        ppf
        "@[<v 2>Cannot decode the protocol parameter:@ %s@ %a@]"
        msg
        Data_encoding.Json.pp
        json)
    Data_encoding.(obj2 (req "contents" json) (req "error" string))
    (function
      | Failed_to_decode_parameter (json, msg) -> Some (json, msg) | _ -> None)
    (fun (json, msg) -> Failed_to_decode_parameter (json, msg))

let get_proto_param ctxt =
  Context.find ctxt protocol_param_key >>= function
  | None -> failwith "Missing protocol parameters."
  | Some bytes -> (
      match Data_encoding.Binary.of_bytes_opt Data_encoding.json bytes with
      | None -> fail (Failed_to_parse_parameter bytes)
      | Some json -> (
          Context.remove ctxt protocol_param_key >|= fun ctxt ->
          match Data_encoding.Json.destruct Parameters_repr.encoding json with
          | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
              Format.kasprintf
                failwith
                "Invalid protocol_parameters: %a %a"
                (fun ppf -> Data_encoding.Json.print_error ppf)
                exn
                Data_encoding.Json.pp
                json
          | param ->
              Parameters_repr.check_params param >>? fun () -> ok (param, ctxt))
      )

let add_constants ctxt constants =
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Constants_repr.parametric_encoding
      constants
  in
  Context.add ctxt constants_key bytes

let get_constants ctxt =
  Context.find ctxt constants_key >|= function
  | None -> failwith "Internal error: cannot read constants in context."
  | Some bytes -> (
      match
        Data_encoding.Binary.of_bytes_opt
          Constants_repr.parametric_encoding
          bytes
      with
      | None -> failwith "Internal error: cannot parse constants in context."
      | Some constants -> ok constants)

let patch_constants ctxt f =
  let constants = f (constants ctxt) in
  add_constants (context ctxt) constants >|= fun context ->
  let ctxt = update_context ctxt context in
  update_constants ctxt constants

let check_inited ctxt =
  Context.find ctxt version_key >|= function
  | None -> failwith "Internal error: un-initialized context."
  | Some bytes ->
      let s = Bytes.to_string bytes in
      if Compare.String.(s = version_value) then ok_unit
      else storage_error (Incompatible_protocol_version s)

let check_cycle_eras (cycle_eras : Level_repr.cycle_eras)
    (constants : Constants_repr.parametric) =
  let current_era = Level_repr.current_era cycle_eras in
  assert (
    Compare.Int32.(current_era.blocks_per_cycle = constants.blocks_per_cycle)) ;
  assert (
    Compare.Int32.(
      current_era.blocks_per_commitment = constants.blocks_per_commitment))

let prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt =
  Raw_level_repr.of_int32 level >>?= fun level ->
  Fitness_repr.to_int64 fitness >>?= fun fitness ->
  check_inited ctxt >>=? fun () ->
  get_constants ctxt >>=? fun constants ->
  get_cycle_eras ctxt >|=? fun cycle_eras ->
  check_cycle_eras cycle_eras constants ;
  let level = Level_repr.level_from_raw ~cycle_eras level in
  {
    remaining_operation_gas = Gas_limit_repr.Arith.zero;
    back =
      {
        context = ctxt;
        constants;
        level;
        predecessor_timestamp;
        timestamp;
        fitness;
        cycle_eras;
        allowed_endorsements = Signature.Public_key_hash.Map.empty;
        included_endorsements = 0;
        fees = Tez_repr.zero;
        rewards = Tez_repr.zero;
        deposits = Signature.Public_key_hash.Map.empty;
        storage_space_to_pay = None;
        allocated_contracts = None;
        origination_nonce = None;
        temporary_lazy_storage_ids = Lazy_storage_kind.Temp_ids.init;
        internal_nonce = 0;
        internal_nonces_used = Int_set.empty;
        remaining_block_gas =
          Gas_limit_repr.Arith.fp
            constants.Constants_repr.hard_gas_limit_per_block;
        unlimited_operation_gas = true;
      };
  }

type previous_protocol = Genesis of Parameters_repr.t | Florence_009

let check_and_update_protocol_version ctxt =
  (Context.find ctxt version_key >>= function
   | None ->
       failwith "Internal error: un-initialized context in check_first_block."
   | Some bytes ->
       let s = Bytes.to_string bytes in
       if Compare.String.(s = version_value) then
         failwith "Internal error: previously initialized context."
       else if Compare.String.(s = "genesis") then
         get_proto_param ctxt >|=? fun (param, ctxt) -> (Genesis param, ctxt)
       else if Compare.String.(s = "florence_009") then
         return (Florence_009, ctxt)
       else Lwt.return @@ storage_error (Incompatible_protocol_version s))
  >>=? fun (previous_proto, ctxt) ->
  Context.add ctxt version_key (Bytes.of_string version_value) >|= fun ctxt ->
  ok (previous_proto, ctxt)

(* only for the migration *)
let get_previous_protocol_constants ctxt =
  Context.find ctxt constants_key >>= function
  | None ->
      failwith
        "Internal error: cannot read previous protocol constants in context."
  | Some bytes -> (
      match
        Data_encoding.Binary.of_bytes_opt
          Constants_repr.Proto_previous.parametric_encoding
          bytes
      with
      | None ->
          failwith
            "Internal error: cannot parse previous protocol constants in \
             context."
      | Some constants -> Lwt.return constants)

(* You should ensure that if the type `Constant_repr.parametric` is
   different from the previous protocol or the value of these
   constants is modified, is changed from the previous protocol, then
   you `propagate` these constants to the new protocol by writing them
   onto the context via the function `add_constants` or
   `patch_constants`.

   This migration can be achieved also implicitely by modifying the
   encoding directly in a way which is compatible with the previous
   protocol. However, by doing so, you do not change the value of
   these constants inside the context. *)
let prepare_first_block ~level ~timestamp ~fitness ctxt =
  check_and_update_protocol_version ctxt >>=? fun (previous_proto, ctxt) ->
  (match previous_proto with
  | Genesis param ->
      Raw_level_repr.of_int32 level >>?= fun first_level ->
      let cycle_era =
        Level_repr.
          {
            first_level;
            first_cycle = Cycle_repr.root;
            blocks_per_cycle = param.constants.blocks_per_cycle;
            blocks_per_commitment = param.constants.blocks_per_commitment;
          }
      in
      Level_repr.create_cycle_eras [cycle_era] >>?= fun cycle_eras ->
      set_cycle_eras ctxt cycle_eras >>=? fun ctxt ->
      add_constants ctxt param.constants >|= ok
  | Florence_009 ->
      get_first_level ctxt >>=? fun first_level ->
      Context.remove ctxt first_level_key >>= fun ctxt ->
      get_previous_protocol_constants ctxt >>= fun c ->
      let time_between_blocks_at_first_priority =
        (match c.time_between_blocks with
        | [] -> Period_repr.one_minute
        | first_time_between_blocks :: _ -> first_time_between_blocks)
        |> Period_repr.to_seconds
      in
      let mainnet_constants = Compare.Int.(c.initial_endorsers = 24) in
      let constants =
        Constants_repr.
          {
            minimal_block_delay =
              Period_repr.of_seconds_exn
                (if Compare.Int64.(time_between_blocks_at_first_priority = 1L)
                then 1L
                else (Int64.div time_between_blocks_at_first_priority) 2L);
            preserved_cycles = c.preserved_cycles;
            blocks_per_cycle =
              (if mainnet_constants then Int32.mul 2l c.blocks_per_cycle
              else c.blocks_per_cycle);
            blocks_per_commitment =
              (if mainnet_constants then Int32.mul 2l c.blocks_per_commitment
              else c.blocks_per_commitment);
            blocks_per_roll_snapshot =
              (if mainnet_constants then Int32.mul 2l c.blocks_per_roll_snapshot
              else c.blocks_per_roll_snapshot);
            blocks_per_voting_period =
              (if mainnet_constants then Int32.mul 2l c.blocks_per_voting_period
              else c.blocks_per_voting_period);
            time_between_blocks = c.time_between_blocks;
            endorsers_per_block = 256;
            hard_gas_limit_per_operation = c.hard_gas_limit_per_operation;
            hard_gas_limit_per_block =
              Gas_limit_repr.Arith.(integral_of_int_exn 5_200_000);
            proof_of_work_threshold = c.proof_of_work_threshold;
            tokens_per_roll = c.tokens_per_roll;
            seed_nonce_revelation_tip = c.seed_nonce_revelation_tip;
            origination_size = c.origination_size;
            block_security_deposit = Tez_repr.(mul_exn one 640);
            endorsement_security_deposit = Tez_repr.(mul_exn one_cent 250);
            baking_reward_per_endorsement =
              Tez_repr.[of_mutez_exn 78_125L; of_mutez_exn 11_719L];
            endorsement_reward =
              Tez_repr.[of_mutez_exn 78_125L; of_mutez_exn 52_083L];
            hard_storage_limit_per_operation =
              c.hard_storage_limit_per_operation;
            cost_per_byte = c.cost_per_byte;
            quorum_min = c.quorum_min;
            quorum_max = c.quorum_max;
            min_proposal_quorum = c.min_proposal_quorum;
            initial_endorsers =
              (if mainnet_constants then 192 else c.initial_endorsers);
            delay_per_missing_endorsement =
              (if mainnet_constants then Period_repr.of_seconds_exn 4L
              else c.delay_per_missing_endorsement);
            liquidity_baking_subsidy = Tez_repr.of_mutez_exn 2_500_000L;
            (* Approximately 6 month after the first activation of Liquidity Baking on mainnet *)
            liquidity_baking_sunset_level = 2_032_928l;
            liquidity_baking_escape_ema_threshold = 1_000_000l;
          }
      in
      add_constants ctxt constants >>= fun ctxt ->
      let first_cycle_era =
        Level_repr.
          {
            first_level;
            first_cycle = Cycle_repr.root;
            blocks_per_cycle = c.blocks_per_cycle;
            blocks_per_commitment = c.blocks_per_commitment;
          }
      in
      let current_cycle =
        let level_position =
          Int32.sub level (Raw_level_repr.to_int32 first_level)
        in
        Cycle_repr.of_int32_exn (Int32.div level_position c.blocks_per_cycle)
      in
      let second_cycle_era =
        Level_repr.
          {
            first_level =
              Raw_level_repr.of_int32_exn (Int32.succ (Int32.succ level));
            first_cycle = Cycle_repr.succ current_cycle;
            blocks_per_cycle = constants.blocks_per_cycle;
            blocks_per_commitment = constants.blocks_per_commitment;
          }
      in
      Level_repr.create_cycle_eras [second_cycle_era; first_cycle_era]
      >>?= fun cycle_eras -> set_cycle_eras ctxt cycle_eras)
  >>=? fun ctxt ->
  prepare ctxt ~level ~predecessor_timestamp:timestamp ~timestamp ~fitness
  >|=? fun ctxt -> (previous_proto, ctxt)

let activate ctxt h = Updater.activate (context ctxt) h >|= update_context ctxt

(* Generic context ********************************************************)

type key = string list

type value = bytes

type tree = Context.tree

module type T =
  Raw_context_intf.T
    with type root := root
     and type key := key
     and type value := value
     and type tree := tree

let mem ctxt k = Context.mem (context ctxt) k

let mem_tree ctxt k = Context.mem_tree (context ctxt) k

let get ctxt k =
  Context.find (context ctxt) k >|= function
  | None -> storage_error (Missing_key (k, Get))
  | Some v -> ok v

let get_tree ctxt k =
  Context.find_tree (context ctxt) k >|= function
  | None -> storage_error (Missing_key (k, Get))
  | Some v -> ok v

let find ctxt k = Context.find (context ctxt) k

let find_tree ctxt k = Context.find_tree (context ctxt) k

let add ctxt k v = Context.add (context ctxt) k v >|= update_context ctxt

let add_tree ctxt k v =
  Context.add_tree (context ctxt) k v >|= update_context ctxt

let init ctxt k v =
  Context.mem (context ctxt) k >>= function
  | true -> Lwt.return @@ storage_error (Existing_key k)
  | _ ->
      Context.add (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

let init_tree ctxt k v : _ tzresult Lwt.t =
  Context.mem_tree (context ctxt) k >>= function
  | true -> Lwt.return @@ storage_error (Existing_key k)
  | _ ->
      Context.add_tree (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

let update ctxt k v =
  Context.mem (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
  | _ ->
      Context.add (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

let update_tree ctxt k v =
  Context.mem_tree (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
  | _ ->
      Context.add_tree (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

(* Verify that the key is present before deleting *)
let remove_existing ctxt k =
  Context.mem (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
  | _ ->
      Context.remove (context ctxt) k >|= fun context ->
      ok (update_context ctxt context)

(* Verify that the key is present before deleting *)
let remove_existing_tree ctxt k =
  Context.mem_tree (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
  | _ ->
      Context.remove (context ctxt) k >|= fun context ->
      ok (update_context ctxt context)

(* Do not verify before deleting *)
let remove ctxt k = Context.remove (context ctxt) k >|= update_context ctxt

let add_or_remove ctxt k = function
  | None -> remove ctxt k
  | Some v -> add ctxt k v

let add_or_remove_tree ctxt k = function
  | None -> remove ctxt k
  | Some v -> add_tree ctxt k v

let list ctxt ?offset ?length k = Context.list (context ctxt) ?offset ?length k

let fold ?depth ctxt k ~init ~f = Context.fold ?depth (context ctxt) k ~init ~f

module Tree :
  Raw_context_intf.TREE
    with type t := t
     and type key := key
     and type value := value
     and type tree := tree = struct
  include Context.Tree

  let empty ctxt = Context.Tree.empty (context ctxt)

  let get t k =
    find t k >|= function
    | None -> storage_error (Missing_key (k, Get))
    | Some v -> ok v

  let get_tree t k =
    find_tree t k >|= function
    | None -> storage_error (Missing_key (k, Get))
    | Some v -> ok v

  let init t k v =
    mem t k >>= function
    | true -> Lwt.return @@ storage_error (Existing_key k)
    | _ -> add t k v >|= ok

  let init_tree t k v =
    mem_tree t k >>= function
    | true -> Lwt.return @@ storage_error (Existing_key k)
    | _ -> add_tree t k v >|= ok

  let update t k v =
    mem t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
    | _ -> add t k v >|= ok

  let update_tree t k v =
    mem_tree t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
    | _ -> add_tree t k v >|= ok

  (* Verify that the key is present before deleting *)
  let remove_existing t k =
    mem t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
    | _ -> remove t k >|= ok

  (* Verify that the key is present before deleting *)
  let remove_existing_tree t k =
    mem_tree t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
    | _ -> remove t k >|= ok

  let add_or_remove t k = function None -> remove t k | Some v -> add t k v

  let add_or_remove_tree t k = function
    | None -> remove t k
    | Some v -> add_tree t k v
end

let project x = x

let absolute_key _ k = k

let description = Storage_description.create ()

let fold_map_temporary_lazy_storage_ids ctxt f =
  f (temporary_lazy_storage_ids ctxt) |> fun (temporary_lazy_storage_ids, x) ->
  (update_temporary_lazy_storage_ids ctxt temporary_lazy_storage_ids, x)

let map_temporary_lazy_storage_ids_s ctxt f =
  f (temporary_lazy_storage_ids ctxt)
  >|= fun (ctxt, temporary_lazy_storage_ids) ->
  update_temporary_lazy_storage_ids ctxt temporary_lazy_storage_ids

module Cache = struct
  type key = Context.Cache.key

  type value = Context.Cache.value = ..

  let key_of_identifier = Context.Cache.key_of_identifier

  let identifier_of_key = Context.Cache.identifier_of_key

  let pp fmt ctxt = Context.Cache.pp fmt (context ctxt)

  let find c k = Context.Cache.find (context c) k

  let set_cache_layout c layout =
    Context.Cache.set_cache_layout (context c) layout >>= fun ctxt ->
    Lwt.return (update_context c ctxt)

  let update c k v = Context.Cache.update (context c) k v |> update_context c

  let sync c ~cache_nonce =
    Context.Cache.sync (context c) ~cache_nonce >>= fun ctxt ->
    Lwt.return (update_context c ctxt)

  let clear c = Context.Cache.clear (context c) |> update_context c

  let list_keys c ~cache_index =
    Context.Cache.list_keys (context c) ~cache_index

  let key_rank c key = Context.Cache.key_rank (context c) key

  let future_cache_expectation c ~time_in_blocks =
    Context.Cache.future_cache_expectation (context c) ~time_in_blocks
    |> update_context c
end
