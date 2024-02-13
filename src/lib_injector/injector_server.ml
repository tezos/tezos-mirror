(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Configuration = struct
  type tag = Transaction

  type fee_parameters = Injector_common.fee_parameter

  let tags = [Transaction]

  let string_of_purpose = function Transaction -> "Transaction"

  let default_fee_parameters =
    {
      Injector_common.minimal_fees = {Injector_common.mutez = 100L};
      minimal_nanotez_per_byte = Q.of_int 1000;
      minimal_nanotez_per_gas_unit = Q.of_int 100;
      force_low_fee = false;
      fee_cap = {Injector_common.mutez = 1_000_000L};
      burn_cap = {Injector_common.mutez = 1_000_000L};
    }
end

(* The rest of this  module is adapted from
   [lib_smart_rollup_node/injector.ml] *)

type state = {
  cctxt : Client_context.full;
  fee_parameters : Configuration.fee_parameters;
  minimal_block_delay : int64;
  delay_increment_per_round : int64;
}

open Injector_sigs

module Parameters :
  PARAMETERS
    with type state = state
     and type Tag.t = Configuration.tag
     and type Operation.t = Injector_server_operation.t = struct
  type nonrec state = state

  let events_section = ["injector"; "server"]

  module Tag : TAG with type t = Configuration.tag = struct
    type t = Configuration.tag

    let compare = Stdlib.compare

    let equal = Stdlib.( = )

    let hash = Hashtbl.hash

    let string_of_tag = Configuration.string_of_purpose

    let pp ppf t = Format.pp_print_string ppf (string_of_tag t)

    let encoding : t Data_encoding.t =
      let open Data_encoding in
      match Configuration.tags with
      (* first case can be removed once we have multiple tags *)
      | [tag] -> conv string_of_tag (fun _ -> tag) string
      | tags -> string_enum (List.map (fun t -> (string_of_tag t, t)) tags)
  end

  module Operation = Injector_server_operation

  (* Very coarse approximation for the number of operation we
     expect for each block *)
  let table_estimated_size : Tag.t -> int = function Transaction -> 100

  let operation_tag : Operation.t -> Tag.t = function
    | Transaction _ -> Transaction

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6281
     revise if multiple operation kinds have different fee parameter
     structures *)
  let fee_parameter {fee_parameters; _} _ = fee_parameters

  let safety_guard _ = None

  let persist_operation _ = true

  let retry_unsuccessful_operation _node_ctxt (_op : Operation.t) status =
    let open Lwt_syntax in
    match status with
    | Backtracked | Skipped | Other_branch ->
        (* Always retry backtracked or skipped operations, or operations that
           are on another branch because of a reorg. *)
        return Retry
    | Failed error -> (
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/4071
           Think about which operations should be retried and when. *)
        match classify_trace error with
        | Permanent | Outdated -> return Forget
        | Branch | Temporary -> return Retry)
    | Never_included ->
        (* Forget operations that are never included *)
        return Forget
end

include Injector_functor.Make (Parameters)
