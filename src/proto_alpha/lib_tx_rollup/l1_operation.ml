(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Manager_operation = struct
  type t = packed_manager_operation

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    let open Operation.Encoding.Manager_operations in
    let make (MCase {tag; name; encoding; select; proj; inj}) =
      case
        (Tag tag)
        ~title:name
        (merge_objs (obj1 (req "kind" (constant name))) encoding)
        (fun o ->
          match select o with None -> None | Some o -> Some ((), proj o))
        (fun ((), x) -> Manager (inj x))
    in
    def "manager_operation"
    @@ union
         [
           make reveal_case;
           make transaction_case;
           make origination_case;
           make delegation_case;
           make set_deposits_limit_case;
           make register_global_constant_case;
           make tx_rollup_origination_case;
           make tx_rollup_submit_batch_case;
           make tx_rollup_commit_case;
           make tx_rollup_return_bond_case;
           make tx_rollup_finalize_commitment_case;
           make tx_rollup_remove_commitment_case;
           make tx_rollup_rejection_case;
           make tx_rollup_dispatch_tickets_case;
           make transfer_ticket_case;
           make sc_rollup_originate_case;
           make sc_rollup_add_messages_case;
           make sc_rollup_cement_case;
           make sc_rollup_publish_case;
           make sc_rollup_refute_case;
           make sc_rollup_timeout_case;
         ]

  let get_case :
      type kind.
      kind manager_operation -> kind Operation.Encoding.Manager_operations.case
      =
    let open Operation.Encoding.Manager_operations in
    function
    | Reveal _ -> reveal_case
    | Transaction _ -> transaction_case
    | Origination _ -> origination_case
    | Delegation _ -> delegation_case
    | Register_global_constant _ -> register_global_constant_case
    | Set_deposits_limit _ -> set_deposits_limit_case
    | Tx_rollup_origination -> tx_rollup_origination_case
    | Tx_rollup_submit_batch _ -> tx_rollup_submit_batch_case
    | Tx_rollup_commit _ -> tx_rollup_commit_case
    | Tx_rollup_return_bond _ -> tx_rollup_return_bond_case
    | Tx_rollup_finalize_commitment _ -> tx_rollup_finalize_commitment_case
    | Tx_rollup_remove_commitment _ -> tx_rollup_remove_commitment_case
    | Tx_rollup_rejection _ -> tx_rollup_rejection_case
    | Tx_rollup_dispatch_tickets _ -> tx_rollup_dispatch_tickets_case
    | Transfer_ticket _ -> transfer_ticket_case
    | Sc_rollup_originate _ -> sc_rollup_originate_case
    | Sc_rollup_add_messages _ -> sc_rollup_add_messages_case
    | Sc_rollup_cement _ -> sc_rollup_cement_case
    | Sc_rollup_publish _ -> sc_rollup_publish_case
    | Sc_rollup_refute _ -> sc_rollup_refute_case
    | Sc_rollup_timeout _ -> sc_rollup_timeout_case

  let pp_kind ppf op =
    let open Operation.Encoding.Manager_operations in
    let (MCase {name; _}) = get_case op in
    Format.pp_print_string ppf name

  let pp ppf (Manager op) =
    match op with
    | Tx_rollup_commit {commitment = {level; _}; _} ->
        Format.fprintf
          ppf
          "commitment for rollup level %a"
          Tx_rollup_level.pp
          level
    | Tx_rollup_rejection {level; message_position; _} ->
        Format.fprintf
          ppf
          "rejection for commitment at level %a for message %d"
          Tx_rollup_level.pp
          level
          message_position
    | Tx_rollup_dispatch_tickets {level; tickets_info; _} ->
        let pp_rollup_reveal ppf
            Tx_rollup_reveal.{contents; ty; amount; ticketer; claimer; _} =
          let pp_lazy_expr ppf e =
            Michelson_v1_printer.print_expr_unwrapped
              ppf
              (Result.value
                 (Script_repr.force_decode e)
                 ~default:(Micheline.strip_locations (Micheline.Seq ((), []))))
          in
          Format.fprintf
            ppf
            "%a tickets (%a, %a, %a) to %a"
            Tx_rollup_l2_qty.pp
            amount
            Contract.pp
            ticketer
            pp_lazy_expr
            ty
            pp_lazy_expr
            contents
            Signature.Public_key_hash.pp
            claimer
        in
        Format.fprintf
          ppf
          "@[<v 2>dispatch withdrawals at rollup level %a: %a@]"
          Tx_rollup_level.pp
          level
          (Format.pp_print_list pp_rollup_reveal)
          tickets_info
    | _ -> pp_kind ppf op
end

module Hash =
  Blake2B.Make
    (Base58)
    (struct
      let name = "manager_operation_hash"

      let title = "A manager operation hash"

      let b58check_prefix = "\068\160\013" (* mop(53) *)

      let size = None
    end)

let () = Base58.check_encoded_prefix Hash.b58check_encoding "mop" 53

type hash = Hash.t

type t = {
  hash : hash;
  source : public_key_hash;
  manager_operation : packed_manager_operation;
}

let hash_manager_operation op =
  Hash.hash_bytes
    [Data_encoding.Binary.to_bytes_exn Manager_operation.encoding op]

let make ~source manager_operation =
  let manager_operation = Manager manager_operation in
  let hash = hash_manager_operation manager_operation in
  {hash; source; manager_operation}

let encoding =
  let open Data_encoding in
  conv
    (fun {hash; source; manager_operation} -> (hash, source, manager_operation))
    (fun (hash, source, manager_operation) -> {hash; source; manager_operation})
  @@ obj3
       (req "hash" Hash.encoding)
       (req "source" Signature.Public_key_hash.encoding)
       (req "manager_operation" Manager_operation.encoding)

let pp ppf op =
  Format.fprintf
    ppf
    "%a (%a)"
    Manager_operation.pp
    op.manager_operation
    Hash.pp
    op.hash
