(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Alpha_context

type error +=
  | (* Permanent *) Sc_rollup_invalid_parameters_type
  | (* Permanent *) Sc_rollup_invalid_atomic_batch

let () =
  let description = "Invalid parameters type for rollup" in
  register_error_kind
    `Permanent
    ~id:"Sc_rollup_invalid_parameters_type"
    ~title:"Invalid parameters type"
    ~description
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" description)
    Data_encoding.unit
    (function Sc_rollup_invalid_parameters_type -> Some () | _ -> None)
    (fun () -> Sc_rollup_invalid_parameters_type) ;
  let description =
    "Smart-contract rollup atomic batch operation is not yet supported"
  in
  register_error_kind
    `Permanent
    ~id:"Sc_rollup_invalid_atomic_batch"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_invalid_atomic_batch -> Some () | _ -> None)
    (fun () -> Sc_rollup_invalid_atomic_batch)

type origination_result = {address : Sc_rollup.Address.t; size : Z.t}

type 'ret continuation = unit -> 'ret tzresult

(* Only a subset of types are supported for rollups.
   This function checks whether or not a type can be used for a rollup. *)
let rec validate_ty :
    type a ac ret.
    (a, ac) Script_typed_ir.ty -> ret continuation -> ret tzresult =
 fun ty k ->
  let open Script_typed_ir in
  match ty with
  (* Valid primitive types. *)
  | Unit_t -> (k [@ocaml.tailcall]) ()
  | Int_t -> (k [@ocaml.tailcall]) ()
  | Nat_t -> (k [@ocaml.tailcall]) ()
  | Signature_t -> (k [@ocaml.tailcall]) ()
  | String_t -> (k [@ocaml.tailcall]) ()
  | Bytes_t -> (k [@ocaml.tailcall]) ()
  | Key_hash_t -> (k [@ocaml.tailcall]) ()
  | Key_t -> (k [@ocaml.tailcall]) ()
  | Timestamp_t -> (k [@ocaml.tailcall]) ()
  | Address_t -> (k [@ocaml.tailcall]) ()
  | Bls12_381_g1_t -> (k [@ocaml.tailcall]) ()
  | Bls12_381_g2_t -> (k [@ocaml.tailcall]) ()
  | Bls12_381_fr_t -> (k [@ocaml.tailcall]) ()
  | Bool_t -> (k [@ocaml.tailcall]) ()
  | Never_t -> (k [@ocaml.tailcall]) ()
  | Tx_rollup_l2_address_t -> (k [@ocaml.tailcall]) ()
  | Chain_id_t -> (k [@ocaml.tailcall]) ()
  (* Valid collection types. *)
  | Ticket_t (ty, _) -> (validate_ty [@ocaml.tailcall]) ty k
  | Set_t (ty, _) -> (validate_ty [@ocaml.tailcall]) ty k
  | Option_t (ty, _, _) -> (validate_ty [@ocaml.tailcall]) ty k
  | List_t (ty, _) -> (validate_ty [@ocaml.tailcall]) ty k
  | Pair_t (ty1, ty2, _, _) -> (validate_two_tys [@ocaml.tailcall]) ty1 ty2 k
  | Union_t (ty1, ty2, _, _) -> (validate_two_tys [@ocaml.tailcall]) ty1 ty2 k
  | Map_t (key_ty, val_ty, _) ->
      (validate_two_tys [@ocaml.tailcall]) key_ty val_ty k
  (* Invalid types. *)
  | Mutez_t -> error Sc_rollup_invalid_parameters_type
  | Big_map_t (_key_ty, _val_ty, _) -> error Sc_rollup_invalid_parameters_type
  | Contract_t _ -> error Sc_rollup_invalid_parameters_type
  | Sapling_transaction_t _ -> error Sc_rollup_invalid_parameters_type
  | Sapling_transaction_deprecated_t _ ->
      error Sc_rollup_invalid_parameters_type
  | Sapling_state_t _ -> error Sc_rollup_invalid_parameters_type
  | Operation_t -> error Sc_rollup_invalid_parameters_type
  | Chest_t -> error Sc_rollup_invalid_parameters_type
  | Chest_key_t -> error Sc_rollup_invalid_parameters_type
  | Lambda_t (_, _, _) -> error Sc_rollup_invalid_parameters_type

and validate_two_tys :
    type a ac b bc ret.
    (a, ac) Script_typed_ir.ty ->
    (b, bc) Script_typed_ir.ty ->
    ret continuation ->
    ret tzresult =
 fun ty1 ty2 k ->
  (validate_ty [@ocaml.tailcall]) ty1 (fun () ->
      (validate_ty [@ocaml.tailcall]) ty2 k)

let validate_parameters_ty ctxt parameters_ty =
  let open Tzresult_syntax in
  (* Parse the type and check that the entrypoints are well-formed. Using
     [parse_parameter_ty_and_entrypoints] restricts to [passable] types
     (everything but operations), which is OK since [validate_ty] constraints
     the type further. *)
  let* Ex_parameter_ty_and_entrypoints {arg_type; entrypoints = _}, ctxt =
    Script_ir_translator.parse_parameter_ty_and_entrypoints
      ctxt
      ~legacy:false
      (Micheline.root parameters_ty)
  in
  (* Check that the type is valid for rollups. *)
  let* ctxt =
    Gas.consume
      ctxt
      (Sc_rollup_costs.is_valid_parameters_ty_cost
         ~ty_size:Script_typed_ir.(ty_size arg_type |> Type_size.to_int))
  in
  let+ () = validate_ty arg_type ok in
  ctxt

let originate ctxt ~kind ~boot_sector ~parameters_ty =
  let open Lwt_tzresult_syntax in
  let*? ctxt =
    let open Tzresult_syntax in
    let* parameters_ty, ctxt =
      Script.force_decode_in_context
        ~consume_deserialization_gas:When_needed
        ctxt
        parameters_ty
    in
    validate_parameters_ty ctxt parameters_ty
  in
  let+ address, size, ctxt =
    Sc_rollup.originate ctxt ~kind ~boot_sector ~parameters_ty
  in
  ({address; size}, ctxt)

let execute_outbox_message _ctx _rollup _last_cemented_commitment
    ~outbox_level:_ ~message_index:_ ~inclusion_proof:_ ~message:_ =
  (* TODO: 3106
     Implement business logic.
     Involves validate inclusion proofs, transferring tickets  and outputting
     operations etc.
  *)
  fail Sc_rollup_invalid_atomic_batch
