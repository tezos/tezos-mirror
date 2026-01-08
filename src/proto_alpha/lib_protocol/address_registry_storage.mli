(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module manages a registry mapping addresses to unique indexes.
    It supports initialization, querying, and conditional addition of addresses
    with idempotent semantics.

    Invariants:
    - The index 0 is assigned to `{!Contract_repr.zero}.
    - Each address is mapped to a unique index in the registry.
    - The same address will always be assigned the same index.
    - The registry supports idempotent additions: adding an existing address is
      a no-op with [existed = true].
*)

(** [init ctxt] initializes the address registry by setting the starting index
    [Z.zero] to {!Contract_repr.zero}.

    @param ctxt The context in which the registry is initialized.
    @return A context with the registry state initialized.
*)
val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** [find ctxt address] retrieves the index associated with a given address, if any.

    @param ctxt The context in which the registry is accessed.
    @param address The address to look up.
    @return An option containing the index if the address is registered.
*)
val find :
  Raw_context.t ->
  Destination_repr.t ->
  (Raw_context.t * Z.t option) tzresult Lwt.t

(** Type representing the result of an [add] operation.
    - [ctxt]: Updated context after the operation.
    - [index]: The index associated with the address.
    - [existed]: [true] if the address was already registered; [false] otherwise.
*)
type add_result = {ctxt : Raw_context.t; index : Z.t; existed : bool}

(** [add_if_missing ctxt address] adds a new address to the registry if it
    doesn't exist. If the address is already present, it returns the existing
    index.

    @param ctxt The current context.
    @param address The address to register.
    @return A record with:
      - the updated context
      - the index associated with the address
      - a flag indicating whether the address was already present
*)
val add_if_missing :
  Raw_context.t -> Destination_repr.t -> add_result tzresult Lwt.t

module Internal_for_tests : sig
  val set_counter : Raw_context.t -> Z.t -> Raw_context.t tzresult Lwt.t

  val get_counter : Raw_context.t -> Z.t tzresult Lwt.t
end
