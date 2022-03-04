(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module contains various helpers to obtain
 *  instances of {!Requester} *)

(** Wrapper for the types being used, to avoid repetitions. *)
module type PARAMETERS = sig
  type key

  type value
end

(** A disk table, that is in fact entirely in memory. *)
module Disk_memory_table (P : PARAMETERS) = struct
  include Hashtbl.Make (struct
    type t = P.key

    let hash = Hashtbl.hash

    let equal = ( = )
  end)

  type store = P.value t

  type value = P.value

  let known (st : store) (k : P.key) = Lwt.return @@ mem st k

  let read st k =
    match find st k with Some v -> return v | None -> fail_with_exn Not_found

  let read_opt st k = Lwt.return @@ find st k
end

(** A simple memory table backed by [Hashtbl] *)
module Memory_table (P : PARAMETERS) :
  Requester.MEMORY_TABLE with type key = P.key = struct
  module Htbl = Hashtbl.MakeSeeded (struct
    type t = P.key

    let hash = Hashtbl.seeded_hash

    let equal = ( = )
  end)

  type key = Htbl.key

  type 'a t = 'a Htbl.t

  let create ~entry_type:_ ?random s = Htbl.create ?random s

  let find = Htbl.find

  let add = Htbl.add

  let replace = Htbl.replace

  let remove = Htbl.remove

  let length = Htbl.length

  let fold = Htbl.fold
end

(** An instance of [PROBE] that uses a [bool] parameter
 *  to decide whether the check goes through or not *)
module Simple_probe (P : PARAMETERS) :
  Requester.PROBE
    with type key = P.key
     and type param = bool
     and type notified_value = P.value
     and type value = P.value = struct
  type key = P.key

  type param = bool

  type notified_value = P.value

  type value = P.value

  let probe (_ : key) (p : param) (nv : notified_value) =
    if p then Some nv else None
end

(** An instance of [REQUEST] that solely registers incoming requests *)
module Simple_request (P : PARAMETERS) : sig
  include Requester.REQUEST with type key = P.key and type param = unit

  val registered_requests : (param * P2p_peer.Set.elt * key list) list ref

  val clear_registered_requests : unit -> unit
end = struct
  type key = P.key

  type param = unit

  let initial_delay = Time.System.Span.of_seconds_exn 0.01

  let active (_ : param) = P2p_peer.Set.of_list [P2p_peer.Id.zero]

  let registered_requests : (param * P2p_peer.Id.t * key list) list ref = ref []

  let send (requester : param) (id : P2p_peer.Id.t) (kl : key list) =
    registered_requests := (requester, id, kl) :: !registered_requests ;
    ()

  let clear_registered_requests () = registered_requests := []
end

(** A helper to avoid having to use the full-fledged [Requester.Make]
 *  functor. We take the [Requester.REQUEST] module as parameter (instead
 *  of hardcoding the use of [Simple_request]), because
 *  callers that use [Simple_request] likely want to observe
 *  the underlying effects (see the [ref] in [Simple_request]) and hence
 *  want to pass their own instance.
 *
 *  Like {!Requester.Make}, this returns an instance of [FULL_REQUESTER].
 *  Note that, contrary to a production requester, the instance returned
 *  by this functor does not use the disk, it runs entirely in memory. *)
module Make_memory_full_requester
    (H : Requester.HASH)
    (P : PARAMETERS with type key = H.t)
    (R : Requester.REQUEST with type param = unit and type key = H.t) :
  Requester.FULL_REQUESTER
    with type key = H.t
     and type value = P.value
     and type param = bool
     and type request_param = unit
     and type notified_value = P.value
     and type store = Disk_memory_table(P).store =
  Requester.Make (H) (Disk_memory_table (P)) (Memory_table (P)) (R)
    (Simple_probe (P))
