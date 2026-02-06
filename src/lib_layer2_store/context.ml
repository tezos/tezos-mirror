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

let err_implementation_mismatch ~expected ~got =
  Format.kasprintf
    invalid_arg
    "Context implementation mismatch: expecting %s, got %s"
    expected
    got

open Context_sigs

type ('repo, 'state, 'mut_state) pvm_context_impl =
  (module Context_sigs.S
     with type repo = 'repo
      and type state = 'state
      and type mut_state = 'mut_state)

let equiv (r1, s1, m1) (r2, s2, m2) =
  ( Equality_witness.eq r1 r2,
    Equality_witness.eq s1 s2,
    Equality_witness.eq m1 m2 )

module Hash = Smart_rollup_context_hash

type hash = Hash.t

type ('a, 'repo, 'state, 'mut_state, 'loaded_state) container = {
  index : ('a, 'repo) index;
  pvm_context_impl : ('repo, 'state, 'mut_state) pvm_context_impl;
  impl_name : string;
  state : 'loaded_state;
  equality_witness : ('repo, 'state, 'mut_state) equality_witness;
}

type 'a index =
  | Index : ('a, 'repo, 'state, 'mut_state, unit) container -> 'a index

type 'a t =
  | Context : ('a, 'repo, 'state, 'mut_state, 'mut_state) container -> 'a t

type ro = [`Read] t

type rw = [`Read | `Write] t

type ro_index = [`Read] index

type rw_index = [`Read | `Write] index

let make_index ~index ~pvm_context_impl ~equality_witness ~impl_name =
  Index {index; state = (); pvm_context_impl; equality_witness; impl_name}

let load : type state mut_state repo.
    (repo, state, mut_state) pvm_context_impl ->
    cache_size:int ->
    'a Access_mode.t ->
    string ->
    'a index tzresult Lwt.t =
 fun (module Pvm_Context_Impl) ~cache_size mode path ->
  let open Lwt_result_syntax in
  let+ index = Pvm_Context_Impl.load ~cache_size mode path in
  let equality_witness = Pvm_Context_Impl.equality_witness in
  let impl_name = Pvm_Context_Impl.impl_name in
  make_index
    ~index
    ~pvm_context_impl:(module Pvm_Context_Impl)
    ~impl_name
    ~equality_witness

let index (type a) (Context o : a t) : a index = Index {o with state = ()}

let close (type a)
    (Index {pvm_context_impl = (module Pvm_Context_Impl); index; _} : a index) :
    unit Lwt.t =
  Pvm_Context_Impl.close index

let readonly (type a)
    (Index ({pvm_context_impl = (module Pvm_Context_Impl); index; _} as o) :
      a index) : ro_index =
  Index {o with index = Pvm_Context_Impl.readonly index}

let checkout (type a)
    (Index ({pvm_context_impl = (module Pvm_Context_Impl); index; _} as o) :
      a index) hash : a t option Lwt.t =
  let open Lwt_syntax in
  let+ ctx =
    Pvm_Context_Impl.checkout index (Pvm_Context_Impl.hash_of_context_hash hash)
  in
  match ctx with
  | None -> None
  | Some {index; state} -> Some (Context {o with index; state})

let empty (type a)
    (Index ({pvm_context_impl = (module Pvm_Context_Impl); index; _} as o) :
      a index) : a t =
  let {Context_sigs.index; state} = Pvm_Context_Impl.empty index in
  Context {o with index; state}

let commit ?message
    (Context {pvm_context_impl = (module Pvm_Context_Impl); index; state; _} :
      [> `Write] t) =
  let open Lwt_syntax in
  let+ hash = Pvm_Context_Impl.commit ?message {index; state} in
  Pvm_Context_Impl.context_hash_of_hash hash

let is_gc_finished
    (Index {pvm_context_impl = (module Pvm_Context_Impl); index; _} :
      [> `Write] index) =
  Pvm_Context_Impl.is_gc_finished index

let cancel_gc
    (Index {pvm_context_impl = (module Pvm_Context_Impl); index; _} :
      [> `Write] index) =
  Pvm_Context_Impl.cancel_gc index

let split (type a)
    (Index {pvm_context_impl = (module Pvm_Context_Impl); index; _} : a index) =
  Pvm_Context_Impl.split index

let gc
    (Index {pvm_context_impl = (module Pvm_Context_Impl); index; _} :
      [> `Write] index) ?callback hash =
  Pvm_Context_Impl.gc
    index
    ?callback
    (Pvm_Context_Impl.hash_of_context_hash hash)

let wait_gc_completion
    (Index {pvm_context_impl = (module Pvm_Context_Impl); index; _} :
      [> `Write] index) =
  Pvm_Context_Impl.wait_gc_completion index

let export_snapshot (type a)
    (Index {pvm_context_impl = (module Pvm_Context_Impl); index; _} : a index)
    hash =
  Pvm_Context_Impl.export_snapshot
    index
    (Pvm_Context_Impl.hash_of_context_hash hash)

type pvmstate =
  | PVMState : {
      pvm_context_impl : ('repo, 'state, 'mut_state) pvm_context_impl;
      impl_name : string;
      pvmstate : 'mut_state;
      equality_witness : ('repo, 'state, 'mut_state) equality_witness;
    }
      -> pvmstate

type imm_pvmstate =
  | Imm_PVMState : {
      pvm_context_impl : ('repo, 'state, 'mut_state) pvm_context_impl;
      impl_name : string;
      pvmstate : 'state;
      equality_witness : ('repo, 'state, 'mut_state) equality_witness;
    }
      -> imm_pvmstate

let make_pvmstate ~pvm_context_impl ~equality_witness ~impl_name ~pvmstate =
  PVMState {pvm_context_impl; impl_name; pvmstate; equality_witness}

(** State of the PVM that this rollup node deals with *)
module PVMState = struct
  type value = pvmstate

  type immutable_value = imm_pvmstate

  let empty : type a. a index -> value =
   fun (Index
          {
            pvm_context_impl = (module Pvm_Context_Impl);
            equality_witness;
            impl_name;
            _;
          }) ->
    make_pvmstate
      ~pvm_context_impl:(module Pvm_Context_Impl)
      ~equality_witness
      ~pvmstate:(Pvm_Context_Impl.PVMState.empty ())
      ~impl_name

  let find : type a. a t -> value option Lwt.t =
   fun (Context
          {
            pvm_context_impl = (module Pvm_Context_Impl);
            index;
            state;
            equality_witness;
            impl_name;
            _;
          }) ->
    let open Lwt_syntax in
    let+ pvmstate = Pvm_Context_Impl.PVMState.find {index; state} in
    match pvmstate with
    | None -> None
    | Some pvmstate ->
        Some
          (make_pvmstate
             ~pvm_context_impl:(module Pvm_Context_Impl)
             ~equality_witness
             ~pvmstate
             ~impl_name)

  let get ctxt =
    let open Lwt_result_syntax in
    let*! pvm_state = find ctxt in
    match pvm_state with
    | None ->
        failwith
          "Could not retrieve PVM state from context, this shouldn't happen."
    | Some pvm_state -> return pvm_state

  let lookup : value -> string list -> bytes option Lwt.t =
   fun (PVMState {pvm_context_impl = (module Pvm_Context_Impl); pvmstate; _})
       path ->
    Pvm_Context_Impl.PVMState.lookup pvmstate path

  let set : type a. a t -> value -> unit Lwt.t =
   fun (Context
          ({pvm_context_impl = (module Pvm_Context_Impl); index; state; _} as o1))
       (PVMState o2) ->
    match equiv o1.equality_witness o2.equality_witness with
    | Some Refl, _, Some Refl ->
        Pvm_Context_Impl.PVMState.set {index; state} o2.pvmstate
    | _ -> err_implementation_mismatch ~expected:o1.impl_name ~got:o2.impl_name

  let copy : value -> value =
   fun (PVMState
          ({pvm_context_impl = (module Pvm_Context_Impl); pvmstate; _} as o)) ->
    PVMState
      {
        o with
        pvmstate =
          pvmstate |> Pvm_Context_Impl.to_imm |> Pvm_Context_Impl.from_imm;
      }

  let imm_copy : value -> immutable_value =
   fun (PVMState
          {
            pvm_context_impl = (module Pvm_Context_Impl);
            pvmstate;
            impl_name;
            equality_witness;
          }) ->
    Imm_PVMState
      {
        pvm_context_impl = (module Pvm_Context_Impl);
        pvmstate = Pvm_Context_Impl.to_imm pvmstate;
        impl_name;
        equality_witness;
      }

  let mut_copy : immutable_value -> value =
   fun (Imm_PVMState
          {
            pvm_context_impl = (module Pvm_Context_Impl);
            pvmstate;
            impl_name;
            equality_witness;
          }) ->
    PVMState
      {
        pvm_context_impl = (module Pvm_Context_Impl);
        pvmstate = Pvm_Context_Impl.from_imm pvmstate;
        impl_name;
        equality_witness;
      }
    |> copy
end

module Internal_for_tests = struct
  let get_a_tree : (module Context_sigs.S) -> string -> pvmstate Lwt.t =
   fun (module Pvm_Context_Impl) key ->
    let open Lwt_syntax in
    let+ state = Pvm_Context_Impl.Internal_for_tests.get_a_tree key in
    make_pvmstate
      ~pvm_context_impl:(module Pvm_Context_Impl)
      ~equality_witness:Pvm_Context_Impl.equality_witness
      ~impl_name:Pvm_Context_Impl.impl_name
      ~pvmstate:(Pvm_Context_Impl.from_imm state)
end

module Version = struct
  type t = V0

  let version = V0

  let encoding =
    let open Data_encoding in
    conv_with_guard
      (fun V0 -> 0)
      (function
        | 0 -> Ok V0
        | v -> Error ("Unsupported context version " ^ string_of_int v))
      int31

  let check = function V0 -> Result.return_unit

  let to_string = function V0 -> "0"
end

module Wrapper = struct
  module type S = sig
    type repo

    type state

    type mut_state

    val of_node_context : 'a index -> ('a, repo) Context_sigs.index

    val to_node_context : ('a, repo) Context_sigs.index -> 'a index

    val of_node_pvmstate : pvmstate -> mut_state

    val to_node_pvmstate : mut_state -> pvmstate

    val from_imm : state -> mut_state

    val to_imm : mut_state -> state
  end

  (* Context *)
  let of_node_context : type repo state mut_state.
      (repo, state, mut_state) equality_witness ->
      'a index ->
      ('a, repo) Context_sigs.index =
   fun eqw (Index {equality_witness; index; _}) ->
    match equiv equality_witness eqw with
    | Some Refl, _, _ -> index
    | _ ->
        (* This could happen if the context backend was to change for a
         given pvm/rollup. For now we only use Irmin, if this changes,
         this will demand to provide migration functions from prior
         pmv_context to the next one. *)
        assert false

  let to_node_context : type repo state mut_state.
      (module Context_sigs.S
         with type repo = repo
          and type state = state
          and type mut_state = mut_state) ->
      ('a, repo) Context_sigs.index ->
      'a index =
   fun (module C) index ->
    make_index
      ~index
      ~pvm_context_impl:(module C)
      ~equality_witness:C.equality_witness
      ~impl_name:C.impl_name

  (* PVMState *)
  let of_node_pvmstate : type repo state mut_state.
      (repo, state, mut_state) equality_witness -> pvmstate -> mut_state =
   fun eqw (PVMState {equality_witness; pvmstate; _}) ->
    match equiv equality_witness eqw with
    | _, _, Some Refl -> pvmstate
    | _ -> assert false

  let to_node_pvmstate : type mut_state.
      (module Context_sigs.S with type mut_state = mut_state) ->
      mut_state ->
      pvmstate =
   fun (module C) pvmstate ->
    make_pvmstate
      ~pvmstate
      ~pvm_context_impl:(module C)
      ~equality_witness:C.equality_witness
      ~impl_name:C.impl_name

  module Make (C : Context_sigs.S) :
    S
      with type repo = C.repo
       and type state = C.state
       and type mut_state = C.mut_state = struct
    type repo = C.repo

    type state = C.state

    type mut_state = C.mut_state

    let of_node_context : 'a index -> ('a, repo) Context_sigs.index =
     fun ctxt -> of_node_context C.equality_witness ctxt

    let to_node_context : ('a, repo) Context_sigs.index -> 'a index =
     fun ctxt -> to_node_context (module C) ctxt

    let of_node_pvmstate : pvmstate -> mut_state =
     fun c -> of_node_pvmstate C.equality_witness c

    let to_node_pvmstate : mut_state -> pvmstate = to_node_pvmstate (module C)

    let from_imm : state -> mut_state = C.from_imm

    let to_imm : mut_state -> state = C.to_imm
  end
end
