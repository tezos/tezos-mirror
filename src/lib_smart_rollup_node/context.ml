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

type ('repo, 'tree) pvm_context_impl =
  (module Context_sigs.S with type repo = 'repo and type tree = 'tree)

let equiv (a, b) (c, d) = (Equality_witness.eq a c, Equality_witness.eq b d)

type hash = Smart_rollup_context_hash.t

type 'a t =
  | Context : {
      index : ('a, 'repo) index;
      pvm_context_impl : ('repo, 'tree) pvm_context_impl;
      impl_name : string;
      tree : 'tree;
      equality_witness : ('repo, 'tree) equality_witness;
    }
      -> 'a t

type ro = [`Read] t

type rw = [`Read | `Write] t

let make ~index ~tree ~pvm_context_impl ~equality_witness ~impl_name =
  Context {index; tree; pvm_context_impl; equality_witness; impl_name}

let load :
    type tree repo.
    (repo, tree) pvm_context_impl ->
    cache_size:int ->
    'a Store_sigs.mode ->
    string ->
    'a t tzresult Lwt.t =
 fun (module Pvm_Context_Impl) ~cache_size mode path ->
  let open Lwt_result_syntax in
  let* index = Pvm_Context_Impl.load ~cache_size mode path in
  let equality_witness = Pvm_Context_Impl.equality_witness in
  let impl_name = Pvm_Context_Impl.impl_name in
  return
  @@ make
       ~index
       ~tree:(Pvm_Context_Impl.PVMState.empty ())
       ~pvm_context_impl:(module Pvm_Context_Impl)
       ~equality_witness
       ~impl_name

let index c = c

let close (type a)
    (Context {pvm_context_impl = (module Pvm_Context_Impl); index; _} : a t) :
    unit Lwt.t =
  Pvm_Context_Impl.close index

let readonly (type a)
    (Context ({pvm_context_impl = (module Pvm_Context_Impl); index; _} as o) :
      a t) : ro =
  Context {o with index = Pvm_Context_Impl.readonly index}

let checkout (type a)
    (Context ({pvm_context_impl = (module Pvm_Context_Impl); index; _} as o) :
      a t) hash : a t option Lwt.t =
  let open Lwt_syntax in
  let+ ctx = Pvm_Context_Impl.checkout index hash in
  match ctx with
  | None -> None
  | Some {index; tree} -> Some (Context {o with index; tree})

let empty (type a)
    (Context ({pvm_context_impl = (module Pvm_Context_Impl); index; _} as o) :
      a t) : a t =
  let {index; tree} = Pvm_Context_Impl.empty index in
  Context {o with index; tree}

let commit ?message
    (Context {pvm_context_impl = (module Pvm_Context_Impl); index; tree; _} :
      [> `Write] t) =
  Pvm_Context_Impl.commit ?message {index; tree}

let is_gc_finished
    (Context {pvm_context_impl = (module Pvm_Context_Impl); index; _} :
      [> `Write] t) =
  Pvm_Context_Impl.is_gc_finished index

let split (type a)
    (Context {pvm_context_impl = (module Pvm_Context_Impl); index; _} : a t) =
  Pvm_Context_Impl.split index

let gc
    (Context {pvm_context_impl = (module Pvm_Context_Impl); index; _} :
      [> `Write] t) ?callback hash =
  Pvm_Context_Impl.gc index ?callback hash

let wait_gc_completion
    (Context {pvm_context_impl = (module Pvm_Context_Impl); index; _} :
      [> `Write] t) =
  Pvm_Context_Impl.wait_gc_completion index

type pvmstate =
  | PVMState : {
      pvm_context_impl : ('repo, 'tree) pvm_context_impl;
      impl_name : string;
      pvmstate : 'tree;
      equality_witness : ('repo, 'tree) equality_witness;
    }
      -> pvmstate

let make_pvmstate ~pvm_context_impl ~equality_witness ~impl_name ~pvmstate =
  PVMState {pvm_context_impl; impl_name; pvmstate; equality_witness}

(** State of the PVM that this rollup node deals with *)
module PVMState = struct
  type value = pvmstate

  let empty : type a. a t -> value =
   fun (Context
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
           tree;
           equality_witness;
           impl_name;
           _;
         }) ->
    let open Lwt_syntax in
    let+ pvmstate = Pvm_Context_Impl.PVMState.find {index; tree} in
    match pvmstate with
    | None -> None
    | Some pvmstate ->
        Some
          (make_pvmstate
             ~pvm_context_impl:(module Pvm_Context_Impl)
             ~equality_witness
             ~pvmstate
             ~impl_name)

  let lookup : value -> string list -> bytes option Lwt.t =
   fun (PVMState {pvm_context_impl = (module Pvm_Context_Impl); pvmstate; _})
       path ->
    Pvm_Context_Impl.PVMState.lookup pvmstate path

  let set : type a. a t -> value -> a t Lwt.t =
   fun (Context
         ({pvm_context_impl = (module Pvm_Context_Impl); index; tree; _} as o1))
       (PVMState o2) ->
    let open Lwt_syntax in
    match equiv o1.equality_witness o2.equality_witness with
    | Some Refl, Some Refl ->
        let+ ctxt = Pvm_Context_Impl.PVMState.set {index; tree} o2.pvmstate in
        Context {o1 with index = ctxt.index; tree = ctxt.tree}
    | _ -> err_implementation_mismatch ~expected:o1.impl_name ~got:o2.impl_name
end

module Internal_for_tests = struct
  let get_a_tree : (module Context_sigs.S) -> string -> pvmstate Lwt.t =
   fun (module Pvm_Context_Impl) key ->
    let open Lwt_syntax in
    let+ tree = Pvm_Context_Impl.Internal_for_tests.get_a_tree key in
    make_pvmstate
      ~pvm_context_impl:(module Pvm_Context_Impl)
      ~equality_witness:Pvm_Context_Impl.equality_witness
      ~impl_name:Pvm_Context_Impl.impl_name
      ~pvmstate:tree
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
end
