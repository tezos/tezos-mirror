(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* This module is directly inspired by the lib_error_monad/core_maker module *)

(** Cache that stores the current kind encoding.

    This cache is erased if a new kind is registered with {!register_case}
    allowing to avoid recomputing this encoding if no new kind has been
    registered.
*)
let kind_encoding_cache = ref None

(** Function used to reset the kind encoding cache *)
let set_kind_encoding_cache_dirty = ref (fun () -> kind_encoding_cache := None)

(** List storing all the registered cases *)
let kind_encoding_cases = ref []

(** [register_case case] resets the {!kind_encoding_cache} and
    registers the [case] *)
let register_case (case : Profiler.view case) =
  !set_kind_encoding_cache_dirty () ;
  kind_encoding_cases := case :: !kind_encoding_cases

(** Function used to compute the [kind encoding] based on {!kind_encoding_cases}
    if {!kind_encoding_cache} is [None]. This is used by {!Data_encoding.delayed}
*)
let kind_encoding () =
  match !kind_encoding_cache with
  | None ->
      let encoding =
        let union_encoding = Data_encoding.union !kind_encoding_cases in
        let open Data_encoding in
        splitted
          ~json:union_encoding
          ~binary:
            (conv
               (Json.construct union_encoding)
               (Json.destruct union_encoding)
               json)
      in
      kind_encoding_cache := Some encoding ;
      encoding
  | Some encoding -> encoding

let kind_encoding = Data_encoding.delayed kind_encoding

let () =
  (* HACK
     See the explanation in lib_error_monad/core_maker *)
  let set_older_caches_dirty = !set_kind_encoding_cache_dirty in
  set_kind_encoding_cache_dirty :=
    fun () ->
      set_older_caches_dirty () ;
      kind_encoding.json_encoding <- None
