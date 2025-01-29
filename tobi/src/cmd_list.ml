(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

let run ~verbose =
  let* config = Config.load () in
  list_iter_r (Config.components config) @@ fun component ->
  (* Output component name. *)
  echo "%s" component.name ;
  if not verbose then unit
  else
    (* Output component paths. *)
    let path, other_paths = component.paths in
    if other_paths = [] then echo "- path: %s" path
    else (
      echo "- paths:" ;
      List.iter (echo "  - %s") (path :: other_paths)) ;
    (* Output opam file path. *)
    echo "- opam: %s" component.opam ;
    unit
