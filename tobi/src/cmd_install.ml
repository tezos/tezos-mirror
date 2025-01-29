(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

let run ~verbose components =
  if components = [] then (
    echo "Nothing to do (no component was specified on the command-line)." ;
    unit)
  else
    (* Compute the set of components to install and decide on an order. *)
    let* plan = Solver.solve components in
    (* Install each component one by one. *)
    list_iter_r plan @@ fun {component; reasons} ->
    if verbose then
      echo
        "Installing: %s.%s (%s)"
        component.name
        (Version.show component.version)
        (Solver.show_reasons reasons)
    else echo "Installing: %s" component.name ;
    match component.version with
    | Dev -> fail "dev versions cannot be installed"
    | Old _version ->
        echo "TODO (next MRs)" ;
        unit
