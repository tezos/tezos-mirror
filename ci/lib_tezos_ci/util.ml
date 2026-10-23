(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Shared helpers, at the bottom of the dependency order. *)

let failwith fmt = Format.kasprintf (fun s -> failwith s) fmt

let () = Printexc.register_printer @@ function Failure s -> Some s | _ -> None
