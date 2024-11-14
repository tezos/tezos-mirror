(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Rewriter = Rewriter.Rewriter

(** Checks that this ppx should be handled at compile time.

    To use it at compile time, do [TEZOS_PPX_PROFILER=... make] *)
let make_ppx_profiler () =
  match Sys.getenv_opt "TEZOS_PPX_PROFILER" with
  | Some "" | None -> false
  | _ -> true

(** [mapper] inherits from Ast_traverse.map because we want to take a parsetree
    and return a new parsetree that have been rewritten or not by this ppx *)
let mapper =
  if make_ppx_profiler () then
    object
      inherit Ppxlib.Ast_traverse.map as super

      method! expression e =
        let detected_rewriters =
          (* The list of attributes is reverted to make sure that we preprocess
             them from left to right *)
          Rewriter.extract_rewriters (List.rev e.pexp_attributes)
        in
        (* Remove the handled attributes that have been transformed in rewriters *)
        Expression.remove_attributes e
        (* Transform the expression with the help of the list of rewriters *)
        |> Expression.rewrite detected_rewriters
        (* Gives the hand back to Ast_travers.map to keep iterating *)
        |> super#expression
    end
  else
    object
      inherit Ppxlib.Ast_traverse.map
    end

let () =
  Ppxlib.Driver.register_transformation "ppx_profiler" ~impl:mapper#structure
