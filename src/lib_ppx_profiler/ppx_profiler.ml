(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Checks that this ppx should be handled at compile time.

    To use it at compile time, do [TEZOS_PPX_PROFILER=... make]

    You can provide a dummy value or a list of drivers:
    - dummy value:
      + `[@profiler.<function> ...]` with no `driver_ids` provided
        will be preprocessed
      + `[@profiler.<function> {driver_ids=[list of driver_id]}...]`
        will not be preprocessed
    - `[list of driver ids]`
      + `[@profiler.<function> ...]` with no `driver_ids` provided
        will be preprocessed
      + `[@profiler.<function> {driver_ids=[list of driver_id]}...]`
        will be preprocessed if their `driver_ids` list has a non empty
        intersection with the provided list *)
let make_ppx_profiler () =
  match Sys.getenv_opt "TEZOS_PPX_PROFILER" with
  | Some "" | None -> None
  | Some s -> (
      match Handled_drivers.of_string s with
      | drivers -> Some drivers
      | exception Failure _exn ->
          (* TODO: Need to find a way to inform that some attributes won't be
             preprocessed without spamming the terminal *)
          Some Handled_drivers.empty)

(** [mapper] inherits from Ast_traverse.map because we want to take a parsetree
    and return a new parsetree that have been rewritten or not by this ppx *)
let mapper =
  match make_ppx_profiler () with
  | Some handled_drivers ->
      object
        inherit Ppxlib.Ast_traverse.map as super

        method! expression e =
          let detected_rewriters =
            (* The list of attributes is reverted to make sure that we preprocess
               them from left to right *)
            Rewriter.extract_rewriters
              handled_drivers
              (List.rev e.pexp_attributes)
          in
          (* Remove the handled attributes that have been transformed in rewriters *)
          Expression.remove_attributes e
          (* Transform the expression with the help of the list of rewriters *)
          |> Expression.rewrite detected_rewriters
          (* Gives the hand back to Ast_traverse.map to keep iterating *)
          |> super#expression
      end
  | None ->
      object
        inherit Ppxlib.Ast_traverse.map
      end

let () =
  Ppxlib.Driver.register_transformation "ppx_profiler" ~impl:mapper#structure
