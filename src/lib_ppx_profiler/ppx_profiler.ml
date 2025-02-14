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
            (* By choice and to be coherent with how OCamlformat parses two
               attributes attached to the same value. For more details
               (https://gitlab.com/tezos/tezos/-/issues/7691):

               - Previous implementation was creating the following issue:

                 Let's take a simple example:

                 {[
                   let wrap_1 f = Format.printf "wrap_1\n%!"; f ()
                   let wrap_2 f = Format.printf "wrap_2\n%!"; f ()

                   let f x =
                     g x [@profiler.wrap_f wrap_1] [@profiler.wrap_f wrap_2]
                 ]}

                 With the current implementation, it will be compiled as:

                 {[
                   let f x =
                     wrap_1 (fun () ->
                         wrap_2 (fun () -> g x) ())
                       ()
                 ]}

               - With the current implementation

                 In the OCaml syntax [f [@attr1] [@attr2]] is equivalent to [(f [@attr1]) [@attr2]].

                 It is even assessed by ocamlformat that would remove these unneeded parenthesis.

                 As such, the right compilation should be:

                 {[
                   let f x =
                     wrap_2 (fun () ->
                         wrap_1 (fun () -> g x) ())
                       ()
                 ]} *)
            Rewriter.extract_rewriters handled_drivers e.pexp_attributes
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
