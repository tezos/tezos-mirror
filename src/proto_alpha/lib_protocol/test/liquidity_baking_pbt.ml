(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Testing
    -------
    Component:    pbt for liquidity baking
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/liquidity_baking_pbt.exe [-- --seed SEED]
    Subject:      Test liquidity baking contracts using randomly generated inputs.
*)

let extract_qcheck_result : (unit, error trace) result Lwt.t -> bool =
 fun f ->
  let pp_print_condensed ppf =
    let pp_error ppf e =
      let len_max = 200 in
      let s = Format.asprintf "%a" Error_monad.pp_print_error [e] in
      let len = String.length s in
      let s = String.sub s 0 (min (String.length s) len_max) in
      Format.fprintf ppf "%s%s" s (if len > len_max then " [...]" else "")
    in
    function
    | [] -> assert false
    | [error] -> Format.fprintf ppf "@[<v 2>Error:@ %a@]@." pp_error error
    | error :: _ as errors ->
        Format.fprintf
          ppf
          "@[<v 2>Error:@ %a,@ trace (%d):@ %a@]@."
          pp_error
          error
          (List.length errors)
          (Format.pp_print_list pp_error)
          (List.rev errors)
  in

  match Lwt_main.run f with
  | Ok () -> true
  | Error err ->
      Format.printf "@\n%a@." pp_print_condensed err ;
      false

let tests = []

let _ =
  let open Sys in
  QCheck_base_runner.run_tests_main ~argv tests
