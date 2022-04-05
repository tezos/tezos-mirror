(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(* TODO:https://gitlab.com/tezos/tezos/-/issues/2791 
   This module should be deprecated in favour of a data structure that 
   contains rollup address, rollup node operator address, and rollup 
   origination level.
*)

let unstarted_failure () =
  Format.eprintf "Sc rollup state is not initialised.\n" ;
  Lwt_exit.exit_and_raise 1

let make_ref () =
  let reference = ref None in
  ( (fun x -> reference := Some x),
    fun () -> match !reference with None -> unstarted_failure () | Some a -> a
  )

let (set_sc_rollup_address, get_sc_rollup_address) = make_ref ()

let (set_sc_rollup_node_operator, get_sc_rollup_node_operator) = make_ref ()

let (set_sc_rollup_initial_level, get_sc_rollup_initial_level) = make_ref ()

let get_operator_keys cctxt =
  let open Lwt_result_syntax in
  let pkh = get_sc_rollup_node_operator () in
  let+ (_, pk, sk) = Client_keys.get_key cctxt pkh in
  (pkh, pk, sk)

let init (cctxt : Protocol_client_context.full) sc_rollup_address
    sc_rollup_node_operator =
  let open Lwt_result_syntax in
  set_sc_rollup_address sc_rollup_address ;
  set_sc_rollup_node_operator sc_rollup_node_operator ;
  let+ initial_level =
    Plugin.RPC.Sc_rollup.initial_level
      cctxt
      (cctxt#chain, cctxt#block)
      sc_rollup_address
  in
  set_sc_rollup_initial_level initial_level
