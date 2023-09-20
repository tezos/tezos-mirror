(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type fa12_script = {
  name : string list;
  build_storage : Account.key -> string;
  mint_entrypoint : string;
  mint_arg : Account.key -> Tez.t -> string;
}

(** {!fa12_script} for [mini_scenarios/fa12_reference]. *)
val fa12_reference : fa12_script

(** {!fa12_script} for [mini_scenarios/lqt_fa12.mligo]. *)
val lqt_fa12 : fa12_script

(** List of {!fa12_scripts} that can be used in generic FA1.2 tests. *)
val fa12_scripts : fa12_script list

(** [originate_fa12 ~src ~admin ~fa12_script client protocol] originates
    FA1.2 contract [fa12_script] from [src] with administrator set to [admin].

    Returns a pair (alias * address) for the originated contract.
*)
val originate_fa12 :
  src:string ->
  admin:Account.key ->
  fa12_script:fa12_script ->
  Client.t ->
  Protocol.t ->
  (string * string) Lwt.t

(** [mint ~admin ~mint ~dest ~fa12_address ~fa12_script client] mints [mint]
    for [dest] using the administrator [admin]. *)
val mint :
  admin:Account.key ->
  mint:Tez.t ->
  dest:Account.key ->
  fa12_address:string ->
  fa12_script:fa12_script ->
  Client.t ->
  unit Lwt.t
