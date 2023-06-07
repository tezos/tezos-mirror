(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2022 Tarides <contact@tarides.com>                     *)
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

(** See [src/lib_context/tezos_context.ml] for some information. *)

module Make (Encoding : module type of Tezos_context_encoding.Context) = struct
  include Context.Make (Encoding)

  let make_empty_context ?(root = "/tmp") () =
    let open Lwt_syntax in
    let context_promise =
      let+ index = init root in
      empty index
    in
    match Lwt.state context_promise with
    | Lwt.Return result -> result
    | Lwt.Fail exn -> raise exn
    | Lwt.Sleep ->
        (* The in-memory context should never block *)
        assert false

  let make_empty_tree =
    let dummy_context = make_empty_context ~root:"dummy" () in
    fun () -> Tree.empty dummy_context
end

(** Variant of [Tezos_context.Context_binary] purely in-memory. *)
module Context_binary = Make (Tezos_context_encoding.Context_binary)

(** Variant of [Tezos_context.Context] purely in-memory. *)
module Context = Make (Tezos_context_encoding.Context)
