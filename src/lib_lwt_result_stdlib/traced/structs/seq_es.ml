(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Make
    (Monad : Traced_sigs.Monad.S)
    (Seq_e : Traced_sigs.Seq_e.S)
    (Seq_s : Traced_sigs.Seq_s.S with type 'error trace := 'error Monad.trace) :
  Traced_sigs.Seq_es.S
    with type ('a, 'e) node = ('a, 'e) Bare_structs.Seq_es.node
     and type ('a, 'e) t = ('a, 'e) Bare_structs.Seq_es.t
     and type ('a, 'e) seq_e_t := ('a, 'e) Seq_e.t
     and type 'a seq_s_t := 'a Seq_s.t = struct
  include Bare_structs.Seq_es

  let rec of_seqe seq () =
    match seq () with
    | Ok Seq_e.Nil -> Lwt.return_ok Nil
    | Ok (Seq_e.Cons (item, seq)) -> Lwt.return_ok (Cons (item, of_seqe seq))
    | Error _ as e -> Lwt.return e

  let rec of_seqs seq () =
    Lwt.bind (seq ()) @@ function
    | Seq_s.Nil -> Lwt.return_ok Nil
    | Seq_s.Cons (e, seq) -> Lwt.return_ok (Cons (e, of_seqs seq))
end
