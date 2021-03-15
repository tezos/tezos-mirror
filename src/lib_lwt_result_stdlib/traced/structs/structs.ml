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

(** [Make] is a functor that takes a [Trace] as argument and instantiates all
    the Traced modules based on it. *)
module Make (Trace : Traced_sigs.Trace.S) = struct
  module Monad = Monad.Make (Trace)
  module Seq = Seq.Make (Monad)
  module Hashtbl = Hashtbl.Make (Monad) (Seq)
  module List = List.Make (Monad)
  module Map = Map.Make (Monad) (Seq)
  module Option = Bare_structs.Option
  module Result = Bare_structs.Result
  module Set = Set.Make (Monad) (Seq)
  module Seq_e = Seq_e
  module Seq_s = Seq_s.Make (Monad)
  module Seq_es = Seq_es.Make (Monad) (Seq_e) (Seq_s)
  module WithExceptions = Bare_structs.WithExceptions
end
