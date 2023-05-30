(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Initializes Python *)
val pyinit : unit -> unit

(** Import [numpy] and returns its Python object.
    It also initialize Python runtime if not yet.
*)
val numpy : unit -> Pytypes.pyobject

(** Import [sklearn.linear_model] and returns its Python object.
    It also initialize Python runtime if not yet.
*)
val linear_model : unit -> Pytypes.pyobject

(** Import [scipy.optimize] and returns its Python object.
    It also initialize Python runtime if not yet.
*)
val scipy_optimize : unit -> Pytypes.pyobject

(** Import [sklearn.metrics] and returns its Python object.
    It also initialize Python runtime if not yet.
*)
val sklearn_metrics : unit -> Pytypes.pyobject

(** Import [statsmodels.api] and returns its Python object.
    It also initialize Python runtime if not yet.
*)
val statsmodels_api : unit -> Pytypes.pyobject

(** Import [statsmodels.stats] and returns its Python object.
    It also initialize Python runtime if not yet.
*)
val statsmodels_stats : unit -> Pytypes.pyobject
