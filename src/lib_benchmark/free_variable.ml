(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type t = Namespace.t

let compare = Namespace.compare

let equal = Namespace.equal

let of_string = Namespace.of_string

let to_string = Namespace.to_string

let of_namespace x = x

let to_namespace x = x

let pp = Namespace.pp

let pp_short = Namespace.pp_short

module Table = Hashtbl.Make (struct
  include Namespace

  let hash = Stdlib.Hashtbl.hash
end)

module Map = Map.Make (Namespace)

module Set = struct
  include Set.Make (Namespace)

  let pp_sep s ppf () = Format.fprintf ppf "%s@;" s

  let pp fmtr set =
    let open Format in
    let elts = elements set in
    fprintf fmtr "{ @[<hv>%a@] }" (pp_print_list ~pp_sep:(pp_sep "; ") pp) elts
end

module Sparse_vec = Sparse_vec.Make (Map)
