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

module type S = Bare_functor_outputs.Map.S

module Make (Ord : Stdlib.Map.OrderedType) : S with type key = Ord.t = struct
  open Seq
  include Stdlib.Map.Make (Ord)

  let iter_e f t = E.iter (fun (k, v) -> f k v) (to_seq t)

  let iter_s f t = S.iter (fun (k, v) -> f k v) (to_seq t)

  let iter_es f t = ES.iter (fun (k, v) -> f k v) (to_seq t)

  let iter_ep f t = iter_ep (fun (k, v) -> f k v) (to_seq t)

  let iter_p f t = iter_p (fun (k, v) -> f k v) (to_seq t)

  let fold_e f t init =
    E.fold_left (fun acc (k, v) -> f k v acc) init (to_seq t)

  let fold_s f t init =
    S.fold_left (fun acc (k, v) -> f k v acc) init (to_seq t)

  let fold_es f t init =
    ES.fold_left (fun acc (k, v) -> f k v acc) init (to_seq t)

  let min_binding = min_binding_opt

  let max_binding = max_binding_opt

  let choose = choose_opt

  let find = find_opt

  let find_first = find_first_opt

  let find_last = find_last_opt
end
