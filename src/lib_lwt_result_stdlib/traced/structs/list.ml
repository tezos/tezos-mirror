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

module Make (Monad : Traced_sigs.Monad.S) :
  Traced_sigs.List.S with type 'error trace := 'error Monad.trace = struct
  open Monad
  include Bare_structs.List

  let init_ep ~when_negative_length l f =
    let rec aux acc i =
      if i >= l then all_ep (rev acc)
      else (aux [@ocaml.tailcall]) (Lwt.apply f i :: acc) (i + 1)
    in
    if l < 0 then Monad.fail_trace when_negative_length
    else if l = 0 then nil_es
    else aux [] 0

  let iter_ep f l = join_ep (rev_map (Lwt.apply f) l)

  let lwt_apply2 f x y = try f x y with exn -> Lwt.fail exn

  let iteri_ep f l = join_ep (mapi (lwt_apply2 f) l)

  let rev_map_ep f l = all_ep @@ rev_map (Lwt.apply f) l

  let map_ep f l = rev_map_ep f l >|=? rev

  let rev_mapi_ep f l = all_ep @@ rev_mapi f l

  let mapi_ep f l = rev_mapi_ep f l >|=? rev

  let filter_ep f l =
    rev_map_ep (fun x -> f x >|=? fun b -> if b then Some x else None) l
    >|=? rev_filter_some

  let filter_map_ep f l = rev_map_ep f l >|=? rev_filter_some

  let for_all_ep f l = rev_map_ep f l >|=? for_all Fun.id

  let exists_ep f l = rev_map_ep f l >|=? exists Fun.id

  let partition_ep f l =
    rev_map_ep (fun x -> f x >|=? fun b -> (b, x)) l >|=? fun bxs ->
    fold_left
      (fun (trues, falses) (b, x) ->
        if b then (x :: trues, falses) else (trues, x :: falses))
      ([], [])
      bxs
end
