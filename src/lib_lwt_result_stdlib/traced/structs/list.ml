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
    let open Lwt_traced_result_syntax in
    let rec aux acc i =
      if i >= l then all (rev acc)
      else (aux [@ocaml.tailcall]) (Lwt.apply f i :: acc) (i + 1)
    in
    if l < 0 then fail when_negative_length
    else if l = 0 then nil_es
    else aux [] 0

  let iter_ep f l = Lwt_traced_result_syntax.join (rev_map (Lwt.apply f) l)

  let lwt_apply2 f x y = try f x y with exn -> Lwt.fail exn

  let iteri_ep f l = Lwt_traced_result_syntax.join (mapi (lwt_apply2 f) l)

  let rev_map_ep f l = Lwt_traced_result_syntax.all @@ rev_map (Lwt.apply f) l

  let map_ep f l = rev_map_ep f l |> Lwt_result.map rev

  let rev_mapi_ep f l = Lwt_traced_result_syntax.all @@ rev_mapi f l

  let mapi_ep f l = rev_mapi_ep f l |> Lwt_result.map rev

  let filter_ep f l =
    rev_map_ep
      (fun x ->
        let open Lwt_traced_result_syntax in
        let* b = f x in
        if b then return_some x else return_none)
      l
    |> Lwt_result.map rev_filter_some

  let filter_map_ep f l = rev_map_ep f l |> Lwt_result.map rev_filter_some

  let concat_map_ep f xs =
    let open Lwt_traced_result_syntax in
    let+ r = all (map f xs) in
    flatten r

  let for_all_ep f l = rev_map_ep f l |> Lwt_result.map (for_all Fun.id)

  let exists_ep f l = rev_map_ep f l |> Lwt_result.map (exists Fun.id)

  let partition_ep f l =
    let open Lwt_traced_result_syntax in
    let* bxs =
      rev_map_ep
        (fun x ->
          let* b = f x in
          return (b, x))
        l
    in
    return
    @@ fold_left
         (fun (trues, falses) (b, x) ->
           if b then (x :: trues, falses) else (trues, x :: falses))
         ([], [])
         bxs
end
