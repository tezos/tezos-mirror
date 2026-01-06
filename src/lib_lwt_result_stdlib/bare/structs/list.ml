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

(* A note about the implementation of recursive Lwt and Lwt-result functions:

   [_s] and [_es] functions are implemented following this pattern:

   {[
   let rec traverse f xs =
     | [] -> ..
     | x :: xs -> .. f x .. traverse xs ..

   let traverse f xs =
     | [] -> ..
     | x :: xs -> .. Lwt.apply f x .. traverse xs ..
   ]}

   with variations for when [f] takes more than one parameter, when the
   matching is slightly different, and so on. Whatever the variation, the
   pattern remains: one recursive function immediately shadowed by a
   non-recursive function which only handles the head of the list.

   This is necessary because the application of [f] to the head of the list [x]
   is not on the right-hand side of an Lwt bind. As such, the call [f x] is not
   wrapped in a [try]-[with] to convert exceptions into promise rejection.

   We add the shadowing function which uses [Lwt.apply] in order to wrap this
   specific (head) call. As a result, the behaviour of [traverse f xs] is the
   same whether [f] raises an exception during the head call or during a
   subsequent call. *)

open Monad
include Stdlib.List

let nil = []

let nil_e = Result_syntax.return_nil

let nil_s = Lwt_syntax.return_nil

let nil_es = Lwt_result_syntax.return_nil

let is_empty = function [] -> true | _ :: _ -> false

let hd = function x :: _ -> Some x | [] -> None

let tl = function _ :: xs -> Some xs | [] -> None

let nth xs n =
  if n < 0 then None
  else
    let rec aux xs n =
      match (xs, n) with
      | [], _ -> None
      | x :: _, 0 -> Some x
      | _ :: xs, n -> (aux [@ocaml.tailcall]) xs (n - 1)
    in
    aux xs n

let rec last hd = function
  | [] -> hd
  | [last] -> last
  | hd :: (_ :: _ as tl) -> (last [@ocaml.tailcall]) hd tl

let last_opt = function [] -> None | hd :: tl -> Some (last hd tl)

let find = find_opt

let rec iter2 ~when_different_lengths f xs ys =
  (* NOTE: We could do the following but we would need to assume [f] does not
       raise [Invalid_argument]
       [try
          Ok (iter2 f xs ys)
        with Invalid_argument _ ->
          Error when_different_lengths]
      The same remark applies to the other 2-list iterators.
  *)
  match (xs, ys) with
  | [], [] -> Result_syntax.return_unit
  | [], _ :: _ | _ :: _, [] -> Error when_different_lengths
  | x :: xs, y :: ys ->
      f x y ;
      (iter2 [@ocaml.tailcall]) ~when_different_lengths f xs ys

let rev_map2 ~when_different_lengths f xs ys =
  let rec aux zs xs ys =
    match (xs, ys) with
    | [], [] -> Ok zs
    | [], _ :: _ | _ :: _, [] -> Error when_different_lengths
    | x :: xs, y :: ys ->
        let z = f x y in
        (aux [@ocaml.tailcall]) (z :: zs) xs ys
  in
  aux [] xs ys

let map2 ~when_different_lengths f xs ys =
  rev_map2 ~when_different_lengths f xs ys |> Result.map rev

let fold_left2 ~when_different_lengths f a xs ys =
  let rec aux acc xs ys =
    match (xs, ys) with
    | [], [] -> Ok acc
    | [], _ :: _ | _ :: _, [] -> Error when_different_lengths
    | x :: xs, y :: ys ->
        let acc = f acc x y in
        (aux [@ocaml.tailcall]) acc xs ys
  in
  aux a xs ys

let fold_right2 ~when_different_lengths f xs ys a =
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> Ok a
    | [], _ :: _ | _ :: _, [] -> Error when_different_lengths
    | x :: xs, y :: ys ->
        let open Result_syntax in
        let* acc = aux xs ys in
        return (f x y acc)
  in
  aux xs ys

let for_all2 ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> Ok true
    | [], _ :: _ | _ :: _, [] -> Error when_different_lengths
    | x :: xs, y :: ys -> (
        match f x y with
        | true -> (aux [@ocaml.tailcall]) xs ys
        | false -> Ok false)
  in
  aux xs ys

let exists2 ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> Ok false
    | [], _ :: _ | _ :: _, [] -> Error when_different_lengths
    | x :: xs, y :: ys -> (
        match f x y with
        | true -> Ok true
        | false -> (aux [@ocaml.tailcall]) xs ys)
  in
  aux xs ys

let fold_left_map f accu l =
  let rec aux accu rev_list_accu = function
    | [] -> (accu, rev rev_list_accu)
    | x :: xs ->
        let accu, y = f accu x in
        (aux [@ocaml.tailcall]) accu (y :: rev_list_accu) xs
  in
  aux accu [] l

let fold_left_map_e f accu l =
  let rec aux accu rev_list_accu = function
    | [] -> Ok (accu, rev rev_list_accu)
    | x :: xs ->
        let open Result_syntax in
        let* accu, y = f accu x in
        (aux [@ocaml.tailcall]) accu (y :: rev_list_accu) xs
  in
  aux accu [] l

let fold_left_map_s f accu l =
  let open Lwt_syntax in
  let rec aux accu rev_list_accu = function
    | [] -> return (accu, rev rev_list_accu)
    | x :: xs ->
        let* accu, y = f accu x in
        (aux [@ocaml.tailcall]) accu (y :: rev_list_accu) xs
  in
  match l with
  | [] -> return (accu, [])
  | x :: xs ->
      let* accu, y = lwt_apply2 f accu x in
      (aux [@ocaml.tailcall]) accu [y] xs

let fold_left_map_es f accu l =
  let open Lwt_result_syntax in
  let rec aux accu rev_list_accu = function
    | [] -> return (accu, rev rev_list_accu)
    | x :: xs ->
        let* accu, y = f accu x in
        (aux [@ocaml.tailcall]) accu (y :: rev_list_accu) xs
  in
  match l with
  | [] -> return (accu, [])
  | x :: xs ->
      let* accu, y = lwt_apply2 f accu x in
      (aux [@ocaml.tailcall]) accu [y] xs

let rec mem ~equal x = function
  | [] -> false
  | y :: ys -> equal x y || mem ~equal x ys

let rec assoc ~equal k = function
  | [] -> None
  | (kk, v) :: kvs -> if equal k kk then Some v else assoc ~equal k kvs

let assoc_opt = assoc

let assq = assq_opt

let rec mem_assoc ~equal k = function
  | [] -> false
  | (kk, _) :: kvs -> equal k kk || mem_assoc ~equal k kvs

let rec remove_assoc ~equal k = function
  | [] -> []
  | ((kk, _) as kv) :: kvs ->
      if equal k kk then kvs else kv :: remove_assoc ~equal k kvs

let init ~when_negative_length l f =
  if l < 0 then Error when_negative_length
  else if l = 0 then nil_e
  else Ok (Stdlib.List.init l f)

let init_e ~when_negative_length l f =
  let open Result_syntax in
  let rec aux acc i =
    if i >= l then return (rev acc)
    else
      let* v = f i in
      (aux [@ocaml.tailcall]) (v :: acc) (i + 1)
  in
  if l < 0 then Error when_negative_length
  else if l = 0 then return_nil
  else aux [] 0

let init_s ~when_negative_length l f =
  let open Lwt_syntax in
  let rec aux acc i =
    if i >= l then return (rev acc)
    else
      let* v = f i in
      (aux [@ocaml.tailcall]) (v :: acc) (i + 1)
  in
  if l < 0 then return_error when_negative_length
  else if l = 0 then nil_es
  else
    let* v = Lwt.apply f 0 in
    let* r = aux [v] 1 in
    return_ok r

let init_es ~when_negative_length l f =
  let open Lwt_result_syntax in
  let rec aux acc i =
    if i >= l then return (rev acc)
    else
      let* v = f i in
      (aux [@ocaml.tailcall]) (v :: acc) (i + 1)
  in
  if l < 0 then fail when_negative_length
  else if l = 0 then return_nil
  else
    let* v = Lwt.apply f 0 in
    aux [v] 1

let init_ep ~when_negative_length l f =
  let rec aux acc i =
    if i >= l then Lwt_result_syntax.all (rev acc)
    else (aux [@ocaml.tailcall]) (Lwt.apply f i :: acc) (i + 1)
  in
  let open Lwt_result_syntax in
  if l < 0 then fail [when_negative_length]
  else if l = 0 then return_nil
  else aux [] 0

let init_p ~when_negative_length l f =
  let open Lwt_syntax in
  let rec aux acc i =
    if i >= l then
      let* xs = all (rev acc) in
      return xs
    else (aux [@ocaml.tailcall]) (Lwt.apply f i :: acc) (i + 1)
  in
  if l < 0 then return_error when_negative_length
  else if l = 0 then nil_es
  else
    let* r = aux [] 0 in
    return_ok r

let rec find_e f =
  let open Result_syntax in
  function
  | [] -> return_none
  | x :: xs ->
      let* found = f x in
      if found then return_some x else (find_e [@ocaml.tailcall]) f xs

let rec find_s f =
  let open Lwt_syntax in
  function
  | [] -> return_none
  | x :: xs ->
      let* found = f x in
      if found then return_some x else (find_s [@ocaml.tailcall]) f xs

let find_s f =
  let open Lwt_syntax in
  function
  | [] -> return_none
  | x :: xs ->
      let* found = Lwt.apply f x in
      if found then return_some x else (find_s [@ocaml.tailcall]) f xs

let rec find_es f =
  let open Lwt_result_syntax in
  function
  | [] -> return_none
  | x :: xs ->
      let* found = f x in
      if found then return_some x else (find_es [@ocaml.tailcall]) f xs

let find_es f =
  let open Lwt_result_syntax in
  function
  | [] -> return_none
  | x :: xs ->
      let* found = Lwt.apply f x in
      if found then return_some x else (find_es [@ocaml.tailcall]) f xs

let find_index_e f xs =
  let open Result_syntax in
  let rec aux i = function
    | [] -> return_none
    | x :: xs ->
        let* found = f x in
        if found then return_some i else (aux [@ocaml.tailcall]) (i + 1) xs
  in
  aux 0 xs

let find_index_s f xs =
  let open Lwt_syntax in
  let rec aux i = function
    | [] -> return_none
    | x :: xs ->
        let* found = f x in
        if found then return_some i else (aux [@ocaml.tailcall]) (i + 1) xs
  in
  match xs with
  | [] -> return_none
  | x :: rest ->
      let* found = Lwt.apply f x in
      if found then return_some 0 else aux 1 rest

let find_index_es f xs =
  let open Lwt_result_syntax in
  let rec aux i = function
    | [] -> return_none
    | x :: xs ->
        let* found = f x in
        if found then return_some i else (aux [@ocaml.tailcall]) (i + 1) xs
  in
  match xs with
  | [] -> return_none
  | x :: rest ->
      let* found = Lwt.apply f x in
      if found then return_some 0 else aux 1 rest

let rec find_map_e f =
  let open Result_syntax in
  function
  | [] -> return_none
  | x :: xs -> (
      let* found = f x in
      match found with
      | Some _ -> return found
      | None -> (find_map_e [@ocaml.tailcall]) f xs)

let rec find_map_s f =
  let open Lwt_syntax in
  function
  | [] -> return_none
  | x :: xs -> (
      let* found = f x in
      match found with
      | Some _ -> return found
      | None -> (find_map_s [@ocaml.tailcall]) f xs)

let find_map_s f =
  let open Lwt_syntax in
  function
  | [] -> return_none
  | x :: xs -> (
      let* found = Lwt.apply f x in
      match found with
      | Some _ -> return found
      | None -> (find_map_s [@ocaml.tailcall]) f xs)

let rec find_map_es f =
  let open Lwt_result_syntax in
  function
  | [] -> return_none
  | x :: xs -> (
      let* found = f x in
      match found with
      | Some _ -> return found
      | None -> (find_map_es [@ocaml.tailcall]) f xs)

let find_map_es f =
  let open Lwt_result_syntax in
  function
  | [] -> return_none
  | x :: xs -> (
      let* found = Lwt.apply f x in
      match found with
      | Some _ -> return found
      | None -> (find_map_es [@ocaml.tailcall]) f xs)

let find_mapi_e f xs =
  let open Result_syntax in
  let rec aux i = function
    | [] -> return_none
    | x :: xs -> (
        let* found = f i x in
        match found with
        | Some _ -> return found
        | None -> (aux [@ocaml.tailcall]) (i + 1) xs)
  in
  aux 0 xs

let find_mapi_s f xs =
  let open Lwt_syntax in
  let rec aux i = function
    | [] -> return_none
    | x :: xs -> (
        let* found = f i x in
        match found with
        | Some _ -> return found
        | None -> (aux [@ocaml.tailcall]) (i + 1) xs)
  in
  match xs with
  | [] -> return_none
  | x :: rest -> (
      let* found = lwt_apply2 f 0 x in
      match found with Some _ -> return found | None -> aux 1 rest)

let find_mapi_es f xs =
  let open Lwt_result_syntax in
  let rec aux i = function
    | [] -> return_none
    | x :: xs -> (
        let* found = f i x in
        match found with
        | Some _ -> return found
        | None -> (aux [@ocaml.tailcall]) (i + 1) xs)
  in
  match xs with
  | [] -> return_none
  | x :: rest -> (
      let* found = lwt_apply2 f 0 x in
      match found with Some _ -> return found | None -> aux 1 rest)

let rev_filter f xs =
  fold_left (fun rev_xs x -> if f x then x :: rev_xs else rev_xs) [] xs

let rev_filter_e f xs =
  let open Result_syntax in
  let rec aux acc = function
    | [] -> return acc
    | x :: xs ->
        let* b = f x in
        if b then (aux [@ocaml.tailcall]) (x :: acc) xs
        else (aux [@ocaml.tailcall]) acc xs
  in
  aux [] xs

let filter_e f xs = rev_filter_e f xs |> Result.map rev

let rev_filter_s f xs =
  let open Lwt_syntax in
  let rec aux acc = function
    | [] -> return acc
    | x :: xs ->
        let* b = f x in
        if b then (aux [@ocaml.tailcall]) (x :: acc) xs
        else (aux [@ocaml.tailcall]) acc xs
  in
  match xs with
  | [] -> return_nil
  | x :: xs ->
      let* b = Lwt.apply f x in
      if b then (aux [@ocaml.tailcall]) [x] xs
      else (aux [@ocaml.tailcall]) [] xs

let filter_s f xs = rev_filter_s f xs |> Lwt.map rev

let rev_filter_es f xs =
  let open Lwt_result_syntax in
  let rec aux acc = function
    | [] -> return acc
    | x :: xs ->
        let* b = f x in
        if b then (aux [@ocaml.tailcall]) (x :: acc) xs
        else (aux [@ocaml.tailcall]) acc xs
  in
  match xs with
  | [] -> return_nil
  | x :: xs ->
      let* b = Lwt.apply f x in
      if b then (aux [@ocaml.tailcall]) [x] xs
      else (aux [@ocaml.tailcall]) [] xs

let filter_es f xs = rev_filter_es f xs |> Lwt_result.map rev

let rev_filteri f xs =
  let rec aux acc i = function
    | [] -> acc
    | x :: xs ->
        let b = f i x in
        if b then (aux [@ocaml.tailcall]) (x :: acc) (i + 1) xs
        else (aux [@ocaml.tailcall]) acc (i + 1) xs
  in
  aux [] 0 xs

let rev_filteri_e f xs =
  let open Result_syntax in
  let rec aux acc i = function
    | [] -> return acc
    | x :: xs ->
        let* b = f i x in
        if b then (aux [@ocaml.tailcall]) (x :: acc) (i + 1) xs
        else (aux [@ocaml.tailcall]) acc (i + 1) xs
  in
  aux [] 0 xs

let filteri_e f xs = rev_filteri_e f xs |> Result.map rev

let rev_filteri_s f xs =
  let open Lwt_syntax in
  let rec aux acc i = function
    | [] -> return acc
    | x :: xs ->
        let* b = f i x in
        if b then (aux [@ocaml.tailcall]) (x :: acc) (i + 1) xs
        else (aux [@ocaml.tailcall]) acc (i + 1) xs
  in
  match xs with
  | [] -> return_nil
  | x :: xs ->
      let* b = Lwt.apply (fun x -> f 0 x) x in
      if b then (aux [@ocaml.tailcall]) [x] 1 xs
      else (aux [@ocaml.tailcall]) [] 1 xs

let filteri_s f xs = rev_filteri_s f xs |> Lwt.map rev

let rev_filteri_es f xs =
  let open Lwt_result_syntax in
  let rec aux acc i = function
    | [] -> return acc
    | x :: xs ->
        let* b = f i x in
        if b then (aux [@ocaml.tailcall]) (x :: acc) (i + 1) xs
        else (aux [@ocaml.tailcall]) acc (i + 1) xs
  in
  match xs with
  | [] -> return_nil
  | x :: xs ->
      let* b = Lwt.apply (fun x -> f 0 x) x in
      if b then (aux [@ocaml.tailcall]) [x] 1 xs
      else (aux [@ocaml.tailcall]) [] 1 xs

let filteri_es f xs = rev_filteri_es f xs |> Lwt_result.map rev

let rev_filter_some oxs =
  let rec aux xs = function
    | [] -> xs
    | Some x :: oxs -> (aux [@ocaml.tailcall]) (x :: xs) oxs
    | None :: oxs -> (aux [@ocaml.tailcall]) xs oxs
  in
  aux [] oxs

let filter_some oxs = rev_filter_some oxs |> rev

let rev_filter_ok rxs =
  let rec aux xs = function
    | [] -> xs
    | Ok x :: rxs -> (aux [@ocaml.tailcall]) (x :: xs) rxs
    | Error _ :: rxs -> (aux [@ocaml.tailcall]) xs rxs
  in
  aux [] rxs

let filter_ok rxs = rev_filter_ok rxs |> rev

let rev_filter_error rxs =
  let rec aux xs = function
    | [] -> xs
    | Error x :: rxs -> (aux [@ocaml.tailcall]) (x :: xs) rxs
    | Ok _ :: rxs -> (aux [@ocaml.tailcall]) xs rxs
  in
  aux [] rxs

let filter_error rxs = rev_filter_error rxs |> rev

let rev_filter_left exs =
  let rec aux xs = function
    | [] -> xs
    | Either.Left x :: exs -> (aux [@ocaml.tailcall]) (x :: xs) exs
    | Either.Right _ :: exs -> (aux [@ocaml.tailcall]) xs exs
  in
  aux [] exs

let filter_left exs = rev_filter_left exs |> rev

let rev_filter_right exs =
  let rec aux xs = function
    | [] -> xs
    | Either.Right x :: exs -> (aux [@ocaml.tailcall]) (x :: xs) exs
    | Either.Left _ :: exs -> (aux [@ocaml.tailcall]) xs exs
  in
  aux [] exs

let filter_right exs = rev_filter_right exs |> rev

let rec iter_e f =
  let open Result_syntax in
  function
  | [] -> return_unit
  | h :: t ->
      let* () = f h in
      (iter_e [@ocaml.tailcall]) f t

let rec iter_s f =
  let open Lwt_syntax in
  function
  | [] -> return_unit
  | h :: t ->
      let* () = f h in
      (iter_s [@ocaml.tailcall]) f t

let iter_s f =
  let open Lwt_syntax in
  function
  | [] -> return_unit
  | h :: t ->
      let* () = Lwt.apply f h in
      (iter_s [@ocaml.tailcall]) f t

let rec iter_es f =
  let open Lwt_result_syntax in
  function
  | [] -> return_unit
  | h :: t ->
      let* () = f h in
      (iter_es [@ocaml.tailcall]) f t

let iter_es f =
  let open Lwt_result_syntax in
  function
  | [] -> return_unit
  | h :: t ->
      let* () = Lwt.apply f h in
      (iter_es [@ocaml.tailcall]) f t

let iter_ep f l = Lwt_result_syntax.join (rev_map (Lwt.apply f) l)

let iter_p f l = Lwt_syntax.join (rev_map (Lwt.apply f) l)

let iteri_e f l =
  let open Result_syntax in
  let rec aux i = function
    | [] -> return_unit
    | x :: xs ->
        let* () = f i x in
        (aux [@ocaml.tailcall]) (i + 1) xs
  in
  aux 0 l

let iteri_s f l =
  let open Lwt_syntax in
  let rec aux i = function
    | [] -> return_unit
    | x :: xs ->
        let* () = f i x in
        (aux [@ocaml.tailcall]) (i + 1) xs
  in
  match l with
  | [] -> return_unit
  | x :: xs ->
      let* () = lwt_apply2 f 0 x in
      aux 1 xs

let iteri_es f l =
  let open Lwt_result_syntax in
  let rec aux i = function
    | [] -> return_unit
    | x :: xs ->
        let* () = f i x in
        (aux [@ocaml.tailcall]) (i + 1) xs
  in
  match l with
  | [] -> return_unit
  | x :: xs ->
      let* () = lwt_apply2 f 0 x in
      aux 1 xs

let iteri_ep f l = Lwt_result_syntax.join (mapi (lwt_apply2 f) l)

let iteri_p f l = Lwt_syntax.join (mapi (lwt_apply2 f) l)

let rev_map_e f l =
  let open Result_syntax in
  let rec aux ys = function
    | [] -> return ys
    | x :: xs ->
        let* y = f x in
        (aux [@ocaml.tailcall]) (y :: ys) xs
  in
  aux [] l

let map_e f l = rev_map_e f l |> Result.map rev

let rev_map_s f l =
  let open Lwt_syntax in
  let rec aux ys = function
    | [] -> return ys
    | x :: xs ->
        let* y = f x in
        (aux [@ocaml.tailcall]) (y :: ys) xs
  in
  match l with
  | [] -> return_nil
  | x :: xs ->
      let* y = Lwt.apply f x in
      aux [y] xs

let map_s f l = rev_map_s f l |> Lwt.map rev

let rev_map_es f l =
  let open Lwt_result_syntax in
  let rec aux ys = function
    | [] -> return ys
    | x :: xs ->
        let* y = f x in
        (aux [@ocaml.tailcall]) (y :: ys) xs
  in
  match l with
  | [] -> return_nil
  | x :: xs ->
      let* y = Lwt.apply f x in
      aux [y] xs

let rev_map_ep f l = Lwt_result_syntax.all @@ rev_map (Lwt.apply f) l

let map_es f l = rev_map_es f l |> Lwt_result.map rev

let map_ep f l = rev_map_ep f l |> Lwt_result.map rev

let rev_map_p f l = Lwt_syntax.all @@ rev_map (Lwt.apply f) l

let map_p f l = rev_map_p f l |> Lwt.map rev

let rev_mapi_e f l =
  let open Result_syntax in
  let rec aux i ys = function
    | [] -> return ys
    | x :: xs ->
        let* y = f i x in
        (aux [@ocaml.tailcall]) (i + 1) (y :: ys) xs
  in
  aux 0 [] l

let mapi_e f l = rev_mapi_e f l |> Result.map rev

let rev_mapi_s f l =
  let open Lwt_syntax in
  let rec aux i ys = function
    | [] -> return ys
    | x :: xs ->
        let* y = f i x in
        (aux [@ocaml.tailcall]) (i + 1) (y :: ys) xs
  in
  match l with
  | [] -> return_nil
  | x :: xs ->
      let* y = lwt_apply2 f 0 x in
      aux 1 [y] xs

let mapi_s f l = rev_mapi_s f l |> Lwt.map rev

let rev_mapi_es f l =
  let open Lwt_result_syntax in
  let rec aux i ys = function
    | [] -> return ys
    | x :: xs ->
        let* y = f i x in
        (aux [@ocaml.tailcall]) (i + 1) (y :: ys) xs
  in
  match l with
  | [] -> return_nil
  | x :: xs ->
      let* y = lwt_apply2 f 0 x in
      aux 1 [y] xs

let mapi_es f l = rev_mapi_es f l |> Lwt_result.map rev

let rev_mapi f l =
  let rec aux i ys = function
    | [] -> ys
    | x :: xs -> (aux [@ocaml.tailcall]) (i + 1) (f i x :: ys) xs
  in
  aux 0 [] l

let rev_mapi_p f l = Lwt_syntax.all @@ rev_mapi f l

let rev_mapi_ep f l = Lwt_result_syntax.all @@ rev_mapi f l

let mapi_p f l = rev_mapi_p f l |> Lwt.map rev

let mapi_ep f l = rev_mapi_ep f l |> Lwt_result.map rev

let rec fold_left_e f acc =
  let open Result_syntax in
  function
  | [] -> return acc
  | x :: xs ->
      let* acc = f acc x in
      (fold_left_e [@ocaml.tailcall]) f acc xs

let rec fold_left_s f acc =
  let open Lwt_syntax in
  function
  | [] -> return acc
  | x :: xs ->
      let* acc = f acc x in
      (fold_left_s [@ocaml.tailcall]) f acc xs

let fold_left_s f acc =
  let open Lwt_syntax in
  function
  | [] -> return acc
  | x :: xs ->
      let* acc = lwt_apply2 f acc x in
      fold_left_s f acc xs

let rec fold_left_es f acc =
  let open Lwt_result_syntax in
  function
  | [] -> return acc
  | x :: xs ->
      let* acc = f acc x in
      (fold_left_es [@ocaml.tailcall]) f acc xs

let fold_left_es f acc =
  let open Lwt_result_syntax in
  function
  | [] -> return acc
  | x :: xs ->
      let* acc = lwt_apply2 f acc x in
      fold_left_es f acc xs

let fold_left_i f init l =
  fold_left
    (fun (i, accu) x ->
      let accu = f i accu x in
      (i + 1, accu))
    (0, init)
    l
  |> snd

let fold_left_i_e f acc l =
  let open Result_syntax in
  let* _, acc =
    fold_left_e
      (fun (i, acc) x ->
        let* acc = f i acc x in
        return (i + 1, acc))
      (0, acc)
      l
  in
  return acc

let fold_left_i_s f acc l =
  let open Lwt_syntax in
  let* _, acc =
    fold_left_s
      (fun (i, acc) x ->
        let* acc = f i acc x in
        return (i + 1, acc))
      (0, acc)
      l
  in
  return acc

let fold_left_i_es f acc l =
  let open Lwt_result_syntax in
  let* _, acc =
    fold_left_es
      (fun (i, acc) x ->
        let* acc = f i acc x in
        return (i + 1, acc))
      (0, acc)
      l
  in
  return acc

let filter_p f l =
  rev_map_p
    (fun x ->
      let open Lwt_syntax in
      let* b = f x in
      if b then return_some x else return_none)
    l
  |> Lwt.map rev_filter_some

let filter_ep f l =
  rev_map_ep
    (fun x ->
      let open Lwt_result_syntax in
      let* b = f x in
      if b then return_some x else return_none)
    l
  |> Lwt_result.map rev_filter_some

let filteri_p f l =
  rev_mapi_p
    (fun i x ->
      let open Lwt_syntax in
      let* b = f i x in
      if b then return_some x else return_none)
    l
  |> Lwt.map rev_filter_some

let filteri_ep f l =
  rev_mapi_ep
    (fun i x ->
      let open Lwt_result_syntax in
      let* b = f i x in
      if b then return_some x else return_none)
    l
  |> Lwt_result.map rev_filter_some

let rev_filter_map f l =
  fold_left
    (fun acc x -> match f x with None -> acc | Some y -> y :: acc)
    []
    l

let filter_map f l = rev_filter_map f l |> rev

let rev_filter_map_e f l =
  fold_left_e
    (fun acc x ->
      let open Result_syntax in
      let* o = f x in
      match o with None -> return acc | Some y -> return (y :: acc))
    []
    l

let filter_map_e f l = rev_filter_map_e f l |> Result.map rev

let rev_filter_map_s f l =
  fold_left_s
    (fun acc x ->
      let open Lwt_syntax in
      let* o = f x in
      match o with None -> return acc | Some y -> return (y :: acc))
    []
    l

let filter_map_s f l = rev_filter_map_s f l |> Lwt.map rev

let rev_filter_map_es f l =
  fold_left_es
    (fun acc x ->
      let open Lwt_result_syntax in
      let* o = f x in
      match o with None -> return acc | Some y -> return (y :: acc))
    []
    l

let filter_map_es f l = rev_filter_map_es f l |> Lwt_result.map rev

let filter_map_ep f l = rev_map_ep f l |> Lwt_result.map rev_filter_some

let filter_map_p f l = rev_map_p f l |> Lwt.map rev_filter_some

let rev_concat_map f xs =
  let rec aux f acc = function
    | [] -> acc
    | x :: xs ->
        let ys = f x in
        (aux [@ocaml.tailcall]) f (rev_append ys acc) xs
  in
  aux f [] xs

let concat_map f xs = rev (rev_concat_map f xs)

let rev_concat_map_s f xs =
  let open Lwt_syntax in
  let rec aux f acc = function
    | [] -> return acc
    | x :: xs ->
        let* ys = f x in
        (aux [@ocaml.tailcall]) f (rev_append ys acc) xs
  in
  match xs with
  | [] -> return_nil
  | x :: xs ->
      let* ys = Lwt.apply f x in
      (aux [@ocaml.tailcall]) f (rev ys) xs

let concat_map_s f xs =
  let open Lwt_syntax in
  let+ ys = rev_concat_map_s f xs in
  rev ys

let rev_concat_map_e f xs =
  let open Result_syntax in
  let rec aux f acc = function
    | [] -> return acc
    | x :: xs ->
        let* ys = f x in
        (aux [@ocaml.tailcall]) f (rev_append ys acc) xs
  in
  aux f [] xs

let concat_map_e f xs =
  let open Result_syntax in
  let+ ys = rev_concat_map_e f xs in
  rev ys

let rev_concat_map_es f xs =
  let open Lwt_result_syntax in
  let rec aux f acc = function
    | [] -> return acc
    | x :: xs ->
        let* ys = f x in
        (aux [@ocaml.tailcall]) f (rev_append ys acc) xs
  in
  match xs with
  | [] -> return_nil
  | x :: xs ->
      let* ys = Lwt.apply f x in
      (aux [@ocaml.tailcall]) f (rev ys) xs

let concat_map_es f xs =
  let open Lwt_result_syntax in
  let+ ys = rev_concat_map_es f xs in
  rev ys

let concat_map_p f xs = Lwt.map flatten @@ Lwt_syntax.all (map f xs)

let concat_map_ep f xs =
  let open Lwt_result_syntax in
  let+ r = all (map f xs) in
  flatten r

let rec fold_right_e f l acc =
  let open Result_syntax in
  match l with
  | [] -> return acc
  | x :: xs ->
      let* acc = fold_right_e f xs acc in
      f x acc

let rec fold_right_s f l acc =
  let open Lwt_syntax in
  match l with
  | [] -> return acc
  | x :: xs ->
      let* acc = fold_right_s f xs acc in
      f x acc

let rec fold_right_es f l acc =
  let open Lwt_result_syntax in
  match l with
  | [] -> return acc
  | x :: xs ->
      let* acc = fold_right_es f xs acc in
      f x acc

let rev_map2_e ~when_different_lengths f xs ys =
  let open Result_syntax in
  let rec aux zs xs ys =
    match (xs, ys) with
    | [], [] -> return zs
    | x :: xs, y :: ys ->
        let* z = f x y in
        (aux [@ocaml.tailcall]) (z :: zs) xs ys
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
  in
  aux [] xs ys

let rev_map2_s ~when_different_lengths f xs ys =
  let open Lwt_syntax in
  let rec aux zs xs ys =
    match (xs, ys) with
    | [], [] -> return_ok zs
    | x :: xs, y :: ys ->
        let* z = f x y in
        (aux [@ocaml.tailcall]) (z :: zs) xs ys
    | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths
  in
  match (xs, ys) with
  | [], [] -> return_ok_nil
  | x :: xs, y :: ys ->
      let* z = lwt_apply2 f x y in
      aux [z] xs ys
  | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths

let rev_map2_es ~when_different_lengths f xs ys =
  let open Lwt_result_syntax in
  let rec aux zs xs ys =
    match (xs, ys) with
    | [], [] -> return zs
    | x :: xs, y :: ys ->
        let* z = f x y in
        (aux [@ocaml.tailcall]) (z :: zs) xs ys
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
  in
  match (xs, ys) with
  | [], [] -> return_nil
  | x :: xs, y :: ys ->
      let* z = lwt_apply2 f x y in
      aux [z] xs ys
  | [], _ :: _ | _ :: _, [] -> fail when_different_lengths

let map2_e ~when_different_lengths f xs ys =
  rev_map2_e ~when_different_lengths f xs ys |> Result.map rev

let map2_s ~when_different_lengths f xs ys =
  rev_map2_s ~when_different_lengths f xs ys |> Lwt_result.map rev

let map2_es ~when_different_lengths f xs ys =
  rev_map2_es ~when_different_lengths f xs ys |> Lwt_result.map rev

let iter2_e ~when_different_lengths f xs ys =
  let open Result_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> return_unit
    | x :: xs, y :: ys ->
        let* () = f x y in
        (aux [@ocaml.tailcall]) xs ys
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
  in
  aux xs ys

let iter2_s ~when_different_lengths f xs ys =
  let open Lwt_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> return_ok_unit
    | x :: xs, y :: ys ->
        let* () = f x y in
        (aux [@ocaml.tailcall]) xs ys
    | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths
  in
  match (xs, ys) with
  | [], [] -> return_ok_unit
  | x :: xs, y :: ys ->
      let* () = lwt_apply2 f x y in
      aux xs ys
  | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths

let iter2_es ~when_different_lengths f xs ys =
  let open Lwt_result_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> return_unit
    | x :: xs, y :: ys ->
        let* () = f x y in
        (aux [@ocaml.tailcall]) xs ys
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
  in
  match (xs, ys) with
  | [], [] -> return_unit
  | x :: xs, y :: ys ->
      let* () = lwt_apply2 f x y in
      aux xs ys
  | [], _ :: _ | _ :: _, [] -> fail when_different_lengths

let fold_left2_e ~when_different_lengths f init xs ys =
  let open Result_syntax in
  let rec aux acc xs ys =
    match (xs, ys) with
    | [], [] -> return acc
    | x :: xs, y :: ys ->
        let* acc = f acc x y in
        (aux [@ocaml.tailcall]) acc xs ys
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
  in
  aux init xs ys

let fold_left2_s ~when_different_lengths f init xs ys =
  let open Lwt_syntax in
  let rec aux acc xs ys =
    match (xs, ys) with
    | [], [] -> return_ok acc
    | x :: xs, y :: ys ->
        let* acc = f acc x y in
        (aux [@ocaml.tailcall]) acc xs ys
    | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths
  in
  match (xs, ys) with
  | [], [] -> return_ok init
  | x :: xs, y :: ys ->
      let* acc = lwt_apply3 f init x y in
      aux acc xs ys
  | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths

let fold_left2_es ~when_different_lengths f init xs ys =
  let open Lwt_result_syntax in
  let rec aux acc xs ys =
    match (xs, ys) with
    | [], [] -> return acc
    | x :: xs, y :: ys ->
        let* acc = f acc x y in
        (aux [@ocaml.tailcall]) acc xs ys
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
  in
  match (xs, ys) with
  | [], [] -> return init
  | x :: xs, y :: ys ->
      let* acc = lwt_apply3 f init x y in
      (aux [@ocaml.tailcall]) acc xs ys
  | [], _ :: _ | _ :: _, [] -> fail when_different_lengths

let fold_right2_e ~when_different_lengths f xs ys init =
  let open Result_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> return init
    | x :: xs, y :: ys ->
        let* acc = aux xs ys in
        f x y acc
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
  in
  aux xs ys

let fold_right2_s ~when_different_lengths f xs ys init =
  let open Lwt_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths
    | [], [] -> return_ok init
    | x :: xs, y :: ys -> (
        let* acc = aux xs ys in
        match acc with
        | Error _ -> return acc
        | Ok acc ->
            let* v = f x y acc in
            return_ok v)
  in
  aux xs ys

let fold_right2_es ~when_different_lengths f xs ys init =
  let open Lwt_result_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
    | [], [] -> return init
    | x :: xs, y :: ys ->
        let* acc = aux xs ys in
        f x y acc
  in
  aux xs ys

let rec for_all_e f =
  let open Result_syntax in
  function
  | [] -> return_true
  | x :: xs ->
      let* b = f x in
      if b then (for_all_e [@ocaml.tailcall]) f xs else return_false

let rec for_all_s f =
  let open Lwt_syntax in
  function
  | [] -> return_true
  | x :: xs ->
      let* b = f x in
      if b then (for_all_s [@ocaml.tailcall]) f xs else return_false

let for_all_s f =
  let open Lwt_syntax in
  function
  | [] -> return_true
  | x :: xs ->
      let* b = Lwt.apply f x in
      if b then (for_all_s [@ocaml.tailcall]) f xs else return_false

let rec for_all_es f =
  let open Lwt_result_syntax in
  function
  | [] -> return_true
  | x :: xs ->
      let* b = f x in
      if b then (for_all_es [@ocaml.tailcall]) f xs else return_false

let for_all_es f =
  let open Lwt_result_syntax in
  function
  | [] -> return_true
  | x :: xs ->
      let* b = Lwt.apply f x in
      if b then (for_all_es [@ocaml.tailcall]) f xs else return_false

let for_all_ep f l = rev_map_ep f l |> Lwt_result.map (for_all Fun.id)

let for_all_p f l = rev_map_p f l |> Lwt.map (for_all Fun.id)

let rec exists_e f =
  let open Result_syntax in
  function
  | [] -> return_false
  | x :: xs ->
      let* b = f x in
      if b then return_true else (exists_e [@ocaml.tailcall]) f xs

let rec exists_s f =
  let open Lwt_syntax in
  function
  | [] -> return_false
  | x :: xs ->
      let* b = f x in
      if b then return_true else (exists_s [@ocaml.tailcall]) f xs

let exists_s f =
  let open Lwt_syntax in
  function
  | [] -> return_false
  | x :: xs ->
      let* b = Lwt.apply f x in
      if b then return_true else exists_s f xs

let rec exists_es f =
  let open Lwt_result_syntax in
  function
  | [] -> return_false
  | x :: xs ->
      let* b = f x in
      if b then return_true else (exists_es [@ocaml.tailcall]) f xs

let exists_es f =
  let open Lwt_result_syntax in
  function
  | [] -> return_false
  | x :: xs ->
      let* b = Lwt.apply f x in
      if b then return_true else exists_es f xs

let exists_ep f l = rev_map_ep f l |> Lwt_result.map (exists Fun.id)

let exists_p f l = rev_map_p f l |> Lwt.map (exists Fun.id)

let for_all2_e ~when_different_lengths f xs ys =
  let open Result_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
    | [], [] -> return_true
    | x :: xs, y :: ys ->
        let* b = f x y in
        if b then (aux [@ocaml.tailcall]) xs ys else return_false
  in
  aux xs ys

let for_all2_s ~when_different_lengths f xs ys =
  let open Lwt_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths
    | [], [] -> return_ok_true
    | x :: xs, y :: ys ->
        let* b = f x y in
        if b then (aux [@ocaml.tailcall]) xs ys else return_ok_false
  in
  match (xs, ys) with
  | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths
  | [], [] -> return_ok_true
  | x :: xs, y :: ys ->
      let* b = lwt_apply2 f x y in
      if b then aux xs ys else return_ok_false

let for_all2_es ~when_different_lengths f xs ys =
  let open Lwt_result_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
    | [], [] -> return_true
    | x :: xs, y :: ys ->
        let* b = f x y in
        if b then (aux [@ocaml.tailcall]) xs ys else return_false
  in
  match (xs, ys) with
  | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
  | [], [] -> return_true
  | x :: xs, y :: ys ->
      let* b = lwt_apply2 f x y in
      if b then aux xs ys else return_false

let exists2_e ~when_different_lengths f xs ys =
  let open Result_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
    | [], [] -> return_false
    | x :: xs, y :: ys ->
        let* b = f x y in
        if b then return_true else (aux [@ocaml.tailcall]) xs ys
  in
  aux xs ys

let exists2_s ~when_different_lengths f xs ys =
  let open Lwt_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths
    | [], [] -> return_ok_false
    | x :: xs, y :: ys ->
        let* b = f x y in
        if b then return_ok_true else (aux [@ocaml.tailcall]) xs ys
  in
  match (xs, ys) with
  | [], _ :: _ | _ :: _, [] -> return_error when_different_lengths
  | [], [] -> return_ok_false
  | x :: xs, y :: ys ->
      let* b = lwt_apply2 f x y in
      if b then return_ok_true else aux xs ys

let exists2_es ~when_different_lengths f xs ys =
  let open Lwt_result_syntax in
  let rec aux xs ys =
    match (xs, ys) with
    | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
    | [], [] -> return_false
    | x :: xs, y :: ys ->
        let* b = f x y in
        if b then return_true else (aux [@ocaml.tailcall]) xs ys
  in
  match (xs, ys) with
  | [], _ :: _ | _ :: _, [] -> fail when_different_lengths
  | [], [] -> return_false
  | x :: xs, y :: ys ->
      let* b = lwt_apply2 f x y in
      if b then return_true else aux xs ys

let rev_partition f xs =
  let rec aux trues falses = function
    | [] -> (trues, falses)
    | x :: xs ->
        if f x then (aux [@ocaml.tailcall]) (x :: trues) falses xs
        else (aux [@ocaml.tailcall]) trues (x :: falses) xs
  in
  aux [] [] xs

let partition f xs =
  rev_partition f xs |> fun (trues, falses) -> (rev trues, rev falses)

let rev_partition_result xs =
  let rec aux oks errors = function
    | [] -> (oks, errors)
    | Ok ok :: xs -> (aux [@ocaml.tailcall]) (ok :: oks) errors xs
    | Error error :: xs -> (aux [@ocaml.tailcall]) oks (error :: errors) xs
  in
  aux [] [] xs

let partition_result xs =
  let rev_oks, rev_errors = rev_partition_result xs in
  (rev rev_oks, rev rev_errors)

let rev_partition_either xs =
  let rec aux lefts rights = function
    | [] -> (lefts, rights)
    | Either.Left left :: xs ->
        (aux [@ocaml.tailcall]) (left :: lefts) rights xs
    | Either.Right right :: xs ->
        (aux [@ocaml.tailcall]) lefts (right :: rights) xs
  in
  aux [] [] xs

let partition_either xs =
  let rev_lefts, rev_rights = rev_partition_either xs in
  (rev rev_lefts, rev rev_rights)

let rev_partition_e f l =
  let open Result_syntax in
  let rec aux trues falses = function
    | [] -> return (trues, falses)
    | x :: xs ->
        let* b = f x in
        if b then (aux [@ocaml.tailcall]) (x :: trues) falses xs
        else (aux [@ocaml.tailcall]) trues (x :: falses) xs
  in
  aux [] [] l

let partition_e f l =
  rev_partition_e f l
  |> Result.map (fun (trues, falses) -> (rev trues, rev falses))

let rev_partition_s f l =
  let open Lwt_syntax in
  let rec aux trues falses = function
    | [] -> return (trues, falses)
    | x :: xs ->
        let* b = f x in
        if b then (aux [@ocaml.tailcall]) (x :: trues) falses xs
        else (aux [@ocaml.tailcall]) trues (x :: falses) xs
  in
  match l with
  | [] -> return ([], [])
  | x :: xs ->
      let* b = Lwt.apply f x in
      if b then aux [x] [] xs else aux [] [x] xs

let partition_s f l =
  rev_partition_s f l
  |> Lwt.map (fun (trues, falses) -> (rev trues, rev falses))

let rev_partition_es f l =
  let open Lwt_result_syntax in
  let rec aux trues falses = function
    | [] -> return (trues, falses)
    | x :: xs ->
        let* b = f x in
        if b then (aux [@ocaml.tailcall]) (x :: trues) falses xs
        else (aux [@ocaml.tailcall]) trues (x :: falses) xs
  in
  match l with
  | [] -> return ([], [])
  | x :: xs ->
      let* b = Lwt.apply f x in
      if b then aux [x] [] xs else aux [] [x] xs

let partition_es f l =
  rev_partition_es f l
  |> Lwt_result.map (fun (trues, falses) -> (rev trues, rev falses))

let partition_ep f l =
  let open Lwt_result_syntax in
  let* bxs =
    rev_map_ep
      (fun x ->
        let* b = f x in
        return (b, x))
      l
  in
  let r =
    fold_left
      (fun (trues, falses) (b, x) ->
        if b then (x :: trues, falses) else (trues, x :: falses))
      ([], [])
      bxs
  in
  return r

let partition_p f l =
  let open Lwt_syntax in
  let* bxs =
    rev_map_p
      (fun x ->
        let* b = f x in
        return (b, x))
      l
  in
  let r =
    fold_left
      (fun (trues, falses) (b, x) ->
        if b then (x :: trues, falses) else (trues, x :: falses))
      ([], [])
      bxs
  in
  return r

let rev_partition_map f xs =
  let rec aux lefts rights = function
    | [] -> (lefts, rights)
    | x :: xs -> (
        match f x with
        | Either.Left l -> (aux [@ocaml.tailcall]) (l :: lefts) rights xs
        | Either.Right r -> (aux [@ocaml.tailcall]) lefts (r :: rights) xs)
  in
  aux [] [] xs

let partition_map f xs =
  rev_partition_map f xs |> fun (lefts, rights) -> (rev lefts, rev rights)

let rev_partition_map_e f l =
  let open Result_syntax in
  let rec aux lefts rights = function
    | [] -> return (lefts, rights)
    | x :: xs -> (
        let* e = f x in
        match e with
        | Either.Left l -> (aux [@ocaml.tailcall]) (l :: lefts) rights xs
        | Either.Right r -> (aux [@ocaml.tailcall]) lefts (r :: rights) xs)
  in
  aux [] [] l

let partition_map_e f l =
  rev_partition_map_e f l
  |> Result.map (fun (lefts, rights) -> (rev lefts, rev rights))

let rev_partition_map_s f l =
  let open Lwt_syntax in
  let rec aux lefts rights = function
    | [] -> return (lefts, rights)
    | x :: xs -> (
        let* e = f x in
        match e with
        | Either.Left l -> (aux [@ocaml.tailcall]) (l :: lefts) rights xs
        | Either.Right r -> (aux [@ocaml.tailcall]) lefts (r :: rights) xs)
  in
  match l with
  | [] -> return ([], [])
  | x :: xs -> (
      let* e = Lwt.apply f x in
      match e with
      | Either.Left l -> (aux [@ocaml.tailcall]) [l] [] xs
      | Either.Right r -> (aux [@ocaml.tailcall]) [] [r] xs)

let partition_map_s f l =
  rev_partition_map_s f l
  |> Lwt.map (fun (lefts, rights) -> (rev lefts, rev rights))

let rev_partition_map_es f l =
  let open Lwt_result_syntax in
  let rec aux lefts rights = function
    | [] -> return (lefts, rights)
    | x :: xs -> (
        let* e = f x in
        match e with
        | Either.Left l -> (aux [@ocaml.tailcall]) (l :: lefts) rights xs
        | Either.Right r -> (aux [@ocaml.tailcall]) lefts (r :: rights) xs)
  in
  match l with
  | [] -> return ([], [])
  | x :: xs -> (
      let* e = Lwt.apply f x in
      match e with
      | Either.Left l -> (aux [@ocaml.tailcall]) [l] [] xs
      | Either.Right r -> (aux [@ocaml.tailcall]) [] [r] xs)

let partition_map_es f l =
  rev_partition_map_es f l
  |> Lwt_result.map (fun (lefts, rights) -> (rev lefts, rev rights))

let partition_map_ep f l =
  let open Lwt_result_syntax in
  let* es = rev_map_ep f l in
  let r = rev_partition_either es in
  return r

let partition_map_p f l =
  let open Lwt_syntax in
  let* es = rev_map_p f l in
  let r = rev_partition_either es in
  return r

let combine ~when_different_lengths xs ys =
  map2 ~when_different_lengths (fun x y -> (x, y)) xs ys

let rev_combine ~when_different_lengths xs ys =
  rev_map2 ~when_different_lengths (fun x y -> (x, y)) xs ys

let combine_with_leftovers xs ys =
  let rec aux rev_combined xs ys =
    match (xs, ys) with
    | [], [] -> (rev rev_combined, None)
    | (_ :: _ as left), [] -> (rev rev_combined, Some (Either.Left left))
    | [], (_ :: _ as right) -> (rev rev_combined, Some (Either.Right right))
    | x :: xs, y :: ys -> (aux [@ocaml.tailcall]) ((x, y) :: rev_combined) xs ys
  in
  aux [] xs ys

let combine_drop xs ys =
  let rec aux rev_combined xs ys =
    match (xs, ys) with
    | x :: xs, y :: ys -> (aux [@ocaml.tailcall]) ((x, y) :: rev_combined) xs ys
    | [], [] | _ :: _, [] | [], _ :: _ -> rev rev_combined
  in
  aux [] xs ys

let product xs ys = rev_concat_map (fun x -> rev_map (fun y -> (x, y)) ys) xs

(* Use Fisher-Yates shuffle as described by Knuth
   https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle *)
let shuffle ~rng l =
  let a = Array.of_list l in
  let len = Array.length a in
  for i = len downto 2 do
    let m = Random.State.int rng i in
    let n' = i - 1 in
    if m <> n' then (
      let tmp = a.(m) in
      a.(m) <- a.(n') ;
      a.(n') <- tmp)
  done ;
  Array.to_list a

let rec compare ecomp xs ys =
  match (xs, ys) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | x :: xs, y :: ys ->
      let ec = ecomp x y in
      if ec = 0 then compare ecomp xs ys else ec

let rec equal eeq xs ys =
  match (xs, ys) with
  | [], [] -> true
  | [], _ :: _ | _ :: _, [] -> false
  | x :: xs, y :: ys -> eeq x y && equal eeq xs ys
