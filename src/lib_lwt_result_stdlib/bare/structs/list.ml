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

open Monad
module Legacy = Stdlib.List
include Legacy

let nil = []

let nil_e = Ok []

let nil_s = Lwt.return_nil

let nil_es = Lwt.return nil_e

let hd = function x :: _ -> Some x | [] -> None

let tl = function _ :: xs -> Some xs | [] -> None

let nth xs n =
  if n < 0 then None
  else
    let rec aux xs n =
      match (xs, n) with
      | ([], _) ->
          None
      | (x :: _, 0) ->
          Some x
      | (_ :: xs, n) ->
          (aux [@ocaml.tailcall]) xs (n - 1)
    in
    aux xs n

let rec last hd = function
  | [] ->
      hd
  | [last] ->
      last
  | hd :: (_ :: _ as tl) ->
      (last [@ocaml.tailcall]) hd tl

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
  | ([], []) ->
      Monad.unit_e
  | ([], _ :: _) | (_ :: _, []) ->
      Error when_different_lengths
  | (x :: xs, y :: ys) ->
      f x y ;
      (iter2 [@ocaml.tailcall]) ~when_different_lengths f xs ys

let rev_map2 ~when_different_lengths f xs ys =
  let rec aux zs xs ys =
    match (xs, ys) with
    | ([], []) ->
        Ok zs
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
    | (x :: xs, y :: ys) ->
        let z = f x y in
        (aux [@ocaml.tailcall]) (z :: zs) xs ys
  in
  aux [] xs ys

let map2 ~when_different_lengths f xs ys =
  rev_map2 ~when_different_lengths f xs ys >|? rev

let fold_left2 ~when_different_lengths f a xs ys =
  let rec aux acc xs ys =
    match (xs, ys) with
    | ([], []) ->
        Ok acc
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
    | (x :: xs, y :: ys) ->
        let acc = f acc x y in
        (aux [@ocaml.tailcall]) acc xs ys
  in
  aux a xs ys

let fold_right2 ~when_different_lengths f xs ys a =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], []) ->
        Ok a
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
    | (x :: xs, y :: ys) ->
        aux xs ys >|? fun acc -> f x y acc
  in
  aux xs ys

let for_all2 ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], []) ->
        Ok true
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
    | (x :: xs, y :: ys) -> (
      match f x y with
      | true ->
          (aux [@ocaml.tailcall]) xs ys
      | false ->
          Ok false )
  in
  aux xs ys

let exists2 ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], []) ->
        Ok false
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
    | (x :: xs, y :: ys) -> (
      match f x y with
      | true ->
          Ok true
      | false ->
          (aux [@ocaml.tailcall]) xs ys )
  in
  aux xs ys

let rec mem ~equal x = function
  | [] ->
      false
  | y :: ys ->
      equal x y || mem ~equal x ys

let rec assoc ~equal k = function
  | [] ->
      None
  | (kk, v) :: kvs ->
      if equal k kk then Some v else assoc ~equal k kvs

let assoc_opt = assoc

let assq = assq_opt

let rec mem_assoc ~equal k = function
  | [] ->
      false
  | (kk, _) :: kvs ->
      equal k kk || mem_assoc ~equal k kvs

let rec remove_assoc ~equal k = function
  | [] ->
      []
  | ((kk, _) as kv) :: kvs ->
      if equal k kk then kvs else kv :: remove_assoc ~equal k kvs

let init ~when_negative_length l f =
  if l < 0 then Error when_negative_length
  else if l = 0 then nil_e
  else Ok (Legacy.init l f)

let init_e ~when_negative_length l f =
  let rec aux acc i =
    if i >= l then Ok (rev acc)
    else f i >>? fun v -> (aux [@ocaml.tailcall]) (v :: acc) (i + 1)
  in
  if l < 0 then Error when_negative_length
  else if l = 0 then nil_e
  else aux [] 0

let init_s ~when_negative_length l f =
  let rec aux acc i =
    if i >= l then Lwt.return (Ok (rev acc))
    else f i >>= fun v -> (aux [@ocaml.tailcall]) (v :: acc) (i + 1)
  in
  if l < 0 then Lwt.return (Error when_negative_length)
  else if l = 0 then nil_es
  else Lwt.apply f 0 >>= fun v -> aux [v] 1

let init_es ~when_negative_length l f =
  let rec aux acc i =
    if i >= l then Lwt.return (Ok (rev acc))
    else f i >>=? fun v -> (aux [@ocaml.tailcall]) (v :: acc) (i + 1)
  in
  if l < 0 then Lwt.return (Error when_negative_length)
  else if l = 0 then nil_es
  else Lwt.apply f 0 >>=? fun v -> aux [v] 1

let init_ep ~when_negative_length l f =
  let rec aux acc i =
    if i >= l then all_ep (rev acc)
    else (aux [@ocaml.tailcall]) (Lwt.apply f i :: acc) (i + 1)
  in
  if l < 0 then Lwt.return (Error [when_negative_length])
  else if l = 0 then nil_es
  else aux [] 0

let init_p ~when_negative_length l f =
  let rec aux acc i =
    if i >= l then all_p (rev acc) >>= fun xs -> Lwt.return (Ok xs)
    else (aux [@ocaml.tailcall]) (Lwt.apply f i :: acc) (i + 1)
  in
  if l < 0 then Lwt.return (Error when_negative_length)
  else if l = 0 then nil_es
  else aux [] 0

let rec find_e f = function
  | [] ->
      none_e
  | x :: xs -> (
      f x
      >>? function
      | true -> Ok (Some x) | false -> (find_e [@ocaml.tailcall]) f xs )

let rec find_s f = function
  | [] ->
      none_s
  | x :: xs -> (
      f x
      >>= function
      | true -> Lwt.return (Some x) | false -> (find_s [@ocaml.tailcall]) f xs
      )

let find_s f = function
  | [] ->
      none_s
  | x :: xs -> (
      Lwt.apply f x
      >>= function
      | true -> Lwt.return (Some x) | false -> (find_s [@ocaml.tailcall]) f xs
      )

let rec find_es f = function
  | [] ->
      none_es
  | x :: xs -> (
      f x
      >>=? function
      | true ->
          Lwt.return (Ok (Some x))
      | false ->
          (find_es [@ocaml.tailcall]) f xs )

let find_es f = function
  | [] ->
      none_es
  | x :: xs -> (
      Lwt.apply f x
      >>=? function
      | true ->
          Lwt.return (Ok (Some x))
      | false ->
          (find_es [@ocaml.tailcall]) f xs )

let rev_filter f xs =
  fold_left (fun rev_xs x -> if f x then x :: rev_xs else rev_xs) [] xs

let rev_filter_e f xs =
  let rec aux acc = function
    | [] ->
        Ok acc
    | x :: xs -> (
        f x
        >>? function
        | true ->
            (aux [@ocaml.tailcall]) (x :: acc) xs
        | false ->
            (aux [@ocaml.tailcall]) acc xs )
  in
  aux [] xs

let rev_filter_some oxs =
  let rec aux xs = function
    | [] ->
        xs
    | Some x :: oxs ->
        (aux [@ocaml.tailcall]) (x :: xs) oxs
    | None :: oxs ->
        (aux [@ocaml.tailcall]) xs oxs
  in
  aux [] oxs

let filter_some oxs = rev_filter_some oxs |> rev

let rev_filter_ok rxs =
  let rec aux xs = function
    | [] ->
        xs
    | Ok x :: rxs ->
        (aux [@ocaml.tailcall]) (x :: xs) rxs
    | Error _ :: rxs ->
        (aux [@ocaml.tailcall]) xs rxs
  in
  aux [] rxs

let filter_ok rxs = rev_filter_ok rxs |> rev

let rev_filter_error rxs =
  let rec aux xs = function
    | [] ->
        xs
    | Error x :: rxs ->
        (aux [@ocaml.tailcall]) (x :: xs) rxs
    | Ok _ :: rxs ->
        (aux [@ocaml.tailcall]) xs rxs
  in
  aux [] rxs

let filter_error rxs = rev_filter_error rxs |> rev

let filter_e f xs = rev_filter_e f xs >|? rev

let rev_filter_s f xs =
  let rec aux acc = function
    | [] ->
        Lwt.return acc
    | x :: xs -> (
        f x
        >>= function
        | true ->
            (aux [@ocaml.tailcall]) (x :: acc) xs
        | false ->
            (aux [@ocaml.tailcall]) acc xs )
  in
  match xs with
  | [] ->
      Lwt.return []
  | x :: xs -> (
      Lwt.apply f x
      >>= function
      | true ->
          (aux [@ocaml.tailcall]) [x] xs
      | false ->
          (aux [@ocaml.tailcall]) [] xs )

let filter_s f xs = rev_filter_s f xs >|= rev

let rev_filter_es f xs =
  let rec aux acc = function
    | [] ->
        Lwt.return (Ok acc)
    | x :: xs -> (
        f x
        >>=? function
        | true ->
            (aux [@ocaml.tailcall]) (x :: acc) xs
        | false ->
            (aux [@ocaml.tailcall]) acc xs )
  in
  match xs with
  | [] ->
      Lwt.return (Ok [])
  | x :: xs -> (
      Lwt.apply f x >>=? function true -> aux [x] xs | false -> aux [] xs )

let filter_es f xs = rev_filter_es f xs >|=? rev

let rec iter_e f = function
  | [] ->
      unit_e
  | h :: t ->
      f h >>? fun () -> (iter_e [@ocaml.tailcall]) f t

let rec iter_s f = function
  | [] ->
      unit_s
  | h :: t ->
      f h >>= fun () -> (iter_s [@ocaml.tailcall]) f t

let iter_s f = function
  | [] ->
      unit_s
  | h :: t ->
      Lwt.apply f h >>= fun () -> (iter_s [@ocaml.tailcall]) f t

let rec iter_es f = function
  | [] ->
      unit_es
  | h :: t ->
      f h >>=? fun () -> (iter_es [@ocaml.tailcall]) f t

let iter_es f = function
  | [] ->
      unit_es
  | h :: t ->
      Lwt.apply f h >>=? fun () -> (iter_es [@ocaml.tailcall]) f t

let iter_ep f l = join_ep (rev_map (Lwt.apply f) l)

let iter_p f l = join_p (rev_map (Lwt.apply f) l)

let iteri_e f l =
  let rec aux i = function
    | [] ->
        unit_e
    | x :: xs ->
        f i x >>? fun () -> (aux [@ocaml.tailcall]) (i + 1) xs
  in
  aux 0 l

let lwt_apply2 f x y = try f x y with exc -> Lwt.fail exc

let iteri_s f l =
  let rec aux i = function
    | [] ->
        unit_s
    | x :: xs ->
        f i x >>= fun () -> (aux [@ocaml.tailcall]) (i + 1) xs
  in
  match l with
  | [] ->
      unit_s
  | x :: xs ->
      lwt_apply2 f 0 x >>= fun () -> aux 1 xs

let iteri_es f l =
  let rec aux i = function
    | [] ->
        unit_es
    | x :: xs ->
        f i x >>=? fun () -> (aux [@ocaml.tailcall]) (i + 1) xs
  in
  match l with
  | [] ->
      unit_es
  | x :: xs ->
      lwt_apply2 f 0 x >>=? fun () -> aux 1 xs

let iteri_ep f l = join_ep (mapi (lwt_apply2 f) l)

let iteri_p f l = join_p (mapi (lwt_apply2 f) l)

let rev_map_e f l =
  let rec aux ys = function
    | [] ->
        Ok ys
    | x :: xs ->
        f x >>? fun y -> (aux [@ocaml.tailcall]) (y :: ys) xs
  in
  aux [] l

let map_e f l = rev_map_e f l >|? rev

let rev_map_s f l =
  let rec aux ys = function
    | [] ->
        Lwt.return ys
    | x :: xs ->
        f x >>= fun y -> (aux [@ocaml.tailcall]) (y :: ys) xs
  in
  match l with
  | [] ->
      Lwt.return []
  | x :: xs ->
      Lwt.apply f x >>= fun y -> aux [y] xs

let map_s f l = rev_map_s f l >|= rev

let rev_map_es f l =
  let rec aux ys = function
    | [] ->
        return ys
    | x :: xs ->
        f x >>=? fun y -> (aux [@ocaml.tailcall]) (y :: ys) xs
  in
  match l with
  | [] ->
      return []
  | x :: xs ->
      Lwt.apply f x >>=? fun y -> aux [y] xs

let rev_map_ep f l = all_ep @@ rev_map (Lwt.apply f) l

let map_es f l = rev_map_es f l >|=? rev

let map_ep f l = rev_map_ep f l >|=? rev

let rev_map_p f l = all_p @@ rev_map (Lwt.apply f) l

let map_p f l = rev_map_p f l >|= rev

let rev_mapi_e f l =
  let rec aux i ys = function
    | [] ->
        Ok ys
    | x :: xs ->
        f i x >>? fun y -> (aux [@ocaml.tailcall]) (i + 1) (y :: ys) xs
  in
  aux 0 [] l

let mapi_e f l = rev_mapi_e f l >|? rev

let rev_mapi_s f l =
  let rec aux i ys = function
    | [] ->
        Lwt.return ys
    | x :: xs ->
        f i x >>= fun y -> (aux [@ocaml.tailcall]) (i + 1) (y :: ys) xs
  in
  match l with
  | [] ->
      Lwt.return []
  | x :: xs ->
      lwt_apply2 f 0 x >>= fun y -> aux 1 [y] xs

let mapi_s f l = rev_mapi_s f l >|= rev

let rev_mapi_es f l =
  let rec aux i ys = function
    | [] ->
        return ys
    | x :: xs ->
        f i x >>=? fun y -> (aux [@ocaml.tailcall]) (i + 1) (y :: ys) xs
  in
  match l with
  | [] ->
      return []
  | x :: xs ->
      lwt_apply2 f 0 x >>=? fun y -> aux 1 [y] xs

let mapi_es f l = rev_mapi_es f l >|=? rev

let rev_mapi f l =
  let rec aux i ys = function
    | [] ->
        ys
    | x :: xs ->
        (aux [@ocaml.tailcall]) (i + 1) (f i x :: ys) xs
  in
  aux 0 [] l

let rev_mapi_p f l = all_p @@ rev_mapi f l

let rev_mapi_ep f l = all_ep @@ rev_mapi f l

let mapi_p f l = rev_mapi_p f l >|= rev

let mapi_ep f l = rev_mapi_ep f l >|=? rev

let rec fold_left_e f acc = function
  | [] ->
      Ok acc
  | x :: xs ->
      f acc x >>? fun acc -> (fold_left_e [@ocaml.tailcall]) f acc xs

let rec fold_left_s f acc = function
  | [] ->
      Lwt.return acc
  | x :: xs ->
      f acc x >>= fun acc -> (fold_left_s [@ocaml.tailcall]) f acc xs

let fold_left_s f acc = function
  | [] ->
      Lwt.return acc
  | x :: xs ->
      lwt_apply2 f acc x >>= fun acc -> fold_left_s f acc xs

let rec fold_left_es f acc = function
  | [] ->
      return acc
  | x :: xs ->
      f acc x >>=? fun acc -> (fold_left_es [@ocaml.tailcall]) f acc xs

let fold_left_es f acc = function
  | [] ->
      return acc
  | x :: xs ->
      lwt_apply2 f acc x >>=? fun acc -> fold_left_es f acc xs

let filter_p f l =
  rev_map_p (fun x -> f x >|= fun b -> if b then Some x else None) l
  >|= rev_filter_some

let filter_ep f l =
  rev_map_ep (fun x -> f x >|=? fun b -> if b then Some x else None) l
  >|=? rev_filter_some

let rev_filter_map f l =
  fold_left
    (fun acc x -> match f x with None -> acc | Some y -> y :: acc)
    []
    l

let filter_map f l = rev_filter_map f l |> rev

let rev_filter_map_e f l =
  fold_left_e
    (fun acc x -> f x >|? function None -> acc | Some y -> y :: acc)
    []
    l

let filter_map_e f l = rev_filter_map_e f l >|? rev

let rev_filter_map_s f l =
  fold_left_s
    (fun acc x -> f x >|= function None -> acc | Some y -> y :: acc)
    []
    l

let filter_map_s f l = rev_filter_map_s f l >|= rev

let rev_filter_map_es f l =
  fold_left_es
    (fun acc x -> f x >|=? function None -> acc | Some y -> y :: acc)
    []
    l

let filter_map_es f l = rev_filter_map_es f l >|=? rev

let filter_map_ep f l = rev_map_ep f l >|=? rev_filter_some

let filter_map_p f l = rev_map_p f l >|= rev_filter_some

let rec fold_right_e f l acc =
  match l with
  | [] ->
      Ok acc
  | x :: xs ->
      fold_right_e f xs acc >>? fun acc -> f x acc

let rec fold_right_s f l acc =
  match l with
  | [] ->
      Lwt.return acc
  | x :: xs ->
      fold_right_s f xs acc >>= fun acc -> f x acc

let rec fold_right_es f l acc =
  match l with
  | [] ->
      return acc
  | x :: xs ->
      fold_right_es f xs acc >>=? fun acc -> f x acc

let rev_map2_e ~when_different_lengths f xs ys =
  let rec aux zs xs ys =
    match (xs, ys) with
    | ([], []) ->
        Ok zs
    | (x :: xs, y :: ys) ->
        f x y >>? fun z -> (aux [@ocaml.tailcall]) (z :: zs) xs ys
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
  in
  aux [] xs ys

let rev_map2_s ~when_different_lengths f xs ys =
  let rec aux zs xs ys =
    match (xs, ys) with
    | ([], []) ->
        Lwt.return (Ok zs)
    | (x :: xs, y :: ys) ->
        f x y >>= fun z -> (aux [@ocaml.tailcall]) (z :: zs) xs ys
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
  in
  match (xs, ys) with
  | ([], []) ->
      Lwt.return (Ok [])
  | (x :: xs, y :: ys) ->
      lwt_apply2 f x y >>= fun z -> aux [z] xs ys
  | ([], _ :: _) | (_ :: _, []) ->
      fail when_different_lengths

let rev_map2_es ~when_different_lengths f xs ys =
  let rec aux zs xs ys =
    match (xs, ys) with
    | ([], []) ->
        Lwt.return (Ok zs)
    | (x :: xs, y :: ys) ->
        f x y >>=? fun z -> (aux [@ocaml.tailcall]) (z :: zs) xs ys
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
  in
  match (xs, ys) with
  | ([], []) ->
      Lwt.return (Ok [])
  | (x :: xs, y :: ys) ->
      lwt_apply2 f x y >>=? fun z -> aux [z] xs ys
  | ([], _ :: _) | (_ :: _, []) ->
      fail when_different_lengths

let map2_e ~when_different_lengths f xs ys =
  rev_map2_e ~when_different_lengths f xs ys >|? rev

let map2_s ~when_different_lengths f xs ys =
  rev_map2_s ~when_different_lengths f xs ys >|=? rev

let map2_es ~when_different_lengths f xs ys =
  rev_map2_es ~when_different_lengths f xs ys >|=? rev

let iter2_e ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], []) ->
        unit_e
    | (x :: xs, y :: ys) ->
        f x y >>? fun () -> (aux [@ocaml.tailcall]) xs ys
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
  in
  aux xs ys

let iter2_s ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], []) ->
        Lwt.return (Ok ())
    | (x :: xs, y :: ys) ->
        f x y >>= fun () -> (aux [@ocaml.tailcall]) xs ys
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
  in
  match (xs, ys) with
  | ([], []) ->
      Lwt.return (Ok ())
  | (x :: xs, y :: ys) ->
      lwt_apply2 f x y >>= fun () -> aux xs ys
  | ([], _ :: _) | (_ :: _, []) ->
      fail when_different_lengths

let iter2_es ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], []) ->
        Monad.unit_es
    | (x :: xs, y :: ys) ->
        f x y >>=? fun () -> (aux [@ocaml.tailcall]) xs ys
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
  in
  match (xs, ys) with
  | ([], []) ->
      Monad.unit_es
  | (x :: xs, y :: ys) ->
      lwt_apply2 f x y >>=? fun () -> aux xs ys
  | ([], _ :: _) | (_ :: _, []) ->
      fail when_different_lengths

let fold_left2_e ~when_different_lengths f init xs ys =
  let rec aux acc xs ys =
    match (xs, ys) with
    | ([], []) ->
        Ok acc
    | (x :: xs, y :: ys) ->
        f acc x y >>? fun acc -> (aux [@ocaml.tailcall]) acc xs ys
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
  in
  aux init xs ys

let lwt_apply3 f a x y = try f a x y with exc -> Lwt.fail exc

let fold_left2_s ~when_different_lengths f init xs ys =
  let rec aux acc xs ys =
    match (xs, ys) with
    | ([], []) ->
        Lwt.return (Ok acc)
    | (x :: xs, y :: ys) ->
        f acc x y >>= fun acc -> (aux [@ocaml.tailcall]) acc xs ys
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
  in
  match (xs, ys) with
  | ([], []) ->
      Lwt.return (Ok init)
  | (x :: xs, y :: ys) ->
      lwt_apply3 f init x y >>= fun acc -> aux acc xs ys
  | ([], _ :: _) | (_ :: _, []) ->
      fail when_different_lengths

let fold_left2_es ~when_different_lengths f init xs ys =
  let rec aux acc xs ys =
    match (xs, ys) with
    | ([], []) ->
        Lwt.return (Ok acc)
    | (x :: xs, y :: ys) ->
        f acc x y >>=? fun acc -> (aux [@ocaml.tailcall]) acc xs ys
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
  in
  match (xs, ys) with
  | ([], []) ->
      Lwt.return (Ok init)
  | (x :: xs, y :: ys) ->
      lwt_apply3 f init x y >>=? fun acc -> (aux [@ocaml.tailcall]) acc xs ys
  | ([], _ :: _) | (_ :: _, []) ->
      fail when_different_lengths

let fold_right2_e ~when_different_lengths f xs ys init =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], []) ->
        Ok init
    | (x :: xs, y :: ys) ->
        aux xs ys >>? fun acc -> f x y acc
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
  in
  aux xs ys

let fold_right2_s ~when_different_lengths f xs ys init =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
    | ([], []) ->
        Lwt.return (Ok init)
    | (x :: xs, y :: ys) ->
        (* We could use a specific operator for that. It'd need the following type
           ('a, 'err) result Lwt.t -> ('a -> 'b Lwt.t) -> ('b, 'err) result Lwt.t
          *)
        aux xs ys >>=? fun acc -> f x y acc >|= ok
  in
  aux xs ys

let fold_right2_es ~when_different_lengths f xs ys init =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
    | ([], []) ->
        Lwt.return (Ok init)
    | (x :: xs, y :: ys) ->
        aux xs ys >>=? fun acc -> f x y acc
  in
  aux xs ys

let rec for_all_e f = function
  | [] ->
      true_e
  | x :: xs -> (
      f x
      >>? function
      | true -> (for_all_e [@ocaml.tailcall]) f xs | false -> false_e )

let rec for_all_s f = function
  | [] ->
      true_s
  | x :: xs -> (
      f x
      >>= function
      | true -> (for_all_s [@ocaml.tailcall]) f xs | false -> false_s )

let for_all_s f = function
  | [] ->
      true_s
  | x :: xs -> (
      Lwt.apply f x
      >>= function
      | true -> (for_all_s [@ocaml.tailcall]) f xs | false -> false_s )

let rec for_all_es f = function
  | [] ->
      true_es
  | x :: xs -> (
      f x
      >>=? function
      | true -> (for_all_es [@ocaml.tailcall]) f xs | false -> false_es )

let for_all_es f = function
  | [] ->
      true_es
  | x :: xs -> (
      Lwt.apply f x
      >>=? function
      | true -> (for_all_es [@ocaml.tailcall]) f xs | false -> false_es )

let for_all_ep f l = rev_map_ep f l >|=? for_all Fun.id

let for_all_p f l = rev_map_p f l >|= for_all Fun.id

let rec exists_e f = function
  | [] ->
      false_e
  | x :: xs -> (
      f x
      >>? function
      | false -> (exists_e [@ocaml.tailcall]) f xs | true -> true_e )

let rec exists_s f = function
  | [] ->
      false_s
  | x :: xs -> (
      f x
      >>= function
      | false -> (exists_s [@ocaml.tailcall]) f xs | true -> true_s )

let exists_s f = function
  | [] ->
      false_s
  | x :: xs -> (
      Lwt.apply f x >>= function false -> exists_s f xs | true -> true_s )

let rec exists_es f = function
  | [] ->
      false_es
  | x :: xs -> (
      f x
      >>=? function
      | false -> (exists_es [@ocaml.tailcall]) f xs | true -> true_es )

let exists_es f = function
  | [] ->
      false_es
  | x :: xs -> (
      Lwt.apply f x >>=? function false -> exists_es f xs | true -> true_es )

let exists_ep f l = rev_map_ep f l >|=? exists Fun.id

let exists_p f l = rev_map_p f l >|= exists Fun.id

let for_all2_e ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
    | ([], []) ->
        true_e
    | (x :: xs, y :: ys) -> (
        f x y
        >>? function true -> (aux [@ocaml.tailcall]) xs ys | false -> false_e )
  in
  aux xs ys

let for_all2_s ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
    | ([], []) ->
        true_es
    | (x :: xs, y :: ys) -> (
        f x y
        >>= function
        | true -> (aux [@ocaml.tailcall]) xs ys | false -> false_es )
  in
  match (xs, ys) with
  | ([], _ :: _) | (_ :: _, []) ->
      fail when_different_lengths
  | ([], []) ->
      true_es
  | (x :: xs, y :: ys) -> (
      lwt_apply2 f x y >>= function true -> aux xs ys | false -> false_es )

let for_all2_es ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
    | ([], []) ->
        true_es
    | (x :: xs, y :: ys) -> (
        f x y
        >>=? function
        | true -> (aux [@ocaml.tailcall]) xs ys | false -> false_es )
  in
  match (xs, ys) with
  | ([], _ :: _) | (_ :: _, []) ->
      fail when_different_lengths
  | ([], []) ->
      true_es
  | (x :: xs, y :: ys) -> (
      lwt_apply2 f x y >>=? function true -> aux xs ys | false -> false_es )

let exists2_e ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], _ :: _) | (_ :: _, []) ->
        Error when_different_lengths
    | ([], []) ->
        false_e
    | (x :: xs, y :: ys) -> (
        f x y
        >>? function false -> (aux [@ocaml.tailcall]) xs ys | true -> true_e )
  in
  aux xs ys

let exists2_s ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
    | ([], []) ->
        false_es
    | (x :: xs, y :: ys) -> (
        f x y
        >>= function false -> (aux [@ocaml.tailcall]) xs ys | true -> true_es )
  in
  match (xs, ys) with
  | ([], _ :: _) | (_ :: _, []) ->
      fail when_different_lengths
  | ([], []) ->
      false_es
  | (x :: xs, y :: ys) -> (
      lwt_apply2 f x y >>= function false -> aux xs ys | true -> true_es )

let exists2_es ~when_different_lengths f xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | ([], _ :: _) | (_ :: _, []) ->
        fail when_different_lengths
    | ([], []) ->
        false_es
    | (x :: xs, y :: ys) -> (
        f x y
        >>=? function
        | false -> (aux [@ocaml.tailcall]) xs ys | true -> true_es )
  in
  match (xs, ys) with
  | ([], _ :: _) | (_ :: _, []) ->
      fail when_different_lengths
  | ([], []) ->
      false_es
  | (x :: xs, y :: ys) -> (
      lwt_apply2 f x y >>=? function false -> aux xs ys | true -> true_es )

let rev_partition_result xs =
  let rec aux oks errors = function
    | [] ->
        (oks, errors)
    | Ok ok :: xs ->
        (aux [@ocaml.tailcall]) (ok :: oks) errors xs
    | Error error :: xs ->
        (aux [@ocaml.tailcall]) oks (error :: errors) xs
  in
  aux [] [] xs

let partition_result xs =
  let (rev_oks, rev_errors) = rev_partition_result xs in
  (rev rev_oks, rev rev_errors)

let rev_partition_e f l =
  let rec aux trues falses = function
    | [] ->
        Ok (trues, falses)
    | x :: xs ->
        f x
        >>? fun b ->
        if b then (aux [@ocaml.tailcall]) (x :: trues) falses xs
        else (aux [@ocaml.tailcall]) trues (x :: falses) xs
  in
  aux [] [] l

let partition_e f l =
  rev_partition_e f l >|? fun (trues, falses) -> (rev trues, rev falses)

let rev_partition_s f l =
  let rec aux trues falses = function
    | [] ->
        Lwt.return (trues, falses)
    | x :: xs ->
        f x
        >>= fun b ->
        if b then (aux [@ocaml.tailcall]) (x :: trues) falses xs
        else (aux [@ocaml.tailcall]) trues (x :: falses) xs
  in
  match l with
  | [] ->
      Lwt.return ([], [])
  | x :: xs ->
      Lwt.apply f x >>= fun b -> if b then aux [x] [] xs else aux [] [x] xs

let partition_s f l =
  rev_partition_s f l >|= fun (trues, falses) -> (rev trues, rev falses)

let rev_partition_es f l =
  let rec aux trues falses = function
    | [] ->
        return (trues, falses)
    | x :: xs ->
        f x
        >>=? fun b ->
        if b then (aux [@ocaml.tailcall]) (x :: trues) falses xs
        else (aux [@ocaml.tailcall]) trues (x :: falses) xs
  in
  match l with
  | [] ->
      return ([], [])
  | x :: xs ->
      Lwt.apply f x >>=? fun b -> if b then aux [x] [] xs else aux [] [x] xs

let partition_es f l =
  rev_partition_es f l >|=? fun (trues, falses) -> (rev trues, rev falses)

let partition_ep f l =
  rev_map_ep (fun x -> f x >|=? fun b -> (b, x)) l
  >|=? fun bxs ->
  fold_left
    (fun (trues, falses) (b, x) ->
      if b then (x :: trues, falses) else (trues, x :: falses))
    ([], [])
    bxs

let partition_p f l =
  rev_map_p (fun x -> f x >|= fun b -> (b, x)) l
  >|= fun bxs ->
  fold_left
    (fun (trues, falses) (b, x) ->
      if b then (x :: trues, falses) else (trues, x :: falses))
    ([], [])
    bxs

let combine ~when_different_lengths xs ys =
  map2 ~when_different_lengths (fun x y -> (x, y)) xs ys

let rev_combine ~when_different_lengths xs ys =
  rev_map2 ~when_different_lengths (fun x y -> (x, y)) xs ys

let combine_with_leftovers xs ys =
  let rec aux rev_combined xs ys =
    match (xs, ys) with
    | ([], []) ->
        (rev rev_combined, None)
    | ((_ :: _ as left), []) ->
        (rev rev_combined, Some (`Left left))
    | ([], (_ :: _ as right)) ->
        (rev rev_combined, Some (`Right right))
    | (x :: xs, y :: ys) ->
        (aux [@ocaml.tailcall]) ((x, y) :: rev_combined) xs ys
  in
  aux [] xs ys

let combine_drop xs ys =
  let rec aux rev_combined xs ys =
    match (xs, ys) with
    | (x :: xs, y :: ys) ->
        (aux [@ocaml.tailcall]) ((x, y) :: rev_combined) xs ys
    | ([], []) | (_ :: _, []) | ([], _ :: _) ->
        rev rev_combined
  in
  aux [] xs ys

let rec compare ecomp xs ys =
  match (xs, ys) with
  | ([], []) ->
      0
  | ([], _ :: _) ->
      -1
  | (_ :: _, []) ->
      1
  | (x :: xs, y :: ys) ->
      let ec = ecomp x y in
      if ec = 0 then compare ecomp xs ys else ec

let rec equal eeq xs ys =
  match (xs, ys) with
  | ([], []) ->
      true
  | ([], _ :: _) | (_ :: _, []) ->
      false
  | (x :: xs, y :: ys) ->
      eeq x y && equal eeq xs ys
