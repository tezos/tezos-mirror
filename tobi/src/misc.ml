(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let ( // ) = Filename.concat

let default x = function None -> x | Some x -> x

module String_set = Set.Make (String)
module String_map = Map.Make (String)

let echo x = Printf.ksprintf print_endline x

let memoize f =
  let table = Hashtbl.create 64 in
  fun x ->
    match Hashtbl.find_opt table x with
    | Some y -> y
    | None ->
        let y = f x in
        Hashtbl.replace table x y ;
        y

let str_split_once str char =
  match String.index_opt str char with
  | None -> None
  | Some pos ->
      let left = String.sub str 0 pos in
      let right = String.sub str (pos + 1) (String.length str - pos - 1) in
      Some (left, right)

type 'a error = {code : 'a; message : string list}

type ('a, 'e) r = ('a, 'e error) result

let error ?(reason = []) code x =
  Printf.ksprintf (fun message -> Error {code; message = message :: reason}) x

let fail ?reason x = error ?reason `failed x

let unit = Ok ()

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let iter_r (type e) iter container f =
  let exception E of e error in
  try
    Ok
      (iter
         (fun x -> match f x with Ok x -> x | Error e -> raise (E e))
         container)
  with E e -> Error e

let list_iter_r l f = iter_r List.iter l f

let list_map_r list f =
  let rec list_map_r ?(acc = []) list f =
    match list with
    | [] -> Ok (List.rev acc)
    | head :: tail ->
        let* new_head = f head in
        list_map_r ~acc:(new_head :: acc) tail f
  in
  list_map_r list f
