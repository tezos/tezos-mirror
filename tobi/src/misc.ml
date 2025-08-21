(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let sf = Format.asprintf

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

(* Copied from Tezt. *)
let quote_shell s =
  let contains_single_quote = ref false in
  let needs_quotes = ref false in
  let categorize = function
    | '\'' ->
        needs_quotes := true ;
        contains_single_quote := true
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '-' | '_' | '.' | '+' | '/' | ':' | '@' | '%' ->
        ()
    | _ -> needs_quotes := true
  in
  String.iter categorize s ;
  if not !needs_quotes then s
  else if not !contains_single_quote then "'" ^ s ^ "'"
  else Filename.quote s

let quote_command command arguments =
  String.concat " " (List.map quote_shell (command :: arguments))

let close fd = try Unix.close fd with Unix.Unix_error _ -> ()

let closedir fd = try Unix.closedir fd with Unix.Unix_error _ -> ()

type 'a error = {code : 'a; message : string list}

type ('a, 'e) r = ('a, 'e error) result

let error ?(reason = []) code x =
  Printf.ksprintf (fun message -> Error {code; message = message :: reason}) x

let fail ?reason x = error ?reason `failed x

let unit = Ok ()

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let wrap_errors context = function
  | Ok _ as x -> x
  | Error {code; message} -> Error {code; message = context :: message}

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

module PP = struct
  type t =
    | Bool of bool
    | Char of char
    | Int of int
    | Float of float
    | String of string
    | List of t list
    | Variant of string * t list
    | Tuple of t list
    | Record of (string * t) list

  let rec pp context_requires_parentheses fmt value =
    let pp_par = pp true in
    let pp = pp false in
    let fp x = Format.fprintf fmt x in
    match value with
    | Bool b -> fp "%b" b
    | Char c -> fp "%C" c
    | Int i -> fp "%d" i
    | Float f -> fp "%g" f
    | String s -> fp "%S" s
    | List [] -> fp "[]"
    | List [item] -> fp "[ %a ]" pp item
    | List (head :: tail) ->
        fp "@[@[<hv 2>[@ %a" pp head ;
        List.iter (fp ";@ %a" pp) tail ;
        fp "@]@ ]@]"
    | Variant (name, []) -> fp "%s" name
    | Variant (name, items) ->
        if context_requires_parentheses then
          fp "@[<hov 2>(%s@ %a)@]" name pp (Tuple items)
        else fp "@[<hov 2>%s@ %a@]" name pp (Tuple items)
    | Tuple [] -> fp "()"
    | Tuple [item] -> pp_par fmt item
    | Tuple (head :: tail) ->
        fp "@[<hov 2>(%a" pp head ;
        List.iter (fp ",@ %a" pp) tail ;
        fp ")@]"
    | Record [] -> fp "{}"
    | Record [(k, v)] -> fp "@[<hov 2>{ %s =@ %a }@]" k pp v
    | Record (head :: tail) ->
        let pp_item _fmt (k, v) = fp "@[<hov 2>%s =@ %a@]" k pp v in
        fp "@[@[<hv 2>{@ %a" pp_item head ;
        List.iter (fp ";@ %a" pp_item) tail ;
        fp "@]@ }@]"

  let pp = pp false
end
