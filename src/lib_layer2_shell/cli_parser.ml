(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type 'a t = string list -> ('a * string list, string list) result

let fail fmt = Format.kasprintf (fun x -> Error [x]) fmt

let constant str = function
  | x :: rst when String.equal x str -> Ok ((), rst)
  | y :: _ -> fail "Expected %s, got %s" str y
  | _ -> fail "Expected %s, got nothing" str

let ( let+ ) (type a b) : a t -> (a -> b) -> b t =
 fun p1 f args ->
  let open Result_syntax in
  let+ x, args = p1 args in
  (f x, args)

let ( and+ ) (type a b) : a t -> b t -> (a * b) t =
 fun p1 p2 args ->
  let open Result_syntax in
  let* x, args = p1 args in
  let* y, args = p2 args in
  return ((x, y), args)

let ( let* ) (type a b) : a t -> (a -> b t) -> b t =
 fun p1 f args ->
  let open Result_syntax in
  let* x, args = p1 args in
  f x args

let return (type a) : a -> a t = fun x args -> Ok (x, args)

let switch char =
  let rec loop acc = function
    | x :: rst when String.equal x (Format.sprintf "-%c" char) -> Ok (true, rst)
    | x :: rst -> loop (x :: acc) rst
    | _ -> Ok (false, List.rev acc)
  in
  loop []

let long name =
  let rec loop acc = function
    | x :: rst -> (
        match String.remove_prefix x ~prefix:(Format.sprintf "--%s" name) with
        | Some "" -> (
            match rst with
            | y :: rst -> Ok (Some y, List.rev acc @ rst)
            | [] -> fail "Expected a value for --%s, got nothing" name)
        | Some arg
          when String.starts_with ~prefix:"=" arg && 1 < String.length arg ->
            Ok
              ( Some (String.sub arg 1 (String.length arg - 1)),
                List.rev acc @ rst )
        | Some _ | None -> loop (x :: acc) rst)
    | [] -> Ok (None, List.rev acc)
  in
  loop []

let default_long ~default name =
  let+ v = long name in
  match v with Some v -> v | None -> default

let short char =
  let rec loop acc = function
    | x :: rst -> (
        match String.remove_prefix x ~prefix:(Format.sprintf "-%c" char) with
        | Some "" -> (
            match rst with
            | y :: rst -> Ok (Some y, List.rev acc @ rst)
            | [] -> fail "Expected a value for -%c, got nothing" char)
        | Some arg -> Ok (Some arg, List.rev acc @ rst)
        | None -> loop (x :: acc) rst)
    | [] -> Ok (None, List.rev acc)
  in
  loop []

let rec pos_arg placeholder = function
  | x :: rst when String.starts_with ~prefix:"-" x -> pos_arg placeholder rst
  | x :: rst -> Ok (x, rst)
  | [] -> fail "Missing positional argument %s" placeholder

let validation_error fmt = Format.kasprintf (fun x -> Error x) fmt

let validate k p args =
  match p args with
  | Ok (x, args) -> (
      match k x with Ok () -> Ok (x, args) | Error err -> Error [err])
  | Error err -> Error err

let seal : 'a t -> 'a t =
 fun p args ->
  let f = fail in
  let open Result_syntax in
  let* x, args = p args in
  match args with
  | x :: _ -> f "Unexpected argument %s" x
  | [] -> return (x, args)

let union (type a) : a t list -> a t =
 fun parsers args ->
  let rec union errors parsers =
    match parsers with
    | p1 :: rst -> (
        match (seal p1) args with
        | Error err -> union (err @ errors) rst
        | Ok (x, rst) -> Ok (x, rst))
    | [] -> Error (List.rev errors)
  in
  union [] parsers

let select all_parsers =
  let rec select parsers args =
    match parsers with
    | (name, p) :: rst -> (
        match constant name args with
        | Ok ((), args) -> seal p args
        | Error _ -> select rst args)
    | [] ->
        fail
          "Expected one of the following command: %a"
          Format.(
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt ", ")
              pp_print_string)
          (List.map fst all_parsers)
  in
  select all_parsers

let fail fmt = Format.kasprintf (fun x _args -> Error [x]) fmt

let required_short char =
  let* res = short char in
  match res with
  | Some x -> return x
  | None -> fail "Expected option -%c which was missing" char

let default_int_positive_long ?(default = 0) name =
  let* v = default_long ~default:(string_of_int default) name in
  match int_of_string_opt v with
  | Some i when i >= 0 -> return i
  | _ -> fail "Expected a positive integer for --%s" name
