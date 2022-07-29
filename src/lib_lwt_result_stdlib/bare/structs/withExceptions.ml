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

let invalid name loc =
  Invalid_argument (Printf.sprintf "%s called from %s" name loc)

module Option = struct
  let get ~loc = function
    | Some v -> v
    | None -> raise (invalid "Lwtreslib.WithExceptions.Option.get" loc)

  let to_exn ~none = function Some v -> v | None -> raise none

  let to_exn_f ~none = function Some v -> v | None -> raise (none ())
end

module Result = struct
  let get_ok ~loc = function
    | Ok v -> v
    | Error _ -> raise (invalid "Lwtreslib.WithExceptions.Result.get_ok" loc)

  let get_error ~loc = function
    | Error e -> e
    | Ok _ -> raise (invalid "Lwtreslib.WithExceptions.Result.get_error" loc)

  let to_exn = function Ok v -> v | Error exc -> raise exc

  let to_exn_f ~error = function Ok v -> v | Error b -> raise (error b)
end

module List = struct
  let rev_combine ~loc xs ys =
    let rec aux acc xs ys =
      match (xs, ys) with
      | [], [] -> acc
      | x :: xs, y :: ys -> aux ((x, y) :: acc) xs ys
      | [], _ :: _ | _ :: _, [] ->
          raise (invalid "Lwtreslib.WithExceptions.List.rev_combine" loc)
    in
    aux [] xs ys

  let combine ~loc xs ys =
    let rec aux acc xs ys =
      match (xs, ys) with
      | [], [] -> acc
      | x :: xs, y :: ys -> aux ((x, y) :: acc) xs ys
      | [], _ :: _ | _ :: _, [] ->
          raise (invalid "Lwtreslib.WithExceptions.List.combine" loc)
    in
    Stdlib.List.rev (aux [] xs ys)

  let map2 ~loc f l1 l2 =
    match List.map2 ~when_different_lengths:() f l1 l2 with
    | Ok v -> v
    | Error _ -> raise (invalid "Lwtreslib.WithExceptions.List.map2" loc)

  let init ~loc l f =
    if l < 0 then raise (invalid "Lwtreslib.WithExceptions.List.init" loc)
    else Stdlib.List.init l f
end
