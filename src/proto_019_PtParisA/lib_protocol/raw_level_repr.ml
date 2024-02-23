(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = int32

type raw_level = t

include (Compare.Int32 : Compare.S with type t := t)

module Set = Set.Make (Compare.Int32)
module Map = Map.Make (Compare.Int32)

let pp ppf level = Format.fprintf ppf "%ld" level

let rpc_arg =
  let construct raw_level = Int32.to_string raw_level in
  let destruct str =
    Int32.of_string_opt str |> Option.to_result ~none:"Cannot parse level"
  in
  RPC_arg.make
    ~descr:"A level integer"
    ~name:"block_level"
    ~construct
    ~destruct
    ()

let root = 0l

let succ = Int32.succ

let add l i =
  assert (Compare.Int.(i >= 0)) ;
  Int32.add l (Int32.of_int i)

let sub l i =
  assert (Compare.Int.(i >= 0)) ;
  let res = Int32.sub l (Int32.of_int i) in
  if Compare.Int32.(res >= 0l) then Some res else None

let pred l = if l = 0l then None else Some (Int32.pred l)

let pred_dontreturnzero l = if l <= 1l then None else Some (Int32.pred l)

let diff = Int32.sub

let to_int32 l = l

let to_int32_non_negative l =
  match Bounded.Non_negative_int32.of_value l with
  | Some x -> x
  | _ -> assert false (* invariant: raw_levels are non-negative *)

type error += Unexpected_level of Int32.t (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"unexpected_level"
    ~title:"Unexpected level"
    ~description:"Level must be non-negative."
    ~pp:(fun ppf l ->
      Format.fprintf
        ppf
        "The level is %s but should be non-negative."
        (Int32.to_string l))
    Data_encoding.(obj1 (req "level" int32))
    (function Unexpected_level l -> Some l | _ -> None)
    (fun l -> Unexpected_level l)

let of_int32 l =
  let open Result_syntax in
  if Compare.Int32.(l >= 0l) then return l else tzfail (Unexpected_level l)

let of_int32_exn l =
  match of_int32 l with
  | Ok l -> l
  | Error _ -> invalid_arg "Level_repr.of_int32"

let of_int32_non_negative l =
  match of_int32 (Bounded.Non_negative_int32.to_value l) with
  | Ok l -> l
  | Error _ -> assert false (* invariant: raw_levels are non-negative *)

let encoding =
  Data_encoding.conv_with_guard
    ~schema:Data_encoding.positive_int32_schema
    to_int32
    (fun l ->
      match of_int32 l with
      | Ok l -> Ok l
      | Error _ -> Error "Level_repr.of_int32")
    Data_encoding.int32

module Index = struct
  type t = raw_level

  let path_length = 1

  let to_path level l = Int32.to_string level :: l

  let of_path = function [s] -> Int32.of_string_opt s | _ -> None

  let rpc_arg = rpc_arg

  let encoding = encoding

  let compare = compare
end
