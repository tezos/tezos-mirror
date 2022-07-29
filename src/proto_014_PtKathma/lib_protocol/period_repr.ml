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

(* `Permanent *)
type error += Malformed_period of int64 | Invalid_arg | Period_overflow

let () =
  let open Data_encoding in
  (* Malformed period *)
  register_error_kind
    `Permanent
    ~id:"malformed_period"
    ~title:"Malformed period"
    ~description:"Period is negative."
    ~pp:(fun ppf period ->
      Format.fprintf ppf "The given period '%Ld' is negative " period)
    (obj1 (req "malformed_period" int64))
    (function Malformed_period n -> Some n | _ -> None)
    (fun n -> Malformed_period n) ;
  (* Invalid arg *)
  register_error_kind
    `Permanent
    ~id:"invalid_arg"
    ~title:"Invalid arg"
    ~description:"Negative multiple of periods are not allowed."
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid arg")
    empty
    (function Invalid_arg -> Some () | _ -> None)
    (fun () -> Invalid_arg) ;
  let title = "Period overflow" in
  register_error_kind
    `Permanent
    ~id:"period_overflow"
    ~title
    ~description:"Last operation generated an integer overflow."
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" title)
    empty
    (function Period_overflow -> Some () | _ -> None)
    (fun () -> Period_overflow)

module type INTERNAL = sig
  type t = private int64

  val create : int64 -> t option

  val zero : t

  val one : t

  val mult_ : t -> t -> t option

  val add_ : t -> t -> t option

  val encoding : t Data_encoding.t

  val rpc_arg : t RPC_arg.arg

  val pp : Format.formatter -> t -> unit

  include Compare.S with type t := t
end

(* Internal module implementing natural numbers using int64. These are different
   from usual (wrapping up) unsigned integers in that if one overflows the
   representation bounds for int64 through [add] or [mul], a [None] value is
   returned *)
module Internal : INTERNAL = struct
  type t = Int64.t

  let encoding =
    Data_encoding.(
      with_decoding_guard
        (fun t ->
          if Compare.Int64.(t >= 0L) then Ok ()
          else Error "Positive int64 required")
        int64)

  let rpc_arg = RPC_arg.uint63

  let pp ppf v = Format.fprintf ppf "%Ld" v

  include (Compare.Int64 : Compare.S with type t := t)

  let zero = 0L

  let one = 1L

  let create t = if t >= zero then Some t else None

  (* The create function is not used in the [mul_] and [add_] below to not add
      extra Some | None pattern matching to handle since the overflow checks are
      generic and apply as well to negative as positive integers .

     To handle overflows, both [add_] and [mult_] return option types. [None] is
      returned on detected overflow, [Some value] when everything went well. *)
  let mult_ a b =
    if a <> zero then
      let res = Int64.mul a b in
      if Int64.div res a <> b then None else Some res
    else Some zero

  let add_ a b =
    let res = Int64.add a b in
    if res < a || res < b then None else Some res
end

include Internal

type period = Internal.t

let to_seconds (t : Internal.t) = (t :> int64)

let of_seconds secs =
  match Internal.create secs with
  | Some v -> ok v
  | None -> error (Malformed_period secs)

let of_seconds_exn t =
  match Internal.create t with
  | Some t -> t
  | None -> invalid_arg "Period.of_seconds_exn"

let mult i p =
  match Internal.create (Int64.of_int32 i) with
  | None -> error Invalid_arg
  | Some iper -> (
      match Internal.mult_ iper p with
      | None -> error Period_overflow
      | Some res -> ok res)

let add p1 p2 =
  match Internal.add_ p1 p2 with
  | None -> error Period_overflow
  | Some res -> ok res

let ( +? ) = add

let one_second = Internal.one

let one_minute = of_seconds_exn 60L

let one_hour = of_seconds_exn 3600L
