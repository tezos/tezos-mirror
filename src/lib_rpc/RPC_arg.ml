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

type ('i, 'j) eq = ('i, 'j) Resto.eq = Eq : ('a, 'a) eq

include Resto.Arg

let uint =
  make
    ~name:"uint"
    ~destruct:(fun s ->
      match int_of_string_opt s with
      | None -> Error "Cannot parse natural number (number expected)"
      | Some n when n < 0 ->
          Error "Cannot parse natural number (positive number expected)"
      | Some n -> Ok n)
    ~construct:string_of_int
    ()

let uint31 =
  make
    ~name:"uint31"
    ~destruct:(fun s ->
      match Int32.of_string_opt s with
      | None -> Error "Cannot parse natural number (number expected)"
      | Some n when n < 0l ->
          Error "Cannot parse natural number (positive number expected)"
      | Some n -> Ok n)
    ~construct:Int32.to_string
    ()

let uint63 =
  make
    ~name:"uint63"
    ~destruct:(fun s ->
      match Int64.of_string_opt s with
      | None -> Error "Cannot parse natural number (number expected)"
      | Some n when n < 0L ->
          Error "Cannot parse natural number (positive number expected)"
      | Some n -> Ok n)
    ~construct:Int64.to_string
    ()

let eq_descr a b =
  String.equal a.name b.name && Option.equal String.equal a.descr b.descr
