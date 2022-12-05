(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

type 'a case_incr = int -> 'a Data_encoding.case

let case_incr title enc test pack id =
  Data_encoding.case ~title (Tag id) enc test pack

let unit_case_incr title value id =
  Data_encoding.case
    ~title
    (Tag id)
    (Data_encoding.constant title)
    (fun test -> if test = value then Some () else None)
    (fun () -> value)

let union_incr cases = Data_encoding.union (List.mapi ( |> ) cases)

module Little_endian = struct
  let int32 =
    Data_encoding.(
      splitted
        ~binary:
          (conv
             (fun x ->
               let buffer = Bytes.create 4 in
               Bytes.set_int32_le buffer 0 x ;
               buffer)
             (fun buffer -> Bytes.get_int32_le buffer 0)
             (Fixed.bytes 4))
        ~json:int32)
end
