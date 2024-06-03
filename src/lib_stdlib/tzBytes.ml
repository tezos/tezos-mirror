(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
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

external logand : bytes -> bytes -> bytes = "bytes_logand"

external logor : bytes -> bytes -> bytes = "bytes_logor"

external logxor : bytes -> bytes -> bytes = "bytes_logxor"

external lognot : bytes -> bytes = "bytes_lognot"

external shift_left : bytes -> int -> bytes = "bytes_shift_left"

external shift_right : bytes -> int -> bytes = "bytes_shift_right"

let chunk_bytes_strict error_on_partial_chunk n b =
  let l = Bytes.length b in
  if l mod n <> 0 then Error error_on_partial_chunk
  else
    let rec split seq offset =
      if offset = l then List.rev seq
      else
        let s = Bytes.sub b offset n in
        split (s :: seq) (offset + n)
    in
    Ok (split [] 0)

let chunk_bytes_loose n b =
  let l = Bytes.length b in
  let rec split seq offset =
    if offset = l then List.rev seq
    else if offset + n > l then List.rev (Bytes.sub b offset (l - offset) :: seq)
    else
      let s = Bytes.sub b offset n in
      split (s :: seq) (offset + n)
  in
  split [] 0

let chunk_bytes ?error_on_partial_chunk n b =
  if n <= 0 then raise @@ Invalid_argument "chunk_bytes"
  else
    match error_on_partial_chunk with
    | Some error_on_partial_chunk ->
        chunk_bytes_strict error_on_partial_chunk n b
    | None -> Ok (chunk_bytes_loose n b)
