(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type ('a, 'store) t = {
  result : 'a;
  write : ('store -> unit tzresult Lwt.t) option;
}

let no_write result = {result; write = None}

let delay_write result write = {result; write = Some write}

let map f {result; write} = {result = f result; write}

let map_es f {result; write} =
  let open Lwt_result_syntax in
  let+ result = f result in
  {result; write}

let bind f {result; write = write1} =
  let open Lwt_result_syntax in
  let {result; write = write2} = f result in
  let write =
    match (write1, write2) with
    | None, None -> None
    | Some write, None | None, Some write -> Some write
    | Some write1, Some write2 ->
        Some
          (fun node_ctxt ->
            let* () = write1 node_ctxt in
            write2 node_ctxt)
  in
  {result; write}

let apply node_ctxt {result; write} =
  let open Lwt_result_syntax in
  let+ () =
    match write with None -> return_unit | Some write -> write node_ctxt
  in
  result

let ignore {result; _} = result

module Lwt_result_syntax = struct
  let bind f a =
    let open Lwt_result_syntax in
    let* a in
    let* b = f a.result in
    let write =
      match (a.write, b.write) with
      | None, None -> None
      | Some write, None | None, Some write -> Some write
      | Some write1, Some write2 ->
          Some
            (fun node_ctxt ->
              let* () = write1 node_ctxt in
              write2 node_ctxt)
    in
    return {result = b.result; write}

  let map f a = Lwt_result.map (map f) a

  let ( let>* ) a f = bind f a

  let ( let>+ ) a f = map f a

  let return x = Lwt_result.return (no_write x)

  let list_fold_left_i_es f acc l =
    List.fold_left_i_es
      (fun i acc x ->
        let>* acc = Lwt.return_ok acc in
        f i acc x)
      (no_write acc)
      l
end
