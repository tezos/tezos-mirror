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

open Injector_sigs

module Make (O : PARAM_OPERATION) :
  INJECTOR_OPERATION with type operation = O.t = struct
  type operation = O.t

  type id = Id.t

  type errors = {count : int; last_error : tztrace option}

  type t = {id : id; operation : O.t; mutable errors : errors}

  let hash_inner_operation nonce op =
    Id.hash_string
      [
        Option.fold ~none:"" ~some:Z.to_bits nonce;
        Data_encoding.Binary.to_string_exn O.encoding op;
      ]

  let counter = ref Z.zero

  let no_errors = {count = 0; last_error = None}

  let make operation =
    let nonce =
      if not @@ O.unique operation then (
        let c = !counter in
        counter := Z.succ !counter ;
        Some c)
      else None
    in
    let id = hash_inner_operation nonce operation in
    {id; operation; errors = no_errors}

  let errors_encoding =
    let open Data_encoding in
    conv
      (fun {count; last_error} -> (count, last_error))
      (fun (count, last_error) -> {count; last_error})
    @@ obj2 (req "count" int31) (opt "last_error" trace_encoding)

  let encoding =
    let open Data_encoding in
    conv
      (fun {id; operation; errors} -> (id, operation, errors))
      (fun (id, operation, errors) -> {id; operation; errors})
    @@ obj3
         (req "id" Id.encoding)
         (req "operation" O.encoding)
         (dft "errors" errors_encoding no_errors)

  let pp ppf {id; operation; errors} =
    let pp_errors ppf errors =
      if errors.count = 0 then ()
      else Format.fprintf ppf " [%d errors]" errors.count
    in
    Format.fprintf ppf "%a (%a)%a" O.pp operation Id.pp id pp_errors errors

  let register_error op error_trace =
    op.errors <- {count = op.errors.count + 1; last_error = Some error_trace}
end
