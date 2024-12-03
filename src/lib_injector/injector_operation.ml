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

  type t = {
    id : id;
    order : Z.t option;
    counter : Z.t;
    operation : O.t;
    mutable errors : errors;
  }

  let hash_inner_operation nonce op =
    Id.hash_string
      [
        Option.fold ~none:"" ~some:Z.to_bits nonce;
        Data_encoding.Binary.to_string_exn O.encoding op;
      ]

  let counter = ref Z.zero

  let no_errors = {count = 0; last_error = None}

  let make ?order operation =
    let nonce_for_hash_id =
      if not @@ O.unique operation then Some !counter else None
    in
    let id = hash_inner_operation nonce_for_hash_id operation in
    let op = {id; order; counter = !counter; operation; errors = no_errors} in
    counter := Z.succ !counter ;
    op

  let errors_encoding =
    let open Data_encoding in
    conv
      (fun {count; last_error} -> (count, last_error))
      (fun (count, last_error) -> {count; last_error})
    @@ obj2 (req "count" int31) (opt "last_error" trace_encoding)

  let encoding =
    let open Data_encoding in
    conv
      (fun {id; order; counter; operation; errors} ->
        (id, order, counter, operation, errors))
      (fun (id, order, counter, operation, errors) ->
        {id; order; counter; operation; errors})
    @@ obj5
         (req "id" Id.encoding)
         (opt "order" n)
         (req "counter" n)
         (req "operation" O.encoding)
         (dft "errors" errors_encoding no_errors)

  let pp ppf {id; order; counter = _; operation; errors} =
    let pp_errors ppf errors =
      if errors.count = 0 then ()
      else Format.fprintf ppf " [%d errors]" errors.count
    in
    let pp_order =
      Format.pp_print_option @@ fun ppf ->
      Format.fprintf ppf " [priority order %a]" Z.pp_print
    in
    Format.fprintf
      ppf
      "%a%a (%a)%a"
      O.pp
      operation
      pp_order
      order
      Id.pp
      id
      pp_errors
      errors

  let register_error op error_trace =
    op.errors <- {count = op.errors.count + 1; last_error = Some error_trace}

  let id {id; _} = id

  (* Compare operations with the following logic:

       - Operations without an explicit `order` or lesser than operations with
         one
       - The value of `order` is used to compare two operations with `order` set;
       - Otherwise, use the timestamp to compare *)
  let compare op1 op2 =
    let op_compare = O.compare op1.operation op2.operation in
    if not (op_compare = 0) then op_compare
    else
      let counter_cmp () = Z.compare op1.counter op2.counter in
      match (op1.order, op2.order) with
      | Some o1, Some o2 ->
          let cmp = Z.compare o1 o2 in
          if cmp = 0 then counter_cmp () else cmp
      | None, None -> counter_cmp ()
      | Some _p, _ -> -1
      | _, Some _p -> 1
end
