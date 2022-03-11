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
open Protocol
open Alpha_context

module Request = struct
  type 'a t =
    | Register : {
        tr : L2_transaction.t;
        apply : bool;
        eager_batch : bool;
      }
        -> L2_transaction.hash t
    | New_head : L2block.t -> unit t
    | Batch : unit t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Register"
          (obj4
             (req "request" (constant "register"))
             (req "transaction" L2_transaction.encoding)
             (dft "apply" bool true)
             (dft "eager_batch" bool false))
          (function
            | View (Register {tr; apply; eager_batch}) ->
                Some ((), tr, apply, eager_batch)
            | _ -> None)
          (fun ((), tr, apply, eager_batch) ->
            View (Register {tr; apply; eager_batch}));
        case
          (Tag 1)
          ~title:"New_head"
          (obj2
             (req "request" (constant "new_head"))
             (req "block" L2block.encoding))
          (function View (New_head b) -> Some ((), b) | _ -> None)
          (fun ((), b) -> View (New_head b));
        case
          (Tag 2)
          ~title:"Batch"
          (obj1 (req "request" (constant "batch")))
          (function View Batch -> Some () | _ -> None)
          (fun () -> View Batch);
      ]

  let pp ppf (View r) =
    match r with
    | Register {tr; _} ->
        Format.fprintf
          ppf
          "register new L2 transaction %a"
          L2_transaction.Hash.pp
          (L2_transaction.hash tr)
    | New_head b ->
        Format.fprintf
          ppf
          "switching to new L2 head %a"
          L2block.Hash.pp
          (L2block.hash_header b.header)
    | Batch -> Format.pp_print_string ppf "batch"
end

module Name = struct
  type t = Tx_rollup.t

  let encoding = Tx_rollup.encoding

  let base = ["tx_rollup_batcher"]

  let pp = Tx_rollup.pp

  let equal = Tx_rollup.equal
end

module Dummy_event = struct
  type t = unit

  let pp = Format.pp_print_cut

  let encoding = Data_encoding.unit

  let level () = Internal_event.Debug
end

module Logger =
  Tezos_shell.Worker_logger.Make (Dummy_event) (Request)
    (struct
      let worker_name = "tx_rollup_batcher"
    end)
