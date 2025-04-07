(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

module Request (Tag : TAG) (L1_operation : INJECTOR_OPERATION) : sig
  (** Criteria to specify which operation to remove from the
      injector. *)
  type removal_criteria =
    | Operation_tag of Tag.t
        (** [Operation_tag kind] Clear all operation of type [kind] *)
    | Order_below of (Z.t * bool)
        (** [Order_below (order, drop_no_order] Clear all operation
            that have an order specified below [order]. If
            [drop_no_order] also clear all operation that has no order
            specified.  *)
    | Tag_and_order_below of (Tag.t * Z.t * bool)
        (** [Tag_and_order_below (kind, order, drop_no_order)] Apply
            {!Order_below} but only for operation of type [kind]. *)

  type ('a, 'b) t =
    | Inject : (unit, error trace) t
    | Clear : removal_criteria option -> (unit, error trace) t

  type view = View : _ t -> view

  include
    Worker_intf.REQUEST
      with type ('a, 'request_error) t := ('a, 'request_error) t
       and type view := view
end
