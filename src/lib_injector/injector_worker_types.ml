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

module Request (Tag : TAG) (L1_operation : INJECTOR_OPERATION) = struct
  type removal_criteria =
    | Operation_tag of Tag.t
    | Order_below of (Z.t * bool)
    | Tag_and_order_below of (Tag.t * Z.t * bool)

  let removal_criteria_encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Operation_tag"
          (obj2 (req "request" (constant "only_tag")) (req "tag" Tag.encoding))
          (function Operation_tag tag -> Some ((), tag) | _ -> None)
          (fun ((), tag) -> Operation_tag tag);
        case
          (Tag 1)
          ~title:"Order_below"
          (obj3
             (req "request" (constant "only_order_below"))
             (req "order_below" z)
             (dft "drop_no_order" bool false))
          (function
            | Order_below (order_below, drop_no_order) ->
                Some ((), order_below, drop_no_order)
            | _ -> None)
          (fun ((), order_below, drop_no_order) ->
            Order_below (order_below, drop_no_order));
        case
          (Tag 2)
          ~title:"Tag_and_order_below"
          (obj4
             (req "request" (constant "tag_and_order"))
             (req "tag" Tag.encoding)
             (req "order_below" z)
             (dft "drop_no_order" bool false))
          (function
            | Tag_and_order_below (tag, order_below, drop_no_order) ->
                Some ((), tag, order_below, drop_no_order)
            | _ -> None)
          (fun ((), tag, order_below, drop_no_order) ->
            Tag_and_order_below (tag, order_below, drop_no_order));
      ]

  let pp_removal_criteria fmt = function
    | Operation_tag tag -> Format.fprintf fmt "only tag %a" Tag.pp tag
    | Order_below (order_below, drop_no_order) ->
        Format.fprintf
          fmt
          "Only order below %a, and drop no order: %b"
          Z.pp_print
          order_below
          drop_no_order
    | Tag_and_order_below (tag, order_below, drop_no_order) ->
        Format.fprintf
          fmt
          "Operation_tag %a, order below %a, and drop no order: %b"
          Tag.pp
          tag
          Z.pp_print
          order_below
          drop_no_order

  type ('a, 'b) t =
    | Inject : (unit, error trace) t
    | Clear : removal_criteria option -> (unit, error trace) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 2)
          ~title:"Inject"
          (obj1 (req "request" (constant "inject")))
          (function View Inject -> Some () | _ -> None)
          (fun () -> View Inject);
        case
          (Tag 3)
          ~title:"Clear"
          (obj2
             (req "request" (constant "clear"))
             (opt "criteria" removal_criteria_encoding))
          (function
            | View (Clear predicate) -> Some ((), predicate) | _ -> None)
          (fun ((), predicate) -> View (Clear predicate));
      ]

  let pp ppf (View r) =
    match r with
    | Inject -> Format.fprintf ppf "injection"
    | Clear predicate ->
        Format.fprintf
          ppf
          "clear %a"
          (Format.pp_print_option pp_removal_criteria)
          predicate
end
