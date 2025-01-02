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

type order_request = {drop_no_order : bool; order_below : Z.t}

module Request = struct
  type ('a, 'b) t =
    | Register : {
        order : Z.t option;
        messages : string list;
        drop_duplicate : bool;
      }
        -> (L2_message.id list, error trace) t
    | Produce_batches : (unit, error trace) t
    | Clear_queues : (unit, error trace) t
    | Remove_messages : order_request -> (unit, error trace) t

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
             (opt "order" n)
             (req "messages" (list L2_message.content_encoding))
             (req "drop_duplicate" bool))
          (function
            | View (Register {order; messages; drop_duplicate}) ->
                Some ((), order, messages, drop_duplicate)
            | _ -> None)
          (fun ((), order, messages, drop_duplicate) ->
            View (Register {order; messages; drop_duplicate}));
        case
          (Tag 1)
          ~title:"Produce_batches"
          (obj1 (req "request" (constant "produce_batches")))
          (function View Produce_batches -> Some () | _ -> None)
          (fun () -> View Produce_batches);
        case
          (Tag 2)
          ~title:"Clear_queues"
          (obj1 (req "request" (constant "clear_queues")))
          (function View Clear_queues -> Some () | _ -> None)
          (fun () -> View Clear_queues);
        case
          (Tag 3)
          ~title:"Remove_messages"
          (obj3
             (req "request" (constant "remove_operations"))
             (req "drop_no_order" bool)
             (req "order_below" n))
          (function
            | View (Remove_messages {drop_no_order; order_below}) ->
                Some ((), drop_no_order, order_below)
            | _ -> None)
          (fun ((), drop_no_order, order_below) ->
            View (Remove_messages {drop_no_order; order_below}));
      ]

  let pp ppf (View r) =
    match r with
    | Register {order = _; messages; drop_duplicate} ->
        Format.fprintf
          ppf
          "register %d new L2 message%s"
          (List.length messages)
          (if drop_duplicate then ", checking if messages were already injected"
           else "")
    | Produce_batches -> Format.fprintf ppf "Producing messages batches."
    | Clear_queues -> Format.fprintf ppf "Clear queues."
    | Remove_messages {drop_no_order; order_below} ->
        Format.fprintf
          ppf
          "Remove messages with order inferior to %a%s."
          Z.pp_print
          order_below
          (if drop_no_order then ",and drop messages with node order" else "")
end
