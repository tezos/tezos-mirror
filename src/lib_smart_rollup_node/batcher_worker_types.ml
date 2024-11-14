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

module Request = struct
  type ('a, 'b) t =
    | Register : {
        messages : string list;
        drop_duplicate : bool;
      }
        -> (L2_message.id list, error trace) t
    | Produce_batches : (unit, error trace) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Register"
          (obj3
             (req "request" (constant "register"))
             (req "messages" (list L2_message.content_encoding))
             (req "drop_duplicate" bool))
          (function
            | View (Register {messages; drop_duplicate}) ->
                Some ((), messages, drop_duplicate)
            | _ -> None)
          (fun ((), messages, drop_duplicate) ->
            View (Register {messages; drop_duplicate}));
        case
          (Tag 1)
          ~title:"Produce_batches"
          (obj1 (req "request" (constant "produce_batches")))
          (function View Produce_batches -> Some () | _ -> None)
          (fun () -> View Produce_batches);
      ]

  let pp ppf (View r) =
    match r with
    | Register {messages; drop_duplicate} ->
        Format.fprintf
          ppf
          "register %d new L2 message%a"
          (List.length messages)
          (fun fmt () ->
            if drop_duplicate then Format.pp_print_string fmt ""
            else Format.fprintf fmt ", checking if message was already injected")
          ()
    | Produce_batches -> Format.fprintf ppf "Producing messages batches."
end
