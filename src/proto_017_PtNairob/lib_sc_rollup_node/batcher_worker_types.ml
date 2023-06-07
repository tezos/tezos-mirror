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
    | Register : string list -> (L2_message.hash list, error trace) t
    | New_head : Layer1.head -> (unit, error trace) t
    | Batch : (unit, error trace) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Register"
          (obj2
             (req "request" (constant "register"))
             (req "messages" (list L2_message.content_encoding)))
          (function
            | View (Register messages) -> Some ((), messages) | _ -> None)
          (fun ((), messages) -> View (Register messages));
        case
          (Tag 1)
          ~title:"New_head"
          (obj2
             (req "request" (constant "new_head"))
             (req "block" Layer1.head_encoding))
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
    | Register messages ->
        Format.fprintf ppf "register %d new L2 message" (List.length messages)
    | New_head {Layer1.hash; level} ->
        Format.fprintf
          ppf
          "switching to new L1 head %a at level %ld"
          Block_hash.pp
          hash
          level
    | Batch -> Format.pp_print_string ppf "batch"
end
