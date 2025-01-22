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
  type ('a, 'b) t =
    | Inject : (unit, error trace) t
    | Clear : Tag.t option -> (unit, error trace) t

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
          (obj2 (req "request" (constant "clear")) (opt "tag" Tag.encoding))
          (function View (Clear tag) -> Some ((), tag) | _ -> None)
          (fun ((), tag) -> View (Clear tag));
      ]

  let pp ppf (View r) =
    match r with
    | Inject -> Format.fprintf ppf "injection"
    | Clear tag ->
        Format.fprintf ppf "clear %a" (Format.pp_print_option Tag.pp) tag
end
