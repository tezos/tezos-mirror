(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    | Publish : (unit, error trace) t
    | Cement : (unit, error trace) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Publish"
          (obj1 (req "request" (constant "publish")))
          (function View Publish -> Some () | _ -> None)
          (fun () -> View Publish);
        case
          (Tag 1)
          ~title:"Cement"
          (obj1 (req "request" (constant "cement")))
          (function View Cement -> Some () | _ -> None)
          (fun () -> View Cement);
      ]

  let pp ppf (View r) =
    match r with
    | Publish -> Format.pp_print_string ppf "publish"
    | Cement -> Format.pp_print_string ppf "cement"
end

module Name = struct
  (* We only have a single commitment publisher right now *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["sc_rollup_commitment_publisher"]

  let pp _ _ = ()

  let equal () () = true
end
