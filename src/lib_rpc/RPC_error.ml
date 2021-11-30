(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let list ctxt = RPC_context.make_call RPC_service.error_service ctxt () () ()

let encoding = RPC_service.error_encoding

let opt_encoding = RPC_service.error_opt_encoding

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/1753

   the error should not be in the error monad. *)
type Error_monad.error += Bad_version of {given : int; supported : int list}

let () =
  let open Error_monad in
  register_error_kind
    `Permanent
    ~id:"RPC_error.bad_version"
    ~title:"Unknown RPC version"
    ~description:"The RPC was called with a bad version number."
    ~pp:(fun ppf (given, supported) ->
      Format.fprintf
        ppf
        "@[<v 2>The RPC was called with version number '%d' which is not \
         supported. Accepted version numbers are '%a'.@]@."
        given
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           Format.pp_print_int)
        supported)
    Data_encoding.(
      obj2 (req "given" int8) (req "supported" (list Data_encoding.int8)))
    (function
      | Bad_version {given; supported} -> Some (given, supported) | _ -> None)
    (fun (given, supported) -> Bad_version {given; supported})

let bad_version ~given ~supported : RPC_service.error =
  [Bad_version {given; supported}]
