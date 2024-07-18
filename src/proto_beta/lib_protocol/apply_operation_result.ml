(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Data_encoding

type ('kind, 'manager, 'successful) operation_result =
  | Applied of 'successful
  | Backtracked of 'successful * error trace option
  | Failed :
      'manager * error trace
      -> ('kind, 'manager, 'successful) operation_result
  | Skipped : 'manager -> ('kind, 'manager, 'successful) operation_result

let error_encoding =
  def
    "error"
    ~description:
      "The full list of RPC errors would be too long to include.\n\
       It is available at RPC `/errors` (GET).\n\
       Errors specific to protocol Alpha have an id that starts with \
       `proto.alpha`."
  @@ splitted
       ~json:
         (conv
            (fun err ->
              Data_encoding.Json.construct Error_monad.error_encoding err)
            (fun json ->
              Data_encoding.Json.destruct Error_monad.error_encoding json)
            json)
       ~binary:Error_monad.error_encoding

let trace_encoding = make_trace_encoding error_encoding
