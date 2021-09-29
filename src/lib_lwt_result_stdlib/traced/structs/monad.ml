(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Make (Trace : Traced_sigs.Trace.S) :
  Traced_sigs.Monad.S with type 'error trace = 'error Trace.trace = struct
  include Bare_structs.Monad

  type 'error trace = 'error Trace.trace

  module Traced_result_syntax = struct
    include Result_syntax

    let (fail[@ocaml.inline "always"]) = fun e -> fail (Trace.make e)

    let rec join_errors trace_acc = function
      | Ok _ :: ts -> join_errors trace_acc ts
      | Error trace :: ts -> join_errors (Trace.conp trace_acc trace) ts
      | [] -> Error trace_acc

    let rec join = function
      | [] -> Result_syntax.return_unit
      | Ok () :: ts -> join ts
      | Error trace :: ts -> join_errors trace ts

    let all ts =
      let rec aux acc = function
        | [] -> Ok (Stdlib.List.rev acc)
        | Ok v :: ts -> aux (v :: acc) ts
        | Error trace :: ts -> join_errors trace ts
      in
      aux [] ts

    let both a b =
      match (a, b) with
      | (Ok a, Ok b) -> Ok (a, b)
      | (Error err, Ok _) | (Ok _, Error err) -> Error err
      | (Error erra, Error errb) -> Error (Trace.conp erra errb)

    let ( and* ) = both

    let ( and+ ) = both
  end

  module Lwt_traced_result_syntax = struct
    include Lwt_result_syntax

    let (fail[@ocaml.inline "always"]) = fun e -> fail (Trace.make e)

    let join ts =
      let open Lwt_syntax in
      let+ ps = all ts in
      Traced_result_syntax.join ps

    let all ts =
      let open Lwt_syntax in
      let+ ps = all ts in
      Traced_result_syntax.all ps

    let both a b =
      let open Lwt_syntax in
      let+ (a, b) = both a b in
      Traced_result_syntax.both a b

    let ( and* ) = both

    let ( and+ ) = both
  end
end
