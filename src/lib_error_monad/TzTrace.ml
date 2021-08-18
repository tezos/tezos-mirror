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

(* INVARIANT: traces are never empty, they must contain at least one error *)
type 'err trace = 'err list

let make err = [err]

let cons err trace = err :: trace

let cons_list err errs = err :: errs

(* This is temporary. Eventually, the traces might have a more structured
   semantic. *)
let conp trace _trace = trace

let conp_list tr _trs = tr

let pp_print pp_error ppf = function
  | [] -> assert false
  | [error] -> Format.fprintf ppf "@[<v 2>Error:@ %a@]@." pp_error error
  | error :: _ as errors ->
      Format.fprintf
        ppf
        "@[<v 2>Error:@ %a,@ trace:@ %a@]@."
        pp_error
        error
        (Format.pp_print_list pp_error)
        (List.rev errors)

let pp_print_top pp_error fmt = function
  | [] -> assert false
  | error :: _ -> pp_error fmt error

let encoding error_encoding = Data_encoding.list error_encoding

let fold = List.fold_left
