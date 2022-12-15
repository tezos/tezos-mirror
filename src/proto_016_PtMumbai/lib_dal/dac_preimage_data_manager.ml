(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

open Protocol
open Environment.Error_monad

type error += Cannot_write_dac_page_to_disk of string

let () =
  register_error_kind
    `Permanent
    ~id:"cannot_write_dac_page_to_disk"
    ~title:"Cannot write Dac page to disk"
    ~description:"Cannot write Dac page to disk"
    ~pp:(fun ppf b58_hash ->
      Format.fprintf ppf "Could not write dac page for hash %s" b58_hash)
    Data_encoding.(obj1 (req "hash" string))
    (function
      | Cannot_write_dac_page_to_disk b58_hash -> Some b58_hash | _ -> None)
    (fun b58_hash -> Cannot_write_dac_page_to_disk b58_hash)

module type REVEAL_HASH = module type of Sc_rollup_reveal_hash

module Make (Hash : REVEAL_HASH) = struct
  let path data_dir hash = Filename.(concat data_dir @@ Hash.to_b58check hash)

  let save_bytes data_dir hash page_contents =
    let open Lwt_result_syntax in
    let path = path data_dir hash in
    let*! result =
      Lwt_utils_unix.with_atomic_open_out path @@ fun chan ->
      Lwt_utils_unix.write_bytes chan page_contents
    in
    match result with
    | Ok () -> return ()
    | Error _ -> tzfail @@ Cannot_write_dac_page_to_disk (Hash.to_b58check hash)
end

module Reveal_hash = Make (Sc_rollup_reveal_hash)
