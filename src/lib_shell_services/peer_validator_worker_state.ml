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

module Request = struct
  type view = New_head of Block_hash.t | New_branch of Block_hash.t * int

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"New_head"
          (obj2
             (req "request" (constant "new_head"))
             (req "block" Block_hash.encoding))
          (function New_head h -> Some ((), h) | _ -> None)
          (fun ((), h) -> New_head h);
        case
          (Tag 1)
          ~title:"New_branch"
          (obj3
             (req "request" (constant "new_branch"))
             (req "block" Block_hash.encoding)
             (req "locators" int31))
          (function New_branch (h, l) -> Some ((), h, l) | _ -> None)
          (fun ((), h, l) -> New_branch (h, l));
      ]

  let pp ppf = function
    | New_head hash -> Format.fprintf ppf "New head %a" Block_hash.pp hash
    | New_branch (hash, len) ->
        Format.fprintf
          ppf
          "New branch %a, locator length %d"
          Block_hash.pp
          hash
          len
end

type pipeline_length = {fetched_header_length : int; fetched_block_length : int}

let pipeline_length_encoding =
  let open Data_encoding in
  conv
    (function
      | {fetched_header_length; fetched_block_length} ->
          (fetched_header_length, fetched_block_length))
    (function
      | fetched_header_length, fetched_block_length ->
          {fetched_header_length; fetched_block_length})
    (obj2 (req "fetched_headers" int31) (req "fetched_blocks" int31))
