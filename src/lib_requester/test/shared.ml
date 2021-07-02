(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Simple code sharing for files of this directory *)

module Parameters :
  Requester_impl.PARAMETERS with type key = string and type value = int = struct
  type key = string

  type value = int
end

module Hash : Requester.HASH with type t = Parameters.key = struct
  type t = Parameters.key

  let name = "test_with_key_string"

  let encoding = Data_encoding.string

  let pp = Format.pp_print_string
end

module Test_request = Requester_impl.Simple_request (Parameters)
module Test_disk_table_hash = Requester_impl.Disk_memory_table (Parameters)
module Test_Requester =
  Requester_impl.Make_memory_full_requester (Hash) (Parameters) (Test_request)

let init_full_requester_disk ?global_input () :
    Test_Requester.t * Test_Requester.store =
  let (st : Test_Requester.store) = Test_disk_table_hash.create 16 in
  let requester = Test_Requester.create ?global_input () st in
  (requester, st)

let init_full_requester ?global_input () : Test_Requester.t =
  fst (init_full_requester_disk ?global_input ())
