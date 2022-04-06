(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** [assert_lib] contains Alcotest convenience assertions depending on Tezos_base. *)

module Assert = Lib_test.Assert

module Crypto = struct
  let equal_operation ?msg op1 op2 =
    let eq op1 op2 =
      match (op1, op2) with
      | (None, None) -> true
      | (Some op1, Some op2) -> Tezos_base.Operation.equal op1 op2
      | _ -> false
    in
    let prn = function
      | None -> "none"
      | Some op ->
          Tezos_crypto.Operation_hash.to_b58check (Tezos_base.Operation.hash op)
    in
    Assert.equal ?msg ~prn ~eq op1 op2

  let equal_block ?msg st1 st2 =
    let eq st1 st2 = Tezos_base.Block_header.equal st1 st2 in
    let prn st =
      Format.asprintf
        "%a (%ld)"
        Tezos_crypto.Block_hash.pp
        (Tezos_base.Block_header.hash st)
        st.shell.level
    in
    Assert.equal ?msg ~prn ~eq st1 st2

  let equal_block_set ?msg set1 set2 =
    let b1 = Tezos_crypto.Block_hash.Set.elements set1
    and b2 = Tezos_crypto.Block_hash.Set.elements set2 in
    Assert.make_equal_list
      ?msg
      Tezos_crypto.Block_hash.equal
      Tezos_crypto.Block_hash.to_string
      b1
      b2

  let equal_block_map ?msg ~eq map1 map2 =
    let b1 = Tezos_crypto.Block_hash.Map.bindings map1
    and b2 = Tezos_crypto.Block_hash.Map.bindings map2 in
    Assert.make_equal_list
      ?msg
      (fun (h1, b1) (h2, b2) -> Tezos_crypto.Block_hash.equal h1 h2 && eq b1 b2)
      (fun (h1, _) -> Tezos_crypto.Block_hash.to_string h1)
      b1
      b2

  let equal_block_hash_list ?msg l1 l2 =
    let pr_block_hash = Tezos_crypto.Block_hash.to_short_b58check in
    Assert.make_equal_list
      ?msg
      Tezos_crypto.Block_hash.equal
      pr_block_hash
      l1
      l2

  let equal_block_descriptor ?msg bd1 bd2 =
    let eq (l1, h1) (l2, h2) =
      Int32.equal l1 l2 && Tezos_crypto.Block_hash.equal h1 h2
    in
    let prn (l, h) =
      Format.asprintf "(%ld, %a)" l Tezos_crypto.Block_hash.pp h
    in
    Assert.equal ?msg ~prn ~eq bd1 bd2
end

module Shell_services = struct
  let equal_history_mode ?msg hm1 hm2 =
    let eq hm1 hm2 = hm1 = hm2 in
    let prn = Format.asprintf "%a" Tezos_shell_services.History_mode.pp in
    Assert.equal ?msg ~prn ~eq hm1 hm2
end
