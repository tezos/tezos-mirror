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
  let equal_operation ?loc ?msg op1 op2 =
    let eq = Option.equal Tezos_base.Operation.equal in
    let pp ppf = function
      | None -> Format.pp_print_string ppf "none"
      | Some op ->
          Format.pp_print_string ppf
          @@ Tezos_crypto.Operation_hash.to_b58check
               (Tezos_base.Operation.hash op)
    in
    Assert.equal ?loc ?msg ~pp ~eq op1 op2

  let equal_block ?loc ?msg st1 st2 =
    let eq st1 st2 = Tezos_base.Block_header.equal st1 st2 in
    let pp ppf st =
      Format.fprintf
        ppf
        "%a (%ld)"
        Tezos_crypto.Block_hash.pp
        (Tezos_base.Block_header.hash st)
        st.shell.level
    in
    Assert.equal ?loc ?msg ~pp ~eq st1 st2

  let equal_block_set ?loc ?msg set1 set2 =
    let b1 = Tezos_crypto.Block_hash.Set.elements set1
    and b2 = Tezos_crypto.Block_hash.Set.elements set2 in
    Assert.equal_list
      ?loc
      ?msg
      ~eq:Tezos_crypto.Block_hash.equal
      ~pp:Tezos_crypto.Block_hash.pp
      b1
      b2

  let equal_block_map ~eq ?loc ?msg map1 map2 =
    let b1 = Tezos_crypto.Block_hash.Map.bindings map1
    and b2 = Tezos_crypto.Block_hash.Map.bindings map2 in
    Assert.equal_list
      ?loc
      ?msg
      ~eq:(fun (h1, b1) (h2, b2) ->
        Tezos_crypto.Block_hash.equal h1 h2 && eq b1 b2)
      ~pp:(fun ppf (h, _) -> Tezos_crypto.Block_hash.pp ppf h)
      b1
      b2

  let equal_block_hash_list ?loc ?msg l1 l2 =
    let pp = Tezos_crypto.Block_hash.pp_short in
    Assert.equal_list ?loc ?msg ~eq:Tezos_crypto.Block_hash.equal ~pp l1 l2

  let equal_block_descriptor ?loc ?msg bd1 bd2 =
    let eq (l1, h1) (l2, h2) =
      Int32.equal l1 l2 && Tezos_crypto.Block_hash.equal h1 h2
    in
    let pp ppf (l, h) =
      Format.fprintf ppf "(%ld, %a)" l Tezos_crypto.Block_hash.pp h
    in
    Assert.equal ?loc ?msg ~pp ~eq bd1 bd2
end

module Shell_services = struct
  let equal_history_mode ?loc ?msg hm1 hm2 =
    let eq = ( = ) in
    let pp = Tezos_shell_services.History_mode.pp in
    Assert.equal ?loc ?msg ~pp ~eq hm1 hm2
end

module Raw_Tree = struct
  let equal ?loc ?msg r1 r2 =
    let rec aux r1 r2 =
      match (r1, r2) with
      | (`Value v1, `Value v2) ->
          Assert.Bytes.equal ?loc ?msg v1 v2 ;
          true
      | (`Tree t1, `Tree t2) ->
          if not (Tezos_base.TzPervasives.String.Map.equal aux t1 t2) then
            Assert.String.fail "<tree>" "<tree>" ?msg ?loc
          else true
      | (`Tree _, `Value v) ->
          Assert.String.fail ?loc ?msg "<tree>" (Bytes.to_string v)
      | (`Value v, `Tree _) ->
          Assert.String.fail ?loc ?msg (Bytes.to_string v) "<tree>"
    in
    let _b : bool = aux r1 r2 in
    ()
end
