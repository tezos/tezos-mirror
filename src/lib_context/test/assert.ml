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
module A = Lib_test.Assert

let equal_key_dir_list ?loc ?msg l1 l2 =
  A.equal_list
    ?loc
    ~eq:( = )
    ~pp:
      (fun ppf -> function
        | `Key k -> Format.pp_print_string ppf @@ "Key " ^ String.concat "/" k
        | `Dir k -> Format.pp_print_string ppf @@ "Dir " ^ String.concat "/" k)
    ?msg
    l1
    l2

let equal_context_hash ?loc ?msg l1 l2 =
  A.equal ?loc ~eq:Context_hash.( = ) ~pp:Context_hash.pp ?msg l1 l2

let equal_context_hash_list ?loc ?msg l1 l2 =
  A.equal_list ?loc ~eq:Context_hash.( = ) ~pp:Context_hash.pp ?msg l1 l2

let equal_raw_tree ?loc ?msg r1 r2 =
  let rec aux r1 r2 =
    match (r1, r2) with
    | (`Value v1, `Value v2) ->
        A.Bytes.equal ?loc ?msg v1 v2 ;
        true
    | (`Tree t1, `Tree t2) ->
        if not (String.Map.equal aux t1 t2) then
          A.fail Format.pp_print_string "<tree>" "<tree>" ?msg ?loc
        else true
    | (`Tree _, `Value v) ->
        A.fail Format.pp_print_string "<tree>" (Bytes.to_string v) ?msg ?loc
    | (`Value v, `Tree _) ->
        A.fail Format.pp_print_string "<tree>" (Bytes.to_string v) ?msg ?loc
  in
  let _b : bool = aux r1 r2 in
  ()

include A
