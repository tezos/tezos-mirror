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

(** Testing
    -------
    Component:    Crypto
    Invocation:   dune build @src/lib_crypto/runtest
    Subject:      Tests the consistency between the [DIRECT_HASH] and
                  [INCREMENTAL_HASH] interfaces of hashes SHA256 and
                  SHA512.
*)

module Hash_Properties (Desc : sig
  val name : string
end)
(X : Hacl.Hash.S) =
struct
  let pp_bytes fmt d = Crowbar.pp_string fmt (Bytes.to_string d)

  (** Verifies equivalence between the hash of [msg_s] obtained through the
      direct and incremental interface of [X].
   *)
  let test_prop_incremental_one (msg_s : string) =
    let st = X.init () in
    let msg = Bytes.of_string msg_s in
    X.update st msg ;
    let d = X.finish st in
    let d' = X.digest msg in
    Crowbar.check_eq ~pp:pp_bytes d d'

  (** Verifies equivalence between the hash of the concatenation of [msg_ss]
      obtained through the direct and incremental interface of [X].
   *)
  let test_prop_incremental_list (msg_ss : string list) =
    let st = X.init () in
    let msgs = List.map Bytes.of_string msg_ss in
    List.iter (X.update st) msgs ;
    let d = X.finish st in
    let d' = X.digest (Bytes.concat Bytes.empty msgs) in
    Crowbar.check_eq ~pp:pp_bytes d d'

  let () =
    let open Crowbar in
    add_test
      ~name:(Desc.name ^ "_incremental_one")
      [bytes]
      test_prop_incremental_one ;
    add_test
      ~name:(Desc.name ^ "_incremental_list")
      [list bytes]
      test_prop_incremental_list
end

module SHA256_Props =
  Hash_Properties
    (struct
      let name = "SHA256"
    end)
    (Hacl.Hash.SHA256)

module SHA512_Props =
  Hash_Properties
    (struct
      let name = "SHA512"
    end)
    (Hacl.Hash.SHA512)
