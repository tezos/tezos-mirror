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
    Invocation:   dune exec src/lib_hacl/test/main.exe
    Subject:      Tests the consistency between the [DIRECT_HASH] and
                  [INCREMENTAL_HASH] interfaces of hashes SHA256 and
                  SHA512.
*)

open Qcheck2_helpers
open QCheck2

module Hash_Properties (Desc : sig
  val name : string
end)
(X : Hacl.Hash.S) =
struct
  let pp_bytes fmt d = Format.fprintf fmt "%S" (Bytes.to_string d)

  (** Verifies equivalence between the hash of [msg_s] obtained through the
      direct and incremental interface of [X].
   *)
  let test_prop_incremental_one (msg_s : string) =
    let st = X.init () in
    let msg = Bytes.of_string msg_s in
    X.update st msg ;
    let expected = X.finish st in
    let actual = X.digest msg in
    qcheck_eq' ~pp:pp_bytes ~expected ~actual ()

  let test_prop_incremental_one =
    Test.make
      ~name:(Desc.name ^ "_incremental_one")
      ~print:Print.string
      Gen.string
      test_prop_incremental_one

  (** Verifies equivalence between the hash of the concatenation of [msg_ss]
      obtained through the direct and incremental interface of [X].
   *)
  let test_prop_incremental_list (msg_ss : string list) =
    let st = X.init () in
    let msgs = List.map Bytes.of_string msg_ss in
    List.iter (X.update st) msgs ;
    let expected = X.finish st in
    let actual = X.digest (Bytes.concat Bytes.empty msgs) in
    qcheck_eq' ~pp:pp_bytes ~expected ~actual ()

  let test_prop_incremental_list =
    Test.make
      ~name:(Desc.name ^ "_incremental_list")
      ~print:Print.(list string)
      Gen.(list string)
      test_prop_incremental_list

  let tests = [test_prop_incremental_one; test_prop_incremental_list]
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

let () =
  match Sys.backend_type with
  | Other "js_of_ocaml" -> ()
  | Other _ | Native | Bytecode ->
      Alcotest.run
        ~__FILE__
        "tezos-crypto-shaX-props"
        [
          ("SHA256_Props", qcheck_wrap SHA256_Props.tests);
          ("SHA512_Props", qcheck_wrap SHA512_Props.tests);
        ]
