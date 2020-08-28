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

let test_seeded_hash (module H : S.HASH) seed input =
  ignore @@ H.seeded_hash seed @@ H.hash_string input

let hashes =
  [ (module Chain_id : S.HASH);
    (module Block_hash : S.HASH);
    (module Operation_hash : S.HASH);
    (module Protocol_hash : S.HASH);
    (module Context_hash : S.HASH);
    (module Crypto_box.Public_key_hash : S.HASH) ]

let seeds = Array.init 256 (fun i -> i - 128)

let inputs =
  [ [];
    [""];
    ["0"];
    ["lkjahflkjahdlfkajshlfkjlfjhalsdfjhlasdjfhlasdjfhlasjdfhlajdflajshdflaksf"];
    ["asldkfj"; "alksjdhfl"; "lkajshdlfkasj"];
    [""; ""; ""; ""; ""; ""];
    ["\x00"; "\x00"; "a"; "\x00"; "a"; "a"] ]

let seeded_hash_safety () =
  ListLabels.iter hashes ~f:(fun hash ->
      ArrayLabels.iter seeds ~f:(fun seed ->
          ListLabels.iter inputs ~f:(fun input ->
              test_seeded_hash hash seed input)))

let seeded_hash_safety_test =
  [("seeded_hash safety", `Quick, seeded_hash_safety)]

let () = Alcotest.run "tezos-crypto" [("INDEXES", seeded_hash_safety_test)]
