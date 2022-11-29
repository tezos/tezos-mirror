(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, Inc. <contact@nomadic-labs.com>          *)
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

let large_lists =
  (* strands of
     - 12 lists of consecutive lengths (the implementation uses chunks of 12 so
       it's important to test all modulos of those lengths)
     - spaced out by 1000-long chunks (arbitrary but also it's not 0mod12 so it
       helps spread out the tests)
     - starting at 4500 (so one test is under the cutoff of the 5000-long prefix
       in the lib and the rest is beyond that limit) *)
  let rec make12 acc prev i =
    if i < 0 then prev :: acc
    else
      let acc = prev :: acc in
      let prev = (6000 + i) :: prev in
      make12 acc prev (i - 1)
  in
  let make12 acc base = make12 acc base 12 in
  let rec make1000 acc prev i =
    if i < 0 then prev :: acc
    else
      let acc = make12 acc prev in
      let prev = List.rev_append (List.init 1000 Fun.id) prev in
      make1000 acc prev (i - 1)
  in
  make1000 [] (List.init 4500 Fun.id) 5

let genf =
  let open Crowbar in
  choose
    [
      const Fun.id;
      const succ;
      const pred;
      const (Fun.const 0);
      const (Stdlib.max 1024);
      const (Stdlib.min 1024);
    ]

let genl =
  let open Crowbar in
  let large_list_gen =
    List.map
      (fun l ->
        with_printer
          (fun fmt _ ->
            let l = List.length l in
            Format.fprintf fmt "large_list(length:%d,mod12:%d)" l (l mod 12))
          (const l))
      large_lists
  in
  choose (list int :: large_list_gen)

let test f l =
  Crowbar.check_eq
    Stdlib.List.(rev (rev_map f l))
    (Json_data_encoding_stdlib.List.map f l)

let () = Crowbar.add_test ~name:"tail-rec list map" [genf; genl] test

let genfi =
  let open Crowbar in
  choose
    [
      const ( + );
      const ( * );
      const Stdlib.max;
      const Stdlib.min;
      const (fun _ _ -> 0);
      const (fun i v -> Stdlib.max i (Stdlib.min 1024 v));
    ]

let rev_mapi f l =
  let rec rev_mapi acc i xs =
    match xs with [] -> acc | x :: xs -> rev_mapi (f i x :: acc) (i + 1) xs
  in
  rev_mapi [] 0 l

let testi f l =
  Crowbar.check_eq
    (Stdlib.List.rev (rev_mapi f l))
    (Json_data_encoding_stdlib.List.mapi f l)

let () = Crowbar.add_test ~name:"tail-rec list mapi" [genfi; genl] testi

let testa l1 l2 =
  Crowbar.check_eq
    Stdlib.List.(rev_append (List.rev l1) l2)
    (Json_data_encoding_stdlib.List.append l1 l2)

let () = Crowbar.add_test ~name:"tail-rec list append" [genl; genl] testa
