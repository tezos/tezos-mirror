(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Client
    Invocation:   dune exec src/proto_018_Proxford/lib_client/test/main.exe \
                  -- --file test_proxy.ml
    Subject:      Test of --mode proxy and tezos-proxy-server heuristic
*)

let proxy_mode_gen = QCheck2.Gen.oneofl Tezos_proxy.Proxy.[Client; Server]

let key_gen =
  (* Segments taken from the implementation of split_key in src/proto_alpha/lib_client/proxy.ml *)
  let keys =
    QCheck2.Gen.oneofl
      [
        "big_maps";
        "index";
        "contents";
        "contracts";
        "cycle";
        "cycle";
        "rolls";
        "owner";
        "snapshot";
        "v1";
      ]
    |> QCheck2.Gen.list
  in
  QCheck2.Gen.frequency QCheck2.Gen.[(9, keys); (1, list string)]

(** Whether [t1] is a prefix of [t2] *)
let rec is_prefix t1 t2 =
  match (t1, t2) with
  | [], _ -> true
  | _, [] -> false
  | x1 :: rest1, x2 :: rest2 when x1 = x2 -> is_prefix rest1 rest2
  | _ -> false

let test_split_key =
  let fmt =
    let pp_sep fmt () = Format.fprintf fmt "/" in
    Format.pp_print_list ~pp_sep Format.pp_print_string
  in
  QCheck2.Test.make
    ~name:"[fst (split_key s)] is a prefix of [s]"
    QCheck2.Gen.(pair proxy_mode_gen key_gen)
  @@ fun (mode, key) ->
  match Proxy.ProtoRpc.split_key mode key with
  | None -> true
  | Some (shorter, _) ->
      if is_prefix shorter key then true
      else
        QCheck2.Test.fail_reportf
          "Expected result of split_key to be a prefix of the input key. But \
           %a is not a prefix of %a."
          fmt
          shorter
          fmt
          key

let () =
  Alcotest.run
    ~__FILE__
    Protocol.name
    [("proxy", Qcheck2_helpers.qcheck_wrap [test_split_key])]
