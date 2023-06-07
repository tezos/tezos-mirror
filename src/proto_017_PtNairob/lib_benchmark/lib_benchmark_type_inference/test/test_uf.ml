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

let _ =
  print_newline () ;
  Printf.printf "Testing union-find algorithm\n"

module UF = Uf.UF

let test =
  let open UF.M in
  UF.add 0 >>= fun () ->
  UF.add 1 >>= fun () ->
  UF.add 2 >>= fun () ->
  UF.add 3 >>= fun () ->
  UF.add 4 >>= fun () ->
  UF.find 0 >>= fun v0_repr ->
  UF.find 1 >>= fun v1_repr ->
  assert (v0_repr <> v1_repr) ;
  UF.union 0 1 >>= fun _ ->
  UF.find 0 >>= fun v0_repr ->
  UF.find 1 >>= fun v1_repr ->
  UF.find 2 >>= fun v2_repr ->
  assert (v0_repr = v1_repr) ;
  assert (v0_repr <> v2_repr) ;
  UF.union 2 3 >>= fun _ ->
  UF.union 0 3 >>= fun _ ->
  UF.find 1 >>= fun v1_repr ->
  UF.find 2 >>= fun v2_repr ->
  UF.find 3 >>= fun v3_repr ->
  UF.find 4 >>= fun v4_repr ->
  assert (v1_repr = v2_repr) ;
  UF.union 4 4 >>= fun _ ->
  assert (v3_repr <> v4_repr) ;
  UF.show >>= fun s ->
  Printf.printf "UF state:%s\n" s ;
  return ()

let () = UF.M.run test

let _ = Printf.printf "Success.\n"
