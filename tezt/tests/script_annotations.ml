(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(* Testing
   -------
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file script_annotations.ml
   Subject:      Runs Michelson annotation tests using [octez-client typecheck data ...].
*)

let register =
  Protocol.register_test
    ~__FILE__
    ~title:"Tests of Michelson annotations"
    ~tags:["client"; "michelson"; "annotations"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  (* annotation length limit positive case *)
  let* () =
    Client.typecheck_data
      ~data:"3"
      ~typ:(sf "(int :%s)" @@ String.make 254 'a')
      client
  in
  (* annotation length limit negative case *)
  let* () =
    Process.check_error
      ~exit_code:1
      ~msg:(rex "annotation exceeded maximum length \\(255 chars\\)")
    @@ Client.spawn_typecheck_data
         ~data:"3"
         ~typ:(sf "(int :%s)" @@ String.make 255 'a')
         client
  in
  (* alphabetic field annotation in type positive case *)
  let* () =
    Client.typecheck_data ~data:"Pair 0 0" ~typ:"pair (nat %x) (int %y)" client
  in
  (* numeric field annotation in type positive case *)
  let* () =
    Client.typecheck_data ~data:"Pair 0 0" ~typ:"pair (nat %1) (int %2)" client
  in
  (* field annotation with invalid characters in type *)
  let* () =
    Process.check_error ~exit_code:1 ~msg:(rex "unexpected annotation")
    @@ Client.spawn_typecheck_data
         ~data:"Pair 0 0"
         ~typ:"pair (nat %.) (int %.)"
         client
  in
  (* alphabetic field annotation in lambda data *)
  let* () =
    Client.typecheck_data
      ~data:"{ CAR %x }"
      ~typ:"lambda (pair (nat %x) (int %y)) nat"
      client
  in
  (* numeric field annotation in lambda data *)
  let* () =
    Client.typecheck_data
      ~data:"{ CAR %1 }"
      ~typ:"lambda (pair (nat %1) (int %2)) nat"
      client
  in
  (* field annotation with invalid characters in lambda data *)
  let* () =
    Process.check_error ~exit_code:1 ~msg:(rex "unexpected annotation")
    @@ Client.spawn_typecheck_data
         ~data:"{ CAR %. }"
         ~typ:"lambda (pair (nat %.) (int %.)) nat"
         client
  in
  (* LEGACY: alphabetic field annotation in parameter root *)
  let* () =
    Process.check_error ~exit_code:1 ~msg:(rex "unexpected annotation")
    @@ Client.spawn_typecheck_script
         ~legacy:true
         ~script:"parameter %r unit; storage unit; code { FAILWITH }"
         client
  in
  (* LEGACY: numeric field annotation in parameter root *)
  let* () =
    Process.check_error ~exit_code:1 ~msg:(rex "unexpected annotation")
    @@ Client.spawn_typecheck_script
         ~legacy:true
         ~script:"parameter %1 unit; storage unit; code { FAILWITH }"
         client
  in
  (* alphabetic field annotation in parameter root *)
  let* () =
    Process.check_error ~exit_code:1 ~msg:(rex "unexpected annotation")
    @@ Client.spawn_typecheck_script
         ~script:"parameter %r unit; storage unit; code { FAILWITH }"
         client
  in
  (* numeric field annotation in parameter root *)
  let* () =
    Process.check_error ~exit_code:1 ~msg:(rex "unexpected annotation")
    @@ Client.spawn_typecheck_script
         ~script:"parameter %1 unit; storage unit; code { FAILWITH }"
         client
  in
  (* field annotation with invalid characters in parameter root *)
  let* () =
    Process.check_error ~exit_code:1 ~msg:(rex "unexpected annotation")
    @@ Client.spawn_typecheck_script
         ~script:"parameter %. unit; storage unit; code { FAILWITH }"
         client
  in
  (* alphabetic field annotation in parameter root *)
  let* () =
    Client.typecheck_script
      ~script:"parameter (unit %r); storage unit; code { FAILWITH }"
      client
  in
  (* numeric field annotation in parameter root *)
  let* () =
    Client.typecheck_script
      ~script:"parameter (unit %1); storage unit; code { FAILWITH }"
      client
  in
  (* field annotation with invalid characters in parameter root *)
  let* () =
    Process.check_error ~exit_code:1 ~msg:(rex "unexpected annotation")
    @@ Client.spawn_typecheck_script
         ~script:"parameter (unit %.); storage unit; code { FAILWITH }"
         client
  in
  unit

let register ~protocols = register protocols
