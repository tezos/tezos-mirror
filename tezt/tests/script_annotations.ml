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

let typecheck_wrapper ?res (f : Client.t -> Process.t) client =
  match res with
  | None -> Process.check @@ f client
  | Some msg -> Process.check_error ~exit_code:1 ~msg @@ f client

let typecheck_data ?res ?legacy ~data ~typ client =
  typecheck_wrapper ?res (Client.spawn_typecheck_data ?legacy ~data ~typ) client

let typecheck_script ?res ?legacy ~script client =
  typecheck_wrapper
    ?res
    (Client.spawn_typecheck_script ?legacy ~scripts:[script])
    client

let register =
  Protocol.register_test
    ~__FILE__
    ~title:"Tests of Michelson annotations"
    ~tags:["client"; "michelson"; "annotations"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  (* annotation length limit positive case *)
  let* () =
    typecheck_data ~data:"3" ~typ:(sf "(int :%s)" @@ String.make 254 'a') client
  in
  (* annotation length limit negative case *)
  let* () =
    typecheck_data
      ~res:(rex "annotation exceeded maximum length \\(255 chars\\)")
      ~data:"3"
      ~typ:(sf "(int :%s)" @@ String.make 255 'a')
      client
  in
  (* alphabetic field annotation in type positive case *)
  let* () =
    typecheck_data ~data:"Pair 0 0" ~typ:"pair (nat %x) (int %y)" client
  in
  (* numeric field annotation in type positive case *)
  let* () =
    typecheck_data ~data:"Pair 0 0" ~typ:"pair (nat %1) (int %2)" client
  in
  (* field annotation with invalid characters in type *)
  let* () =
    typecheck_data
      ~res:(rex "unexpected annotation")
      ~data:"Pair 0 0"
      ~typ:"pair (nat %.) (int %.)"
      client
  in
  (* alphabetic field annotation in lambda data *)
  let* () =
    typecheck_data
      ~data:"{ CAR %x }"
      ~typ:"lambda (pair (nat %x) (int %y)) nat"
      client
  in
  (* numeric field annotation in lambda data *)
  let* () =
    typecheck_data
      ~data:"{ CAR %1 }"
      ~typ:"lambda (pair (nat %1) (int %2)) nat"
      client
  in
  (* field annotation with invalid characters in lambda data *)
  let* () =
    typecheck_data
      ~res:(rex "unexpected annotation")
      ~data:"{ CAR %. }"
      ~typ:"lambda (pair (nat %.) (int %.)) nat"
      client
  in
  (* LEGACY: until Nairobi alphabetic field annotation in parameter
     root was allowed in legacy mode *)
  let* () =
    typecheck_script
      ?res:
        (if Protocol.(number protocol >= number Oxford) then
         Some (rex "unexpected annotation")
        else None)
      ~legacy:true
      ~script:"parameter %r unit; storage unit; code { FAILWITH }"
      client
  in
  (* LEGACY: until Nairobi numeric field annotation in parameter root
     was allowed in legacy mode *)
  let* () =
    typecheck_script
      ~res:(rex "unexpected annotation")
      ~legacy:true
      ~script:"parameter %1 unit; storage unit; code { FAILWITH }"
      client
  in
  (* alphabetic field annotation in parameter root *)
  let* () =
    typecheck_script
      ~res:(rex "unexpected annotation")
      ~script:"parameter %r unit; storage unit; code { FAILWITH }"
      client
  in
  (* numeric field annotation in parameter root *)
  let* () =
    typecheck_script
      ~res:(rex "unexpected annotation")
      ~script:"parameter %1 unit; storage unit; code { FAILWITH }"
      client
  in
  (* field annotation with invalid characters in parameter root *)
  let* () =
    typecheck_script
      ~res:(rex "unexpected annotation")
      ~script:"parameter %. unit; storage unit; code { FAILWITH }"
      client
  in
  (* alphabetic field annotation in parameter root *)
  let* () =
    typecheck_script
      ~script:"parameter (unit %r); storage unit; code { FAILWITH }"
      client
  in
  (* numeric field annotation in parameter root *)
  let* () =
    typecheck_script
      ~script:"parameter (unit %1); storage unit; code { FAILWITH }"
      client
  in
  (* field annotation with invalid characters in parameter root *)
  let* () =
    typecheck_script
      ~res:(rex "unexpected annotation")
      ~script:"parameter (unit %.); storage unit; code { FAILWITH }"
      client
  in
  unit

let register ~protocols = register protocols
