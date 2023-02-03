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

(* Testing
   -------
   Component:    Operation size
   Invocation:   dune exec tezt/tests/main.exe -- --file operation_size.ml
   Subject:      Tests handling oversized contract call arguments
*)

let test_operation_size =
  Protocol.register_test
    ~__FILE__
    ~title:"operation size munch and bytes contract"
    ~tags:["operation"; "script"; "size"; "munch"; "bytes"]
  @@ fun protocol ->
  let repeat_s n s =
    let buf = Buffer.create (n * String.length s) in
    let rec aux n =
      if n <= 0 then ()
      else (
        Buffer.add_string buf s ;
        aux (n - 1))
    in
    aux n ;
    Buffer.contents buf
  in
  let* _node, client = Client.init_with_protocol ~protocol `Client () in
  let* munch, _contract_address_munch =
    Client.originate_contract_at
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~src:"bootstrap1"
      client
      ["opcodes"; "munch"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let* bytes, _contract_address_bytes =
    Client.originate_contract_at
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~src:"bootstrap1"
      client
      ["opcodes"; "bytes"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let test (receiver, entrypoint, arg, expect_failure_message) =
    let transfer_process =
      Client.spawn_transfer
        ~log_output:false
        ~amount:(Tez.of_int 10)
        ~giver:"bootstrap1"
        ~receiver
        ?entrypoint
        ~arg
        client
    in
    match expect_failure_message with
    | None ->
        let* () = Process.check transfer_process in
        (* Bake to clear the mempool *)
        Client.bake_for_and_wait client
    | Some msg -> Process.check_error transfer_process ~msg:(rex msg)
  in
  Lwt_list.iter_s
    test
    [
      (* Operations that fit *)
      (bytes, None, (* 6 KB of data. *)
                    "0x" ^ repeat_s (6 * 1024) "00", None);
      (* Test that operations between 16KB and 32KB can be injected in the node. *)
      (bytes, None, (* 24 KB of data. *)
                    "0x" ^ repeat_s (24 * 1024) "00", None);
      (* Test that an operation of exactly 32KB can be injected in the node. *)
      ( bytes,
        None,
        (* 32 KB of data. The operation has a fixed overhead of 162 bytes *)
        (let overhead = 162 in
         "0x" ^ repeat_s ((32 * 1024) - overhead) "00"),
        None );
      (* Test that a large operation under 32KB can be injected in the node
         (variant using a big nat). *)
      ( munch,
        Some "nat",
        (* The encoding for nat uses a byte to encode 7 bits of the number so
           the size of 2 ** (7 * n) is about n bytes *)
        (let exp = 7 * 30 * 1024 in
         Z.(pow (of_int 2) exp |> to_string)),
        None );
      (* (variant using a lambda with deep nesting). *)
      ( munch,
        Some "lambda",
        (* Each pair of braces is encoded on 5 bytes so this takes
           5 * 6 * 1024 = 30 KB < 32KB *)
        (let n = 6 * 1024 in
         String.(make n '{' ^ make n '}')),
        None );
      (* (variant using a long list) *)
      ( munch,
        Some "list_nat",
        (* Each element in the list takes 2 bytes so about 30KB in total *)
        (let n = 15 * 1024 in
         "{" ^ repeat_s n "0; " ^ "}"),
        None );
      (* Over-sized operations *)
      (* Test that an operation of exactly 32KB + 1 byte cannot be injected in
         the node. *)
      ( bytes,
        None,
        (* 32 KB + 1 byte of data. The operation has a fixed overhead of 162 bytes. *)
        (let overhead = 162 in
         "0x" ^ repeat_s ((32 * 1024) - overhead + 1) "00"),
        Some "Oversized operation" );
      (* Test that a large operation over 32KB cannot be injected in the
         node, and the error is not a stack overflow *)
      ( bytes,
        None,
        (* 36 KB of data. *)
        "0x" ^ repeat_s (36 * 1024) "00",
        Some "Oversized operation" );
      (* (variant using a big nat). *)
      ( munch,
        Some "nat",
        (let exp = 7 * 33 * 1024 in
         Z.(pow (of_int 2) exp |> to_string)),
        Some "Oversized operation" );
      (* (variant using a lambda). *)
      ( munch,
        Some "lambda",
        (* Each pair of braces is encoded on 5 bytes so this takes
           5 * 6 * 1024 = 35 KB > 32KB *)
        (let n = 7 * 1024 in
         String.(make n '{' ^ make n '}')),
        Some "Oversized operation" );
      (* (variant using a long list) *)
      ( munch,
        Some "list_nat",
        (let n = 17 * 1024 in
         "{" ^ repeat_s n "0; " ^ "}"),
        Some "Oversized operation" );
      (* Syntax error *)
      ( munch,
        Some "list_nat",
        (let n = 15 * 1024 in
         "{" ^ repeat_s n "0; " ^ "'foo;'" ^ "}"),
        Some "transfer simulation failed" );
      (* Ill-typed *)
      ( munch,
        Some "list_nat",
        (let n = 15 * 1024 in
         "{" ^ repeat_s n "0; " ^ "Unit;" ^ "}"),
        Some "transfer simulation failed" );
    ]

let register ~protocols = test_operation_size protocols
