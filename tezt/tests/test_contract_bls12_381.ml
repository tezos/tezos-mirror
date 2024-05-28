(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Michelson: bls12_381
   Invocation:   dune exec tezt/tests/main.exe -- --file test_contract_bls12_381.ml
   Subject:      Test Michelson bls12_381 primitives
*)

let random_iterations = 10

let hooks = Tezos_regression.hooks

let check_contract protocol client ~contract ~input ~expected_storage =
  let* {storage; _} =
    Client.run_script_at ~hooks ~storage:"None" ~input client contract protocol
  in
  Check.(
    (storage = Format.asprintf "(Some %s)" expected_storage)
      ~__LOC__
      string
      ~error_msg:"Expected results %R, got %L") ;
  unit

let check_binop protocol client ~contract ~arg0 ~arg1 ~expected_storage =
  check_contract
    protocol
    client
    ~contract
    ~input:(sf "Pair %s %s" arg0 arg1)
    ~expected_storage

(* prefix a type name with 'bls12_381_' *)
let bls tname = "bls12_381_" ^ tname

(* Bundles a field/curve with a set of utility and test functions *)
module type CLASS = sig
  type t

  val name : string

  val zero : t

  val one : t

  val negate : t -> t

  val add : t -> t -> t

  val mul : t -> Bls12_381.Fr.t -> t

  val random : state:Random.State.t -> t

  val gen : state:Random.State.t -> int -> t list

  val to_hex_string : t -> string

  val check_store : Protocol.t -> Client.t -> t -> unit Lwt.t

  val check_add : Protocol.t -> Client.t -> t -> t -> unit Lwt.t

  val check_mul : Protocol.t -> Client.t -> t -> Bls12_381.Fr.t -> unit Lwt.t

  val check_negate : Protocol.t -> Client.t -> t -> unit Lwt.t
end

(* Creates a CLASS from modules in Bls12_381 *)
module Make (M : sig
  type t

  val zero : t

  val one : t

  val name : string

  val to_bytes : t -> bytes

  val add : t -> t -> t

  val negate : t -> t

  val mul : t -> Bls12_381.Fr.t -> t

  val random : ?state:Random.State.t -> unit -> t
end) : CLASS with type t = M.t = struct
  include M

  let hex_to_string hex = Format.asprintf "0x%a" Hex.pp hex

  let to_hex_string v = hex_to_string @@ Hex.of_bytes @@ M.to_bytes v

  let random ~state = M.random ~state ()

  let gen ~state n = [one; zero] @ List.init n (fun _ -> random ~state)

  let check_store protocol client arg =
    let input = to_hex_string arg in
    check_contract
      protocol
      client
      ~contract:["opcodes"; "store_" ^ bls name]
      ~input
      ~expected_storage:input

  let check_add protocol client arg0 arg1 =
    let expected_storage = to_hex_string (add arg0 arg1) in
    let arg0 = to_hex_string arg0 in
    let arg1 = to_hex_string arg1 in
    check_binop
      protocol
      client
      ~contract:["opcodes"; "add_" ^ bls name]
      ~expected_storage
      ~arg0
      ~arg1

  let check_mul protocol client arg0 arg1 =
    let expected_storage = to_hex_string (mul arg0 arg1) in
    let arg0 = to_hex_string arg0 in
    let fr_to_hex v = Hex.of_bytes @@ Bls12_381.Fr.to_bytes v in
    let arg1 = hex_to_string (fr_to_hex arg1) in
    check_binop
      protocol
      client
      ~contract:["opcodes"; "mul_" ^ bls name]
      ~expected_storage
      ~arg0
      ~arg1

  let check_negate protocol client arg =
    let input = to_hex_string arg in
    let expected_storage = to_hex_string (negate arg) in
    check_contract
      protocol
      client
      ~contract:["opcodes"; "neg_" ^ bls name]
      ~input
      ~expected_storage
end

module Fr : CLASS with type t = Bls12_381.Fr.t = Make (struct
  include Bls12_381.Fr

  let name = "fr"
end)

module G1 = Make (struct
  include Bls12_381.G1

  let name = "g1"
end)

module G2 = Make (struct
  include Bls12_381.G2

  let name = "g2"
end)

let check_pairing_check protocol client args =
  let expected_storage =
    if Bls12_381.Pairing.pairing_check args then "True" else "False"
  in
  let input =
    "{"
    ^ String.concat
        "; "
        (List.map
           (fun (g1_point, g2_point) ->
             sf
               "Pair %s %s"
               (G1.to_hex_string g1_point)
               (G2.to_hex_string g2_point))
           args)
    ^ "}"
  in
  check_contract
    protocol
    client
    ~contract:["opcodes"; "pairing_check"]
    ~input
    ~expected_storage

let register ~protocols =
  let iter xs f = Lwt_list.iter_s f xs in
  let iter2 xs ys f =
    iter xs @@ fun x ->
    iter ys @@ fun y -> f x y
  in
  List.iter
    (fun cls ->
      let module Class = (val cls : CLASS) in
      Protocol.register_regression_test
        ~__FILE__
        ~title:(sf "Bls12_381 contract primitives, %s: store" Class.name)
        ~tags:
          ["michelson"; "crypto"; "contract"; "bls12_381"; Class.name; "store"]
        ~uses_node:false
        (fun protocol ->
          let open Class in
          let state = Random.State.make [||] in
          let* client = Client.init_mockup ~protocol () in
          iter (gen ~state random_iterations) @@ check_store protocol client)
        protocols ;
      Protocol.register_regression_test
        ~__FILE__
        ~title:(sf "Bls12_381 contract primitives, %s: add" Class.name)
        ~tags:
          ["michelson"; "crypto"; "contract"; "bls12_381"; Class.name; "add"]
        ~uses_node:false
        (fun protocol ->
          let open Class in
          let state = Random.State.make [||] in
          let* client = Client.init_mockup ~protocol () in
          let args = gen ~state random_iterations in
          iter2 args args @@ check_add protocol client)
        protocols ;
      Protocol.register_regression_test
        ~__FILE__
        ~title:(sf "Bls12_381 contract primitives, %s: mul" Class.name)
        ~tags:
          ["michelson"; "crypto"; "contract"; "bls12_381"; Class.name; "mul"]
        ~uses_node:false
        (fun protocol ->
          let open Class in
          let state = Random.State.make [||] in
          let* client = Client.init_mockup ~protocol () in
          let args = gen ~state random_iterations in
          let args_scalar = Fr.gen ~state random_iterations in
          iter2 args args_scalar @@ check_mul protocol client)
        protocols ;
      Protocol.register_regression_test
        ~__FILE__
        ~title:(sf "Bls12_381 contract primitives, %s: negate" Class.name)
        ~tags:
          ["michelson"; "crypto"; "contract"; "bls12_381"; Class.name; "negate"]
        ~uses_node:false
        (fun protocol ->
          let open Class in
          let state = Random.State.make [||] in
          let* client = Client.init_mockup ~protocol () in
          let args = gen ~state random_iterations in
          iter args (check_negate protocol client))
        protocols ;
      ())
    [(module Fr : CLASS); (module G1 : CLASS); (module G2 : CLASS)] ;
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "Bls12_381 contract primitives: pairing_check empty")
    ~tags:["michelson"; "crypto"; "contract"; "bls12_381"; "pairing_check"]
    ~uses_node:false
    (fun protocol ->
      let* client = Client.init_mockup ~protocol () in
      check_pairing_check protocol client [])
    protocols ;
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "Bls12_381 contract primitives: pairing_check(pos, pos)")
    ~tags:["michelson"; "crypto"; "contract"; "bls12_381"; "pairing_check"]
    ~uses_node:false
    (fun protocol ->
      let state = Random.State.make [||] in
      let* client = Client.init_mockup ~protocol () in
      let args_g1 = G1.(gen ~state random_iterations) in
      let args_g2 = G2.(gen ~state random_iterations) in
      iter2 args_g1 args_g2 @@ fun g1 g2 ->
      check_pairing_check protocol client [(g1, g2)])
    protocols ;
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "Bls12_381 contract primitives: pairing_check(neg, pos)")
    ~tags:["michelson"; "crypto"; "contract"; "bls12_381"; "pairing_check"]
    ~uses_node:false
    (fun protocol ->
      let state = Random.State.make [||] in
      let* client = Client.init_mockup ~protocol () in
      let args_g1 = G1.(gen ~state random_iterations) in
      let args_g2 = G2.(gen ~state random_iterations) in
      iter2 args_g1 args_g2 @@ fun g1 g2 ->
      check_pairing_check protocol client [(G1.negate g1, g2)])
    protocols ;
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "Bls12_381 contract primitives: pairing_check(pos, neg)")
    ~tags:["michelson"; "crypto"; "contract"; "bls12_381"; "pairing_check"]
    ~uses_node:false
    (fun protocol ->
      let state = Random.State.make [||] in
      let* client = Client.init_mockup ~protocol () in
      let args_g1 = G1.(gen ~state random_iterations) in
      let args_g2 = G2.(gen ~state random_iterations) in
      iter2 args_g1 args_g2 @@ fun g1 g2 ->
      check_pairing_check protocol client [(g1, G2.negate g2)])
    protocols ;
  (* Pairing Check test based on signature aggregation *)
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "Bls12_381 contract primitives: signature_aggregation")
    ~tags:
      [
        "michelson";
        "crypto";
        "contract";
        "bls12_381";
        "pairing_check";
        "signature_aggregation";
      ]
    ~uses_node:false
    (fun protocol ->
      let* client = Client.init_mockup ~protocol () in
      repeat random_iterations @@ fun () ->
      let state = Random.State.make [||] in

      (* secret key *)
      let sk0 = Fr.random ~state in
      (* public key *)
      let pk0 = G2.mul G2.one sk0 in
      (* we don't have hash-to-curve on g1, so compute a random point *)
      let msg_hash = G1.random ~state in
      let sig0 = G1.mul msg_hash sk0 in
      let args0 = [(msg_hash, pk0); (G1.negate sig0, G2.one)] in
      let* () = check_pairing_check protocol client args0 in

      (* secret key *)
      let sk1 = Fr.random ~state in
      (* public key *)
      let pk1 = G2.mul G2.one sk1 in
      (* we don't have hash-to-curve on g1, so compute a random point *)
      let sig1 = G1.mul msg_hash sk1 in
      let args1 =
        [
          (G1.add msg_hash msg_hash, G2.add pk0 pk1);
          (G1.negate (G1.add sig0 sig1), G2.add G2.one G2.one);
        ]
      in
      let* () = check_pairing_check protocol client args1 in
      unit)
    protocols ;
  (* Pairing Check test based on signature aggregation *)
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "Bls12_381 contract primitives: test_groth16")
    ~tags:["michelson"; "crypto"; "contract"; "bls12_381"; "groth16"]
    ~uses_node:false
    (fun protocol ->
      let* client = Client.init_mockup ~protocol () in

      (* The verifying key, proof, and inputs are generated from *)
      (* ZoKrates, modified to use BLS12-381. *)
      (* The circuit proves knowledge of a square root of 113569. *)
      let input_x =
        "0xa1bb010000000000000000000000000000000000000000000000000000000000"
      in
      let input_y =
        "0x0100000000000000000000000000000000000000000000000000000000000000"
      in
      let proof_a =
        "0x0a2841423326ab08f5f406409775e43fa0f9a0b97631fa85d2dd9242507d25059e9cf48b8b98f99a0008671423a148ec106d70637056972ef49fb6f62de2e89ba3682b9972292b6bb4e6f53799a75d2f8001ccfde280d8ac05fc209352236cbd"
      in
      let proof_b =
        "0x0fced939fb1ad733f99669f50a383ef632f6d41dfbde434a6715afd5c7dfbb7bc5835e058ad8b590c7b38dd137d0bd0f0e1540f1b45d8aa626c360e2ea484a116243f7c802034de915db6b18d5303946f676e423cbd6046d37a82208d500625a11c7250ccb953a7ee49d704ad14de4b727733cff7cf06875d8b6444f3c0a8cbf0bd980e539c74bd5b37bb15fe816f23407d269193105fda71adf35fae9309d9d46729fcd4685699097a86f0460a2bc8b16293940cabfdcfe0f27e4107e74e90c"
      in
      let proof_c =
        "0x0a1fb5a144ca3bdfe4ad0f183cf71dd7fdd28cbef4fcd47b5b419f65186703f62ecaaa1255fa21a6ebdd917ab1f9bd9707de7066865e2ff3875e22088619125a0d4088a622ab42224425ef89a5a149ce2db9c8292b62c7e7aaa7e87f3535304b"
      in

      let inputs = sf "Pair %s %s" input_x input_y in
      let proof = sf "Pair (Pair %s %s) %s" proof_a proof_b proof_c in
      let input = sf "Pair (%s) (%s)" inputs proof in

      let* _res =
        Client.run_script_at
          ~hooks
          ~storage:"Unit"
          ~input
          client
          ["mini_scenarios"; "groth16"]
          protocol
      in
      unit)
    protocols ;
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      (sf
         "Bls12_381 contract primitives: fr bytes parameters more than 32 bytes")
    ~tags:["michelson"; "crypto"; "contract"; "bls12_381"; "fr"]
    ~uses_node:false
    (fun protocol ->
      let* client = Client.init_mockup ~protocol () in

      let random_bytes =
        "0xf7ef66f95c90b2f953eb0555af65f22095d4f54b40ea8c6dcc2014740e8662c16bb8786723"
      in

      let* () =
        Client.spawn_run_script_at
          ~hooks
          ~storage:"0"
          ~input:random_bytes
          client
          ["mini_scenarios"; "groth16"]
          protocol
        |> Process.check_error ~msg:(rex "error running script")
      in
      unit)
    protocols
