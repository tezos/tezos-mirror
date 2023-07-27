(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
    Component:  Protocol (activation)
    Invocation: dune exec src/proto_018_Proxford/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_activation.ml
    Subject:    The activation operation creates an implicit contract from a
                registered commitment present in the context. It is
                parametrized by a public key hash (pkh) and a secret.

                The commitments are composed of :
                - a blinded pkh that can be revealed by the secret ;
                - an amount.

                The commitments and the secrets are generated from
                /scripts/create_genesis/create_genesis.py and should be
                coherent.
*)

open Protocol
open Alpha_context
open Test_tez

(* Generated commitments and secrets  *)

let commitments =
  List.map
    (fun (bpkh, a) ->
      Commitment.
        {
          blinded_public_key_hash = Blinded_public_key_hash.of_b58check_exn bpkh;
          amount = Tez.of_mutez_exn (Int64.of_string a);
        })
    [
      ("btz1bRL4X5BWo2Fj4EsBdUwexXqgTf75uf1qa", "23932454669343");
      ("btz1SxjV1syBgftgKy721czKi3arVkVwYUFSv", "72954577464032");
      ("btz1LtoNCjiW23txBTenALaf5H6NKF1L3c1gw", "217487035428349");
      ("btz1SUd3mMhEBcWudrn8u361MVAec4WYCcFoy", "4092742372031");
      ("btz1MvBXf4orko1tsGmzkjLbpYSgnwUjEe81r", "17590039016550");
      ("btz1LoDZ3zsjgG3k3cqTpUMc9bsXbchu9qMXT", "26322312350555");
      ("btz1RMfq456hFV5AeDiZcQuZhoMv2dMpb9hpP", "244951387881443");
      ("btz1Y9roTh4A7PsMBkp8AgdVFrqUDNaBE59y1", "80065050465525");
      ("btz1Q1N2ePwhVw5ED3aaRVek6EBzYs1GDkSVD", "3569618927693");
      ("btz1VFFVsVMYHd5WfaDTAt92BeQYGK8Ri4eLy", "9034781424478");
    ]

type secret_account = {
  account : public_key_hash;
  activation_code : Blinded_public_key_hash.activation_code;
  amount : Tez.t;
}

let secrets () =
  (* Exported from proto_alpha client - TODO : remove when relocated to lib_crypto *)
  let read_key mnemonic email password =
    match Tezos_client_base.Bip39.of_words mnemonic with
    | None -> assert false
    | Some t ->
        (* TODO: unicode normalization (NFKD)... *)
        let passphrase = Bytes.(cat (of_string email) (of_string password)) in
        let sk = Tezos_client_base.Bip39.to_seed ~passphrase t in
        let sk = Bytes.sub sk 0 32 in
        let sk : Signature.Secret_key.t =
          Ed25519
            (Data_encoding.Binary.of_bytes_exn
               Signature.Ed25519.Secret_key.encoding
               sk)
        in
        let pk = Signature.Secret_key.to_public_key sk in
        let pkh = Signature.Public_key.hash pk in
        (pkh, pk, sk)
  in
  List.map
    (fun (mnemonic, secret, amount, pkh, password, email) ->
      let pkh', pk, sk = read_key mnemonic email password in
      let pkh = Signature.Public_key_hash.of_b58check_exn pkh in
      assert (Signature.Public_key_hash.equal pkh pkh') ;
      let account = Account.{pkh; pk; sk} in
      Account.add_account account ;
      {
        account = account.pkh;
        activation_code =
          Stdlib.Option.get
            (Blinded_public_key_hash.activation_code_of_hex secret);
        amount =
          WithExceptions.Option.to_exn
            ~none:(Invalid_argument "tez conversion")
            (Tez.of_mutez (Int64.of_string amount));
      })
    [
      ( [
          "envelope";
          "hospital";
          "mind";
          "sunset";
          "cancel";
          "muscle";
          "leisure";
          "thumb";
          "wine";
          "market";
          "exit";
          "lucky";
          "style";
          "picnic";
          "success";
        ],
        "0f39ed0b656509c2ecec4771712d9cddefe2afac",
        "23932454669343",
        "tz1MawerETND6bqJqx8GV3YHUrvMBCDasRBF",
        "z0eZHQQGKt",
        "cjgfoqmk.wpxnvnup@tezos.example.org" );
      ( [
          "flag";
          "quote";
          "will";
          "valley";
          "mouse";
          "chat";
          "hold";
          "prosper";
          "silk";
          "tent";
          "cruel";
          "cause";
          "demise";
          "bottom";
          "practice";
        ],
        "41f98b15efc63fa893d61d7d6eee4a2ce9427ac4",
        "72954577464032",
        "tz1X4maqF9tC1Yn4jULjHRAyzjAtc25Z68TX",
        "MHErskWPE6",
        "oklmcktr.ztljnpzc@tezos.example.org" );
      ( [
          "library";
          "away";
          "inside";
          "paper";
          "wise";
          "focus";
          "sweet";
          "expose";
          "require";
          "change";
          "stove";
          "planet";
          "zone";
          "reflect";
          "finger";
        ],
        "411dfef031eeecc506de71c9df9f8e44297cf5ba",
        "217487035428349",
        "tz1SWBY7rWMutEuWS54Pt33MkzAS6eWkUuTc",
        "0AO6BzQNfN",
        "ctgnkvqm.kvtiybky@tezos.example.org" );
      ( [
          "cruel";
          "fluid";
          "damage";
          "demand";
          "mimic";
          "above";
          "village";
          "alpha";
          "vendor";
          "staff";
          "absent";
          "uniform";
          "fire";
          "asthma";
          "milk";
        ],
        "08d7d355bc3391d12d140780b39717d9f46fcf87",
        "4092742372031",
        "tz1amUjiZaevaxQy5wKn4SSRvVoERCip3nZS",
        "9kbZ7fR6im",
        "bnyxxzqr.tdszcvqb@tezos.example.org" );
      ( [
          "opera";
          "divorce";
          "easy";
          "myself";
          "idea";
          "aim";
          "dash";
          "scout";
          "case";
          "resource";
          "vote";
          "humor";
          "ticket";
          "client";
          "edge";
        ],
        "9b7cad042fba557618bdc4b62837c5f125b50e56",
        "17590039016550",
        "tz1Zaee3QBtD4ErY1SzqUvyYTrENrExu6yQM",
        "suxT5H09yY",
        "iilkhohu.otnyuvna@tezos.example.org" );
      ( [
          "token";
          "similar";
          "ginger";
          "tongue";
          "gun";
          "sort";
          "piano";
          "month";
          "hotel";
          "vote";
          "undo";
          "success";
          "hobby";
          "shell";
          "cart";
        ],
        "124c0ca217f11ffc6c7b76a743d867c8932e5afd",
        "26322312350555",
        "tz1geDUUhfXK1EMj7VQdRjug1MoFe6gHWnCU",
        "4odVdLykaa",
        "kwhlglvr.slriitzy@tezos.example.org" );
      ( [
          "shield";
          "warrior";
          "gorilla";
          "birth";
          "steak";
          "neither";
          "feel";
          "only";
          "liberty";
          "float";
          "oven";
          "extend";
          "pulse";
          "suffer";
          "vapor";
        ],
        "ac7a2125beea68caf5266a647f24dce9fea018a7",
        "244951387881443",
        "tz1h3nY7jcZciJgAwRhWcrEwqfVp7VQoffur",
        "A6yeMqBFG8",
        "lvrmlbyj.yczltcxn@tezos.example.org" );
      ( [
          "waste";
          "open";
          "scan";
          "tip";
          "subway";
          "dance";
          "rent";
          "copper";
          "garlic";
          "laundry";
          "defense";
          "clerk";
          "another";
          "staff";
          "liar";
        ],
        "2b3e94be133a960fa0ef87f6c0922c19f9d87ca2",
        "80065050465525",
        "tz1VzL4Xrb3fL3ckvqCWy6bdGMzU2w9eoRqs",
        "oVZqpq60sk",
        "rfodmrha.zzdndvyk@tezos.example.org" );
      ( [
          "fiber";
          "next";
          "property";
          "cradle";
          "silk";
          "obey";
          "gossip";
          "push";
          "key";
          "second";
          "across";
          "minimum";
          "nice";
          "boil";
          "age";
        ],
        "dac31640199f2babc157aadc0021cd71128ca9ea",
        "3569618927693",
        "tz1RUHg536oRKhPLFfttcB5gSWAhh4E9TWjX",
        "FfytQTTVbu",
        "owecikdy.gxnyttya@tezos.example.org" );
      ( [
          "print";
          "labor";
          "budget";
          "speak";
          "poem";
          "diet";
          "chunk";
          "eternal";
          "book";
          "saddle";
          "pioneer";
          "ankle";
          "happy";
          "only";
          "exclude";
        ],
        "bb841227f250a066eb8429e56937ad504d7b34dd",
        "9034781424478",
        "tz1M1LFbgctcPWxstrao9aLr2ECW1fV4pH5u",
        "zknAl3lrX2",
        "ettilrvh.zsrqrbud@tezos.example.org" );
    ]

(** Helper: Create a genesis block with predefined commitments,
    accounts and balances. *)
let activation_init () =
  Context.init1 ~consensus_threshold:0 ~commitments () >|=? fun (b, c) ->
  secrets () |> fun ss -> (b, c, ss)

(** Verify the genesis block created by [activation_init] can be
    baked. *)
let test_simple_init_with_commitments () =
  activation_init () >>=? fun (blk, _contract, _secrets) ->
  Block.bake blk >>=? fun (_ : Block.t) -> return_unit

(** A single activation *)
let test_single_activation () =
  activation_init () >>=? fun (blk, _contract, secrets) ->
  let ({account; activation_code; amount = expected_amount; _} as _first_one) =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd secrets
  in
  (* Contract does not exist *)
  Assert.balance_is ~loc:__LOC__ (B blk) (Contract.Implicit account) Tez.zero
  >>=? fun () ->
  Op.activation (B blk) account activation_code >>=? fun operation ->
  Block.bake ~operation blk >>=? fun blk ->
  (* Contract does exist *)
  Assert.balance_is
    ~loc:__LOC__
    (B blk)
    (Contract.Implicit account)
    expected_amount

(** 10 activations, one per bake. *)
let test_multi_activation_1 () =
  activation_init () >>=? fun (blk, _contract, secrets) ->
  List.fold_left_es
    (fun blk {account; activation_code; amount = expected_amount; _} ->
      Op.activation (B blk) account activation_code >>=? fun operation ->
      Block.bake ~operation blk >>=? fun blk ->
      Assert.balance_is
        ~loc:__LOC__
        (B blk)
        (Contract.Implicit account)
        expected_amount
      >|=? fun () -> blk)
    blk
    secrets
  >>=? fun (_ : Block.t) -> return_unit

(** All of the 10 activations occur in one bake. *)
let test_multi_activation_2 () =
  activation_init () >>=? fun (blk, _contract, secrets) ->
  List.fold_left_es
    (fun ops {account; activation_code; _} ->
      Op.activation (B blk) account activation_code >|=? fun op -> op :: ops)
    []
    secrets
  >>=? fun ops ->
  Block.bake ~operations:ops blk >>=? fun blk ->
  List.iter_es
    (fun {account; amount = expected_amount; _} ->
      (* Contract does exist *)
      Assert.balance_is
        ~loc:__LOC__
        (B blk)
        (Contract.Implicit account)
        expected_amount)
    secrets

(** Transfer with activated account. *)
let test_activation_and_transfer () =
  activation_init () >>=? fun (blk, bootstrap_contract, secrets) ->
  let ({account; activation_code; _} as _first_one) =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd secrets
  in
  let first_contract = Contract.Implicit account in
  Op.activation (B blk) account activation_code >>=? fun operation ->
  Block.bake ~operation blk >>=? fun blk ->
  Context.Contract.balance (B blk) bootstrap_contract >>=? fun amount ->
  Test_tez.( /? ) amount 2L >>?= fun half_amount ->
  Context.Contract.balance (B blk) first_contract
  >>=? fun activated_amount_before ->
  Op.transaction (B blk) bootstrap_contract first_contract half_amount
  >>=? fun operation ->
  Block.bake ~operation blk >>=? fun blk ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (B blk)
    (Contract.Implicit account)
    activated_amount_before
    half_amount

(** Transfer to an unactivated account and then activating it. *)
let test_transfer_to_unactivated_then_activate () =
  activation_init () >>=? fun (blk, bootstrap_contract, secrets) ->
  let ({account; activation_code; amount} as _first_one) =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd secrets
  in
  let unactivated_commitment_contract = Contract.Implicit account in
  Context.Contract.balance (B blk) bootstrap_contract >>=? fun b_amount ->
  b_amount /? 2L >>?= fun b_half_amount ->
  Incremental.begin_construction blk >>=? fun inc ->
  Op.transaction
    (I inc)
    bootstrap_contract
    unactivated_commitment_contract
    b_half_amount
  >>=? fun op ->
  Incremental.add_operation inc op >>=? fun inc ->
  Op.activation (I inc) account activation_code >>=? fun op' ->
  Incremental.add_operation inc op' >>=? fun inc ->
  Incremental.finalize_block inc >>=? fun blk2 ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (B blk2)
    (Contract.Implicit account)
    amount
    b_half_amount

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Invalid pkh activation: expected to fail as the context does not
    contain any commitment. *)
let test_invalid_activation_with_no_commitments () =
  Context.init1 () >>=? fun (blk, _contract) ->
  let secrets = secrets () in
  let ({account; activation_code; _} as _first_one) =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd secrets
  in
  Op.activation (B blk) account activation_code >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_activation _ -> true
      | _ -> false)

(** Wrong activation: wrong secret given in the operation. *)
let test_invalid_activation_wrong_secret () =
  activation_init () >>=? fun (blk, _contract, secrets) ->
  let ({account; _} as _first_one) =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth secrets 0
  in
  let ({activation_code; _} as _second_one) =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth secrets 1
  in
  Op.activation (B blk) account activation_code >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_activation _ -> true
      | _ -> false)

(** Invalid pkh activation : expected to fail as the context does not
    contain an associated commitment. *)
let test_invalid_activation_inexistent_pkh () =
  activation_init () >>=? fun (blk, _contract, secrets) ->
  let ({activation_code; _} as _first_one) =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd secrets
  in
  let inexistent_pkh =
    Signature.Public_key_hash.of_b58check_exn
      "tz1PeQHGKPWSpNoozvxgqLN9TFsj6rDqNV3o"
  in
  Op.activation (B blk) inexistent_pkh activation_code >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_activation _ -> true
      | _ -> false)

(** Invalid pkh activation : expected to fail as the commitment has
    already been claimed. *)
let test_invalid_double_activation () =
  activation_init () >>=? fun (blk, _contract, secrets) ->
  let ({account; activation_code; _} as _first_one) =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd secrets
  in
  Incremental.begin_construction blk >>=? fun inc ->
  Op.activation (I inc) account activation_code >>=? fun op ->
  Incremental.add_operation inc op >>=? fun inc ->
  Op.activation (I inc) account activation_code >>=? fun op' ->
  Incremental.add_operation inc op' >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Conflicting_activation _ -> true
      | _ -> false)

(** Transfer from an unactivated commitment account. *)
let test_invalid_transfer_from_unactivated_account () =
  activation_init () >>=? fun (blk, bootstrap_contract, secrets) ->
  let ({account; _} as _first_one) =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd secrets
  in
  let unactivated_commitment_contract = Contract.Implicit account in
  (* No activation *)
  Op.transaction
    (B blk)
    unactivated_commitment_contract
    bootstrap_contract
    Tez.one
  >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->
  Assert.proto_error_with_info ~loc:__LOC__ res "Empty implicit contract"

let tests =
  [
    Tztest.tztest
      "init with commitments"
      `Quick
      test_simple_init_with_commitments;
    Tztest.tztest "single activation" `Quick test_single_activation;
    Tztest.tztest "multi-activation one-by-one" `Quick test_multi_activation_1;
    Tztest.tztest
      "multi-activation all at a time"
      `Quick
      test_multi_activation_2;
    Tztest.tztest "activation and transfer" `Quick test_activation_and_transfer;
    Tztest.tztest
      "transfer to unactivated account then activate"
      `Quick
      test_transfer_to_unactivated_then_activate;
    Tztest.tztest
      "invalid activation with no commitments"
      `Quick
      test_invalid_activation_with_no_commitments;
    Tztest.tztest
      "invalid activation with commitments"
      `Quick
      test_invalid_activation_inexistent_pkh;
    Tztest.tztest
      "invalid double activation"
      `Quick
      test_invalid_double_activation;
    Tztest.tztest
      "wrong activation code"
      `Quick
      test_invalid_activation_wrong_secret;
    Tztest.tztest
      "invalid transfer from unactivated account"
      `Quick
      test_invalid_transfer_from_unactivated_account;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("activation", tests)]
  |> Lwt_main.run
