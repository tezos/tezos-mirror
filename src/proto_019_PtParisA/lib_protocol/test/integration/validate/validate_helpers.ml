(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Registered_nonces = Nonce
open Protocol
open Alpha_context
module Manager = Manager_operation_helpers

(** {2 Helpers} *)

(** {3 Randomness } *)

let gen_bounded_int min max = QCheck2.Gen.(generate1 @@ int_range min max)

let pick_one l = QCheck2.Gen.(generate1 @@ oneofl l)

let pick_n (n : int) (l : 'a list) : 'a list =
  List.take_n n QCheck2.Gen.(generate1 @@ shuffle_l l)

(** {3 Helpers for lists } *)

let get_n l n =
  assert (List.length l > n) ;
  Stdlib.List.nth l n

let mycombine l1 l2 =
  let sz_dels = List.length l1 in
  let sz_phs = List.length l2 in
  let dels, phs =
    if sz_dels = sz_phs then (l1, l2)
    else if sz_dels < sz_phs then (l1, List.take_n sz_dels l2)
    else (List.take_n sz_phs l1, l2)
  in
  Stdlib.List.combine dels phs

(** {3 Global Values}*)

let ballots = Vote.[Yay; Nay; Pass]

let protos =
  List.map
    (fun s -> Protocol_hash.of_b58check_exn s)
    [
      "ProtoALphaALphaALphaALphaALphaALphaALpha61322gcLUGH";
      "ProtoALphaALphaALphaALphaALphaALphaALphabc2a7ebx6WB";
      "ProtoALphaALphaALphaALphaALphaALphaALpha84efbeiF6cm";
      "ProtoALphaALphaALphaALphaALphaALphaALpha91249Z65tWS";
      "ProtoALphaALphaALphaALphaALphaALphaALpha537f5h25LnN";
      "ProtoALphaALphaALphaALphaALphaALphaALpha5c8fefgDYkr";
      "ProtoALphaALphaALphaALphaALphaALphaALpha3f31feSSarC";
      "ProtoALphaALphaALphaALphaALphaALphaALphabe31ahnkxSC";
      "ProtoALphaALphaALphaALphaALphaALphaALphabab3bgRb7zQ";
      "ProtoALphaALphaALphaALphaALphaALphaALphaf8d39cctbpk";
      "ProtoALphaALphaALphaALphaALphaALphaALpha3b981byuYxD";
      "ProtoALphaALphaALphaALphaALphaALphaALphaa116bccYowi";
      "ProtoALphaALphaALphaALphaALphaALphaALphacce68eHqboj";
      "ProtoALphaALphaALphaALphaALphaALphaALpha225c7YrWwR7";
      "ProtoALphaALphaALphaALphaALphaALphaALpha58743cJL6FG";
      "ProtoALphaALphaALphaALphaALphaALphaALphac91bcdvmJFR";
      "ProtoALphaALphaALphaALphaALphaALphaALpha1faaadhV7oW";
      "ProtoALphaALphaALphaALphaALphaALphaALpha98232gD94QJ";
      "ProtoALphaALphaALphaALphaALphaALphaALpha9d1d8cijvAh";
      "ProtoALphaALphaALphaALphaALphaALphaALphaeec52dKF6Gx";
      "ProtoALphaALphaALphaALphaALphaALphaALpha841f2cQqajX";
    ]

type secret_account = {
  blinded_public_key_hash : Blinded_public_key_hash.t;
  account : Signature.Ed25519.Public_key_hash.t;
  activation_code : Blinded_public_key_hash.activation_code;
  amount : Tez.t;
}

let secrets =
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
      let pkh = Signature.Ed25519.Public_key_hash.of_b58check_exn pkh in
      assert (Signature.Public_key_hash.equal (Ed25519 pkh) pkh') ;
      let activation_code =
        Stdlib.Option.get
          (Blinded_public_key_hash.activation_code_of_hex secret)
      in
      let bpkh = Blinded_public_key_hash.of_ed25519_pkh activation_code pkh in
      let account = Account.{pkh = Ed25519 pkh; pk; sk} in
      Account.add_account account ;
      {
        blinded_public_key_hash = bpkh;
        account = pkh;
        activation_code;
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

(** {3 Context Manipulations } *)

let pick_two_attesters ctxt =
  let open Lwt_result_syntax in
  let module V = Plugin.RPC.Validators in
  let* attesters = Context.get_attesters ctxt in
  match attesters with
  | a :: b :: _ -> return (a.V.consensus_key, b.V.consensus_key)
  | _ -> assert false

let pick_addr_attester ctxt =
  let open Lwt_result_syntax in
  let module V = Plugin.RPC.Validators in
  let* attesters = Context.get_attesters ctxt in
  match attesters with a :: _ -> return a.V.consensus_key | _ -> assert false

let init_params =
  Tezos_protocol_019_PtParisA_parameters.Default_parameters.parameters_of_constants
    {Context.default_test_constants with consensus_threshold = 0}

let delegates_of_block block =
  let open Lwt_result_syntax in
  let+ validators = Context.get_attesters (B block) in
  List.map
    (fun Plugin.RPC.Validators.{consensus_key; slots; _} ->
      (consensus_key, slots))
    validators

(** Sequential validation of an operation list. *)
let sequential_validate ?(mempool_mode = true) block operations =
  let open Lwt_result_syntax in
  let* inc = Incremental.begin_construction ~mempool_mode block in
  let* (_inc : Incremental.t) =
    List.fold_left_es
      (fun acc op -> Incremental.validate_operation acc op)
      inc
      operations
  in
  return_unit
