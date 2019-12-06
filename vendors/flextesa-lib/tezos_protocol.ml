open Internal_pervasives

module Key = struct
  module Of_name = struct
    type t =
      { name: string
      ; pkh: Tezos_crypto.Ed25519.Public_key_hash.t
      ; pk: Tezos_crypto.Ed25519.Public_key.t
      ; sk: Tezos_crypto.Ed25519.Secret_key.t }

    let make name =
      let seed =
        Bigstring.of_string
          (String.concat ~sep:"" (List.init 42 ~f:(fun _ -> name))) in
      let pkh, pk, sk = Tezos_crypto.Ed25519.generate_key ~seed () in
      {name; pkh; pk; sk}

    let pubkey n = Tezos_crypto.Ed25519.Public_key.to_b58check (make n).pk

    let pubkey_hash n =
      Tezos_crypto.Ed25519.Public_key_hash.to_b58check (make n).pkh

    let private_key n =
      "unencrypted:" ^ Tezos_crypto.Ed25519.Secret_key.to_b58check (make n).sk
  end
end

module Account = struct
  type t =
    | Of_name of string
    | Key_pair of
        {name: string; pubkey: string; pubkey_hash: string; private_key: string}

  let of_name s = Of_name s
  let of_namef fmt = ksprintf of_name fmt
  let name = function Of_name n -> n | Key_pair k -> k.name

  let key_pair name ~pubkey ~pubkey_hash ~private_key =
    Key_pair {name; pubkey; pubkey_hash; private_key}

  let pubkey = function
    | Of_name n -> Key.Of_name.pubkey n
    | Key_pair k -> k.pubkey

  let pubkey_hash = function
    | Of_name n -> Key.Of_name.pubkey_hash n
    | Key_pair k -> k.pubkey_hash

  let private_key = function
    | Of_name n -> Key.Of_name.private_key n
    | Key_pair k -> k.private_key
end

module Voting_period = struct
  type t = [`Proposal | `Testing_vote | `Testing | `Promotion_vote]

  let to_string (p : t) =
    (* This has to mimic: src/proto_alpha/lib_protocol/voting_period_repr.ml *)
    match p with
    | `Promotion_vote -> "promotion_vote"
    | `Testing_vote -> "testing_vote"
    | `Proposal -> "proposal"
    | `Testing -> "testing"
end

module Protocol_kind = struct
  type t = [`Babylon | `Carthage]

  let names = [("Babylon", `Babylon); ("Carthage", `Carthage)]

  let cmdliner_term () : t Cmdliner.Term.t =
    let open Cmdliner in
    Arg.(
      value
        (opt (enum names) `Carthage
           (info ["protocol-kind"] ~doc:"Set the protocol family.")))

  let pp ppf n =
    Fmt.string ppf
      (List.find_map_exn names ~f:(function
        | s, x when x = n -> Some s
        | _ -> None))
end

type t =
  { id: string
  ; kind: Protocol_kind.t
  ; bootstrap_accounts: (Account.t * Int64.t) list
  ; dictator: Account.t
        (* ; bootstrap_contracts: (Account.t * int * Script.origin) list *)
  ; expected_pow: int
  ; name: string (* e.g. alpha *)
  ; hash: string
  ; time_between_blocks: int list
  ; baking_reward_per_endorsement: int list
  ; endorsement_reward: int list
  ; blocks_per_roll_snapshot: int
  ; blocks_per_voting_period: int
  ; blocks_per_cycle: int
  ; preserved_cycles: int
  ; proof_of_work_threshold: int
  ; custom_protocol_parameters: Ezjsonm.t option }

let compare a b = String.compare a.id b.id

let default () =
  let dictator = Account.of_name "dictator-default" in
  { id= "default-bootstrap"
  ; kind= `Carthage
  ; bootstrap_accounts=
      List.init 4 ~f:(fun n ->
          (Account.of_namef "bootacc-%d" n, 4_000_000_000_000L))
  ; dictator
    (* ; bootstrap_contracts= [(dictator, 10_000_000, `Sandbox_faucet)] *)
  ; expected_pow= 1
  ; name= "006_PsCARTHA"
  ; hash= "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb"
  ; time_between_blocks= [2; 3]
  ; baking_reward_per_endorsement= [1_250_000; 187_500]
  ; endorsement_reward= [1_250_000; 833_333]
  ; blocks_per_roll_snapshot= 4
  ; blocks_per_voting_period= 16
  ; blocks_per_cycle= 8
  ; preserved_cycles= 2
  ; proof_of_work_threshold= -1
  ; custom_protocol_parameters= None }

let protocol_parameters_json t : Ezjsonm.t =
  let open Ezjsonm in
  let make_account (account, amount) =
    strings [Account.pubkey account; sprintf "%Ld" amount] in
  (* let make_contract (deleg, amount, script) = dict
      [ ("delegate", string (Account.pubkey_hash deleg))
      ; ("amount", ksprintf string "%d" amount)
      ; ("script", (Script.load script :> Ezjsonm.value)) ] in
  *)
  let common =
    Ezjsonm.
      [ ( "bootstrap_accounts"
        , list make_account (t.bootstrap_accounts @ [(t.dictator, 10_000_000L)])
        )
        (* ; ("bootstrap_contracts", list make_contract t.bootstrap_contracts) *)
      ; ( "time_between_blocks"
        , list (ksprintf string "%d") t.time_between_blocks )
      ; ("blocks_per_roll_snapshot", int t.blocks_per_roll_snapshot)
      ; ("blocks_per_voting_period", int t.blocks_per_voting_period)
      ; ("blocks_per_cycle", int t.blocks_per_cycle)
      ; ("preserved_cycles", int t.preserved_cycles)
      ; ( "proof_of_work_threshold"
        , ksprintf string "%d" t.proof_of_work_threshold )
      ; ("blocks_per_commitment", int 4)
      ; ("endorsers_per_block", int 32)
      ; ("hard_gas_limit_per_operation", string (Int.to_string 100_000_000_000))
      ; ("hard_gas_limit_per_block", string (Int.to_string 10_000_000_000_000))
      ; ("tokens_per_roll", string (Int.to_string 8_000_000_000))
      ; ("michelson_maximum_type_size", int 1_000)
      ; ("seed_nonce_revelation_tip", string (Int.to_string 125_000))
      ; ("origination_size", int 257)
      ; ("block_security_deposit", string (Int.to_string 512_000_000))
      ; ("endorsement_security_deposit", string (Int.to_string 64_000_000))
      ; ( "baking_reward_per_endorsement"
        , list (ksprintf string "%d") t.baking_reward_per_endorsement )
      ; ("endorsement_reward", list (ksprintf string "%d") t.endorsement_reward)
      ; ( "hard_storage_limit_per_operation"
        , string (Int.to_string 10_000_000_000) )
      ; ("cost_per_byte", string (Int.to_string 100))
      ; ("test_chain_duration", string (Int.to_string 1_966_080))
      ; ("quorum_min", int 3_000)
      ; ("quorum_max", int 7_000)
      ; ("min_proposal_quorum", int 500)
      ; ("initial_endorsers", int 1)
      ; ("delay_per_missing_endorsement", string (Int.to_string 1)) ] in
  match t.custom_protocol_parameters with
  | Some s -> s
  | None -> dict (match t.kind with `Carthage -> common | `Babylon -> common)

let sandbox {dictator; _} =
  let pk = Account.pubkey dictator in
  Ezjsonm.to_string (`O [("genesis_pubkey", `String pk)])

let protocol_parameters t =
  Ezjsonm.to_string ~minify:false (protocol_parameters_json t)

let expected_pow t = t.expected_pow
let id t = t.id
let kind t = t.kind
let bootstrap_accounts t = List.map ~f:fst t.bootstrap_accounts
let dictator_name {dictator; _} = Account.name dictator
let dictator_secret_key {dictator; _} = Account.private_key dictator
let make_path config t = Paths.root config // sprintf "protocol-%s" (id t)
let sandbox_path ~config t = make_path config t // "sandbox.json"

let protocol_parameters_path ~config t =
  make_path config t // "protocol_parameters.json"

let ensure_script ~config t =
  let open Genspio.EDSL in
  let file string p =
    let path = p ~config t in
    ( Filename.basename path
    , write_stdout ~path:(str path)
        (feed ~string:(str (string t)) (exec ["cat"])) ) in
  check_sequence
    ~verbosity:(`Announce (sprintf "Ensure-protocol-%s" (id t)))
    [ ("directory", exec ["mkdir"; "-p"; make_path config t])
    ; file sandbox sandbox_path
    ; file protocol_parameters protocol_parameters_path ]

let ensure t ~config =
  match
    Sys.command (Genspio.Compile.to_one_liner (ensure_script ~config t))
  with
  | 0 -> return ()
  | _other ->
      System_error.fail_fatalf "sys.command non-zero"
        ~attach:[("location", `String_value "Tezos_protocol.ensure")]

let cli_term () =
  let open Cmdliner in
  let open Term in
  let def = default () in
  let docs = "PROTOCOL OPTIONS" in
  pure
    (fun remove_default_bas
         (`Blocks_per_voting_period blocks_per_voting_period)
         (`Protocol_hash hash)
         (`Time_between_blocks time_between_blocks)
         (`Blocks_per_cycle blocks_per_cycle)
         (`Preserved_cycles preserved_cycles)
         (`Protocol_parameters custom_protocol_parameters)
         kind
         add_bootstraps
         ->
      let id = "default-and-command-line" in
      let bootstrap_accounts =
        add_bootstraps
        @ if remove_default_bas then [] else def.bootstrap_accounts in
      { def with
        id
      ; kind
      ; custom_protocol_parameters
      ; blocks_per_cycle
      ; hash
      ; bootstrap_accounts
      ; time_between_blocks
      ; preserved_cycles
      ; blocks_per_voting_period })
  $ Arg.(
      value
        (flag
           (info ~doc:"Do not create any of the default bootstrap accounts."
              ~docs
              ["remove-default-bootstrap-accounts"])))
  $ Arg.(
      pure (fun x -> `Blocks_per_voting_period x)
      $ value
          (opt int def.blocks_per_voting_period
             (info ~docs
                ["blocks-per-voting-period"]
                ~doc:"Set the length of voting periods.")))
  $ Arg.(
      pure (fun x -> `Protocol_hash x)
      $ value
          (opt string def.hash
             (info ["protocol-hash"] ~docs
                ~doc:"Set the (initial) protocol hash.")))
  $ Arg.(
      pure (fun x -> `Time_between_blocks x)
      $ value
          (opt (list ~sep:',' int) def.time_between_blocks
             (info ["time-between-blocks"] ~docv:"COMMA-SEPARATED-SECONDS"
                ~docs
                ~doc:
                  "Set the time between blocks bootstrap-parameter, e.g. \
                   `2,3,2`.")))
  $ Arg.(
      pure (fun x -> `Blocks_per_cycle x)
      $ value
          (opt int def.blocks_per_cycle
             (info ["blocks-per-cycle"] ~docv:"NUMBER" ~docs
                ~doc:"Number of blocks per cycle.")))
  $ Arg.(
      pure (fun x -> `Preserved_cycles x)
      $ value
          (opt int def.preserved_cycles
             (info ["preserved-cycles"] ~docv:"NUMBER" ~docs
                ~doc:
                  "Base constant for baking rights (search for \
                   `PRESERVED_CYCLES` in the white paper).")))
  $ Arg.(
      pure (fun f ->
          `Protocol_parameters
            (Option.map f ~f:(fun path ->
                 let i = open_in path in
                 Ezjsonm.from_channel i)))
      $ value
          (opt (some file) None
             (info
                ["override-protocol-parameters"]
                ~doc:
                  "Use these protocol parameters instead of the generated \
                   ones (technically this invalidates most other options from \
                   a tezos-node point of view, use at your own risk)."
                ~docv:"JSON-FILE" ~docs)))
  $ Protocol_kind.cmdliner_term ()
  $ Arg.(
      pure (fun l ->
          List.map l ~f:(fun ((name, pubkey, pubkey_hash, private_key), tez) ->
              (Account.key_pair name ~pubkey ~pubkey_hash ~private_key, tez)))
      $ value
          (opt_all
             (pair ~sep:'@' (t4 ~sep:',' string string string string) int64)
             []
             (info ["add-bootstrap-account"] ~docs
                ~docv:"NAME,PUBKEY,PUBKEY-HASH,PRIVATE-URI@MUTEZ-AMOUNT"
                ~doc:
                  "Add a custom bootstrap account, e.g. \
                   `LedgerBaker,edpku...,tz1YPS...,ledger://crouching-tiger.../ed25519/0'/0'@20_000_000_000`.")))
