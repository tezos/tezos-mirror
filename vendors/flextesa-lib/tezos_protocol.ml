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

type t =
  { id: string
  ; bootstrap_accounts: (Account.t * Int64.t) list
  ; dictator: Account.t
        (* ; bootstrap_contracts: (Account.t * int * Script.origin) list *)
  ; expected_pow: int
  ; name: string (* e.g. alpha *)
  ; hash: string
  ; time_between_blocks: int list
  ; blocks_per_roll_snapshot: int
  ; blocks_per_voting_period: int
  ; blocks_per_cycle: int
  ; preserved_cycles: int
  ; proof_of_work_threshold: int
  ; blocks_per_commitment: int
  ; endorsers_per_block: int
  ; hard_gas_limit_per_operation: int
  ; hard_gas_limit_per_block: int
  ; tokens_per_roll: int
  ; michelson_maximum_type_size: int
  ; seed_nonce_revelation_tip: int
  ; origination_size: int
  ; block_security_deposit: int
  ; endorsement_security_deposit: int
  ; block_reward: int
  ; endorsement_reward: int
  ; hard_storage_limit_per_operation: int
  ; cost_per_byte: int
  ; test_chain_duration: int
  ; quorum_min: int
  ; quorum_max: int
  ; min_proposal_quorum: int
  ; initial_endorsers: int
  ; delay_per_missing_endorsement: int }

let compare a b = String.compare a.id b.id

let default () =
  let dictator = Account.of_name "dictator-default" in
  { id= "default-bootstrap"
  ; bootstrap_accounts=
      List.init 4 ~f:(fun n ->
          (Account.of_namef "bootacc-%d" n, 4_000_000_000_000L))
  ; dictator
    (* ; bootstrap_contracts= [(dictator, 10_000_000, `Sandbox_faucet)] *)
  ; expected_pow= 1
  ; name= "005_PsBabyM1"
  ; hash= "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS"
  ; time_between_blocks= [2; 3]
  ; blocks_per_roll_snapshot= 4
  ; blocks_per_voting_period= 16
  ; blocks_per_cycle= 8
  ; preserved_cycles= 2
  ; proof_of_work_threshold= -1
  ; blocks_per_commitment= 4
  ; endorsers_per_block= 32
  ; hard_gas_limit_per_operation= 800000
  ; hard_gas_limit_per_block= 8000000
  ; tokens_per_roll= 8000000000
  ; michelson_maximum_type_size= 1000
  ; seed_nonce_revelation_tip= 125000
  ; origination_size= 257
  ; block_security_deposit= 512000000
  ; endorsement_security_deposit= 64000000
  ; block_reward= 16000000
  ; endorsement_reward= 2000000
  ; hard_storage_limit_per_operation= 60000
  ; cost_per_byte= 1000
  ; test_chain_duration= 1966080
  ; quorum_min= 3000
  ; quorum_max= 7000
  ; min_proposal_quorum= 500
  ; initial_endorsers= 1
  ; delay_per_missing_endorsement= 1 }

let protocol_parameters_json t : Ezjsonm.t =
  let open Ezjsonm in
  let make_account (account, amount) =
    strings [Account.pubkey account; sprintf "%Ld" amount] in
  (* let make_contract (deleg, amount, script) = dict
      [ ("delegate", string (Account.pubkey_hash deleg))
      ; ("amount", ksprintf string "%d" amount)
      ; ("script", (Script.load script :> Ezjsonm.value)) ] in
  *)
  dict
    [ ( "bootstrap_accounts"
      , list make_account (t.bootstrap_accounts @ [(t.dictator, 10_000_000L)])
      )
      (* ; ("bootstrap_contracts", list make_contract t.bootstrap_contracts) *)
    ; ("time_between_blocks", list (ksprintf string "%d") t.time_between_blocks)
    ; ("blocks_per_roll_snapshot", int t.blocks_per_roll_snapshot)
    ; ("blocks_per_voting_period", int t.blocks_per_voting_period)
    ; ("blocks_per_cycle", int t.blocks_per_cycle)
    ; ("preserved_cycles", int t.preserved_cycles)
    ; ( "proof_of_work_threshold"
      , ksprintf string "%d" t.proof_of_work_threshold )
      (* which constants are encoded in json as int or string looks a bit arbitrary
       e.g. michelson_maximum_type_size=1000 is an int but cost_per_byte=1000 is
       a string *)
    ; ("blocks_per_commitment", int t.blocks_per_commitment)
    ; ("endorsers_per_block", int t.endorsers_per_block)
    ; ( "hard_gas_limit_per_operation"
      , string (Int.to_string t.hard_gas_limit_per_operation) )
    ; ( "hard_gas_limit_per_block"
      , string (Int.to_string t.hard_gas_limit_per_block) )
    ; ("tokens_per_roll", string (Int.to_string t.tokens_per_roll))
    ; ("michelson_maximum_type_size", int t.michelson_maximum_type_size)
    ; ( "seed_nonce_revelation_tip"
      , string (Int.to_string t.seed_nonce_revelation_tip) )
    ; ("origination_size", int t.origination_size)
    ; ( "block_security_deposit"
      , string (Int.to_string t.block_security_deposit) )
    ; ( "endorsement_security_deposit"
      , string (Int.to_string t.endorsement_security_deposit) )
    ; ("block_reward", string (Int.to_string t.block_reward))
    ; ("endorsement_reward", string (Int.to_string t.endorsement_reward))
    ; ( "hard_storage_limit_per_operation"
      , string (Int.to_string t.hard_storage_limit_per_operation) )
    ; ("cost_per_byte", string (Int.to_string t.cost_per_byte))
    ; ("test_chain_duration", string (Int.to_string t.test_chain_duration))
    ; ("quorum_min", int t.quorum_min)
    ; ("quorum_max", int t.quorum_max)
    ; ("min_proposal_quorum", int t.min_proposal_quorum)
    ; ("initial_endorsers", int t.initial_endorsers)
    ; ( "delay_per_missing_endorsement"
      , string (Int.to_string t.delay_per_missing_endorsement) ) ]

let sandbox {dictator; _} =
  let pk = Account.pubkey dictator in
  Ezjsonm.to_string (`O [("genesis_pubkey", `String pk)])

let protocol_parameters t =
  Ezjsonm.to_string ~minify:false (protocol_parameters_json t)

let expected_pow t = t.expected_pow
let id t = t.id
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
      Lwt_exception.fail (Failure "sys.command non-zero")
        ~attach:[("location", `String_value "Tezos_protocol.ensure")]

let cli_term () =
  let open Cmdliner in
  let open Term in
  let docs = "PROTOCOL OPTIONS" in
  pure
    (fun remove_default_bas
         (`Blocks_per_voting_period bpvp)
         (`Protocol_hash hashopt)
         (`Time_between_blocks tbb)
         add_bootstraps
         ->
      let d = default () in
      let id =
        if add_bootstraps = [] && remove_default_bas = false then d.id
        else "default-and-command-line" in
      let time_between_blocks =
        Option.value tbb ~default:d.time_between_blocks in
      let bootstrap_accounts =
        add_bootstraps
        @ if remove_default_bas then [] else d.bootstrap_accounts in
      let blocks_per_voting_period =
        match bpvp with Some v -> v | None -> d.blocks_per_voting_period in
      let hash = Option.value hashopt ~default:d.hash in
      { d with
        id
      ; hash
      ; bootstrap_accounts
      ; time_between_blocks
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
          (opt (some int) None
             (info
                ["blocks-per-voting-period"]
                ~doc:"Set the length of voting periods.")))
  $ Arg.(
      pure (fun x -> `Protocol_hash x)
      $ value
          (opt (some string) None
             (info ["protocol-hash"] ~doc:"Set the (initial) protocol hash.")))
  $ Arg.(
      pure (fun x -> `Time_between_blocks x)
      $ value
          (opt
             (some (list ~sep:',' int))
             None
             (info ["time-between-blocks"] ~docv:"COMMA-SEPARATED-SECONDS"
                ~doc:
                  "Set the time between blocks bootstrap-parameter, e.g. \
                   `2,3,2`.")))
  $ Arg.(
      pure (fun l ->
          List.map l ~f:(fun ((name, pubkey, pubkey_hash, private_key), tez) ->
              (Account.key_pair name ~pubkey ~pubkey_hash ~private_key, tez)))
      $ value
          (opt_all
             (pair ~sep:'@' (t4 ~sep:',' string string string string) int64)
             []
             (info ["add-bootstrap-account"]
                ~docv:"NAME,PUBKEY,PUBKEY-HASH,PRIVATE-URI@MUTEZ-AMOUNT"
                ~doc:
                  "Add a custom bootstrap account, e.g. \
                   `LedgerBaker,edpku...,tz1YPS...,ledger://crouching-tiger.../ed25519/0'/0'@20_000_000_000`.")))
