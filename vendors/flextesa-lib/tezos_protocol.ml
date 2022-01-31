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
        Bytes.of_string
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
  type t = [`Proposal | `Exploration | `Cooldown | `Promotion | `Adoption]
end

module Protocol_kind = struct
  type t =
    [ `Athens
    | `Babylon
    | `Carthage
    | `Delphi
    | `Edo
    | `Florence
    | `Granada
    | `Hangzhou
    | `Ithaca
    | `Alpha ]

  let names =
    [ ("Athens", `Athens); ("Babylon", `Babylon); ("Carthage", `Carthage)
    ; ("Delphi", `Delphi); ("Edo", `Edo); ("Florence", `Florence)
    ; ("Granada", `Granada); ("Hangzhou", `Hangzhou); ("Ithaca", `Ithaca)
    ; ("Alpha", `Alpha) ]

  let ( < ) k1 k2 =
    let rec aux = function
      | [] -> assert false
      | (_, k) :: rest ->
          if Poly.equal k k2 then false
          else if Poly.equal k k1 then true
          else aux rest in
    aux names

  let default = `Alpha

  let cmdliner_term ~docs () : t Cmdliner.Term.t =
    let open Cmdliner in
    Arg.(
      value
        (opt (enum names) default
           (info ["protocol-kind"] ~docs ~doc:"Set the protocol family.") ))

  let pp ppf n =
    Fmt.string ppf
      (List.find_map_exn names ~f:(function
        | s, x when Poly.equal x n -> Some s
        | _ -> None ) )
end

type t =
  { id: string
  ; kind: Protocol_kind.t
  ; bootstrap_accounts: (Account.t * Int64.t) list
  ; dictator: Account.t
  ; expected_pow: int
  ; name: string (* e.g. alpha *)
  ; hash: string
  ; time_between_blocks: int list
  ; minimal_block_delay: int
  ; baking_reward_per_endorsement: int list
  ; endorsement_reward: int list
  ; blocks_per_roll_snapshot: int
  ; blocks_per_voting_period: int
  ; blocks_per_cycle: int
  ; preserved_cycles: int
  ; proof_of_work_threshold: int
  ; timestamp_delay: int option
  ; custom_protocol_parameters: Ezjsonm.t option }

let compare a b = String.compare a.id b.id

let make_bootstrap_accounts ~balance n =
  List.init n ~f:(fun n -> (Account.of_namef "bootacc-%d" n, balance))

let default () =
  let dictator = Account.of_name "dictator-default" in
  { id= "default-bootstrap"
  ; kind= Protocol_kind.default
  ; bootstrap_accounts= make_bootstrap_accounts ~balance:4_000_000_000_000L 4
  ; dictator
  ; expected_pow= 1
  ; name= "alpha"
  ; hash= "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
  ; time_between_blocks= [2; 3]
  ; minimal_block_delay= 2
  ; baking_reward_per_endorsement= [78_125; 11_719]
  ; endorsement_reward= [78_125; 52_083]
  ; blocks_per_roll_snapshot= 4
  ; blocks_per_voting_period= 16
  ; blocks_per_cycle= 8
  ; preserved_cycles= 2
  ; proof_of_work_threshold= -1
  ; timestamp_delay= None
  ; custom_protocol_parameters= None }

let protocol_parameters_json t : Ezjsonm.t =
  let open Ezjsonm in
  let make_account (account, amount) =
    strings [Account.pubkey account; sprintf "%Ld" amount] in
  let extra_post_babylon_stuff subkind =
    let alpha_specific_parameters =
      match subkind with
      | `Alpha ->
          [ ("tx_rollup_enable", bool false)
          ; (* TODO: https://gitlab.com/tezos/tezos/-/issues/2152 *)
            ("tx_rollup_origination_size", int 60_000)
          ; ("tx_rollup_hard_size_limit_per_inbox", int 100_000)
          ; ("tx_rollup_hard_size_limit_per_message", int 5_000)
          ; ("sc_rollup_enable", bool false)
          ; ("sc_rollup_origination_size", int 6_314)
          ]
      | `Hangzhou | `Ithaca -> []
      | _ -> failwith "unsupported protocol" in
    let list_of_zs = list (fun i -> string (Int.to_string i)) in
    let pre_alpha_specific_parameters =
      match subkind with
      | `Ithaca | `Alpha ->
          [ ("max_operations_time_to_live", int 120)
          ; ("blocks_per_stake_snapshot", int t.blocks_per_roll_snapshot)
          ; ("baking_reward_fixed_portion", string "10000000")
          ; ("baking_reward_bonus_per_slot", string "4286")
          ; ("endorsing_reward_per_slot", string "2857")
          ; ("consensus_committee_size", int 67); ("consensus_threshold", int 6)
          ; ( "minimal_participation_ratio"
            , dict [("numerator", int 2); ("denominator", int 3)] )
          ; ("minimal_block_delay", string (Int.to_string t.minimal_block_delay))
          ; ("delay_increment_per_round", string "1")
          ; ("max_slashing_period", int 2)
          ; ("frozen_deposits_percentage", int 10)
          ; ( "ratio_of_frozen_deposits_slashed_per_double_endorsement"
            , dict [("numerator", int 1); ("denominator", int 2)] )
          ; ("double_baking_punishment", string "640000000")
          ]
      | `Hangzhou ->
          [ ("blocks_per_roll_snapshot", int t.blocks_per_roll_snapshot)
          ; ("initial_endorsers", int 1)
          ; ("delay_per_missing_endorsement", string (Int.to_string 1))
          ; ( "time_between_blocks"
            , list (ksprintf string "%d") t.time_between_blocks )
          ; ("endorsers_per_block", int 56)
          ; ("block_security_deposit", string (Int.to_string 640_000_000))
          ; ("endorsement_security_deposit", string (Int.to_string 250_000))
          ; ( "baking_reward_per_endorsement"
            , list_of_zs t.baking_reward_per_endorsement )
          ; ("endorsement_reward", list_of_zs t.endorsement_reward)
          ; ("minimal_block_delay", string (Int.to_string t.minimal_block_delay))
          ]
      | _ -> failwith "unsupported protocol" in
    alpha_specific_parameters @ pre_alpha_specific_parameters in
  let common =
    [ ( "bootstrap_accounts"
      , list make_account (t.bootstrap_accounts @ [(t.dictator, 10_000_000L)])
      ); ("blocks_per_voting_period", int t.blocks_per_voting_period)
    ; ("blocks_per_cycle", int t.blocks_per_cycle)
    ; ("preserved_cycles", int t.preserved_cycles)
    ; ("proof_of_work_threshold", ksprintf string "%d" t.proof_of_work_threshold)
    ; ("blocks_per_commitment", int 4)
    ; ("hard_gas_limit_per_operation", string (Int.to_string 1_040_000))
    ; ("hard_gas_limit_per_block", string (Int.to_string 5_200_000))
    ; ("tokens_per_roll", string (Int.to_string 8_000_000_000))
    ; ("seed_nonce_revelation_tip", string (Int.to_string 125_000))
    ; ("origination_size", int 257)
    ; ("hard_storage_limit_per_operation", string (Int.to_string 60_000))
    ; ("cost_per_byte", string (Int.to_string 250)); ("quorum_min", int 3_000)
    ; ("quorum_max", int 7_000); ("min_proposal_quorum", int 500)
    ; ("liquidity_baking_subsidy", string "2500000")
    ; ("liquidity_baking_sunset_level", int 525600)
    ; ("liquidity_baking_escape_ema_threshold", int 1000000) ] in
  match t.custom_protocol_parameters with
  | Some s -> s
  | None -> dict (common @ extra_post_babylon_stuff t.kind)

let voting_period_to_string t (p : Voting_period.t) =
  (* This has to mimic: src/proto_alpha/lib_protocol/voting_period_repr.ml *)
  match p with
  | `Promotion ->
      if Protocol_kind.(t.kind < `Florence) then "promotion_vote"
      else "promotion"
  | `Exploration ->
      if Protocol_kind.(t.kind < `Florence) then "testing_vote"
      else "exploration"
  | `Proposal -> "proposal"
  | `Cooldown ->
      if Protocol_kind.(t.kind < `Florence) then "testing" else "cooldown"
  | `Adoption -> "adoption"

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
let sandbox_path config t = make_path config t // "sandbox.json"

let protocol_parameters_path config t =
  make_path config t // "protocol_parameters.json"

let ensure_script state t =
  let open Genspio.EDSL in
  let file string p =
    let path = p state t in
    ( Caml.Filename.basename path
    , write_stdout ~path:(str path)
        (feed ~string:(str (string t)) (exec ["cat"])) ) in
  check_sequence
    ~verbosity:(`Announce (sprintf "Ensure-protocol-%s" (id t)))
    [ ("directory", exec ["mkdir"; "-p"; make_path state t])
    ; file sandbox sandbox_path
    ; file protocol_parameters protocol_parameters_path ]

let ensure state t =
  Running_processes.run_successful_cmdf state "sh -c %s"
    (Genspio.Compile.to_one_liner (ensure_script state t) |> Caml.Filename.quote)
  >>= fun _ -> return ()

let cli_term state =
  let open Cmdliner in
  let open Term in
  let def = default () in
  let docs = Manpage_builder.section state ~rank:2 ~name:"PROTOCOL OPTIONS" in
  pure
    (fun
      bootstrap_accounts
      (`Blocks_per_voting_period blocks_per_voting_period)
      (`Protocol_hash hash)
      (`Time_between_blocks time_between_blocks)
      (`Minimal_block_delay minimal_block_delay)
      (`Blocks_per_cycle blocks_per_cycle)
      (`Preserved_cycles preserved_cycles)
      (`Timestamp_delay timestamp_delay)
      (`Protocol_parameters custom_protocol_parameters)
      kind
    ->
      let id = "default-and-command-line" in
      { def with
        id
      ; kind
      ; custom_protocol_parameters
      ; blocks_per_cycle
      ; hash
      ; bootstrap_accounts
      ; time_between_blocks
      ; minimal_block_delay
      ; preserved_cycles
      ; timestamp_delay
      ; blocks_per_voting_period } )
  $ Arg.(
      pure (fun remove_all nb balance add_bootstraps ->
          add_bootstraps
          @ make_bootstrap_accounts ~balance (if remove_all then 0 else nb) )
      $ value
          (flag
             (info
                ~doc:
                  "Do not create any of the default bootstrap accounts (this \
                   overrides `--number-of-bootstrap-accounts` with 0)."
                ~docs
                ["remove-default-bootstrap-accounts"] ) )
      $ value
          (opt int 4
             (info
                ["number-of-bootstrap-accounts"]
                ~docs ~doc:"Set the number of generated bootstrap accounts." ) )
      $ ( pure (function
            | `Tez, f -> f *. 1_000_000. |> Int64.of_float
            | `Mutez, f -> f |> Int64.of_float )
        $ value
            (opt
               (pair ~sep:':'
                  (enum [("tz", `Tez); ("tez", `Tez); ("mutez", `Mutez)])
                  float )
               (`Tez, 4_000_000.)
               (info
                  ["balance-of-bootstrap-accounts"]
                  ~docv:"UNIT:FLOAT" ~docs
                  ~doc:
                    "Set the initial balance of bootstrap accounts, for \
                     instance: `tz:2_000_000.42` or \
                     `mutez:42_000_000_000_000`." ) ) )
      $ Arg.(
          pure (fun l ->
              List.map l
                ~f:(fun ((name, pubkey, pubkey_hash, private_key), tez) ->
                  (Account.key_pair name ~pubkey ~pubkey_hash ~private_key, tez) ) )
          $ value
              (opt_all
                 (pair ~sep:'@' (t4 ~sep:',' string string string string) int64)
                 []
                 (info ["add-bootstrap-account"] ~docs
                    ~docv:"NAME,PUBKEY,PUBKEY-HASH,PRIVATE-URI@MUTEZ-AMOUNT"
                    ~doc:
                      "Add a custom bootstrap account, e.g. \
                       `LedgerBaker,edpku...,tz1YPS...,ledger://crouching-tiger.../ed25519/0'/0'@20_000_000_000`." ) )))
  $ Arg.(
      pure (fun x -> `Blocks_per_voting_period x)
      $ value
          (opt int def.blocks_per_voting_period
             (info ~docs
                ["blocks-per-voting-period"]
                ~doc:"Set the length of voting periods." ) ))
  $ Arg.(
      pure (fun x -> `Protocol_hash x)
      $ value
          (opt string def.hash
             (info ["protocol-hash"] ~docs
                ~doc:"Set the (initial) protocol hash." ) ))
  $ Arg.(
      pure (fun x -> `Time_between_blocks x)
      $ value
          (opt (list ~sep:',' int) def.time_between_blocks
             (info ["time-between-blocks"] ~docv:"COMMA-SEPARATED-SECONDS" ~docs
                ~doc:
                  "Set the time between blocks bootstrap-parameter, e.g. \
                   `2,3,2`." ) ))
  $ Arg.(
      pure (fun x -> `Minimal_block_delay x)
      $ value
          (opt int def.minimal_block_delay
             (info ["minimal-block-delay"] ~docv:"SECONDS" ~docs
                ~doc:
                  "Set the minimal delay between blocks bootstrap-parameter, \
                   e.g. `2`." ) ))
  $ Arg.(
      pure (fun x -> `Blocks_per_cycle x)
      $ value
          (opt int def.blocks_per_cycle
             (info ["blocks-per-cycle"] ~docv:"NUMBER" ~docs
                ~doc:"Number of blocks per cycle." ) ))
  $ Arg.(
      pure (fun x -> `Preserved_cycles x)
      $ value
          (opt int def.preserved_cycles
             (info ["preserved-cycles"] ~docv:"NUMBER" ~docs
                ~doc:
                  "Base constant for baking rights (search for \
                   `PRESERVED_CYCLES` in the white paper)." ) ))
  $ Arg.(
      pure (fun x -> `Timestamp_delay x)
      $ value
          (opt (some int) def.timestamp_delay
             (info ["timestamp-delay"] ~docv:"NUMBER" ~docs
                ~doc:"Protocol activation timestamp delay in seconds." ) ))
  $ Arg.(
      pure (fun f ->
          `Protocol_parameters
            (Option.map f ~f:(fun path ->
                 let i = Caml.open_in path in
                 Ezjsonm.from_channel i ) ) )
      $ value
          (opt (some file) None
             (info
                ["override-protocol-parameters"]
                ~doc:
                  "Use these protocol parameters instead of the generated ones \
                   (technically this invalidates most other options from a \
                   tezos-node point of view, use at your own risk)."
                ~docv:"JSON-FILE" ~docs ) ))
  $ Protocol_kind.cmdliner_term () ~docs

module Pretty_print = struct
  open More_fmt

  let verbatim_protection f ppf json_blob =
    try f ppf json_blob with e -> json ppf json_blob ; cut ppf () ; exn ppf e

  let fail_expecting s = failwith "PP: Expecting %s" s

  let mempool_pending_operations_rpc ppf mempool_json =
    let pp_op_list_short ppf l =
      let kinds =
        List.map l ~f:(fun js -> Jqo.(field ~k:"kind" js |> get_string)) in
      pf ppf "%s"
        ( List.fold kinds ~init:[] ~f:(fun prev k ->
              match prev with
              | (kind, n) :: more when String.equal kind k ->
                  (kind, n + 1) :: more
              | other -> (k, 1) :: other )
        |> List.map ~f:(function k, 1 -> k | k, n -> str "%s×%d" k n)
        |> String.concat ~sep:"+" ) in
    let open Jqo in
    match mempool_json with
    | `O four_fields ->
        List.iter four_fields ~f:(fun (name, content) ->
            pf ppf "@,* `%s`: " (String.capitalize name) ;
            match content with
            | `A [] -> pf ppf "Empty."
            | `A l -> (
              match name with
              | "applied" ->
                  List.iter l ~f:(fun op ->
                      let contents = field ~k:"contents" op |> get_list in
                      let pp_op_long ppf js =
                        match field ~k:"kind" js |> get_string with
                        | "transaction" ->
                            pf ppf "@,       * Mutez:%s: `%s` -> `%s`%s"
                              (field ~k:"amount" js |> get_string)
                              (field ~k:"source" js |> get_string)
                              (field ~k:"destination" js |> get_string)
                              ( try
                                  let _ = field ~k:"parameters" js in
                                  "+parameters"
                                with _ -> "" )
                        | "origination" ->
                            pf ppf
                              "@,       * Mutez:%s, source: `%s`, fee: `%s`"
                              (field ~k:"balance" js |> get_string)
                              (field ~k:"source" js |> get_string)
                              (field ~k:"fee" js |> get_string)
                        | _ -> () in
                      pf ppf "@,   * [%a] %a" pp_op_list_short contents
                        (long_string ~max:15)
                        (field ~k:"hash" op |> get_string) ;
                      List.iter contents ~f:(pp_op_long ppf) )
              | _other ->
                  List.iter l ~f:(function
                    | `A [`String opid; op] ->
                        let contents = field ~k:"contents" op |> get_list in
                        pf ppf "@,    * [%s]: %a" opid pp_op_list_short contents ;
                        pf ppf "@,    TODO: %a" json content
                    | _ -> fail_expecting "a operation tuple" ) )
            | _ -> fail_expecting "a list of operations" )
    | _ -> fail_expecting "a JSON object"

  let block_head_rpc ppf block_json =
    let open Jqo in
    let proto = field ~k:"protocol" block_json |> get_string in
    let hash = field ~k:"hash" block_json |> get_string in
    let metadata = field ~k:"metadata" block_json in
    let next_protocol = metadata |> field ~k:"next_protocol" |> get_string in
    let header = field ~k:"header" block_json in
    let level = field ~k:"level" header |> get_int in
    let timestamp = field ~k:"timestamp" header |> get_string in
    let voting_kind =
      metadata
      |> field ~k:"voting_period_info"
      |> field ~k:"voting_period" |> field ~k:"kind" |> get_string in
    let voting_pos =
      metadata
      |> field ~k:"voting_period_info"
      |> field ~k:"position" |> get_int in
    let voting_nth =
      metadata |> field ~k:"level" |> field ~k:"voting_period" |> get_int in
    let baker = metadata |> field ~k:"baker" |> get_string in
    pf ppf "Level %d | `%s` | %s" level hash timestamp ;
    pf ppf "@,* Protocol: `%s`" proto ;
    if String.equal proto next_protocol then pf ppf " (also next)"
    else pf ppf "@,* Next-protocol: `%s`" next_protocol ;
    pf ppf "@,* Voting period %d: `%s` (level: %d)" voting_nth voting_kind
      voting_pos ;
    pf ppf "@,* Baker: `%s`" baker
end
