(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** This is the module where the type of the CLI arguments configuration is
    created. An argument of this type will be passed to the main running
    function of the agnostic baker. *)

open Errors

type error += Bad_minimal_fees of string

type error += Bad_preserved_levels of string

let () =
  register_error_kind
    `Permanent
    ~id:"agnostic_baker.configuration.badMinimalFeesArg"
    ~title:"Bad -minimal-fees arg"
    ~description:"invalid fee threshold in -fee-threshold"
    ~pp:(fun ppf literal ->
      Format.fprintf ppf "invalid minimal fees '%s'" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_minimal_fees parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_minimal_fees parameter) ;
  register_error_kind
    `Permanent
    ~id:"badPreservedLevelsArg"
    ~title:"Bad -preserved-levels arg"
    ~description:"invalid number of levels in -preserved-levels"
    ~pp:(fun ppf literal ->
      Format.fprintf
        ppf
        "Bad argument value for -preserved_levels. Expected a positive \
         integer, but given '%s'"
        literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_preserved_levels parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_preserved_levels parameter)

(* Primitive argument parsers *)
let string_parameter =
  Tezos_clic.parameter
    (fun (_cctxt : Tezos_client_base.Client_context.full) x ->
      Lwt_result.return x)

let int_parameter =
  Tezos_clic.parameter (fun (cctxt : Tezos_client_base.Client_context.full) p ->
      try Lwt_result.return (int_of_string p)
      with _ -> cctxt#error "Cannot read int")

let uri_parameter =
  Tezos_clic.parameter
    (fun (_cctxt : Tezos_client_base.Client_context.full) x ->
      Lwt_result.return (Uri.of_string x))

let per_block_vote_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter
    ~autocomplete:(fun _ctxt -> return ["on"; "off"; "pass"])
    (fun (_cctxt : Tezos_client_base.Client_context.full) -> function
      | "on" -> return Per_block_votes.Per_block_vote_on
      | "off" -> return Per_block_votes.Per_block_vote_off
      | "pass" -> return Per_block_votes.Per_block_vote_pass
      | s ->
          failwith
            "unexpected vote: %s, expected either \"on\", \"off\", or \"pass\"."
            s)

(* Baker arguments *)
let pidfile_arg =
  let open Lwt_result_syntax in
  Tezos_clic.arg
    ~doc:"write process id in file"
    ~short:'P'
    ~long:"pidfile"
    ~placeholder:"filename"
    (Tezos_clic.parameter
       (fun (_cctxt : Tezos_client_base.Client_context.full) s -> return s))

let node_version_check_bypass_arg :
    (bool, Tezos_client_base.Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"node-version-check-bypass"
    ~doc:
      "If node-version-check-bypass flag is set, the baker will not check its \
       compatibility with the version of the node to which it is connected."
    ()

let node_version_allowed_arg =
  Tezos_clic.arg
    ~long:"node-version-allowed"
    ~placeholder:"<product>-[v]<major>.<minor>[.0]<info>[:<commit>]"
    ~doc:
      "When specified the baker will accept to run with a node of this \
       version. The specified version is composed of the product, for example \
       'octez'; the major and the minor versions that are positive integers; \
       the info, for example '-rc', '-beta1+dev' or realese if none is \
       provided; optionally the commit that is the hash of the last git commit \
       or a prefix of at least 8 characters long."
    string_parameter

let default_minimal_fees = 100L

let default_minimal_nanotez_per_gas_unit = Q.of_int 100

let default_minimal_nanotez_per_byte = Q.of_int 1000

let minimal_fees_arg =
  let open Lwt_result_syntax in
  Tezos_clic.default_arg
    ~long:"minimal-fees"
    ~placeholder:"amount"
    ~doc:"exclude operations with fees lower than this threshold (in tez)"
    ~default:(Int64.to_string default_minimal_fees)
    (Tezos_clic.parameter
       (fun (_cctxt : Tezos_client_base.Client_context.full) s ->
         match Int64.of_string_opt s with
         | Some t -> return t
         | None -> tzfail (Bad_minimal_fees s)))

let minimal_nanotez_per_gas_unit_arg =
  let open Lwt_result_syntax in
  Tezos_clic.default_arg
    ~long:"minimal-nanotez-per-gas-unit"
    ~placeholder:"amount"
    ~doc:
      "exclude operations with fees per gas lower than this threshold (in \
       nanotez)"
    ~default:(Q.to_string default_minimal_nanotez_per_gas_unit)
    (Tezos_clic.parameter
       (fun (_cctxt : Tezos_client_base.Client_context.full) s ->
         try return (Q.of_string s) with _ -> tzfail (Bad_minimal_fees s)))

let minimal_nanotez_per_byte_arg =
  let open Lwt_result_syntax in
  Tezos_clic.default_arg
    ~long:"minimal-nanotez-per-byte"
    ~placeholder:"amount"
    ~default:(Q.to_string default_minimal_nanotez_per_byte)
    ~doc:
      "exclude operations with fees per byte lower than this threshold (in \
       nanotez)"
    (Tezos_clic.parameter
       (fun (_cctxt : Tezos_client_base.Client_context.full) s ->
         try return (Q.of_string s) with _ -> tzfail (Bad_minimal_fees s)))

let force_apply_from_round_arg =
  Tezos_clic.arg
    ~long:"force-apply-from-round"
    ~placeholder:"round"
    ~doc:
      "Force the baker to not only validate but also apply operations starting \
       from the specified round."
    int_parameter

let keep_alive_arg :
    (bool, Tezos_client_base.Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~doc:
      "Keep the daemon process alive: when the connection with the node is \
       lost, the daemon periodically tries to reach it."
    ~short:'K'
    ~long:"keep-alive"
    ()

let liquidity_baking_toggle_vote_arg =
  Tezos_clic.arg
    ~doc:
      "Vote to continue or end the liquidity baking subsidy. The possible \
       values for this option are: \"off\" to request ending the subsidy, \
       \"on\" to request continuing or restarting the subsidy, and \"pass\" to \
       abstain. Note that this \"option\" is mandatory!"
    ~long:"liquidity-baking-toggle-vote"
    ~placeholder:"vote"
    per_block_vote_parameter

(* TODO: https://gitlab.com/tezos/tezos/-/issues/8055
   Remove this argument in Octez v25. *)
let adaptive_issuance_vote_arg =
  Tezos_clic.arg
    ~doc:
      "DEPRECATED: This argument is ignored by the baker and will be removed \
       in the next major version of Octez."
    ~long:"adaptive-issuance-vote"
    ~placeholder:"vote"
    per_block_vote_parameter

let per_block_vote_file_arg =
  Tezos_clic.arg
    ~doc:"read per block votes as json file"
    ~short:'V'
    ~long:"votefile"
    ~placeholder:"filename"
    (Tezos_clic.parameter
       (fun (_cctxt : Tezos_client_base.Client_context.full) file ->
         let open Lwt_result_syntax in
         let* file_exists =
           protect
             ~on_error:(fun _ -> tzfail (Block_vote_file_not_found file))
             (fun () ->
               let*! b = Lwt_unix.file_exists file in
               return b)
         in
         if file_exists then return file
         else tzfail (Block_vote_file_not_found file)))

let http_headers_env_variable =
  "TEZOS_CLIENT_REMOTE_OPERATIONS_POOL_HTTP_HEADERS"

let http_headers =
  match Sys.getenv_opt http_headers_env_variable with
  | None -> None
  | Some contents ->
      let lines = String.split_on_char '\n' contents in
      Some
        (List.fold_left
           (fun acc line ->
             match String.index_opt line ':' with
             | None ->
                 invalid_arg
                   (Printf.sprintf
                      "Http headers: invalid %s environment variable, missing \
                       colon"
                      http_headers_env_variable)
             | Some pos ->
                 let header = String.trim (String.sub line 0 pos) in
                 let header = String.lowercase_ascii header in
                 if header <> "host" then
                   invalid_arg
                     (Printf.sprintf
                        "Http headers: invalid %s environment variable, only \
                         'host' headers are supported"
                        http_headers_env_variable) ;
                 let value =
                   String.trim
                     (String.sub line (pos + 1) (String.length line - pos - 1))
                 in
                 (header, value) :: acc)
           []
           lines)

let operations_arg =
  Tezos_clic.arg
    ~long:"operations-pool"
    ~placeholder:"file|uri"
    ~doc:
      (Printf.sprintf
         "When specified, the baker will try to fetch operations from this \
          file (or uri) and to include retrieved operations in the block. The \
          expected format of the contents is a list of operations [ \
          alpha.operation ].  Environment variable '%s' may also be specified \
          to add headers to the requests (only 'host' headers are supported). \
          If the resource cannot be retrieved, e.g., if the file is absent, \
          unreadable, or the web service returns a 404 error, the resource is \
          simply ignored."
         http_headers_env_variable)
    uri_parameter

let dal_node_endpoint_arg =
  Tezos_clic.arg
    ~long:"dal-node"
    ~placeholder:"uri"
    ~doc:"endpoint of the DAL node, e.g. 'http://localhost:8933'"
    uri_parameter

let without_dal_arg :
    (bool, Tezos_client_base.Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"without-dal"
    ~doc:
      "If without-dal flag is set, the daemon will not try to connect to a DAL \
       node."
    ()

let state_recorder_switch_arg :
    (bool, Tezos_client_base.Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"record-state"
    ~doc:
      "If record-state flag is set, the baker saves all its internal consensus \
       state in the filesystem, otherwise just in memory."
    ()

let pre_emptive_forge_time_arg =
  let open Lwt_result_syntax in
  Tezos_clic.arg
    ~long:"pre-emptive-forge-time"
    ~placeholder:"seconds"
    ~doc:
      "Sets the pre-emptive forge time optimization, in seconds. When set, the \
       baker, if it is the next level round 0 proposer, will start forging \
       after quorum has been reached in the current level while idly waiting \
       for it to end. When it is its time to propose, the baker will inject \
       the pre-emptively forged block immediately, allowing more time for the \
       network to reach quorum on it. Operators should note that the higher \
       this value `t`, the lower the operation inclusion window (specifically \
       `block_time - t`) which may lead to lower baking rewards. Defaults to \
       15/% of block time. Set to 0 to ignore pre-emptive forging."
    (Tezos_clic.parameter
       (fun (_cctxt : Tezos_client_base.Client_context.full) s ->
         try return (Q.of_string s)
         with _ -> failwith "pre-emptive-forge-time expected int or float."))

let remote_calls_timeout_arg =
  let open Lwt_result_syntax in
  Tezos_clic.arg
    ~long:"remote-calls-timeout"
    ~placeholder:"seconds"
    ~doc:
      "Sets a timeout for client calls such as signing block header or \
       attestation and for the creation of deterministic nonce. Use only if \
       your remote signer can handle concurrent requests."
    (Tezos_clic.parameter
       (fun (_cctxt : Tezos_client_base.Client_context.full) s ->
         try return (Q.of_string s)
         with _ -> failwith "remote-calls-timeout expected int or float."))

let allow_signing_delay_arg =
  Tezos_clic.switch
    ~long:"allow-signing-delay"
    ~doc:
      (Format.sprintf
         "Allow the use of a signing delay specified with the environment \
          variable %s for testing purposes. This is insecure and should never \
          be used in production."
         Octez_baking_common.Signing_delay.env_var)
    ()

let baker_args =
  Tezos_clic.args18
    pidfile_arg
    node_version_check_bypass_arg
    node_version_allowed_arg
    minimal_fees_arg
    minimal_nanotez_per_gas_unit_arg
    minimal_nanotez_per_byte_arg
    force_apply_from_round_arg
    keep_alive_arg
    liquidity_baking_toggle_vote_arg
    adaptive_issuance_vote_arg
    per_block_vote_file_arg
    operations_arg
    dal_node_endpoint_arg
    without_dal_arg
    state_recorder_switch_arg
    pre_emptive_forge_time_arg
    remote_calls_timeout_arg
    allow_signing_delay_arg

let directory_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter
    (fun (_cctxt : Tezos_client_base.Client_context.full) p ->
      let*! exists = Tezos_stdlib_unix.Lwt_utils_unix.dir_exists p in
      if not exists then failwith "Directory doesn't exist: '%s'" p
      else return p)

let sources_param =
  Tezos_clic.seq_of_param
    (Tezos_client_base.Client_keys.Public_key_hash.source_param
       ~name:"baker"
       ~desc:
         "name of the delegate owning the attestation/baking right or name of \
          the consensus key signing on the delegate's behalf")

let preserved_levels_arg =
  let open Lwt_result_syntax in
  Tezos_clic.default_arg
    ~long:"preserved-levels"
    ~placeholder:"threshold"
    ~doc:"Number of effective levels kept in the accuser's memory"
    ~default:"200"
    (Tezos_clic.parameter
       (fun (_cctxt : Tezos_client_base.Client_context.full) s ->
         try
           let preserved_levels = int_of_string s in
           if preserved_levels < 0 then tzfail (Bad_preserved_levels s)
           else return preserved_levels
         with _ -> tzfail (Bad_preserved_levels s)))

type t = {
  pidfile : string option;
  node_version_check_bypass : bool;
  node_version_allowed : string option;
  minimal_fees : int64;
  minimal_nanotez_per_gas_unit : Q.t;
  minimal_nanotez_per_byte : Q.t;
  force_apply_from_round : int option;
  keep_alive : bool;
  liquidity_baking_vote : Per_block_votes.per_block_vote option;
  adaptive_issuance_vote : Per_block_votes.per_block_vote option;
  per_block_vote_file : string option;
  extra_operations : Uri.t option;
  dal_node_endpoint : Uri.t option;
  without_dal : bool;
  state_recorder : bool;
  pre_emptive_forge_time : Q.t option;
  remote_calls_timeout : Q.t option;
  allow_signing_delay : bool;
}

(** Create the configuration from the given arguments. *)
let create_config
    ( pidfile,
      node_version_check_bypass,
      node_version_allowed,
      minimal_fees,
      minimal_nanotez_per_gas_unit,
      minimal_nanotez_per_byte,
      force_apply_from_round,
      keep_alive,
      liquidity_baking_vote,
      adaptive_issuance_vote,
      per_block_vote_file,
      extra_operations,
      dal_node_endpoint,
      without_dal,
      state_recorder,
      pre_emptive_forge_time,
      remote_calls_timeout,
      allow_signing_delay ) =
  {
    pidfile;
    node_version_check_bypass;
    node_version_allowed;
    minimal_fees;
    minimal_nanotez_per_gas_unit;
    minimal_nanotez_per_byte;
    force_apply_from_round;
    keep_alive;
    liquidity_baking_vote;
    adaptive_issuance_vote;
    per_block_vote_file;
    extra_operations;
    dal_node_endpoint;
    without_dal;
    state_recorder;
    pre_emptive_forge_time;
    remote_calls_timeout;
    allow_signing_delay;
  }

type per_block_votes_config = {
  vote_file : string option;
  liquidity_baking_vote : Per_block_votes.per_block_vote;
}
