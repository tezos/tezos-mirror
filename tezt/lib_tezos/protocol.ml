(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(* Declaration order must respect the version order. *)
type t = Mumbai | Nairobi | Oxford | Alpha

type constants = Constants_sandbox | Constants_mainnet | Constants_test

let name = function
  | Alpha -> "Alpha"
  | Mumbai -> "Mumbai"
  | Nairobi -> "Nairobi"
  | Oxford -> "Oxford"

let number = function
  | Mumbai -> 016
  | Nairobi -> 017
  | Oxford -> 018
  | Alpha -> 019

let directory = function
  | Alpha -> "proto_alpha"
  | Mumbai -> "proto_016_PtMumbai"
  | Nairobi -> "proto_017_PtNairob"
  | Oxford -> "proto_018_Proxford"

(* Test tags must be lowercase. *)
let tag protocol = String.lowercase_ascii (name protocol)

let hash = function
  | Alpha -> "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
  | Mumbai -> "PtMumbai2TmsJHNGRkD8v8YDbtao7BLUC3wjASn1inAKLFCjaH1"
  | Nairobi -> "PtNairobiyssHuh87hEhfVBGCVrK3WnS8Z2FT4ymB5tAa4r1nQf"
  | Oxford -> "ProxfordLcjScHFgpTURyTjcKR88D5juu1jGgT18pPUoGTVgxiA"

let genesis_hash = "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im"

let demo_noops_hash = "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

let demo_counter_hash = "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT"

let protocol_zero_hash = "PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i"

let default_constants = Constants_sandbox

let parameter_file ?(constants = default_constants) protocol =
  let name =
    match constants with
    | Constants_sandbox -> "sandbox"
    | Constants_mainnet -> "mainnet"
    | Constants_test -> "test"
  in
  sf "src/%s/parameters/%s-parameters.json" (directory protocol) name

let daemon_name = function Alpha -> "alpha" | p -> String.sub (hash p) 0 8

let accuser proto = "./octez-accuser-" ^ daemon_name proto

let baker proto = "./octez-baker-" ^ daemon_name proto

let sc_rollup_node proto = "./octez-smart-rollup-node-" ^ daemon_name proto

let sc_rollup_client proto = "./octez-smart-rollup-client-" ^ daemon_name proto

let encoding_prefix = function
  | Alpha -> "alpha"
  | p -> sf "%03d-%s" (number p) (String.sub (hash p) 0 8)

type parameter_overrides =
  (string list * [`None | `Int of int | `String_of_int of int | JSON.u]) list

let default_bootstrap_accounts =
  Array.to_list Account.Bootstrap.keys |> List.map @@ fun key -> (key, None)

type bootstrap_contract = {
  delegate : string option;
  amount : Tez.t;
  script : Ezjsonm.value;
  hash : string option;
}

type bootstrap_smart_rollup = {
  address : string;
  pvm_kind : string;
  boot_sector : string;
  parameters_ty : Ezjsonm.value;
}

let write_parameter_file :
    ?bootstrap_accounts:(Account.key * int option) list ->
    ?additional_bootstrap_accounts:(Account.key * int option * bool) list ->
    ?bootstrap_smart_rollups:bootstrap_smart_rollup list ->
    ?bootstrap_contracts:bootstrap_contract list ->
    base:(string, t * constants option) Either.t ->
    parameter_overrides ->
    string Lwt.t =
 fun ?(bootstrap_accounts = default_bootstrap_accounts)
     ?(additional_bootstrap_accounts = [])
     ?(bootstrap_smart_rollups = [])
     ?(bootstrap_contracts = [])
     ~base
     parameter_overrides ->
  (* make a copy of the parameters file and update the given constants *)
  let overriden_parameters = Temp.file "parameters.json" in
  let original_parameters =
    let file =
      Either.fold
        ~left:Fun.id
        ~right:(fun (x, constants) -> parameter_file ?constants x)
        base
    in
    JSON.parse_file file |> JSON.unannotate
  in
  let parameter_overrides =
    if List.mem_assoc ["bootstrap_accounts"] parameter_overrides then
      parameter_overrides
    else
      let bootstrap_accounts =
        List.map
          (fun ((account : Account.key), default_balance) ->
            `A
              [
                `String account.public_key;
                `String
                  (string_of_int
                     (Option.value ~default:4000000000000 default_balance));
              ])
          bootstrap_accounts
      in
      (["bootstrap_accounts"], `A bootstrap_accounts) :: parameter_overrides
  in
  let parameter_overrides =
    if List.mem_assoc ["bootstrap_smart_rollups"] parameter_overrides then
      parameter_overrides
    else
      let bootstrap_smart_rollups =
        List.map
          (fun {address; pvm_kind; boot_sector; parameters_ty} ->
            `O
              [
                ("address", `String address);
                ("pvm_kind", `String pvm_kind);
                ("kernel", `String boot_sector);
                ("parameters_ty", parameters_ty);
              ])
          bootstrap_smart_rollups
      in
      match bootstrap_smart_rollups with
      | [] ->
          (* This is useful for tests on protocol before bootstrap smart rollups
             support. Otherwise the protocol cannot decode this new field. *)
          parameter_overrides
      | bootstrap_smart_rollups ->
          (["bootstrap_smart_rollups"], `A bootstrap_smart_rollups)
          :: parameter_overrides
  in
  let parameter_overrides =
    if List.mem_assoc ["bootstrap_contracts"] parameter_overrides then
      parameter_overrides
    else
      let bootstrap_contracts =
        List.map
          (fun {delegate; amount; script; hash} ->
            let delegate =
              match delegate with
              | Some delegate -> [("delegate", `String delegate)]
              | None -> []
            in
            let hash =
              match hash with
              | Some hash -> [("hash", `String hash)]
              | None -> []
            in
            `O
              ([("amount", `String (Tez.to_string amount)); ("script", script)]
              @ delegate @ hash))
          bootstrap_contracts
      in
      match bootstrap_contracts with
      | [] -> parameter_overrides
      | bootstrap_contracts ->
          (["bootstrap_contracts"], `A bootstrap_contracts)
          :: parameter_overrides
  in
  let parameters =
    List.fold_left
      (fun acc (path, value) ->
        let value =
          match value with
          | `None -> None
          | `Int i -> Some (`Float (float i))
          | `String_of_int i -> Some (`String (string_of_int i))
          | #JSON.u as value -> Some value
        in
        Ezjsonm.update acc path value)
      original_parameters
      parameter_overrides
  in
  let parameters =
    let path = ["bootstrap_accounts"] in
    let existing_accounts =
      Ezjsonm.get_list Fun.id (Ezjsonm.find parameters path)
    in
    let additional_bootstrap_accounts =
      List.map
        (fun ((account : Account.key), default_balance, is_revealed) ->
          `A
            [
              `String
                (if is_revealed then account.public_key
                else account.public_key_hash);
              `String
                (string_of_int
                   (Option.value ~default:4000000000000 default_balance));
            ])
        additional_bootstrap_accounts
    in
    Ezjsonm.update
      parameters
      path
      (Some (`A (existing_accounts @ additional_bootstrap_accounts)))
  in
  JSON.encode_to_file_u overriden_parameters parameters ;
  Lwt.return overriden_parameters

let next_protocol = function
  | Mumbai -> Some Nairobi
  | Nairobi -> Some Alpha
  | Oxford -> None
  | Alpha -> None

let previous_protocol = function
  | Alpha -> Some Nairobi
  | Oxford -> Some Nairobi
  | Nairobi -> Some Mumbai
  | Mumbai -> None

let all = [Mumbai; Nairobi; Oxford; Alpha]

type supported_protocols =
  | Any_protocol
  | From_protocol of int
  | Until_protocol of int
  | Between_protocols of int * int

let is_supported supported_protocols protocol =
  match supported_protocols with
  | Any_protocol -> true
  | From_protocol n -> number protocol >= n
  | Until_protocol n -> number protocol <= n
  | Between_protocols (a, b) ->
      let n = number protocol in
      a <= n && n <= b

let show_supported_protocols = function
  | Any_protocol -> "Any_protocol"
  | From_protocol n -> sf "From_protocol %d" n
  | Until_protocol n -> sf "Until_protocol %d" n
  | Between_protocols (a, b) -> sf "Between_protocol (%d, %d)" a b

let iter_on_supported_protocols ~title ~protocols ?(supports = Any_protocol) f =
  match List.filter (is_supported supports) protocols with
  | [] ->
      failwith
        (sf
           "test %s was registered with ~protocols:[%s] %s, which results in \
            an empty list of protocols"
           title
           (String.concat ", " (List.map name protocols))
           (show_supported_protocols supports))
  | supported_protocols -> List.iter f supported_protocols

(* Used to ensure that [register_test] and [register_regression_test]
   share the same conventions. *)
let add_to_test_parameters protocol title tags =
  (name protocol ^ ": " ^ title, tag protocol :: tags)

let register_test ~__FILE__ ~title ~tags ?supports body protocols =
  iter_on_supported_protocols ~title ~protocols ?supports @@ fun protocol ->
  let title, tags = add_to_test_parameters protocol title tags in
  Test.register ~__FILE__ ~title ~tags (fun () -> body protocol)

let register_long_test ~__FILE__ ~title ~tags ?supports ?team ~executors
    ~timeout body protocols =
  iter_on_supported_protocols ~title ~protocols ?supports @@ fun protocol ->
  let title, tags = add_to_test_parameters protocol title tags in
  Long_test.register ~__FILE__ ~title ~tags ?team ~executors ~timeout (fun () ->
      body protocol)

let register_regression_test ~__FILE__ ~title ~tags ?supports body protocols =
  iter_on_supported_protocols ~title ~protocols ?supports @@ fun protocol ->
  let title, tags = add_to_test_parameters protocol title tags in
  Regression.register ~__FILE__ ~title ~tags (fun () -> body protocol)
