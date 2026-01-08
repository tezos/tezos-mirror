(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Default parameter values *)

let default_node_endpoint =
  Format.sprintf
    "http://localhost:%d"
    Octez_node_config.Config_file.default_rpc_port

let default_daily_logs_path = Some "octez-baker"

let extra_levels_for_old_baker = 3

(* Protocol status *)

type status = Active | Frozen

let pp_status fmt status =
  Format.fprintf
    fmt
    "%s"
    (match status with Active -> "active" | Frozen -> "frozen")

let status_encoding =
  let open Data_encoding in
  def
    "status"
    ~title:"protocol status"
    ~description:"Status of a protocol"
    (union
       ~tag_size:`Uint8
       [
         case
           ~title:"active"
           ~description:"Active protocols are currently used on a network."
           (Tag 0)
           (constant "active")
           (function Active -> Some () | _ -> None)
           (fun () -> Active);
         case
           ~title:"frozen"
           ~description:
             "Frozen protocols are not currently used on any network."
           (Tag 1)
           (constant "frozen")
           (function Frozen -> Some () | _ -> None)
           (fun () -> Frozen);
       ])

(** Lists of known protocol hashes for [Frozen] and [Active] protocols, corresponding
    to [Manifest/Product_octez/Protocol] module. *)
let frozen_protocols =
  [
    "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im";
    "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
    "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY";
    "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt";
    "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP";
    "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd";
    "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU";
    "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS";
    "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb";
    "PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo";
    "PtEdoTezd3RHSC31mpxxo1npxFjoWWcFgQtxapi51Z8TLu6v6Uq";
    "PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA";
    "PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i";
    "PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV";
    "PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx";
    "Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A";
    "PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY";
    "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg";
    "PtLimaPtLMwfNinJi9rCfDPWea8dFgTZ1MeJ9f1m2SRic6ayiwW";
    "PtMumbai2TmsJHNGRkD8v8YDbtao7BLUC3wjASn1inAKLFCjaH1";
    "PtNairobiyssHuh87hEhfVBGCVrK3WnS8Z2FT4ymB5tAa4r1nQf";
    "ProxfordYmVfjWnRcgjWH36fW6PArwqykTFzotUxRs6gmTcZDuH";
    "PtParisBxoLz5gzMmn3d9WBQNoPSZakgnkMC2VNuQ3KXfUtUQeZ";
    "PsParisCZo7KAh1Z1smVd9ZMZ1HHn5gkzbM94V3PLCpknFWhUAi";
  ]

(** If this list format is modified, subsequent modifications must be carried onto
    the agnostic baker section from [scripts/proto_manager.sh]. *)
let active_protocols =
  [
    "PsQuebecnLByd3JwTiGadoG4nGWi3HYiLXUjkibeFV8dCFeVMUg";
    "PsRiotumaAMotcRoDWW1bysEhQy2n1M5fy8JgRp8jjRfHGmfeA7";
    "Ps3BVMUNQhj15D1VXdFnjP91y11jYShkZRJnBx3N7koz2dKRndB";
    "PtSEouLov7Fp6XoqXUBqd7XzggUpUarSMcSUsR5MarqspqiuQBY";
    "Psbm9fXge5noCDc2pcwtjnUs4FuAuxmRh6ts8kyPk3CpJVFFQhN";
    "PsD5wVTJc9Rg228rXbXbeoeEo8g3fgWH211U7V3qjUed11g5Gqk";
    "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK";
  ]

let protocol_status proto_hash =
  match Protocol_hash.to_b58check proto_hash with
  | hash when List.mem ~equal:String.equal hash active_protocols -> Active
  | hash when List.mem ~equal:String.equal hash frozen_protocols -> Frozen
  | _ -> (* We assume that unmatched protocols are "next" ones *) Active
