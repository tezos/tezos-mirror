(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let default_node_addr = "http://127.0.0.1:8732"

let default_daily_logs_path = Some "octez-agnostic-baker"

let log_config ~base_dir =
  let base_dir : string =
    match base_dir with
    | Some p -> p
    | None -> Tezos_client_base_unix.Client_config.Cfg_file.default.base_dir
  in
  let daily_logs_path =
    default_daily_logs_path
    |> Option.map Filename.Infix.(fun logdir -> base_dir // "logs" // logdir)
  in
  Tezos_base_unix.Internal_event_unix.make_with_defaults
    ?enable_default_daily_logs_at:daily_logs_path
    ~log_cfg:Tezos_base_unix.Logs_simple_config.default_cfg
    ()

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
           ~description:
             "Active protocols are currently used on a network, and thus, they \
              have dedicated delegate binaries."
           (Tag 0)
           (constant "active")
           (function Active -> Some () | _ -> None)
           (fun () -> Active);
         case
           ~title:"frozen"
           ~description:
             "Frozen protocols are currently unused on any network, and thus, \
              they do nothave dedicated delegate binaries."
           (Tag 1)
           (constant "frozen")
           (function Frozen -> Some () | _ -> None)
           (fun () -> Frozen);
       ])

(* From Manifest/Product_octez/Protocol*)
let protocol_info = function
  | ( "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im"
    | "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"
    | "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY"
    | "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt"
    | "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP"
    | "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
    | "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU"
    | "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS"
    | "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb"
    | "PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo"
    | "PtEdoTezd3RHSC31mpxxo1npxFjoWWcFgQtxapi51Z8TLu6v6Uq"
    | "PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA"
    | "PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i"
    | "PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV"
    | "PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx"
    | "Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A"
    | "PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY"
    | "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg"
    | "PtLimaPtLMwfNinJi9rCfDPWea8dFgTZ1MeJ9f1m2SRic6ayiwW"
    | "PtMumbai2TmsJHNGRkD8v8YDbtao7BLUC3wjASn1inAKLFCjaH1"
    | "PtNairobiyssHuh87hEhfVBGCVrK3WnS8Z2FT4ymB5tAa4r1nQf"
    | "ProxfordYmVfjWnRcgjWH36fW6PArwqykTFzotUxRs6gmTcZDuH"
    | "PtParisBxoLz5gzMmn3d9WBQNoPSZakgnkMC2VNuQ3KXfUtUQeZ" ) as full_hash ->
      (String.sub full_hash 0 8, Frozen)
  | ( "PsParisCZo7KAh1Z1smVd9ZMZ1HHn5gkzbM94V3PLCpknFWhUAi"
    | "PsQuebecnLByd3JwTiGadoG4nGWi3HYiLXUjkibeFV8dCFeVMUg" ) as full_hash ->
      (String.sub full_hash 0 8, Active)
  | "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK" -> ("alpha", Active)
  | _ -> (*We assume that unmatched protocols are beta ones*) ("beta", Active)

let protocol_short_hash h = fst (protocol_info h)

let protocol_status h = snd (protocol_info h)
