(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Dal = struct
  type t =
    | Custom of int list
    | Mimic of {network : Network.public; max_nb_bakers : int option}

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 1)
          ~title:"custom"
          (list int31)
          (function Custom l -> Some l | _ -> None)
          (fun l -> Custom l);
        case
          (Tag 2)
          ~title:"mimic"
          (obj2
             (req "network" Network.public_encoding)
             (opt "max_nb_bakers" int31))
          (function
            | Mimic {network; max_nb_bakers} -> Some (network, max_nb_bakers)
            | _ -> None)
          (fun (network, max_nb_bakers) -> Mimic {network; max_nb_bakers});
      ]

  let typ =
    let open Network in
    let parse_public_network (net : string) : public option =
      try Option.map to_public (parse net) with _ -> None
    in
    Clap.typ
      ~name:"stake_repartition"
      ~dummy:(Custom [100])
      ~parse:(fun str ->
        (* If it is a list of int, then a custom repartition has been selected. *)
        let int_list_regexp = Str.regexp {|\([0-9]+,\( ?\)\)*[0-9]+$|} in
        if Str.string_match int_list_regexp str 0 then
          Some
            (Custom (str |> String.split_on_char ',' |> List.map int_of_string))
          (* Else we expect a network name, potentially followed by how many bakers should be created. *)
        else
          match String.split_on_char '_' str with
          | [network] ->
              Option.map
                (fun network -> Mimic {network; max_nb_bakers = None})
                (parse_public_network network)
          | [network; n_str] -> (
              try
                let n = int_of_string n_str in
                Option.map
                  (fun network -> Mimic {network; max_nb_bakers = Some n})
                  (parse_public_network network)
              with _ -> None)
          | _ -> None)
      ~show:(function
        | Custom l ->
            l |> List.map string_of_int |> String.concat (String.make 1 ',')
        | Mimic {network; max_nb_bakers = None} -> to_string network
        | Mimic {network; max_nb_bakers = Some n} ->
            Format.sprintf "%s_%d" (to_string network) n)

  let parse_arg ~stake_arg ~simulation_arg =
    let open Network in
    match simulation_arg with
    | Network_simulation.Disabled -> (
        match stake_arg with
        | Custom distrib -> return distrib
        | Mimic {network; max_nb_bakers} ->
            let network_string =
              match network with
              | `Mainnet | `Ghostnet | `Seoulnet | `Tallinnnet ->
                  to_string network
              | _ ->
                  failwith
                    (Format.sprintf
                       "Cannot get stake distribution for %s"
                       (to_string network))
            in
            let endpoint =
              Endpoint.make ~host:"rpc.tzkt.io" ~scheme:"https" ~port:443 ()
            in
            let decoder json = JSON.(json |-> "cycle" |> as_int) in
            let rpc =
              RPC_core.(
                make
                  GET
                  [
                    network_string;
                    "chains";
                    "main";
                    "blocks";
                    "head";
                    "helpers";
                    "current_level";
                  ]
                  decoder)
            in
            let* response = RPC_core.call_raw endpoint rpc in
            let cycle =
              RPC_core.decode_raw ~origin:"Network.cycle" rpc response.body
            in
            let get_stake_in_ktez stake =
              JSON.(
                (stake |-> "frozen" |> as_int)
                + (stake |-> "delegated" |> as_int))
              / 1_000_000_000
            in
            let decoder json =
              json |> JSON.as_list
              |> List.map (fun json_account ->
                     let active_stake =
                       JSON.(json_account |-> "active_stake")
                     in
                     get_stake_in_ktez active_stake)
            in
            let rpc =
              RPC_core.(
                make
                  GET
                  [
                    network_string;
                    "chains";
                    "main";
                    "blocks";
                    "head";
                    "context";
                    "raw";
                    "json";
                    "cycle";
                    string_of_int cycle;
                    "selected_stake_distribution";
                  ]
                  decoder)
            in
            let* response = RPC_core.call_raw endpoint rpc in
            let distribution =
              RPC_core.decode_raw ~origin:"Network.cycle" rpc response.body
            in
            let distribution =
              match max_nb_bakers with
              | None -> distribution
              | Some n -> Tezos_stdlib.TzList.take_n n distribution
            in
            return distribution)
    | Scatter _ | Map _ -> (
        match stake_arg with
        | Custom [] ->
            (* As simulate_network and stake are mutually exclusive, only empty
             stake option is allowed. *)
            Lwt.return []
        | _ ->
            Test.fail
              "Options --simulate and --stake are mutually exclusive. We \
               cannot set baker stake while using baking power of bakers from \
               a simulated network.")
end

module Layer1 = struct
  type t = Auto | Manual of int list

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"auto"
          (constant "auto")
          (function Auto -> Some () | _ -> None)
          (fun () -> Auto);
        case
          (Tag 1)
          ~title:"manual"
          (list int31)
          (function Manual d -> Some d | _ -> None)
          (fun d -> Manual d);
      ]

  let typ =
    let parse = function
      | "AUTO" | "auto" -> Some Auto
      | string -> (
          try
            match string |> String.split_on_char ',' with
            | [n] -> Some (Manual (List.init (int_of_string n) (fun _ -> 1)))
            | distribution ->
                Some (Manual (List.map int_of_string distribution))
          with exn -> raise exn)
    in
    let show = function
      | Auto -> "AUTO"
      | Manual dist -> List.map string_of_int dist |> String.concat ","
    in
    Clap.typ ~name:"stake" ~dummy:(Manual [1]) ~parse ~show
end
