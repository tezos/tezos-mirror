(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let wait_for_sync node =
  let filter json =
    let status = JSON.as_string json in
    Log.info "%s: %s" (Node.name node) status ;
    if String.equal status "synced" then Some () else None
  in
  Node.wait_for node "synchronisation_status.v0" filter

module Network = struct
  type network = Dailynet | Weeklynet

  let short_name = function Dailynet -> "dailynet" | Weeklynet -> "weeklynet"

  (* The network name is <kind>-yyyy-mm-dd, where <kind> is
     "dailynet"/"weeklynet". This function thus extracts the date of the last
     start of Dailynet/Weeklynet. Currently, Weeklynet is started on
     Wednesdays. *)
  let name = function
    | Dailynet -> (
        match
          Cli.get ~default:None (fun name -> Some (Some name)) "network"
        with
        | None ->
            let year, month, day = Ptime_clock.now () |> Ptime.to_date in
            Format.sprintf "dailynet-%d-%02d-%02d" year month day
        | Some network -> network)
    | Weeklynet -> (
        match
          Cli.get ~default:None (fun name -> Some (Some name)) "network"
        with
        | None ->
            let now = Ptime_clock.now () in
            (* offset of the current day wrt to Wednesday *)
            let day_offset = (4 + Ptime.weekday_num now) mod 7 in
            let year, month, day =
              Ptime.Span.of_d_ps (day_offset, 0L)
              |> Option.get |> Ptime.sub_span now |> Option.get |> Ptime.to_date
            in
            Format.sprintf "weeklynet-%d-%02d-%02d" year month day
        | Some network -> network)
end

let get_node client =
  let message =
    "Dal.get_node: Internal error: The client should have been initialised \
     with a local node"
  in
  match Client.get_mode client with
  | Client (endpoint, _media_type) -> (
      match endpoint with
      | Some (Node node) -> node
      | _ -> Test.fail "%s" message)
  | _ -> Test.fail "%s" message

let rec wait_for_balance client pkh target_balance =
  let node = get_node client in
  (* Could be racy but in practice should be ok, at worst we
     should wait one more block or do a useless check. *)
  let* level = Node.get_level node in
  let* _ = Node.wait_for_level node (level + 1) in
  let* balance = Client.get_balance_for ~account:pkh client in
  if balance < target_balance then (
    Log.info
      "Delegate current balance: %f. Expect at least: %f"
      (Tez.to_float balance)
      (Tez.to_float target_balance) ;
    wait_for_balance client pkh target_balance)
  else Lwt.return_unit

let reveal_accounts client accounts =
  let node = get_node client in
  let* unrevealed =
    Lwt_list.filter_s
      (fun account ->
        let* res =
          Node.RPC.(
            call node
            @@ get_chain_block_context_contract_manager_key
                 ~id:account.Account.public_key_hash
                 ())
        in
        let revealed = not (JSON.is_null res) in
        if revealed then
          Log.info "Address %s is already revealed." account.alias ;
        (not revealed) |> return)
      accounts
  in
  let* _ =
    Lwt_list.iter_s
      (fun account ->
        Log.info "Revealing address %s" account.Account.alias ;
        let op =
          Operation.Manager.reveal account
          |> Operation.Manager.make ~source:account
        in
        let* _op_hash = Operation.Manager.inject ~signer:account [op] client in
        return ())
      unrevealed
  in
  if List.length unrevealed > 0 then (
    let* level = Node.get_level node in
    Log.info
      "Waiting for a level (namely %d) to be \"sure\" the operations are \
       included..."
      (level + 1) ;
    let* _new_level = Node.wait_for_level node (level + 1) in
    return ())
  else return ()

module Wallet = struct
  let default_wallet network =
    Format.sprintf
      "%s/%s-wallet"
      (Filename.get_temp_dir_name ())
      (Network.name network)

  let wallet = Cli.get ~default:None (fun _ -> Some (Some ())) "wallet"

  let check_wallet client aliases =
    let* existing, missing =
      Lwt_list.fold_left_s
        (fun (existing, missing) alias ->
          let process = Client.spawn_show_address ~alias client in
          let* status = Process.wait process in
          match status with
          | WEXITED n when n = 0 ->
              let* client_output = Process.check_and_read_stdout process in
              let account = Account.parse_client_output ~alias ~client_output in
              (account :: existing, missing) |> return
          | WEXITED _ | WSIGNALED _ | WSTOPPED _ ->
              (existing, alias :: missing) |> return)
        ([], [])
        aliases
    in
    let print_strings =
      let open Format in
      pp_print_list
        ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
        pp_print_string
    in
    if List.length missing > 0 then
      Log.warn
        "Wallet.check_wallet: Aliases %a were not found in the given wallet. \
         Wallet path: %s"
        print_strings
        missing
        (Client.base_dir client) ;
    return (List.rev existing, List.rev missing)

  module Airdrop = struct
    let giver_alias = "giver"

    let giver_account =
      {
        (* That's dal_rich_account cf.
	   https://github.com/tacoinfra/teztnets/blob/main/networks/dailynet/values.yaml *)
        Account.alias = giver_alias;
        public_key_hash = "tz1PEhbjTyVvjQ2Zz8g4bYU2XPTbhvG8JMFh";
        public_key = "edpkuwL7MVYArfQN9jyR8pZTqmFGYFWTYhhF4F8KWjr2vB18ozTqbd";
        secret_key =
          Account.Unencrypted
            "edsk3AWajGUgzzGi3UrQiNWeRZR1YMRYVxfe642AFSKBTFXaoJp5hu";
      }

    let register_giver client =
      let Account.{alias; secret_key; _} = giver_account in
      Client.import_secret_key client ~alias secret_key

    let perform_transfers ~amount ~keys client =
      let* counter =
        Operation.Manager.get_next_counter ~source:giver_account client
      in
      let ops =
        List.map
          (fun dest ->
            Operation.Manager.transfer ~dest ~amount:(Tez.to_mutez amount) ())
          keys
        |> Operation.Manager.make_batch ~counter ~source:giver_account
      in
      Operation.Manager.inject ops client

    let distribute_money ?(amount = Tez.of_int 10_000) client keys =
      let min_balance = Tez.(amount /! 2L) in
      let* low_balance_keys =
        Lwt_list.filter_p
          (fun key ->
            let* balance =
              Client.get_balance_for ~account:key.Account.public_key_hash client
            in
            return (balance < min_balance))
          keys
      in
      let* () = reveal_accounts client [giver_account] in
      let* () =
        if List.length low_balance_keys > 0 then (
          Log.info
            "Transferring %s tez to low-balance accounts..."
            (Tez.to_string amount) ;
          let* (`OpHash _h) =
            perform_transfers ~amount ~keys:low_balance_keys client
          in
          Lwt.join
            (List.map
               (fun key ->
                 wait_for_balance client key.Account.public_key_hash min_balance)
               keys))
        else return ()
      in
      Log.info "Reveal wallet addresses if needed..." ;
      reveal_accounts client keys
  end

  let gen_keys client aliases =
    Lwt_list.map_s
      (fun alias ->
        let* key = Client.gen_and_show_keys ~alias client in
        Lwt.return key)
      aliases

  let initialise_wallet network client aliases =
    let default_wallet = default_wallet network in
    (* Remove directory in case a wallet already exists but is not valid. *)
    let* () = Process.run "rm" ["-rf"; default_wallet] in
    let* () = Process.run "mkdir" [default_wallet] in
    let* () = Airdrop.register_giver client in
    gen_keys client aliases

  let load_wallet network client aliases =
    let* existing, missing = check_wallet client aliases in
    let num_existing = List.length existing in
    if num_existing > 0 then (
      Log.info "Existing wallet found. We reuse the existing keys." ;
      if List.length missing > 0 then
        let* new_keys = gen_keys client missing in
        existing @ new_keys |> return
      else return existing)
    else (
      Log.info "No alias found. A new wallet will be created" ;
      let* keys = initialise_wallet network client aliases in
      return keys)
end
