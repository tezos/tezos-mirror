(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type alias = {alias : string; address : string; public_key : string}

type aliases = File of string | List of alias list

type consensus_key_mapping = {
  name : string;
  public_key_hash : string;
  consensus_public_key_hash : string;
  consensus_public_key : string;
}

type t = {path : string; runner : Runner.t option; name : string option}

let dump_aliases ?runner aliases =
  let filename = Temp.file ?runner "yes-wallet-aliases.json" in
  let pp_alias fmt {alias; address; public_key} =
    Format.fprintf
      fmt
      {|{
      "alias": "%s",
      "address": "%s",
      "publicKey": "%s"
      }|}
      alias
      address
      public_key
  in
  let pp_aliases_list =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") pp_alias
  in
  let contents = Format.asprintf {|[%a]|} pp_aliases_list aliases in
  let* () =
    match runner with
    | None -> write_file filename ~contents |> Lwt.return
    | Some runner ->
        let cmd =
          Runner.Shell.(redirect_stdout (cmd [] "echo" [contents]) filename)
        in
        let cmd, args = Runner.wrap_with_ssh runner cmd in
        Process.run cmd args
  in
  Lwt.return filename

let read_consensus_key_mapping ?runner client =
  match runner with
  | None -> assert false
  | Some runner ->
      let cmd =
        Runner.Shell.(
          cmd [] "cat" [Client.base_dir client // "consensus_keys_mapping"])
      in
      let cmd, args = Runner.wrap_with_ssh runner cmd in
      Process.spawn cmd args |> Process.check_and_read_stdout

let create ?runner ?(path = Uses.path Constant.yes_wallet) ?name () =
  {path; runner; name}

let create_from_context ?rich_accounts ?(aliases = List []) ~node ~client
    ~network t =
  let {path; name; runner} = t in
  let* aliases_filename =
    match aliases with
    | List aliases -> dump_aliases ?runner aliases
    | File filename -> Lwt.return filename
  in
  let* () =
    Process.spawn
      ?runner
      ?name
      path
      ([
         "create";
         "from";
         "context";
         Node.data_dir node;
         "in";
         Client.base_dir client;
         "--network";
         network;
         "--aliases";
         aliases_filename;
         "--force";
         "--active-bakers-only";
       ]
      @
      match rich_accounts with
      | None -> []
      | Some (nb_account, over) ->
          ["--rich-accounts-over"; sf "%d,%d" nb_account over])
    |> Process.check
  in
  Lwt.return aliases_filename

let convert_wallet_inplace ~client t =
  let {path; name; runner} = t in
  let args =
    ["convert"; "wallet"; Client.base_dir client; "inplace"; "--force"]
  in
  Process.spawn ?runner ?name path args |> Process.check

let load_consensus_key_mapping t ~client =
  let* content = read_consensus_key_mapping ?runner:t.runner client in
  let j =
    JSON.parse ~origin:"consensus_key_mapping_file" content |> JSON.as_list
  in
  List.map
    (fun e ->
      let name = JSON.(e |-> "name" |> as_string) in
      let public_key_hash = JSON.(e |-> "public_key_hash" |> as_string) in
      let consensus_public_key_hash =
        JSON.(e |-> "consensus_public_key_hash" |> as_string)
      in
      let consensus_public_key =
        JSON.(e |-> "consensus_public_key" |> as_string)
      in
      {name; public_key_hash; consensus_public_key_hash; consensus_public_key})
    j
  |> return
