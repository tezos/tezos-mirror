(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(*
dune exec -- devtools/cloud-infrastructure/projects/nl-dal/octogram/dal_throughput_gen.exe \
   --working-dir `git rev-parse --show-toplevel`/devtools/cloud-infrastructure/projects/nl-dal/octogram
*)

open Stdlib
module MS = Map.Make (String)

let exe = Sys.argv.(0)

let sf = Format.sprintf

type default_value = {mainnet : string; sandbox : string; test : string}

module Options = struct
  type network = Mainnet | Sandbox | Test

  let network = ref Mainnet

  let auto = ref false

  let initial_values = ref MS.empty

  let output_file = ref "dal-throughput-scenario.yml"

  let input_file = ref (sf "%s.in" !output_file)

  let ip_addresses = ref "127.0.0.1"

  let preallocated_ip_addresses = ref 1

  let working_dir = ref "./"

  let octogram_path = ref "push: octogram"

  let gcloud_binaries_bucket = ref "dal-tfstate"

  let delegates_prefix = ref "delegate_"

  let balance_updates = ref ""
end

module Sys_command = struct
  type ('a, 'b) result_monad = Ok of 'a | Error of 'b

  let return a = Ok a

  let ( let* ) m f = match m with Ok a -> f a | Error b -> Error b

  let run cmd =
    let cmd0 = String.concat " " ["cd"; !Options.working_dir] in
    let cmd1 = String.concat " " cmd in
    let cmd = sf "%s && %s" cmd0 cmd1 in
    let retcode = Sys.command cmd in
    if retcode = 0 then Ok () else Error (cmd, retcode)

  let rec loop cmd = function
    | [] -> return ()
    | (var, value) :: l ->
        let* () = cmd var value in
        loop cmd l
end

module Process = struct
  let gen_agents ~kind ~prefix ~count ~ip_address_from =
    let name, cout = Filename.open_temp_file (sf "%s_" kind) "" in
    let ip_address_from = ref ip_address_from in
    for index = 0 to count - 1 do
      sf
        {|
  - name: "%s%d"
    address: %%%%ADDR_%d%%%%
    user: root
    port: 30000
    identity: ~/.ssh/tf
      |}
        prefix
        index
        !ip_address_from
      |> output_string cout ;
      incr ip_address_from
    done ;
    close_out cout ;
    name

  let sed_file target var subst_file =
    Sys_command.run
      [
        sf
          {|
          sed -i -e '/%%%%%s%%%%/ {
          r %s
          d
        }' %s |}
          var
          subst_file
          target;
      ]

  let sed target var subst =
    Sys_command.run
      ["sed"; "-i"; "-e"; sf "\"s#%%%%%s%%%%#%s#g\"" var subst; target]

  let subst_simple_occurences parameters =
    let open Options in
    let open Sys_command in
    let* () = run ["cp"; !input_file; !output_file] in
    let sed = sed !output_file in
    let* () = sed "OCTOGRAM_PATH" !octogram_path in
    let* () = sed "GCLOUD_BINARIES_BUCKET" !gcloud_binaries_bucket in
    let* () = sed "DELEGATES_PREFIX" !delegates_prefix in
    loop sed (MS.bindings parameters)

  let subst_agents number_of_attesters number_of_slots_producers =
    let open Options in
    let open Sys_command in
    let ip_address_from = !preallocated_ip_addresses + 1 in
    let attestots_agents =
      gen_agents
        ~kind:"attesters"
        ~prefix:!delegates_prefix
        ~count:number_of_attesters
        ~ip_address_from
    in
    let ip_address_from = ip_address_from + number_of_attesters in
    let slots_producers_agents =
      gen_agents
        ~kind:"slots_producers"
        ~prefix:""
        ~count:number_of_slots_producers
        ~ip_address_from
    in
    let* () = sed_file !output_file "ATTESTORS_AGENTS" attestots_agents in
    sed_file !output_file "SLOTS_PRODUCERS_AGENTS" slots_producers_agents

  let subst_ip_addresses number_of_attesters number_of_slots_producers =
    let open Options in
    let open Sys_command in
    let num_addresses =
      !preallocated_ip_addresses + number_of_attesters
      + number_of_slots_producers
    in
    match !ip_addresses with
    | "127.0.0.1" as localhost ->
        loop (sed !output_file)
        @@ List.init num_addresses (fun i -> (sf "ADDR_%d" (i + 1), localhost))
    | file ->
        let file =
          if Sys.file_exists file then file else sf "%s/%s" !working_dir file
        in
        let cin = open_in file in
        let data = Ezjsonm.from_channel cin in
        close_in cin ;
        let error () =
          failwith
            (sf
               "Invalid data format in %S. Expecting an array of addresses"
               file)
        in
        let data =
          match data with
          | `O _ -> error ()
          | `A l ->
              List.map (function `String s -> s | _ -> error ()) l
              |> Array.of_list
        in
        let data_len = Array.length data in
        if data_len < num_addresses then
          failwith
            (sf
               "Error: there are less IP addresses than expected. Given: %d. \
                Expected: %d "
               data_len
               num_addresses) ;
        loop (sed !output_file)
        @@ List.init num_addresses (fun i -> (sf "ADDR_%d" (i + 1), data.(i)))

  let subst_balance_updates number_of_attesters =
    let open Options in
    match !balance_updates with
    | "" -> sed !output_file "BALANCE_UPDATES" ""
    | file ->
        let file =
          if Sys.file_exists file then file else sf "%s/%s" !working_dir file
        in
        let cin = open_in file in
        let data = Ezjsonm.from_channel cin in
        close_in cin ;
        let error () =
          failwith
            (sf
               "Invalid data format in %S. Expecting an array of integers"
               file)
        in
        let data =
          match data with
          | `O _ -> error ()
          | `A l ->
              List.map (function `String s -> s | _ -> error ()) l
              |> (* Only keep balances of more than a roll *)
              List.filter (fun s -> int_of_string s >= 6000000)
              |> Array.of_list
        in
        let len = Array.length data in
        if len < number_of_attesters then
          failwith
            (sf
               "Too few balances: expected: %d, given: %d"
               number_of_attesters
               len) ;
        let gap = len / number_of_attesters in
        assert (gap >= 1) ;
        let balances_file, cout = Filename.open_temp_file "balances_" "" in
        (* If more balances than attesters, select balances uniformely. *)
        List.iter (fun i ->
            let ii = i * gap in
            output_string cout @@ sf "  ['delegate_%d', '%s']" i data.(ii) ;
            if i < number_of_attesters - 1 then output_string cout ",\n")
        @@ List.init number_of_attesters (fun i -> i) ;
        close_out cout ;
        sed_file !output_file "BALANCE_UPDATES" balances_file

  let main parameters =
    let open Sys_command in
    let int_of_string_key key = int_of_string @@ MS.find key parameters in
    let number_of_attesters = int_of_string_key "NUMBER_OF_ATTESTORS" in
    let number_of_slots_producers =
      int_of_string_key "NUMBER_OF_SLOTS_PRODUCERS"
    in
    let* () = subst_simple_occurences parameters in
    let* () = subst_agents number_of_attesters number_of_slots_producers in
    let* () =
      subst_ip_addresses number_of_attesters number_of_slots_producers
    in
    let* () = subst_balance_updates number_of_attesters in
    return ()
end

module Cli = struct
  let variables =
    [
      ("ATTESTATION_LAG", {mainnet = "4"; sandbox = "4"; test = "4"});
      ("REDUNDANCY_FACTOR", {mainnet = "16"; sandbox = "8"; test = "8"});
      ("NUMBER_OF_ATTESTORS", {mainnet = "1"; sandbox = "1"; test = "1"});
      ("NUMBER_OF_SLOTS_PRODUCERS", {mainnet = "1"; sandbox = "1"; test = "1"});
      ("MAX_NUMBER_OF_SLOTS", {mainnet = "256"; sandbox = "16"; test = "16"});
    ]

  (* not exposed via Cli *)
  let internal_variables =
    [
      ("PAGE_SIZE", {mainnet = "4096"; sandbox = "128"; test = "128"});
      ("SLOT_SIZE", {mainnet = "1048576"; sandbox = "32768"; test = "32768"});
      ("NUMBER_OF_SHARDS", {mainnet = "2048"; sandbox = "64"; test = "64"});
    ]

  let get_default_for_network network {mainnet; sandbox; test} =
    match network with
    | Options.Mainnet -> mainnet
    | Sandbox -> sandbox
    | Test -> test

  let set_non_provided_variables network map =
    if MS.cardinal map < List.length variables then
      if !Options.auto then
        Format.eprintf
          "Some parameters are not set and --auto is provided. Using default \
           values for them.\n\
           @."
      else
        Format.eprintf
          "Some parameters are not set. Reading them interactively. Use --auto \
           to use default values.\n\
           @." ;
    let map =
      List.fold_left
        (fun map (k, defaults) ->
          let default = get_default_for_network network defaults in
          if MS.mem k map then map
          else
            let v =
              match Sys.getenv_opt k with
              | Some v -> v
              | None ->
                  if !Options.auto then default
                  else (
                    Format.printf
                      "Provide a value for %S (default is %s): @?"
                      k
                      default ;
                    read_line ())
            in
            MS.add k v map)
        map
        (variables @ internal_variables)
    in
    let find key = MS.find key map |> int_of_string in
    let max_number_of_slots = find "MAX_NUMBER_OF_SLOTS" in
    let num_slots_producers = find "NUMBER_OF_SLOTS_PRODUCERS" in
    if max_number_of_slots < num_slots_producers then
      Format.eprintf
        "Warning: MAX_NUMBER_OF_SLOTS = %d < NUMBER_OF_SLOTS_PRODUCERS = %d\n@."
        max_number_of_slots
        num_slots_producers ;
    map

  let add_value_for_arg arg value =
    Options.initial_values := MS.add arg value !Options.initial_values

  let gen_args_for_vars variables =
    List.map
      (fun (_ARG, {mainnet; sandbox; test}) ->
        let arg = String.lowercase_ascii _ARG in
        ( sf "--%s" arg,
          Arg.String (add_value_for_arg _ARG),
          sf
            " Provide a value for %s. Could be set via env variable '%s'. \
             Default values are mainnet:%s, sandbox:%s, test:%s"
            arg
            _ARG
            mainnet
            sandbox
            test ))
      variables

  let set_network s =
    Options.network :=
      match String.lowercase_ascii s with
      | "mainnet" -> Mainnet
      | "sandbox" -> Sandbox
      | "test" | "testnet" -> Test
      | s -> failwith (sf "Unknown network %S" s)

  let spec =
    Arg.align
      ([
         ( "--input",
           Arg.Set_string Options.input_file,
           sf " A path to the input file. Default is %S" !Options.input_file );
         ( "--output",
           Arg.Set_string Options.output_file,
           sf " A path to the output file. Default is %S" !Options.output_file
         );
         ( "--network",
           Arg.String set_network,
           " The network from which default values are taken. One of \
            'mainnet', 'sandbox', 'test'. Default value is 'mainnet'." );
         ( "--ip-addresses",
           Arg.Set_string Options.ip_addresses,
           sf
             " The source file from which machines' IP addresses are taken for \
              agents. If not set, agents are on %s"
             !Options.ip_addresses );
         ( "--preallocated-ip-addresses",
           Arg.Set_int Options.preallocated_ip_addresses,
           sf
             " Indicate the number of IP addresses' place holders reserved for \
              agents. Default value is %d (address %%%%ADDR_1%%%%). This is \
              useful to correctly number attesters and slots producers' IP \
              place holders."
             !Options.preallocated_ip_addresses );
         ( "--auto",
           Arg.Set Options.auto,
           " Don't prompt for arguments not provided via command-line or via \
            environment variables. Use hardcoded default values instead." );
         ( "--working-dir",
           Arg.Set_string Options.working_dir,
           sf
             " Set the working dir of the script. Default is %S, but this \
              likely needs to be redefined to set the exact path of files."
             !Options.working_dir );
         ( "--octogram-path",
           Arg.Set_string Options.octogram_path,
           sf
             " Set the path and installation method for Octogram. Default \
              value is %S. Other possible values are 'installed: \
              /path/to/octogram' (fastest) or 'pull: \
              'https://storage.googleapis.com/<bucket>/octogram' (faster than \
              scp from local machine)."
             !Options.octogram_path );
         ( "--delegates_prefix",
           Arg.Set_string Options.delegates_prefix,
           sf
             " Prefix in name of delegates' aliases. Default is %S"
             !Options.delegates_prefix );
         ( "--gcloud-binaries-bucket",
           Arg.Set_string Options.gcloud_binaries_bucket,
           sf
             " A public Google Cloud storage bucket in which Octez binaries \
              can be downloaded. We use this method as it's faster than scp \
              from local machine. Default value is %S"
             !Options.gcloud_binaries_bucket );
         ( "--balance-updates",
           Arg.Set_string Options.balance_updates,
           sf
             " A JSON file containing a list of balances, in case we want to \
              read balance updates for delegates from an external file (e.g. \
              minic mainnet bakers' stake)." );
       ]
      @ gen_args_for_vars variables)

  let usage = sf "usage: %s <options>" exe

  let () =
    Arg.parse spec (fun _s -> failwith "No action supported") usage ;
    let map =
      set_non_provided_variables !Options.network !Options.initial_values
    in
    match Process.main map with
    | Ok () ->
        Format.eprintf "Success. Output generated in %s@." !Options.output_file
    | Error (cmd, code) ->
        Format.eprintf "Command %S failed with exit code %d@." cmd code
end
