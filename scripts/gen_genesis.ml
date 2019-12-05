(* run this as: cd scripts; ocaml gen_genesis.ml *)

#use "topfind";;
#thread;;
#require "threads";;
#require "stringext";;
#require "lwt";;
#require "lwt.unix";;
#require "zarith";;
#require "re";;
#require "hacl";;
#mod_use "../src/lib_stdlib/tzString.ml";;
#mod_use "../src/lib_stdlib/option.ml";;
#mod_use "../src/lib_stdlib/tzList.ml";;
#mod_use "../src/lib_stdlib/utils.ml";;
#mod_use "../src/lib_crypto/base58.ml";;

open Lwt.Infix;;

(* Generate a random hash for the genesis block.
   This hash must be a valid Base58 string, so we may have to retry several times. *)
let prefix = "BLockGenesisGenesisGenesisGenesisGenesis"
let rec genesis () =
  let date =
    Lwt_main.run
      (Lwt_process.pread_line (Lwt_process.shell "TZ='AAA+1' date +%FT%TZ")) in
  let suffix = String.sub Digest.(to_hex (string date)) 0 5 in
  match Base58.raw_decode (prefix ^ suffix ^ "crcCRC") with
  | None -> genesis ()
  | Some p ->
      let p = String.sub p 0 (String.length p - 4) in
      Base58.safe_encode p, date

let genesis, date = genesis ()

let echo x = Printf.ksprintf print_endline x

let () =
  echo "Updating alphanet_version (used by Docker images)...";
  Lwt_main.run @@
  let stream = Lwt_io.lines_of_file "alphanet_version" in
  Lwt_stream.to_list stream >>= function
  | [] | _ :: _ :: _ -> failwith "bad alphanet_version file"
  | [ line ] -> match String.split_on_char 'Z' line with
    | [ _ ; branch ] ->
        let contents = if String.trim branch = "" then date else date ^ branch in
        Lwt_io.lines_to_file "alphanet_version" (Lwt_stream.of_list [ contents ])
    | _ -> failwith "bad alphanet_version file"

let chain_name_prefix = "TEZOS_ALPHANET_"
let chain_name = chain_name_prefix ^ date
let sandboxed_chain_name = "SANDBOXED_TEZOS"
let genesis_protocol_hash_placeholder = "HASH OF GENESIS PROTOCOL TO USE"

let () =
  echo "Here is the configuration that you can add to src/bin_node/node_config_file.ml:";
  echo "";
  echo "let blockchain_network_XXXXXXXXnet =";
  echo "  make_blockchain_network";
  echo "    State.Chain.";
  echo "      {";
  echo "        time = Time.Protocol.of_notation_exn %S;" date;
  echo "        block =";
  echo "          Block_hash.of_b58check_exn";
  echo "            %S;" genesis;
  echo "        protocol =";
  echo "          Protocol_hash.of_b58check_exn";
  echo "            %S;" genesis_protocol_hash_placeholder;
  echo "      }";
  echo "    ~chain_name:%S" chain_name;
  echo "    ~sandboxed_chain_name:%S" sandboxed_chain_name;
  echo "    ~default_bootstrap_peers:[ (* FILL THIS LIST *) ]";
  echo "";
  echo "Don't forget to add this network to update builtin_blockchain_networks_with_tags.";
  echo "Here is a configuration file that you can use instead:";
  echo "";
  echo "{";
  echo "  \"p2p\": {},";
  echo "  \"network\": {";
  echo "    \"genesis\": {";
  echo "      \"timestamp\": %S," date;
  echo "      \"block\": %S," genesis;
  echo "      \"protocol\": %S" genesis_protocol_hash_placeholder;
  echo "    },";
  echo "    \"chain_name\": %S," chain_name;
  echo "    \"sandboxed_chain_name\": %S," sandboxed_chain_name;
  echo "    \"default_bootstrap_peers\": []";
  echo "  }";
  echo "}";
  ()
