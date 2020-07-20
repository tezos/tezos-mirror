open Tezos_crypto

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

let network_name = "dalphanet"
let chain_name_prefix = "TEZOS_" ^ String.uppercase_ascii network_name ^ "_"
let chain_name = chain_name_prefix ^ date
let sandboxed_chain_name = "SANDBOXED_TEZOS"
(* The genesis protocol of Carthagenet. *)
let genesis_protocol_hash = "PtYuensgYBb3G3x1hLLbCmcav8ue8Kyd2khADcL5LsT5R1hcXex"

let () =
  echo "Here is the configuration that you can add to src/bin_node/node_config_file.ml:";
  echo "";
  echo "let blockchain_network_%s =" (String.lowercase_ascii network_name);
  echo "  make_blockchain_network";
  echo "    State.Chain.";
  echo "      {";
  echo "        time = Time.Protocol.of_notation_exn %S;" date;
  echo "        block =";
  echo "          Block_hash.of_b58check_exn";
  echo "            %S;" genesis;
  echo "        protocol =";
  echo "          Protocol_hash.of_b58check_exn";
  echo "            %S;" genesis_protocol_hash;
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
  echo "      \"protocol\": %S" genesis_protocol_hash;
  echo "    },";
  echo "    \"chain_name\": %S," chain_name;
  echo "    \"sandboxed_chain_name\": %S," sandboxed_chain_name;
  echo "    \"default_bootstrap_peers\": []";
  echo "  }";
  echo "}";
  ()
