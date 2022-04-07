open Tezos_crypto

let () =
  if Array.length Sys.argv <= 3 then (
    Printf.printf
      "Usage: %s <network_name> <genesis_protocol_hash> [bootstrap_peer..]\n\
       Example: %s dalphanet \
       Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P \
       'paris.bootzero.tzalpha.net:19732'\n\
       %!"
      Sys.argv.(0)
      Sys.argv.(0) ;
    exit 0 )

let network_name = Sys.argv.(1)

let genesis_protocol_hash = Sys.argv.(2)

let default_bootstrap_peers =
  let argc = Array.length Sys.argv in
  if argc >= 3 then
    String.concat
      " ; "
      (List.map
         (fun s -> Printf.sprintf "%S" s)
         (Array.to_list (Array.sub Sys.argv 3 (argc - 3))))
  else ""

(* Generate a random hash for the genesis block.
   This hash must be a valid Base58 string, so we may have to retry several times. *)
let prefix = "BLockGenesisGenesisGenesisGenesisGenesis"

let rec genesis () =
  let date =
    Lwt_main.run
      (Lwt_process.pread_line (Lwt_process.shell "date --utc +%FT%TZ"))
  in
  let suffix = String.sub Digest.(to_hex (string date)) 0 5 in
  match Base58.raw_decode (prefix ^ suffix ^ "crcCRC") with
  | None ->
      genesis ()
  | Some p ->
      let p = String.sub p 0 (String.length p - 4) in
      (Base58.safe_encode p, date)

let (genesis, date) = genesis ()

let echo x = Printf.ksprintf print_endline x

let chain_name_prefix = "TEZOS_" ^ String.uppercase_ascii network_name ^ "_"

let chain_name = chain_name_prefix ^ date

let sandboxed_chain_name = "SANDBOXED_TEZOS"

let () =
  echo
    "Here is the configuration that you can add to \
     src/bin_node/node_config_file.ml:" ;
  echo "" ;
  echo "let blockchain_network_%s =" (String.lowercase_ascii network_name) ;
  echo "  make_blockchain_network" ;
  echo "    ~alias:\"%s\"" (String.lowercase_ascii network_name) ;
  echo "    {" ;
  echo "      time = Time.Protocol.of_notation_exn %S;" date ;
  echo "      block =" ;
  echo "        Block_hash.of_b58check_exn" ;
  echo "          %S;" genesis ;
  echo "      protocol =" ;
  echo "        Protocol_hash.of_b58check_exn" ;
  echo "          %S;" genesis_protocol_hash ;
  echo "    }" ;
  echo "    ~chain_name:%S" chain_name ;
  echo "    ~sandboxed_chain_name:%S" sandboxed_chain_name ;
  echo "    ~default_bootstrap_peers:" ;
  echo "      [ %s ]" default_bootstrap_peers ;
  echo "" ;
  echo
    "Don't forget to add this network to update \
     builtin_blockchain_networks_with_tags." ;
  echo "Here is a configuration file that you can use instead:" ;
  echo "" ;
  echo "{" ;
  echo "  \"p2p\": {}," ;
  echo "  \"network\": {" ;
  echo "    \"genesis\": {" ;
  echo "      \"timestamp\": %S," date ;
  echo "      \"block\": %S," genesis ;
  echo "      \"protocol\": %S" genesis_protocol_hash ;
  echo "    }," ;
  echo "    \"chain_name\": %S," chain_name ;
  echo "    \"sandboxed_chain_name\": %S," sandboxed_chain_name ;
  echo "    \"default_bootstrap_peers\": [ %s ]" default_bootstrap_peers ;
  echo "  }" ;
  echo "}" ;
  ()
