(rule
 (targets v2.ml)
 (deps

   ;; Part of OCaml stdlib 4.09.1
   v2/pervasives.mli
   v2/list.mli
   v2/string.mli
   v2/char.mli
   v2/bytes.mli
   v2/int32.mli
   v2/int64.mli
   v2/format.mli

   ;; Part of external libraries
   v2/hex.mli           ; 1.4.0
   v2/z.mli             ; 1.10
   v2/lwt.mli           ; 5.3.0
   v2/lwt_list.mli
   v2/data_encoding.mli ; 0.2

   ;; Tezos extended stdlib revision 32f04ec483b8d9c62dd0957389eb5268d8b3a38d
   v2/raw_hashes.mli
   v2/compare.mli
   v2/error_monad.mli
   v2/logging.mli
   v2/time.mli
   v2/option.mli
   v2/tzEndian.mli
   v2/bits.mli

   v2/RPC_arg.mli
   v2/RPC_path.mli
   v2/RPC_query.mli
   v2/RPC_service.mli
   v2/RPC_answer.mli
   v2/RPC_directory.mli

   v2/base58.mli
   v2/s.mli
   v2/set.mli
   v2/map.mli
   v2/blake2B.mli
   v2/bls12_381.mli
   v2/ed25519.mli
   v2/secp256k1.mli
   v2/p256.mli
   v2/chain_id.mli
   v2/signature.mli
   v2/block_hash.mli
   v2/operation_hash.mli
   v2/operation_list_hash.mli
   v2/operation_list_list_hash.mli
   v2/protocol_hash.mli
   v2/context_hash.mli
   v2/pvss_secp256k1.mli
   v2/sapling.mli

   ;; Tezos specifics
   v2/micheline.mli
   v2/block_header.mli
   v2/fitness.mli
   v2/operation.mli
   v2/context.mli
   v2/updater.mli
   v2/RPC_context.mli
   v2/equality_witness.mli
 )
 (action (with-stdout-to %{targets} (chdir %{workspace_root}}
          (run %{libexec:tezos-protocol-environment:s_packer} "sigs" %{deps})))))
