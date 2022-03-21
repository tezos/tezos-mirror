(rule
 (targets v0.ml)
 (deps

   ;; Part of OCaml stdlib
   v0/pervasives.mli
   v0/list.mli
   v0/string.mli
   v0/int32.mli
   v0/int64.mli
   v0/format.mli

   ;; Part of external libraries
   v0/mBytes.mli
   v0/z.mli
   v0/lwt.mli
   v0/lwt_list.mli
   v0/raw_hashes.mli

   ;; Tezos extended stdlib
   v0/compare.mli
   v0/data_encoding.mli
   v0/error_monad.mli
   v0/logging.mli
   v0/time.mli
   v0/option.mli

   v0/RPC_arg.mli
   v0/RPC_path.mli
   v0/RPC_query.mli
   v0/RPC_service.mli
   v0/RPC_answer.mli
   v0/RPC_directory.mli

   v0/base58.mli
   v0/s.mli
   v0/set.mli
   v0/map.mli
   v0/blake2B.mli
   v0/ed25519.mli
   v0/secp256k1.mli
   v0/p256.mli
   v0/chain_id.mli
   v0/signature.mli
   v0/block_hash.mli
   v0/operation_hash.mli
   v0/operation_list_hash.mli
   v0/operation_list_list_hash.mli
   v0/protocol_hash.mli
   v0/context_hash.mli

   ;; Tezos specifics
   v0/micheline.mli
   v0/block_header.mli
   v0/fitness.mli
   v0/operation.mli
   v0/protocol.mli
   v0/context.mli
   v0/updater.mli
   v0/RPC_context.mli

 )
 (action (with-stdout-to %{targets} (chdir %{workspace_root}}
          (run %{libexec:tezos-protocol-environment:s_packer} "sigs" %{deps})))))
