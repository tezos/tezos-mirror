(rule
 (targets v1.ml)
 (deps
   v1/s.ml
   v1/blake2B.ml
   v1/ed25519.ml
   v1/secp256k1.ml
   v1/p256.ml
   v1/signature.ml
   v1/chain_id.ml
   v1/block_hash.ml
   v1/operation_hash.ml
   v1/operation_list_hash.ml
   v1/operation_list_list_hash.ml
   v1/protocol_hash.ml
   v1/context_hash.ml
   v1/error_monad_traversors.ml
   v1/option.ml
   v1/data_encoding.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment-packer:s_packer} "structs" %{deps})))))
