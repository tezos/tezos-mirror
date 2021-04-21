(rule
 (targets v2.ml)
 (deps
   v2/s.ml
   v2/blake2B.ml
   v2/ed25519.ml
   v2/secp256k1.ml
   v2/p256.ml
   v2/signature.ml
   v2/chain_id.ml
   v2/block_hash.ml
   v2/operation_hash.ml
   v2/operation_list_hash.ml
   v2/operation_list_list_hash.ml
   v2/protocol_hash.ml
   v2/context_hash.ml
   v2/error_monad_traversors.ml
   v2/data_encoding.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment-packer:s_packer} "structs" %{deps})))))
