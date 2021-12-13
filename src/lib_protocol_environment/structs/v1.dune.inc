(rule
 (targets v1.ml)
 (deps
   v0/s.ml
   v0/blake2B.ml
   v0/ed25519.ml
   v0/secp256k1.ml
   v0/p256.ml
   v0/signature.ml
   v0/chain_id.ml
   v0/block_hash.ml
   v0/operation_hash.ml
   v0/operation_list_hash.ml
   v0/operation_list_list_hash.ml
   v0/protocol_hash.ml
   v0/context_hash.ml
   v0/error_monad_traversors.ml
   v0/data_encoding.ml
   v1/option.ml
   v1/bls12_381.ml
   v1/error_monad_preallocated_values.ml
   v1/hex.ml
   v0/error_monad_trace_eval.ml
   v0/error_monad_classification.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment-packer:s_packer} "structs" %{deps})))))
