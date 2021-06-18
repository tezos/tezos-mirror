(rule
 (targets v3.ml)
 (deps
   v3/s.ml
   v3/blake2B.ml
   v3/ed25519.ml
   v3/secp256k1.ml
   v3/p256.ml
   v3/signature.ml
   v3/chain_id.ml
   v3/block_hash.ml
   v3/operation_hash.ml
   v3/operation_list_hash.ml
   v3/operation_list_list_hash.ml
   v3/protocol_hash.ml
   v3/context_hash.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment-packer:s_packer} "structs" %{deps})))))
