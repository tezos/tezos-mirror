.. raw:: html
  
  
  <style>
     .wy-nav-content {
        max-width: 100%;
     }
    .tab {
      overflow: hidden;
      border: 1px solid #ccc;
      background-color: #f1f1f1;
    }
    .tab button {
      background-color: inherit;
      float: left;
      border: none;
      outline: none;
      cursor: pointer;
      padding: 5px 10px;
    }
    .tab button:hover {
      background-color: #ddd;
    }
    .tab button.active {
      background-color: #ccc;
    }
    .tabcontent {
      display: none;
      padding: 6px 12px;
      border: 1px solid #ccc;
      border-top: none;
      max-height: 40ex;
      margin-bottom: 7ex;
      overflow: auto;
    }
    .tabcontent p {
      margin-bottom: 12px;
    }
    pre {
      font-size: 12px
    }
    .rst-content .section ul p {
      margin-bottom: 0;
    }
    span.query {
      font-family: monospace;
      white-space: pre;
    }
  </style>
  


.. raw:: html
  
  
  <script>
    function showTab(elt, tab, ref) {
      var i, tabcontent, tablinks;
      tabcontent = document.getElementsByClassName(ref);
      for (i = 0; i < tabcontent.length; i++) {
        tabcontent[i].style.display = 'none';
      }
  
      tablinks = elt.parentNode.children;
      for (i = 0; i < tablinks.length; i++) {
        tablinks[i].className = tablinks[i].className.replace(' active', '');
      }
  
      document.getElementById(tab).style.display = 'block';
      elt.className += ' active';
    }
  
    document.addEventListener('DOMContentLoaded', function() {
      var a = document.getElementsByClassName('defaultOpen');
      for (i = 0; i < a.length; i++) { a[i].click() }
    })
  </script>
  


.. _rpc_index_beta :

Beta RPCs - Reference
#####################

.. include:: /include/rpc_introduction.rst.inc

Index
*****

* ../<block_id>
  
  * ../<block_id>/helpers
    
    * ../<block_id>/helpers/preapply
      
      * ../<block_id>/helpers/preapply/block (`POST <POST_..--block_id--helpers--preapply--block_>`_)
  
  * ../<block_id>/live_blocks (`GET <GET_..--block_id--live_blocks_>`_)

Full description
****************

.. _POST_..--block_id--helpers--preapply--block :

**POST ../<block_id>/helpers/preapply/block?[sort]&[timestamp=<date>]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--blockdescr', 'POST_..--block_id--helpers--preapply--block')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--blockinput.json', 'POST_..--block_id--helpers--preapply--block')">Json input</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--blockinput.bin', 'POST_..--block_id--helpers--preapply--block')">Binary input</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--blockoutput.json', 'POST_..--block_id--helpers--preapply--block')">Json output</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--blockoutput.bin', 'POST_..--block_id--helpers--preapply--block')">Binary output</button>
    </div><div id="POST_..--block_id--helpers--preapply--blockdescr" class="POST_..--block_id--helpers--preapply--block tabcontent">
            <p>
            Simulate the validation of a block that would contain the given operations and return the resulting fitness and context hash.</p> <p>Optional query arguments :<ul><li><span class="query">sort</span></li><li><span class="query">timestamp = &lt;date&gt;</span></li></ul></p>
            </div>
  <div id="POST_..--block_id--helpers--preapply--blockinput.json" class="POST_..--block_id--helpers--preapply--block tabcontent">
    <pre>
    { "protocol_data":
        { "protocol": "PtRU6wuXeNfhzbbbNbZNbaahu2eKBDztN5cqG8LbvLspdUnPGVX",
          "payload_hash": $value_hash,
          "payload_round": integer ∈ [-2^31-1, 2^31],
          "proof_of_work_nonce": /^([a-zA-Z0-9][a-zA-Z0-9])*$/,
          "seed_nonce_hash"?: $cycle_nonce,
          "liquidity_baking_toggle_vote": $beta.liquidity_baking_vote,
          "adaptive_issuance_vote": $beta.adaptive_issuance_vote,
          "signature": $Signature.V1 },
      "operations": [ [ $next_operation ... ] ... ] }
    $Context_hash:
      /* A hash of context (Base58Check-encoded) */
      $unistring
    $DAL_commitment:
      /* Commitment representation for the DAL (Base58Check-encoded) */
      $unistring
    $Ed25519.Public_key_hash:
      /* An Ed25519 public key hash (Base58Check-encoded) */
      $unistring
    $Operation_list_list_hash:
      /* A list of list of operations (Base58Check-encoded) */
      $unistring
    $Protocol_hash:
      /* A Tezos protocol ID (Base58Check-encoded) */
      $unistring
    $Signature.Public_key:
      /* A Ed25519, Secp256k1, or P256 public key (Base58Check-encoded) */
      $unistring
    $Signature.Public_key_hash:
      /* A Ed25519, Secp256k1, P256, or BLS public key hash
         (Base58Check-encoded) */
      $unistring
    $Signature.V1:
      /* A Ed25519, Secp256k1, P256 or BLS signature (Base58Check-encoded) */
      $unistring
    $Zk_rollup_hash:
      /* A zk rollup address (Base58Check-encoded) */
      $unistring
    $beta.adaptive_issuance_vote: "on" || "off" || "pass"
    $beta.block_header.alpha.full_header:
      /* Shell header
         Block header's shell-related content. It contains information such as
         the block level, its predecessor and timestamp. */
      { "level": integer ∈ [-2^31-1, 2^31],
        "proto": integer ∈ [0, 255],
        "predecessor": $block_hash,
        "timestamp": $timestamp.protocol,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash": $Operation_list_list_hash,
        "fitness": $fitness,
        "context": $Context_hash,
        "payload_hash": $value_hash,
        "payload_round": integer ∈ [-2^31-1, 2^31],
        "proof_of_work_nonce": /^([a-zA-Z0-9][a-zA-Z0-9])*$/,
        "seed_nonce_hash"?: $cycle_nonce,
        "liquidity_baking_toggle_vote": $beta.liquidity_baking_vote,
        "adaptive_issuance_vote": $beta.adaptive_issuance_vote,
        "signature": $Signature.V1 }
    $beta.contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      $unistring
    $beta.contract_id.originated:
      /* A contract handle -- originated account
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 originated contract hash. */
      $unistring
    $beta.entrypoint:
      /* entrypoint
         Named entrypoint to a Michelson smart contract */
      "default"
      || "root"
      || "do"
      || "set_delegate"
      || "remove_delegate"
      || "deposit"
      || "stake"
      || "unstake"
      || "finalize_unstake"
      || "set_delegate_parameters"
      || $unistring
      /* named */
    $beta.inlined.attestation:
      /* An operation's shell header. */
      { "branch": $block_hash,
        "operations": $beta.inlined.attestation_mempool.contents,
        "signature"?: $Signature.V1 }
    $beta.inlined.attestation_mempool.contents:
      { /* Attestation */
        "kind": "attestation",
        "slot": integer ∈ [0, 2^16-1],
        "level": integer ∈ [0, 2^31],
        "round": integer ∈ [-2^31-1, 2^31],
        "block_payload_hash": $value_hash }
      || { /* Attestation_with_dal */
           "kind": "attestation_with_dal",
           "slot": integer ∈ [0, 2^16-1],
           "level": integer ∈ [0, 2^31],
           "round": integer ∈ [-2^31-1, 2^31],
           "block_payload_hash": $value_hash,
           "dal_attestation": $bignum }
    $beta.inlined.preattestation:
      /* An operation's shell header. */
      { "branch": $block_hash,
        "operations": $beta.inlined.preattestation.contents,
        "signature"?: $Signature.V1 }
    $beta.inlined.preattestation.contents:
      { /* Preattestation */
        "kind": "preattestation",
        "slot": integer ∈ [0, 2^16-1],
        "level": integer ∈ [0, 2^31],
        "round": integer ∈ [-2^31-1, 2^31],
        "block_payload_hash": $value_hash }
    $beta.liquidity_baking_vote: "on" || "off" || "pass"
    $beta.michelson.v1.primitives:
      "SHA256"
      | "GT"
      | "RIGHT"
      | "SAPLING_EMPTY_STATE"
      | "False"
      | "RENAME"
      | "CAST"
      | "tx_rollup_l2_address"
      | "PACK"
      | "BYTES"
      | "timestamp"
      | "bls12_381_g2"
      | "Pair"
      | "IF_LEFT"
      | "contract"
      | "mutez"
      | "storage"
      | "PAIR"
      | "view"
      | "UNPACK"
      | "ADD"
      | "DROP"
      | "big_map"
      | "MUL"
      | "NAT"
      | "SELF"
      | "CONTRACT"
      | "CDR"
      | "SAPLING_VERIFY_UPDATE"
      | "pair"
      | "LSL"
      | "int"
      | "operation"
      | "SHA512"
      | "CREATE_ACCOUNT"
      | "BLAKE2B"
      | "SPLIT_TICKET"
      | "LEFT"
      | "never"
      | "unit"
      | "address"
      | "signature"
      | "CHAIN_ID"
      | "constant"
      | "SLICE"
      | "SENDER"
      | "IMPLICIT_ACCOUNT"
      | "key_hash"
      | "AMOUNT"
      | "CHECK_SIGNATURE"
      | "sapling_state"
      | "LT"
      | "EXEC"
      | "Elt"
      | "EMIT"
      | "NONE"
      | "CREATE_CONTRACT"
      | "LSR"
      | "SET_DELEGATE"
      | "OPEN_CHEST"
      | "TRANSFER_TOKENS"
      | "Some"
      | "parameter"
      | "set"
      | "bls12_381_fr"
      | "EDIV"
      | "None"
      | "STEPS_TO_QUOTA"
      | "key"
      | "ABS"
      | "list"
      | "NEVER"
      | "map"
      | "CAR"
      | "IF"
      | "GET_AND_UPDATE"
      | "CONCAT"
      | "LOOP"
      | "DIG"
      | "KECCAK"
      | "Lambda_rec"
      | "SOME"
      | "option"
      | "SUB"
      | "INT"
      | "PUSH"
      | "CONS"
      | "Unit"
      | "ISNAT"
      | "NEG"
      | "XOR"
      | "APPLY"
      | "UNPAIR"
      | "JOIN_TICKETS"
      | "SIZE"
      | "lambda"
      | "AND"
      | "NEQ"
      | "or"
      | "BALANCE"
      | "UNIT"
      | "VOTING_POWER"
      | "OR"
      | "LAMBDA"
      | "chest"
      | "LOOP_LEFT"
      | "True"
      | "Right"
      | "Ticket"
      | "HASH_KEY"
      | "DUG"
      | "sapling_transaction"
      | "SUB_MUTEZ"
      | "EMPTY_BIG_MAP"
      | "MEM"
      | "IF_NONE"
      | "nat"
      | "TOTAL_VOTING_POWER"
      | "LE"
      | "Left"
      | "chest_key"
      | "READ_TICKET"
      | "ticket"
      | "bls12_381_g1"
      | "LEVEL"
      | "VIEW"
      | "string"
      | "PAIRING_CHECK"
      | "LAMBDA_REC"
      | "NOW"
      | "SHA3"
      | "bool"
      | "MIN_BLOCK_TIME"
      | "GET"
      | "bytes"
      | "sapling_transaction_deprecated"
      | "NIL"
      | "IF_CONS"
      | "GE"
      | "NOT"
      | "SWAP"
      | "ITER"
      | "ADDRESS"
      | "TICKET"
      | "DUP"
      | "EMPTY_MAP"
      | "UPDATE"
      | "chain_id"
      | "TICKET_DEPRECATED"
      | "EMPTY_SET"
      | "FAILWITH"
      | "MAP"
      | "SOURCE"
      | "DIP"
      | "COMPARE"
      | "EQ"
      | "SELF_ADDRESS"
      | "code"
    $beta.mutez: $positive_bignum
    $beta.operation.alpha.contents:
      { /* Preattestation */
        "kind": "preattestation",
        "slot": integer ∈ [0, 2^16-1],
        "level": integer ∈ [0, 2^31],
        "round": integer ∈ [-2^31-1, 2^31],
        "block_payload_hash": $value_hash }
      || { /* Attestation */
           "kind": "attestation",
           "slot": integer ∈ [0, 2^16-1],
           "level": integer ∈ [0, 2^31],
           "round": integer ∈ [-2^31-1, 2^31],
           "block_payload_hash": $value_hash }
      || { /* Attestation_with_dal */
           "kind": "attestation_with_dal",
           "slot": integer ∈ [0, 2^16-1],
           "level": integer ∈ [0, 2^31],
           "round": integer ∈ [-2^31-1, 2^31],
           "block_payload_hash": $value_hash,
           "dal_attestation": $bignum }
      || { /* Double_preattestation_evidence */
           "kind": "double_preattestation_evidence",
           "op1": $beta.inlined.preattestation,
           "op2": $beta.inlined.preattestation }
      || { /* Double_attestation_evidence */
           "kind": "double_attestation_evidence",
           "op1": $beta.inlined.attestation,
           "op2": $beta.inlined.attestation }
      || { /* Seed_nonce_revelation */
           "kind": "seed_nonce_revelation",
           "level": integer ∈ [0, 2^31],
           "nonce": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ }
      || { /* Vdf_revelation */
           "kind": "vdf_revelation",
           "solution":
             [ /^([a-zA-Z0-9][a-zA-Z0-9])*$/, /^([a-zA-Z0-9][a-zA-Z0-9])*$/ ] }
      || { /* Double_baking_evidence */
           "kind": "double_baking_evidence",
           "bh1": $beta.block_header.alpha.full_header,
           "bh2": $beta.block_header.alpha.full_header }
      || { /* Activate_account */
           "kind": "activate_account",
           "pkh": $Ed25519.Public_key_hash,
           "secret": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ }
      || { /* Proposals */
           "kind": "proposals",
           "source": $Signature.Public_key_hash,
           "period": integer ∈ [-2^31-1, 2^31],
           "proposals": [ $Protocol_hash ... ] }
      || { /* Ballot */
           "kind": "ballot",
           "source": $Signature.Public_key_hash,
           "period": integer ∈ [-2^31-1, 2^31],
           "proposal": $Protocol_hash,
           "ballot": "nay" | "yay" | "pass" }
      || { /* Reveal */
           "kind": "reveal",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "public_key": $Signature.Public_key }
      || { /* Transaction */
           "kind": "transaction",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "amount": $beta.mutez,
           "destination": $beta.contract_id,
           "parameters"?: { "entrypoint": $beta.entrypoint,
                            "value": any } }
      || { /* Origination */
           "kind": "origination",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "balance": $beta.mutez,
           "delegate"?: $Signature.Public_key_hash,
           "script": $beta.scripted.contracts }
      || { /* Delegation */
           "kind": "delegation",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "delegate"?: $Signature.Public_key_hash }
      || { /* Set_deposits_limit */
           "kind": "set_deposits_limit",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "limit"?: $beta.mutez }
      || { /* Increase_paid_storage */
           "kind": "increase_paid_storage",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "amount": $bignum,
           "destination": $beta.contract_id.originated }
      || { /* Update_consensus_key */
           "kind": "update_consensus_key",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "pk": $Signature.Public_key }
      || { /* Drain_delegate */
           "kind": "drain_delegate",
           "consensus_key": $Signature.Public_key_hash,
           "delegate": $Signature.Public_key_hash,
           "destination": $Signature.Public_key_hash }
      || { /* Failing_noop */
           "kind": "failing_noop",
           "arbitrary": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ }
      || { /* Register_global_constant */
           "kind": "register_global_constant",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "value": any }
      || { /* Transfer_ticket */
           "kind": "transfer_ticket",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "ticket_contents": any,
           "ticket_ty": any,
           "ticket_ticketer": $beta.contract_id,
           "ticket_amount": $positive_bignum,
           "destination": $beta.contract_id,
           "entrypoint": $unistring }
      || { /* Dal_publish_commitment */
           "kind": "dal_publish_commitment",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "slot_header":
             { "slot_index": integer ∈ [0, 255],
               "commitment": $DAL_commitment,
               "commitment_proof": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ } }
      || { /* Smart_rollup_originate */
           "kind": "smart_rollup_originate",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "pvm_kind": "wasm_2_0_0" | "arith" | "riscv",
           "kernel": /^([a-zA-Z0-9][a-zA-Z0-9])*$/,
           "parameters_ty": any,
           "whitelist"?: [ $Signature.Public_key_hash ... ] }
      || { /* Smart_rollup_add_messages */
           "kind": "smart_rollup_add_messages",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "message": [ /^([a-zA-Z0-9][a-zA-Z0-9])*$/ ... ] }
      || { /* Smart_rollup_cement */
           "kind": "smart_rollup_cement",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "rollup": $smart_rollup_address }
      || { /* Smart_rollup_publish */
           "kind": "smart_rollup_publish",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "rollup": $smart_rollup_address,
           "commitment":
             { "compressed_state": $smart_rollup_state_hash,
               "inbox_level": integer ∈ [0, 2^31],
               "predecessor": $smart_rollup_commitment_hash,
               "number_of_ticks": $int64 } }
      || { /* Smart_rollup_refute */
           "kind": "smart_rollup_refute",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "rollup": $smart_rollup_address,
           "opponent": $Signature.Public_key_hash,
           "refutation":
             { /* Start */
               "refutation_kind": "start",
               "player_commitment_hash": $smart_rollup_commitment_hash,
               "opponent_commitment_hash": $smart_rollup_commitment_hash }
             || { /* Move */
                  "refutation_kind": "move",
                  "choice": $positive_bignum,
                  "step":
                    [ { "state"?: $smart_rollup_state_hash,
                        "tick": $positive_bignum } ... ]
                    /* Dissection */
                    || { /* Proof */
                         "pvm_step": /^([a-zA-Z0-9][a-zA-Z0-9])*$/,
                         "input_proof"?:
                           { /* inbox proof */
                             "input_proof_kind": "inbox_proof",
                             "level": integer ∈ [0, 2^31],
                             "message_counter": $positive_bignum,
                             "serialized_proof": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ }
                           || { /* reveal proof */
                                "input_proof_kind": "reveal_proof",
                                "reveal_proof":
                                  { /* raw data proof */
                                    "reveal_proof_kind": "raw_data_proof",
                                    "raw_data": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ }
                                  || { /* metadata proof */
                                       "reveal_proof_kind": "metadata_proof" }
                                  || { /* dal page proof */
                                       "reveal_proof_kind": "dal_page_proof",
                                       "dal_page_id":
                                         { "published_level":
                                             integer ∈ [0, 2^31],
                                           "slot_index": integer ∈ [0, 255],
                                           "page_index":
                                             integer ∈ [-2^15, 2^15-1] },
                                       "dal_proof":
                                         /^([a-zA-Z0-9][a-zA-Z0-9])*$/ }
                                  || { /* dal parameters proof */
                                       "reveal_proof_kind":
                                         "dal_parameters_proof" } }
                           || { /* first input */
                                "input_proof_kind": "first_input" } } } }
      || { /* Smart_rollup_timeout */
           "kind": "smart_rollup_timeout",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "rollup": $smart_rollup_address,
           "stakers":
             { "alice": $Signature.Public_key_hash,
               "bob": $Signature.Public_key_hash } }
      || { /* Smart_rollup_execute_outbox_message */
           "kind": "smart_rollup_execute_outbox_message",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "rollup": $smart_rollup_address,
           "cemented_commitment": $smart_rollup_commitment_hash,
           "output_proof": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ }
      || { /* Smart_rollup_recover_bond */
           "kind": "smart_rollup_recover_bond",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "rollup": $smart_rollup_address,
           "staker": $Signature.Public_key_hash }
      || { /* Zk_rollup_origination */
           "kind": "zk_rollup_origination",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "public_parameters": /^([a-zA-Z0-9][a-zA-Z0-9])*$/,
           "circuits_info":
             [ [ $unistring,
                 { /* Public */
                   "public": any }
                 || { /* Private */
                      "private": any }
                 || { /* Fee */
                      "fee": any } ] ... ],
           "init_state": [ /^([a-zA-Z0-9][a-zA-Z0-9])*$/ ... ],
           "nb_ops": integer ∈ [-2^30, 2^30] }
      || { /* Zk_rollup_publish */
           "kind": "zk_rollup_publish",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "zk_rollup": $Zk_rollup_hash,
           "op":
             [ [ { "op_code": integer ∈ [-2^30, 2^30],
                   "price": { "id": $script_expr,
                              "amount": $bignum },
                   "l1_dst": $Signature.Public_key_hash,
                   "rollup_id": $Zk_rollup_hash,
                   "payload": [ /^([a-zA-Z0-9][a-zA-Z0-9])*$/ ... ] },
                 { /* Some */
                   "contents": $micheline.beta.michelson_v1.expression,
                   "ty": $micheline.beta.michelson_v1.expression,
                   "ticketer": $beta.contract_id }
                 || null
                 /* None */ ] ... ] }
      || { /* Zk_rollup_update */
           "kind": "zk_rollup_update",
           "source": $Signature.Public_key_hash,
           "fee": $beta.mutez,
           "counter": $positive_bignum,
           "gas_limit": $positive_bignum,
           "storage_limit": $positive_bignum,
           "zk_rollup": $Zk_rollup_hash,
           "update":
             { "pending_pis":
                 [ [ $unistring,
                     { "new_state": [ /^([a-zA-Z0-9][a-zA-Z0-9])*$/ ... ],
                       "fee": /^([a-zA-Z0-9][a-zA-Z0-9])*$/,
                       "exit_validity": boolean } ] ... ],
               "private_pis":
                 [ [ $unistring,
                     { "new_state": [ /^([a-zA-Z0-9][a-zA-Z0-9])*$/ ... ],
                       "fee": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ } ] ... ],
               "fee_pi": { "new_state": [ /^([a-zA-Z0-9][a-zA-Z0-9])*$/ ... ] },
               "proof": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ } }
    $beta.scripted.contracts: { "code": any,
                                "storage": any }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $block_hash:
      /* A block identifier (Base58Check-encoded) */
      $unistring
    $cycle_nonce:
      /* A nonce hash (Base58Check-encoded) */
      $unistring
    $fitness:
      /* Block fitness
         The fitness, or score, of a block, that allow the Tezos to decide
         which chain is the best. A fitness value is a list of byte sequences.
         They are compared as follows: shortest lists are smaller; lists of the
         same length are compared according to the lexicographical order. */
      [ /^([a-zA-Z0-9][a-zA-Z0-9])*$/ ... ]
    $int64:
      /* 64 bit integers
         Decimal representation of 64 bit integers */
      string
    $micheline.beta.michelson_v1.expression:
      { /* Int */
        "int": $bignum }
      || { /* String */
           "string": $unistring }
      || { /* Bytes */
           "bytes": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ }
      || [ $micheline.beta.michelson_v1.expression ... ]
      /* Sequence */
      || { /* Prim__generic
              Generic primitive (any number of args with or without
              annotations) */
           "prim": $beta.michelson.v1.primitives,
           "args"?: [ $micheline.beta.michelson_v1.expression ... ],
           "annots"?: [ $unistring ... ] }
    $next_operation:
      /* An operation's shell header. */
      { "protocol": "PtRU6wuXeNfhzbbbNbZNbaahu2eKBDztN5cqG8LbvLspdUnPGVX",
        "branch": $block_hash,
        "contents": [ $beta.operation.alpha.contents ... ],
        "signature"?: $Signature.V1 }
    $positive_bignum:
      /* Positive big number
         Decimal representation of a positive big number */
      string
    $script_expr:
      /* A script expression ID (Base58Check-encoded) */
      $unistring
    $smart_rollup_address:
      /* A smart rollup address (Base58Check-encoded) */
      $unistring
    $smart_rollup_commitment_hash:
      /* The hash of a commitment of a smart rollup (Base58Check-encoded) */
      $unistring
    $smart_rollup_state_hash:
      /* The hash of the VM state of a smart rollup (Base58Check-encoded) */
      $unistring
    $timestamp.protocol:
      /* A timestamp as seen by the protocol: second-level precision, epoch
         based. */
      $unistring
    $unistring:
      /* Universal string representation
         Either a plain UTF8 string, or a sequence of bytes for strings that
         contain invalid byte sequences. */
      string || { "invalid_utf8_string": [ integer ∈ [0, 255] ... ] }
    $value_hash:
      /* Hash of a consensus value (Base58Check-encoded) */
      $unistring</pre>
    </div>
  <div id="POST_..--block_id--helpers--preapply--blockinput.bin" class="POST_..--block_id--helpers--preapply--block tabcontent">
    <pre>
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | protocol_data         | Determined from data | $X_0                               |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | operations            | Variable             | sequence of $X_1                   |
    +-----------------------+----------------------+------------------------------------+
    
    
    beta.per_block_votes (1 byte, 8-bit tag)
    ****************************************
    
    case_0 (tag 0)
    ==============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    case_1 (tag 1)
    ==============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    case_2 (tag 2)
    ==============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    case_4 (tag 4)
    ==============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    case_5 (tag 5)
    ==============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    case_6 (tag 6)
    ==============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    case_8 (tag 8)
    ==============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    case_9 (tag 9)
    ==============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    case_10 (tag 10)
    ================
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    X_0
    ***
    
    +---------------------------------------+----------+-------------------------------------+
    | Name                                  | Size     | Contents                            |
    +=======================================+==========+=====================================+
    | # bytes in next 7 fields              | 4 bytes  | unsigned 30-bit big-endian integer  |
    +---------------------------------------+----------+-------------------------------------+
    | payload_hash                          | 32 bytes | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    | payload_round                         | 4 bytes  | signed 32-bit big-endian integer    |
    +---------------------------------------+----------+-------------------------------------+
    | proof_of_work_nonce                   | 8 bytes  | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    | ? presence of field "seed_nonce_hash" | 1 byte   | boolean (0 for false, 255 for true) |
    +---------------------------------------+----------+-------------------------------------+
    | seed_nonce_hash                       | 32 bytes | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    | per_block_votes                       | 1 byte   | $beta.per_block_votes               |
    +---------------------------------------+----------+-------------------------------------+
    | signature                             | Variable | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    
    
    bls_signature_prefix (33 bytes, 8-bit tag)
    ******************************************
    
    Bls_prefix (tag 3)
    ==================
    
    +-----------------+----------+------------------------+
    | Name            | Size     | Contents               |
    +=================+==========+========================+
    | Tag             | 1 byte   | unsigned 8-bit integer |
    +-----------------+----------+------------------------+
    | Unnamed field 0 | 32 bytes | bytes                  |
    +-----------------+----------+------------------------+
    
    
    public_key_hash (21 bytes, 8-bit tag)
    *************************************
    
    Ed25519 (tag 0)
    ===============
    
    +-------------------------+----------+------------------------+
    | Name                    | Size     | Contents               |
    +=========================+==========+========================+
    | Tag                     | 1 byte   | unsigned 8-bit integer |
    +-------------------------+----------+------------------------+
    | Ed25519.Public_key_hash | 20 bytes | bytes                  |
    +-------------------------+----------+------------------------+
    
    
    Secp256k1 (tag 1)
    =================
    
    +---------------------------+----------+------------------------+
    | Name                      | Size     | Contents               |
    +===========================+==========+========================+
    | Tag                       | 1 byte   | unsigned 8-bit integer |
    +---------------------------+----------+------------------------+
    | Secp256k1.Public_key_hash | 20 bytes | bytes                  |
    +---------------------------+----------+------------------------+
    
    
    P256 (tag 2)
    ============
    
    +----------------------+----------+------------------------+
    | Name                 | Size     | Contents               |
    +======================+==========+========================+
    | Tag                  | 1 byte   | unsigned 8-bit integer |
    +----------------------+----------+------------------------+
    | P256.Public_key_hash | 20 bytes | bytes                  |
    +----------------------+----------+------------------------+
    
    
    Bls (tag 3)
    ===========
    
    +---------------------------+----------+------------------------+
    | Name                      | Size     | Contents               |
    +===========================+==========+========================+
    | Tag                       | 1 byte   | unsigned 8-bit integer |
    +---------------------------+----------+------------------------+
    | Bls12_381.Public_key_hash | 20 bytes | bytes                  |
    +---------------------------+----------+------------------------+
    
    
    N.t
    ***
    
    A variable-length sequence of bytes encoding a Zarith natural number. Each byte has a running unary size bit: the most significant bit of each byte indicates whether this is the last byte in the sequence (0) or whether the sequence continues (1). Size bits ignored, the data is the binary representation of the number in little-endian order.
    
    +------+----------------------+----------+
    | Name | Size                 | Contents |
    +======+======================+==========+
    | N.t  | Determined from data | bytes    |
    +------+----------------------+----------+
    
    
    X_4
    ***
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+
    
    
    X_5
    ***
    
    +-----------------------+----------+-------------------------------------+
    | Name                  | Size     | Contents                            |
    +=======================+==========+=====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer  |
    +-----------------------+----------+-------------------------------------+
    | new_state             | Variable | sequence of bytes                   |
    +-----------------------+----------+-------------------------------------+
    | fee                   | 32 bytes | bytes                               |
    +-----------------------+----------+-------------------------------------+
    | exit_validity         | 1 byte   | boolean (0 for false, 255 for true) |
    +-----------------------+----------+-------------------------------------+
    
    
    X_3
    ***
    
    +-----------------+----------------------+----------+
    | Name            | Size                 | Contents |
    +=================+======================+==========+
    | Unnamed field 0 | Determined from data | $X_4     |
    +-----------------+----------------------+----------+
    | Unnamed field 1 | Determined from data | $X_5     |
    +-----------------+----------------------+----------+
    
    
    X_8
    ***
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | new_state             | Variable | sequence of bytes                  |
    +-----------------------+----------+------------------------------------+
    | fee                   | 32 bytes | bytes                              |
    +-----------------------+----------+------------------------------------+
    
    
    X_6
    ***
    
    +-----------------+----------------------+----------+
    | Name            | Size                 | Contents |
    +=================+======================+==========+
    | Unnamed field 0 | Determined from data | $X_4     |
    +-----------------+----------------------+----------+
    | Unnamed field 1 | Determined from data | $X_8     |
    +-----------------+----------------------+----------+
    
    
    X_9
    ***
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | sequence of bytes                  |
    +-----------------------+----------+------------------------------------+
    
    
    X_2
    ***
    
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | pending_pis           | Variable             | sequence of $X_3                   |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | private_pis           | Variable             | sequence of $X_6                   |
    +-----------------------+----------------------+------------------------------------+
    | fee_pi                | Determined from data | $X_9                               |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | proof                 | Variable             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    
    
    Z.t
    ***
    
    A variable-length sequence of bytes encoding a Zarith integer. Each byte has a running unary size bit: the most significant bit of each byte indicates whether this is the last byte in the sequence (0) or whether the sequence continues (1). The second most significant bit of the first byte is reserved for the sign (0 for positive, 1 for negative). Size and sign bits ignored, the data is the binary representation of the absolute value of the number in little-endian order.
    
    +------+----------------------+----------+
    | Name | Size                 | Contents |
    +======+======================+==========+
    | Z.t  | Determined from data | bytes    |
    +------+----------------------+----------+
    
    
    X_12
    ****
    
    +--------+----------------------+----------+
    | Name   | Size                 | Contents |
    +========+======================+==========+
    | id     | 32 bytes             | bytes    |
    +--------+----------------------+----------+
    | amount | Determined from data | $Z.t     |
    +--------+----------------------+----------+
    
    
    X_11
    ****
    
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | Name                  | Size                 | Contents                                                                |
    +=======================+======================+=========================================================================+
    | op_code               | 4 bytes              | signed 31-bit big-endian integer in the range -1073741824 to 1073741823 |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | price                 | Determined from data | $X_12                                                                   |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | l1_dst                | 21 bytes             | $public_key_hash                                                        |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | rollup_id             | 20 bytes             | bytes                                                                   |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer                                      |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | payload               | Variable             | sequence of bytes                                                       |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    
    
    beta.michelson.v1.primitives (Enumeration: unsigned 8-bit integer):
    *******************************************************************
    
    +-------------+--------------------------------+
    | Case number | Encoded string                 |
    +=============+================================+
    | 0           | parameter                      |
    +-------------+--------------------------------+
    | 1           | storage                        |
    +-------------+--------------------------------+
    | 2           | code                           |
    +-------------+--------------------------------+
    | 3           | False                          |
    +-------------+--------------------------------+
    | 4           | Elt                            |
    +-------------+--------------------------------+
    | 5           | Left                           |
    +-------------+--------------------------------+
    | 6           | None                           |
    +-------------+--------------------------------+
    | 7           | Pair                           |
    +-------------+--------------------------------+
    | 8           | Right                          |
    +-------------+--------------------------------+
    | 9           | Some                           |
    +-------------+--------------------------------+
    | 10          | True                           |
    +-------------+--------------------------------+
    | 11          | Unit                           |
    +-------------+--------------------------------+
    | 12          | PACK                           |
    +-------------+--------------------------------+
    | 13          | UNPACK                         |
    +-------------+--------------------------------+
    | 14          | BLAKE2B                        |
    +-------------+--------------------------------+
    | 15          | SHA256                         |
    +-------------+--------------------------------+
    | 16          | SHA512                         |
    +-------------+--------------------------------+
    | 17          | ABS                            |
    +-------------+--------------------------------+
    | 18          | ADD                            |
    +-------------+--------------------------------+
    | 19          | AMOUNT                         |
    +-------------+--------------------------------+
    | 20          | AND                            |
    +-------------+--------------------------------+
    | 21          | BALANCE                        |
    +-------------+--------------------------------+
    | 22          | CAR                            |
    +-------------+--------------------------------+
    | 23          | CDR                            |
    +-------------+--------------------------------+
    | 24          | CHECK_SIGNATURE                |
    +-------------+--------------------------------+
    | 25          | COMPARE                        |
    +-------------+--------------------------------+
    | 26          | CONCAT                         |
    +-------------+--------------------------------+
    | 27          | CONS                           |
    +-------------+--------------------------------+
    | 28          | CREATE_ACCOUNT                 |
    +-------------+--------------------------------+
    | 29          | CREATE_CONTRACT                |
    +-------------+--------------------------------+
    | 30          | IMPLICIT_ACCOUNT               |
    +-------------+--------------------------------+
    | 31          | DIP                            |
    +-------------+--------------------------------+
    | 32          | DROP                           |
    +-------------+--------------------------------+
    | 33          | DUP                            |
    +-------------+--------------------------------+
    | 34          | EDIV                           |
    +-------------+--------------------------------+
    | 35          | EMPTY_MAP                      |
    +-------------+--------------------------------+
    | 36          | EMPTY_SET                      |
    +-------------+--------------------------------+
    | 37          | EQ                             |
    +-------------+--------------------------------+
    | 38          | EXEC                           |
    +-------------+--------------------------------+
    | 39          | FAILWITH                       |
    +-------------+--------------------------------+
    | 40          | GE                             |
    +-------------+--------------------------------+
    | 41          | GET                            |
    +-------------+--------------------------------+
    | 42          | GT                             |
    +-------------+--------------------------------+
    | 43          | HASH_KEY                       |
    +-------------+--------------------------------+
    | 44          | IF                             |
    +-------------+--------------------------------+
    | 45          | IF_CONS                        |
    +-------------+--------------------------------+
    | 46          | IF_LEFT                        |
    +-------------+--------------------------------+
    | 47          | IF_NONE                        |
    +-------------+--------------------------------+
    | 48          | INT                            |
    +-------------+--------------------------------+
    | 49          | LAMBDA                         |
    +-------------+--------------------------------+
    | 50          | LE                             |
    +-------------+--------------------------------+
    | 51          | LEFT                           |
    +-------------+--------------------------------+
    | 52          | LOOP                           |
    +-------------+--------------------------------+
    | 53          | LSL                            |
    +-------------+--------------------------------+
    | 54          | LSR                            |
    +-------------+--------------------------------+
    | 55          | LT                             |
    +-------------+--------------------------------+
    | 56          | MAP                            |
    +-------------+--------------------------------+
    | 57          | MEM                            |
    +-------------+--------------------------------+
    | 58          | MUL                            |
    +-------------+--------------------------------+
    | 59          | NEG                            |
    +-------------+--------------------------------+
    | 60          | NEQ                            |
    +-------------+--------------------------------+
    | 61          | NIL                            |
    +-------------+--------------------------------+
    | 62          | NONE                           |
    +-------------+--------------------------------+
    | 63          | NOT                            |
    +-------------+--------------------------------+
    | 64          | NOW                            |
    +-------------+--------------------------------+
    | 65          | OR                             |
    +-------------+--------------------------------+
    | 66          | PAIR                           |
    +-------------+--------------------------------+
    | 67          | PUSH                           |
    +-------------+--------------------------------+
    | 68          | RIGHT                          |
    +-------------+--------------------------------+
    | 69          | SIZE                           |
    +-------------+--------------------------------+
    | 70          | SOME                           |
    +-------------+--------------------------------+
    | 71          | SOURCE                         |
    +-------------+--------------------------------+
    | 72          | SENDER                         |
    +-------------+--------------------------------+
    | 73          | SELF                           |
    +-------------+--------------------------------+
    | 74          | STEPS_TO_QUOTA                 |
    +-------------+--------------------------------+
    | 75          | SUB                            |
    +-------------+--------------------------------+
    | 76          | SWAP                           |
    +-------------+--------------------------------+
    | 77          | TRANSFER_TOKENS                |
    +-------------+--------------------------------+
    | 78          | SET_DELEGATE                   |
    +-------------+--------------------------------+
    | 79          | UNIT                           |
    +-------------+--------------------------------+
    | 80          | UPDATE                         |
    +-------------+--------------------------------+
    | 81          | XOR                            |
    +-------------+--------------------------------+
    | 82          | ITER                           |
    +-------------+--------------------------------+
    | 83          | LOOP_LEFT                      |
    +-------------+--------------------------------+
    | 84          | ADDRESS                        |
    +-------------+--------------------------------+
    | 85          | CONTRACT                       |
    +-------------+--------------------------------+
    | 86          | ISNAT                          |
    +-------------+--------------------------------+
    | 87          | CAST                           |
    +-------------+--------------------------------+
    | 88          | RENAME                         |
    +-------------+--------------------------------+
    | 89          | bool                           |
    +-------------+--------------------------------+
    | 90          | contract                       |
    +-------------+--------------------------------+
    | 91          | int                            |
    +-------------+--------------------------------+
    | 92          | key                            |
    +-------------+--------------------------------+
    | 93          | key_hash                       |
    +-------------+--------------------------------+
    | 94          | lambda                         |
    +-------------+--------------------------------+
    | 95          | list                           |
    +-------------+--------------------------------+
    | 96          | map                            |
    +-------------+--------------------------------+
    | 97          | big_map                        |
    +-------------+--------------------------------+
    | 98          | nat                            |
    +-------------+--------------------------------+
    | 99          | option                         |
    +-------------+--------------------------------+
    | 100         | or                             |
    +-------------+--------------------------------+
    | 101         | pair                           |
    +-------------+--------------------------------+
    | 102         | set                            |
    +-------------+--------------------------------+
    | 103         | signature                      |
    +-------------+--------------------------------+
    | 104         | string                         |
    +-------------+--------------------------------+
    | 105         | bytes                          |
    +-------------+--------------------------------+
    | 106         | mutez                          |
    +-------------+--------------------------------+
    | 107         | timestamp                      |
    +-------------+--------------------------------+
    | 108         | unit                           |
    +-------------+--------------------------------+
    | 109         | operation                      |
    +-------------+--------------------------------+
    | 110         | address                        |
    +-------------+--------------------------------+
    | 111         | SLICE                          |
    +-------------+--------------------------------+
    | 112         | DIG                            |
    +-------------+--------------------------------+
    | 113         | DUG                            |
    +-------------+--------------------------------+
    | 114         | EMPTY_BIG_MAP                  |
    +-------------+--------------------------------+
    | 115         | APPLY                          |
    +-------------+--------------------------------+
    | 116         | chain_id                       |
    +-------------+--------------------------------+
    | 117         | CHAIN_ID                       |
    +-------------+--------------------------------+
    | 118         | LEVEL                          |
    +-------------+--------------------------------+
    | 119         | SELF_ADDRESS                   |
    +-------------+--------------------------------+
    | 120         | never                          |
    +-------------+--------------------------------+
    | 121         | NEVER                          |
    +-------------+--------------------------------+
    | 122         | UNPAIR                         |
    +-------------+--------------------------------+
    | 123         | VOTING_POWER                   |
    +-------------+--------------------------------+
    | 124         | TOTAL_VOTING_POWER             |
    +-------------+--------------------------------+
    | 125         | KECCAK                         |
    +-------------+--------------------------------+
    | 126         | SHA3                           |
    +-------------+--------------------------------+
    | 127         | PAIRING_CHECK                  |
    +-------------+--------------------------------+
    | 128         | bls12_381_g1                   |
    +-------------+--------------------------------+
    | 129         | bls12_381_g2                   |
    +-------------+--------------------------------+
    | 130         | bls12_381_fr                   |
    +-------------+--------------------------------+
    | 131         | sapling_state                  |
    +-------------+--------------------------------+
    | 132         | sapling_transaction_deprecated |
    +-------------+--------------------------------+
    | 133         | SAPLING_EMPTY_STATE            |
    +-------------+--------------------------------+
    | 134         | SAPLING_VERIFY_UPDATE          |
    +-------------+--------------------------------+
    | 135         | ticket                         |
    +-------------+--------------------------------+
    | 136         | TICKET_DEPRECATED              |
    +-------------+--------------------------------+
    | 137         | READ_TICKET                    |
    +-------------+--------------------------------+
    | 138         | SPLIT_TICKET                   |
    +-------------+--------------------------------+
    | 139         | JOIN_TICKETS                   |
    +-------------+--------------------------------+
    | 140         | GET_AND_UPDATE                 |
    +-------------+--------------------------------+
    | 141         | chest                          |
    +-------------+--------------------------------+
    | 142         | chest_key                      |
    +-------------+--------------------------------+
    | 143         | OPEN_CHEST                     |
    +-------------+--------------------------------+
    | 144         | VIEW                           |
    +-------------+--------------------------------+
    | 145         | view                           |
    +-------------+--------------------------------+
    | 146         | constant                       |
    +-------------+--------------------------------+
    | 147         | SUB_MUTEZ                      |
    +-------------+--------------------------------+
    | 148         | tx_rollup_l2_address           |
    +-------------+--------------------------------+
    | 149         | MIN_BLOCK_TIME                 |
    +-------------+--------------------------------+
    | 150         | sapling_transaction            |
    +-------------+--------------------------------+
    | 151         | EMIT                           |
    +-------------+--------------------------------+
    | 152         | Lambda_rec                     |
    +-------------+--------------------------------+
    | 153         | LAMBDA_REC                     |
    +-------------+--------------------------------+
    | 154         | TICKET                         |
    +-------------+--------------------------------+
    | 155         | BYTES                          |
    +-------------+--------------------------------+
    | 156         | NAT                            |
    +-------------+--------------------------------+
    | 157         | Ticket                         |
    +-------------+--------------------------------+
    
    
    micheline.beta.michelson_v1.expression (Determined from data, 8-bit tag)
    ************************************************************************
    
    Int (tag 0)
    ===========
    
    +------+----------------------+------------------------+
    | Name | Size                 | Contents               |
    +======+======================+========================+
    | Tag  | 1 byte               | unsigned 8-bit integer |
    +------+----------------------+------------------------+
    | int  | Determined from data | $Z.t                   |
    +------+----------------------+------------------------+
    
    
    String (tag 1)
    ==============
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | string                | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+
    
    
    Sequence (tag 2)
    ================
    
    +-----------------------+----------+-----------------------------------------------------+
    | Name                  | Size     | Contents                                            |
    +=======================+==========+=====================================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer                              |
    +-----------------------+----------+-----------------------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer                  |
    +-----------------------+----------+-----------------------------------------------------+
    | Unnamed field 0       | Variable | sequence of $micheline.beta.michelson_v1.expression |
    +-----------------------+----------+-----------------------------------------------------+
    
    
    Prim__no_args__no_annots (tag 3)
    ================================
    
    +------+--------+-----------------------------------------------------------------------------------+
    | Name | Size   | Contents                                                                          |
    +======+========+===================================================================================+
    | Tag  | 1 byte | unsigned 8-bit integer                                                            |
    +------+--------+-----------------------------------------------------------------------------------+
    | prim | 1 byte | unsigned 8-bit integer encoding an enumeration (see beta.michelson.v1.primitives) |
    +------+--------+-----------------------------------------------------------------------------------+
    
    
    Prim__no_args__some_annots (tag 4)
    ==================================
    
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    | Name                  | Size     | Contents                                                                          |
    +=======================+==========+===================================================================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer                                                            |
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    | prim                  | 1 byte   | unsigned 8-bit integer encoding an enumeration (see beta.michelson.v1.primitives) |
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer                                                |
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    | annots                | Variable | bytes                                                                             |
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    
    
    Prim__1_arg__no_annots (tag 5)
    ==============================
    
    +------+----------------------+-----------------------------------------------------------------------------------+
    | Name | Size                 | Contents                                                                          |
    +======+======================+===================================================================================+
    | Tag  | 1 byte               | unsigned 8-bit integer                                                            |
    +------+----------------------+-----------------------------------------------------------------------------------+
    | prim | 1 byte               | unsigned 8-bit integer encoding an enumeration (see beta.michelson.v1.primitives) |
    +------+----------------------+-----------------------------------------------------------------------------------+
    | arg  | Determined from data | $micheline.beta.michelson_v1.expression                                           |
    +------+----------------------+-----------------------------------------------------------------------------------+
    
    
    Prim__1_arg__some_annots (tag 6)
    ================================
    
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | Name                  | Size                 | Contents                                                                          |
    +=======================+======================+===================================================================================+
    | Tag                   | 1 byte               | unsigned 8-bit integer                                                            |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | prim                  | 1 byte               | unsigned 8-bit integer encoding an enumeration (see beta.michelson.v1.primitives) |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | arg                   | Determined from data | $micheline.beta.michelson_v1.expression                                           |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer                                                |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | annots                | Variable             | bytes                                                                             |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    
    
    Prim__2_args__no_annots (tag 7)
    ===============================
    
    +------+----------------------+-----------------------------------------------------------------------------------+
    | Name | Size                 | Contents                                                                          |
    +======+======================+===================================================================================+
    | Tag  | 1 byte               | unsigned 8-bit integer                                                            |
    +------+----------------------+-----------------------------------------------------------------------------------+
    | prim | 1 byte               | unsigned 8-bit integer encoding an enumeration (see beta.michelson.v1.primitives) |
    +------+----------------------+-----------------------------------------------------------------------------------+
    | arg1 | Determined from data | $micheline.beta.michelson_v1.expression                                           |
    +------+----------------------+-----------------------------------------------------------------------------------+
    | arg2 | Determined from data | $micheline.beta.michelson_v1.expression                                           |
    +------+----------------------+-----------------------------------------------------------------------------------+
    
    
    Prim__2_args__some_annots (tag 8)
    =================================
    
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | Name                  | Size                 | Contents                                                                          |
    +=======================+======================+===================================================================================+
    | Tag                   | 1 byte               | unsigned 8-bit integer                                                            |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | prim                  | 1 byte               | unsigned 8-bit integer encoding an enumeration (see beta.michelson.v1.primitives) |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | arg1                  | Determined from data | $micheline.beta.michelson_v1.expression                                           |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | arg2                  | Determined from data | $micheline.beta.michelson_v1.expression                                           |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer                                                |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    | annots                | Variable             | bytes                                                                             |
    +-----------------------+----------------------+-----------------------------------------------------------------------------------+
    
    
    Prim__generic (tag 9)
    =====================
    
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    | Name                  | Size     | Contents                                                                          |
    +=======================+==========+===================================================================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer                                                            |
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    | prim                  | 1 byte   | unsigned 8-bit integer encoding an enumeration (see beta.michelson.v1.primitives) |
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer                                                |
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    | args                  | Variable | sequence of $micheline.beta.michelson_v1.expression                               |
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer                                                |
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    | annots                | Variable | bytes                                                                             |
    +-----------------------+----------+-----------------------------------------------------------------------------------+
    
    
    Bytes (tag 10)
    ==============
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | bytes                 | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+
    
    
    beta.contract_id (22 bytes, 8-bit tag)
    **************************************
    
    Implicit (tag 0)
    ================
    
    +---------------------------+----------+------------------------+
    | Name                      | Size     | Contents               |
    +===========================+==========+========================+
    | Tag                       | 1 byte   | unsigned 8-bit integer |
    +---------------------------+----------+------------------------+
    | Signature.Public_key_hash | 21 bytes | $public_key_hash       |
    +---------------------------+----------+------------------------+
    
    
    Originated (tag 1)
    ==================
    
    +---------------+----------+------------------------+
    | Name          | Size     | Contents               |
    +===============+==========+========================+
    | Tag           | 1 byte   | unsigned 8-bit integer |
    +---------------+----------+------------------------+
    | Contract_hash | 20 bytes | bytes                  |
    +---------------+----------+------------------------+
    | padding       | 1 byte   | padding                |
    +---------------+----------+------------------------+
    
    
    X_15 (Determined from data, 8-bit tag)
    **************************************
    
    None (tag 0)
    ============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    Some (tag 1)
    ============
    
    +----------+----------------------+-----------------------------------------+
    | Name     | Size                 | Contents                                |
    +==========+======================+=========================================+
    | Tag      | 1 byte               | unsigned 8-bit integer                  |
    +----------+----------------------+-----------------------------------------+
    | contents | Determined from data | $micheline.beta.michelson_v1.expression |
    +----------+----------------------+-----------------------------------------+
    | ty       | Determined from data | $micheline.beta.michelson_v1.expression |
    +----------+----------------------+-----------------------------------------+
    | ticketer | 22 bytes             | $beta.contract_id                       |
    +----------+----------------------+-----------------------------------------+
    
    
    X_10
    ****
    
    +-----------------+----------------------+----------+
    | Name            | Size                 | Contents |
    +=================+======================+==========+
    | Unnamed field 0 | Determined from data | $X_11    |
    +-----------------+----------------------+----------+
    | Unnamed field 1 | Determined from data | $X_15    |
    +-----------------+----------------------+----------+
    
    
    X_18 (1 byte, 8-bit tag)
    ************************
    
    Public (tag 0)
    ==============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    Private (tag 1)
    ===============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    Fee (tag 2)
    ===========
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    X_16
    ****
    
    +-----------------+----------------------+----------+
    | Name            | Size                 | Contents |
    +=================+======================+==========+
    | Unnamed field 0 | Determined from data | $X_4     |
    +-----------------+----------------------+----------+
    | Unnamed field 1 | 1 byte               | $X_18    |
    +-----------------+----------------------+----------+
    
    
    X_19
    ****
    
    +------------------+----------+------------------------+
    | Name             | Size     | Contents               |
    +==================+==========+========================+
    | slot_index       | 1 byte   | unsigned 8-bit integer |
    +------------------+----------+------------------------+
    | commitment       | 48 bytes | bytes                  |
    +------------------+----------+------------------------+
    | commitment_proof | 96 bytes | bytes                  |
    +------------------+----------+------------------------+
    
    
    X_20
    ****
    
    +-------+----------+------------------+
    | Name  | Size     | Contents         |
    +=======+==========+==================+
    | alice | 21 bytes | $public_key_hash |
    +-------+----------+------------------+
    | bob   | 21 bytes | $public_key_hash |
    +-------+----------+------------------+
    
    
    X_21
    ****
    
    +-----------------+---------+----------------------------------+
    | Name            | Size    | Contents                         |
    +=================+=========+==================================+
    | published_level | 4 bytes | signed 32-bit big-endian integer |
    +-----------------+---------+----------------------------------+
    | slot_index      | 1 byte  | unsigned 8-bit integer           |
    +-----------------+---------+----------------------------------+
    | page_index      | 2 bytes | signed 16-bit big-endian integer |
    +-----------------+---------+----------------------------------+
    
    
    X_22 (Determined from data, 8-bit tag)
    **************************************
    
    raw data proof (tag 0)
    ======================
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 2 bytes  | unsigned 16-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | raw_data              | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+
    
    
    metadata proof (tag 1)
    ======================
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    dal page proof (tag 2)
    ======================
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | dal_page_id           | 7 bytes  | $X_21                              |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | dal_proof             | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+
    
    
    dal parameters proof (tag 3)
    ============================
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    X_23 (Determined from data, 8-bit tag)
    **************************************
    
    inbox proof (tag 0)
    ===================
    
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | Tag                   | 1 byte               | unsigned 8-bit integer             |
    +-----------------------+----------------------+------------------------------------+
    | level                 | 4 bytes              | signed 32-bit big-endian integer   |
    +-----------------------+----------------------+------------------------------------+
    | message_counter       | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | serialized_proof      | Variable             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    
    
    reveal proof (tag 1)
    ====================
    
    +--------------+----------------------+------------------------+
    | Name         | Size                 | Contents               |
    +==============+======================+========================+
    | Tag          | 1 byte               | unsigned 8-bit integer |
    +--------------+----------------------+------------------------+
    | reveal_proof | Determined from data | $X_22                  |
    +--------------+----------------------+------------------------+
    
    
    first input (tag 2)
    ===================
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    X_24
    ****
    
    +-----------------------------+----------------------+-------------------------------------+
    | Name                        | Size                 | Contents                            |
    +=============================+======================+=====================================+
    | ? presence of field "state" | 1 byte               | boolean (0 for false, 255 for true) |
    +-----------------------------+----------------------+-------------------------------------+
    | state                       | 32 bytes             | bytes                               |
    +-----------------------------+----------------------+-------------------------------------+
    | tick                        | Determined from data | $N.t                                |
    +-----------------------------+----------------------+-------------------------------------+
    
    
    X_25 (Determined from data, 8-bit tag)
    **************************************
    
    Dissection (tag 0)
    ==================
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | sequence of $X_24                  |
    +-----------------------+----------+------------------------------------+
    
    
    Proof (tag 1)
    =============
    
    +-----------------------------------+----------------------+-------------------------------------+
    | Name                              | Size                 | Contents                            |
    +===================================+======================+=====================================+
    | Tag                               | 1 byte               | unsigned 8-bit integer              |
    +-----------------------------------+----------------------+-------------------------------------+
    | # bytes in next field             | 4 bytes              | unsigned 30-bit big-endian integer  |
    +-----------------------------------+----------------------+-------------------------------------+
    | pvm_step                          | Variable             | bytes                               |
    +-----------------------------------+----------------------+-------------------------------------+
    | ? presence of field "input_proof" | 1 byte               | boolean (0 for false, 255 for true) |
    +-----------------------------------+----------------------+-------------------------------------+
    | input_proof                       | Determined from data | $X_23                               |
    +-----------------------------------+----------------------+-------------------------------------+
    
    
    X_26 (Determined from data, 8-bit tag)
    **************************************
    
    Start (tag 0)
    =============
    
    +--------------------------+----------+------------------------+
    | Name                     | Size     | Contents               |
    +==========================+==========+========================+
    | Tag                      | 1 byte   | unsigned 8-bit integer |
    +--------------------------+----------+------------------------+
    | player_commitment_hash   | 32 bytes | bytes                  |
    +--------------------------+----------+------------------------+
    | opponent_commitment_hash | 32 bytes | bytes                  |
    +--------------------------+----------+------------------------+
    
    
    Move (tag 1)
    ============
    
    +--------+----------------------+------------------------+
    | Name   | Size                 | Contents               |
    +========+======================+========================+
    | Tag    | 1 byte               | unsigned 8-bit integer |
    +--------+----------------------+------------------------+
    | choice | Determined from data | $N.t                   |
    +--------+----------------------+------------------------+
    | step   | Determined from data | $X_25                  |
    +--------+----------------------+------------------------+
    
    
    X_27
    ****
    
    +------------------+----------+----------------------------------+
    | Name             | Size     | Contents                         |
    +==================+==========+==================================+
    | compressed_state | 32 bytes | bytes                            |
    +------------------+----------+----------------------------------+
    | inbox_level      | 4 bytes  | signed 32-bit big-endian integer |
    +------------------+----------+----------------------------------+
    | predecessor      | 32 bytes | bytes                            |
    +------------------+----------+----------------------------------+
    | number_of_ticks  | 8 bytes  | signed 64-bit big-endian integer |
    +------------------+----------+----------------------------------+
    
    
    X_29 (Enumeration: unsigned 8-bit integer):
    *******************************************
    
    +-------------+----------------+
    | Case number | Encoded string |
    +=============+================+
    | 0           | arith          |
    +-------------+----------------+
    | 1           | wasm_2_0_0     |
    +-------------+----------------+
    | 2           | riscv          |
    +-------------+----------------+
    
    
    X_30
    ****
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | sequence of $public_key_hash       |
    +-----------------------+----------+------------------------------------+
    
    
    public_key (Determined from data, 8-bit tag)
    ********************************************
    
    Ed25519 (tag 0)
    ===============
    
    +--------------------+----------+------------------------+
    | Name               | Size     | Contents               |
    +====================+==========+========================+
    | Tag                | 1 byte   | unsigned 8-bit integer |
    +--------------------+----------+------------------------+
    | Ed25519.Public_key | 32 bytes | bytes                  |
    +--------------------+----------+------------------------+
    
    
    Secp256k1 (tag 1)
    =================
    
    +----------------------+----------+------------------------+
    | Name                 | Size     | Contents               |
    +======================+==========+========================+
    | Tag                  | 1 byte   | unsigned 8-bit integer |
    +----------------------+----------+------------------------+
    | Secp256k1.Public_key | 33 bytes | bytes                  |
    +----------------------+----------+------------------------+
    
    
    P256 (tag 2)
    ============
    
    +-----------------+----------+------------------------+
    | Name            | Size     | Contents               |
    +=================+==========+========================+
    | Tag             | 1 byte   | unsigned 8-bit integer |
    +-----------------+----------+------------------------+
    | P256.Public_key | 33 bytes | bytes                  |
    +-----------------+----------+------------------------+
    
    
    Bls (tag 3)
    ===========
    
    +----------------------+----------+------------------------+
    | Name                 | Size     | Contents               |
    +======================+==========+========================+
    | Tag                  | 1 byte   | unsigned 8-bit integer |
    +----------------------+----------+------------------------+
    | Bls12_381.Public_key | 48 bytes | bytes                  |
    +----------------------+----------+------------------------+
    
    
    beta.contract_id.originated (22 bytes, 8-bit tag)
    *************************************************
    
    Originated (tag 1)
    ==================
    
    +---------------+----------+------------------------+
    | Name          | Size     | Contents               |
    +===============+==========+========================+
    | Tag           | 1 byte   | unsigned 8-bit integer |
    +---------------+----------+------------------------+
    | Contract_hash | 20 bytes | bytes                  |
    +---------------+----------+------------------------+
    | padding       | 1 byte   | padding                |
    +---------------+----------+------------------------+
    
    
    beta.scripted.contracts
    ***********************
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | code                  | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | storage               | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+
    
    
    beta.entrypoint (Determined from data, 8-bit tag)
    *************************************************
    
    default (tag 0)
    ===============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    root (tag 1)
    ============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    do (tag 2)
    ==========
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    set_delegate (tag 3)
    ====================
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    remove_delegate (tag 4)
    =======================
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    deposit (tag 5)
    ===============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    stake (tag 6)
    =============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    unstake (tag 7)
    ===============
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    finalize_unstake (tag 8)
    ========================
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    set_delegate_parameters (tag 9)
    ===============================
    
    +------+--------+------------------------+
    | Name | Size   | Contents               |
    +======+========+========================+
    | Tag  | 1 byte | unsigned 8-bit integer |
    +------+--------+------------------------+
    
    
    named (tag 255)
    ===============
    
    +-----------------------+----------+------------------------+
    | Name                  | Size     | Contents               |
    +=======================+==========+========================+
    | Tag                   | 1 byte   | unsigned 8-bit integer |
    +-----------------------+----------+------------------------+
    | # bytes in next field | 1 byte   | unsigned 8-bit integer |
    +-----------------------+----------+------------------------+
    | Unnamed field 0       | Variable | bytes                  |
    +-----------------------+----------+------------------------+
    
    
    X_31
    ****
    
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | entrypoint            | Determined from data | $beta.entrypoint                   |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | value                 | Variable             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    
    
    X_32
    ****
    
    +-----------------+-----------+----------+
    | Name            | Size      | Contents |
    +=================+===========+==========+
    | Unnamed field 0 | 100 bytes | bytes    |
    +-----------------+-----------+----------+
    | Unnamed field 1 | 100 bytes | bytes    |
    +-----------------+-----------+----------+
    
    
    beta.inlined.preattestation.contents (43 bytes, 8-bit tag)
    **********************************************************
    
    Preattestation (tag 20)
    =======================
    
    +--------------------+----------+------------------------------------+
    | Name               | Size     | Contents                           |
    +====================+==========+====================================+
    | Tag                | 1 byte   | unsigned 8-bit integer             |
    +--------------------+----------+------------------------------------+
    | slot               | 2 bytes  | unsigned 16-bit big-endian integer |
    +--------------------+----------+------------------------------------+
    | level              | 4 bytes  | signed 32-bit big-endian integer   |
    +--------------------+----------+------------------------------------+
    | round              | 4 bytes  | signed 32-bit big-endian integer   |
    +--------------------+----------+------------------------------------+
    | block_payload_hash | 32 bytes | bytes                              |
    +--------------------+----------+------------------------------------+
    
    
    beta.inlined.preattestation
    ***************************
    
    +------------+----------+---------------------------------------+
    | Name       | Size     | Contents                              |
    +============+==========+=======================================+
    | branch     | 32 bytes | bytes                                 |
    +------------+----------+---------------------------------------+
    | operations | 43 bytes | $beta.inlined.preattestation.contents |
    +------------+----------+---------------------------------------+
    | signature  | Variable | bytes                                 |
    +------------+----------+---------------------------------------+
    
    
    fitness.elem
    ************
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+
    
    
    beta.block_header.alpha.full_header
    ***********************************
    
    +---------------------------------------+----------+-------------------------------------+
    | Name                                  | Size     | Contents                            |
    +=======================================+==========+=====================================+
    | level                                 | 4 bytes  | signed 32-bit big-endian integer    |
    +---------------------------------------+----------+-------------------------------------+
    | proto                                 | 1 byte   | unsigned 8-bit integer              |
    +---------------------------------------+----------+-------------------------------------+
    | predecessor                           | 32 bytes | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    | timestamp                             | 8 bytes  | signed 64-bit big-endian integer    |
    +---------------------------------------+----------+-------------------------------------+
    | validation_pass                       | 1 byte   | unsigned 8-bit integer              |
    +---------------------------------------+----------+-------------------------------------+
    | operations_hash                       | 32 bytes | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    | # bytes in field "fitness"            | 4 bytes  | unsigned 30-bit big-endian integer  |
    +---------------------------------------+----------+-------------------------------------+
    | fitness                               | Variable | sequence of $fitness.elem           |
    +---------------------------------------+----------+-------------------------------------+
    | context                               | 32 bytes | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    | payload_hash                          | 32 bytes | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    | payload_round                         | 4 bytes  | signed 32-bit big-endian integer    |
    +---------------------------------------+----------+-------------------------------------+
    | proof_of_work_nonce                   | 8 bytes  | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    | ? presence of field "seed_nonce_hash" | 1 byte   | boolean (0 for false, 255 for true) |
    +---------------------------------------+----------+-------------------------------------+
    | seed_nonce_hash                       | 32 bytes | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    | per_block_votes                       | 1 byte   | $beta.per_block_votes               |
    +---------------------------------------+----------+-------------------------------------+
    | signature                             | Variable | bytes                               |
    +---------------------------------------+----------+-------------------------------------+
    
    
    beta.inlined.attestation_mempool.contents (Determined from data, 8-bit tag)
    ***************************************************************************
    
    Attestation (tag 21)
    ====================
    
    +--------------------+----------+------------------------------------+
    | Name               | Size     | Contents                           |
    +====================+==========+====================================+
    | Tag                | 1 byte   | unsigned 8-bit integer             |
    +--------------------+----------+------------------------------------+
    | slot               | 2 bytes  | unsigned 16-bit big-endian integer |
    +--------------------+----------+------------------------------------+
    | level              | 4 bytes  | signed 32-bit big-endian integer   |
    +--------------------+----------+------------------------------------+
    | round              | 4 bytes  | signed 32-bit big-endian integer   |
    +--------------------+----------+------------------------------------+
    | block_payload_hash | 32 bytes | bytes                              |
    +--------------------+----------+------------------------------------+
    
    
    Attestation_with_dal (tag 23)
    =============================
    
    +--------------------+----------------------+------------------------------------+
    | Name               | Size                 | Contents                           |
    +====================+======================+====================================+
    | Tag                | 1 byte               | unsigned 8-bit integer             |
    +--------------------+----------------------+------------------------------------+
    | slot               | 2 bytes              | unsigned 16-bit big-endian integer |
    +--------------------+----------------------+------------------------------------+
    | level              | 4 bytes              | signed 32-bit big-endian integer   |
    +--------------------+----------------------+------------------------------------+
    | round              | 4 bytes              | signed 32-bit big-endian integer   |
    +--------------------+----------------------+------------------------------------+
    | block_payload_hash | 32 bytes             | bytes                              |
    +--------------------+----------------------+------------------------------------+
    | dal_attestation    | Determined from data | $Z.t                               |
    +--------------------+----------------------+------------------------------------+
    
    
    beta.inlined.attestation
    ************************
    
    +------------+----------------------+--------------------------------------------+
    | Name       | Size                 | Contents                                   |
    +============+======================+============================================+
    | branch     | 32 bytes             | bytes                                      |
    +------------+----------------------+--------------------------------------------+
    | operations | Determined from data | $beta.inlined.attestation_mempool.contents |
    +------------+----------------------+--------------------------------------------+
    | signature  | Variable             | bytes                                      |
    +------------+----------------------+--------------------------------------------+
    
    
    beta.operation.alpha.contents_or_signature_prefix (Determined from data, 8-bit tag)
    ***********************************************************************************
    
    Seed_nonce_revelation (tag 1)
    =============================
    
    +-------+----------+----------------------------------+
    | Name  | Size     | Contents                         |
    +=======+==========+==================================+
    | Tag   | 1 byte   | unsigned 8-bit integer           |
    +-------+----------+----------------------------------+
    | level | 4 bytes  | signed 32-bit big-endian integer |
    +-------+----------+----------------------------------+
    | nonce | 32 bytes | bytes                            |
    +-------+----------+----------------------------------+
    
    
    Double_attestation_evidence (tag 2)
    ===================================
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | op1                   | Variable | $beta.inlined.attestation          |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | op2                   | Variable | $beta.inlined.attestation          |
    +-----------------------+----------+------------------------------------+
    
    
    Double_baking_evidence (tag 3)
    ==============================
    
    +-----------------------+----------+--------------------------------------+
    | Name                  | Size     | Contents                             |
    +=======================+==========+======================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer               |
    +-----------------------+----------+--------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer   |
    +-----------------------+----------+--------------------------------------+
    | bh1                   | Variable | $beta.block_header.alpha.full_header |
    +-----------------------+----------+--------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer   |
    +-----------------------+----------+--------------------------------------+
    | bh2                   | Variable | $beta.block_header.alpha.full_header |
    +-----------------------+----------+--------------------------------------+
    
    
    Activate_account (tag 4)
    ========================
    
    +--------+----------+------------------------+
    | Name   | Size     | Contents               |
    +========+==========+========================+
    | Tag    | 1 byte   | unsigned 8-bit integer |
    +--------+----------+------------------------+
    | pkh    | 20 bytes | bytes                  |
    +--------+----------+------------------------+
    | secret | 20 bytes | bytes                  |
    +--------+----------+------------------------+
    
    
    Proposals (tag 5)
    =================
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | source                | 21 bytes | $public_key_hash                   |
    +-----------------------+----------+------------------------------------+
    | period                | 4 bytes  | signed 32-bit big-endian integer   |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | proposals             | Variable | sequence of at most 20 bytes       |
    +-----------------------+----------+------------------------------------+
    
    
    Ballot (tag 6)
    ==============
    
    +----------+----------+----------------------------------+
    | Name     | Size     | Contents                         |
    +==========+==========+==================================+
    | Tag      | 1 byte   | unsigned 8-bit integer           |
    +----------+----------+----------------------------------+
    | source   | 21 bytes | $public_key_hash                 |
    +----------+----------+----------------------------------+
    | period   | 4 bytes  | signed 32-bit big-endian integer |
    +----------+----------+----------------------------------+
    | proposal | 32 bytes | bytes                            |
    +----------+----------+----------------------------------+
    | ballot   | 1 byte   | signed 8-bit integer             |
    +----------+----------+----------------------------------+
    
    
    Double_preattestation_evidence (tag 7)
    ======================================
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | op1                   | Variable | $beta.inlined.preattestation       |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | op2                   | Variable | $beta.inlined.preattestation       |
    +-----------------------+----------+------------------------------------+
    
    
    Vdf_revelation (tag 8)
    ======================
    
    +----------+-----------+------------------------+
    | Name     | Size      | Contents               |
    +==========+===========+========================+
    | Tag      | 1 byte    | unsigned 8-bit integer |
    +----------+-----------+------------------------+
    | solution | 200 bytes | $X_32                  |
    +----------+-----------+------------------------+
    
    
    Drain_delegate (tag 9)
    ======================
    
    +---------------+----------+------------------------+
    | Name          | Size     | Contents               |
    +===============+==========+========================+
    | Tag           | 1 byte   | unsigned 8-bit integer |
    +---------------+----------+------------------------+
    | consensus_key | 21 bytes | $public_key_hash       |
    +---------------+----------+------------------------+
    | delegate      | 21 bytes | $public_key_hash       |
    +---------------+----------+------------------------+
    | destination   | 21 bytes | $public_key_hash       |
    +---------------+----------+------------------------+
    
    
    Failing_noop (tag 17)
    =====================
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | Tag                   | 1 byte   | unsigned 8-bit integer             |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | arbitrary             | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+
    
    
    Preattestation (tag 20)
    =======================
    
    +--------------------+----------+------------------------------------+
    | Name               | Size     | Contents                           |
    +====================+==========+====================================+
    | Tag                | 1 byte   | unsigned 8-bit integer             |
    +--------------------+----------+------------------------------------+
    | slot               | 2 bytes  | unsigned 16-bit big-endian integer |
    +--------------------+----------+------------------------------------+
    | level              | 4 bytes  | signed 32-bit big-endian integer   |
    +--------------------+----------+------------------------------------+
    | round              | 4 bytes  | signed 32-bit big-endian integer   |
    +--------------------+----------+------------------------------------+
    | block_payload_hash | 32 bytes | bytes                              |
    +--------------------+----------+------------------------------------+
    
    
    Attestation (tag 21)
    ====================
    
    +--------------------+----------+------------------------------------+
    | Name               | Size     | Contents                           |
    +====================+==========+====================================+
    | Tag                | 1 byte   | unsigned 8-bit integer             |
    +--------------------+----------+------------------------------------+
    | slot               | 2 bytes  | unsigned 16-bit big-endian integer |
    +--------------------+----------+------------------------------------+
    | level              | 4 bytes  | signed 32-bit big-endian integer   |
    +--------------------+----------+------------------------------------+
    | round              | 4 bytes  | signed 32-bit big-endian integer   |
    +--------------------+----------+------------------------------------+
    | block_payload_hash | 32 bytes | bytes                              |
    +--------------------+----------+------------------------------------+
    
    
    Attestation_with_dal (tag 23)
    =============================
    
    +--------------------+----------------------+------------------------------------+
    | Name               | Size                 | Contents                           |
    +====================+======================+====================================+
    | Tag                | 1 byte               | unsigned 8-bit integer             |
    +--------------------+----------------------+------------------------------------+
    | slot               | 2 bytes              | unsigned 16-bit big-endian integer |
    +--------------------+----------------------+------------------------------------+
    | level              | 4 bytes              | signed 32-bit big-endian integer   |
    +--------------------+----------------------+------------------------------------+
    | round              | 4 bytes              | signed 32-bit big-endian integer   |
    +--------------------+----------------------+------------------------------------+
    | block_payload_hash | 32 bytes             | bytes                              |
    +--------------------+----------------------+------------------------------------+
    | dal_attestation    | Determined from data | $Z.t                               |
    +--------------------+----------------------+------------------------------------+
    
    
    Reveal (tag 107)
    ================
    
    +---------------+----------------------+------------------------+
    | Name          | Size                 | Contents               |
    +===============+======================+========================+
    | Tag           | 1 byte               | unsigned 8-bit integer |
    +---------------+----------------------+------------------------+
    | source        | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    | fee           | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | counter       | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | gas_limit     | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | storage_limit | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | public_key    | Determined from data | $public_key            |
    +---------------+----------------------+------------------------+
    
    
    Transaction (tag 108)
    =====================
    
    +----------------------------------+----------------------+-------------------------------------+
    | Name                             | Size                 | Contents                            |
    +==================================+======================+=====================================+
    | Tag                              | 1 byte               | unsigned 8-bit integer              |
    +----------------------------------+----------------------+-------------------------------------+
    | source                           | 21 bytes             | $public_key_hash                    |
    +----------------------------------+----------------------+-------------------------------------+
    | fee                              | Determined from data | $N.t                                |
    +----------------------------------+----------------------+-------------------------------------+
    | counter                          | Determined from data | $N.t                                |
    +----------------------------------+----------------------+-------------------------------------+
    | gas_limit                        | Determined from data | $N.t                                |
    +----------------------------------+----------------------+-------------------------------------+
    | storage_limit                    | Determined from data | $N.t                                |
    +----------------------------------+----------------------+-------------------------------------+
    | amount                           | Determined from data | $N.t                                |
    +----------------------------------+----------------------+-------------------------------------+
    | destination                      | 22 bytes             | $beta.contract_id                   |
    +----------------------------------+----------------------+-------------------------------------+
    | ? presence of field "parameters" | 1 byte               | boolean (0 for false, 255 for true) |
    +----------------------------------+----------------------+-------------------------------------+
    | parameters                       | Determined from data | $X_31                               |
    +----------------------------------+----------------------+-------------------------------------+
    
    
    Origination (tag 109)
    =====================
    
    +--------------------------------+----------------------+-------------------------------------+
    | Name                           | Size                 | Contents                            |
    +================================+======================+=====================================+
    | Tag                            | 1 byte               | unsigned 8-bit integer              |
    +--------------------------------+----------------------+-------------------------------------+
    | source                         | 21 bytes             | $public_key_hash                    |
    +--------------------------------+----------------------+-------------------------------------+
    | fee                            | Determined from data | $N.t                                |
    +--------------------------------+----------------------+-------------------------------------+
    | counter                        | Determined from data | $N.t                                |
    +--------------------------------+----------------------+-------------------------------------+
    | gas_limit                      | Determined from data | $N.t                                |
    +--------------------------------+----------------------+-------------------------------------+
    | storage_limit                  | Determined from data | $N.t                                |
    +--------------------------------+----------------------+-------------------------------------+
    | balance                        | Determined from data | $N.t                                |
    +--------------------------------+----------------------+-------------------------------------+
    | ? presence of field "delegate" | 1 byte               | boolean (0 for false, 255 for true) |
    +--------------------------------+----------------------+-------------------------------------+
    | delegate                       | 21 bytes             | $public_key_hash                    |
    +--------------------------------+----------------------+-------------------------------------+
    | script                         | Determined from data | $beta.scripted.contracts            |
    +--------------------------------+----------------------+-------------------------------------+
    
    
    Delegation (tag 110)
    ====================
    
    +--------------------------------+----------------------+-------------------------------------+
    | Name                           | Size                 | Contents                            |
    +================================+======================+=====================================+
    | Tag                            | 1 byte               | unsigned 8-bit integer              |
    +--------------------------------+----------------------+-------------------------------------+
    | source                         | 21 bytes             | $public_key_hash                    |
    +--------------------------------+----------------------+-------------------------------------+
    | fee                            | Determined from data | $N.t                                |
    +--------------------------------+----------------------+-------------------------------------+
    | counter                        | Determined from data | $N.t                                |
    +--------------------------------+----------------------+-------------------------------------+
    | gas_limit                      | Determined from data | $N.t                                |
    +--------------------------------+----------------------+-------------------------------------+
    | storage_limit                  | Determined from data | $N.t                                |
    +--------------------------------+----------------------+-------------------------------------+
    | ? presence of field "delegate" | 1 byte               | boolean (0 for false, 255 for true) |
    +--------------------------------+----------------------+-------------------------------------+
    | delegate                       | 21 bytes             | $public_key_hash                    |
    +--------------------------------+----------------------+-------------------------------------+
    
    
    Register_global_constant (tag 111)
    ==================================
    
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | Tag                   | 1 byte               | unsigned 8-bit integer             |
    +-----------------------+----------------------+------------------------------------+
    | source                | 21 bytes             | $public_key_hash                   |
    +-----------------------+----------------------+------------------------------------+
    | fee                   | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | counter               | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | gas_limit             | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | storage_limit         | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | value                 | Variable             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    
    
    Set_deposits_limit (tag 112)
    ============================
    
    +-----------------------------+----------------------+-------------------------------------+
    | Name                        | Size                 | Contents                            |
    +=============================+======================+=====================================+
    | Tag                         | 1 byte               | unsigned 8-bit integer              |
    +-----------------------------+----------------------+-------------------------------------+
    | source                      | 21 bytes             | $public_key_hash                    |
    +-----------------------------+----------------------+-------------------------------------+
    | fee                         | Determined from data | $N.t                                |
    +-----------------------------+----------------------+-------------------------------------+
    | counter                     | Determined from data | $N.t                                |
    +-----------------------------+----------------------+-------------------------------------+
    | gas_limit                   | Determined from data | $N.t                                |
    +-----------------------------+----------------------+-------------------------------------+
    | storage_limit               | Determined from data | $N.t                                |
    +-----------------------------+----------------------+-------------------------------------+
    | ? presence of field "limit" | 1 byte               | boolean (0 for false, 255 for true) |
    +-----------------------------+----------------------+-------------------------------------+
    | limit                       | Determined from data | $N.t                                |
    +-----------------------------+----------------------+-------------------------------------+
    
    
    Increase_paid_storage (tag 113)
    ===============================
    
    +---------------+----------------------+------------------------------+
    | Name          | Size                 | Contents                     |
    +===============+======================+==============================+
    | Tag           | 1 byte               | unsigned 8-bit integer       |
    +---------------+----------------------+------------------------------+
    | source        | 21 bytes             | $public_key_hash             |
    +---------------+----------------------+------------------------------+
    | fee           | Determined from data | $N.t                         |
    +---------------+----------------------+------------------------------+
    | counter       | Determined from data | $N.t                         |
    +---------------+----------------------+------------------------------+
    | gas_limit     | Determined from data | $N.t                         |
    +---------------+----------------------+------------------------------+
    | storage_limit | Determined from data | $N.t                         |
    +---------------+----------------------+------------------------------+
    | amount        | Determined from data | $Z.t                         |
    +---------------+----------------------+------------------------------+
    | destination   | 22 bytes             | $beta.contract_id.originated |
    +---------------+----------------------+------------------------------+
    
    
    Update_consensus_key (tag 114)
    ==============================
    
    +---------------+----------------------+------------------------+
    | Name          | Size                 | Contents               |
    +===============+======================+========================+
    | Tag           | 1 byte               | unsigned 8-bit integer |
    +---------------+----------------------+------------------------+
    | source        | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    | fee           | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | counter       | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | gas_limit     | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | storage_limit | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | pk            | Determined from data | $public_key            |
    +---------------+----------------------+------------------------+
    
    
    Transfer_ticket (tag 158)
    =========================
    
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | Tag                   | 1 byte               | unsigned 8-bit integer             |
    +-----------------------+----------------------+------------------------------------+
    | source                | 21 bytes             | $public_key_hash                   |
    +-----------------------+----------------------+------------------------------------+
    | fee                   | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | counter               | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | gas_limit             | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | storage_limit         | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | ticket_contents       | Variable             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | ticket_ty             | Variable             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    | ticket_ticketer       | 22 bytes             | $beta.contract_id                  |
    +-----------------------+----------------------+------------------------------------+
    | ticket_amount         | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | destination           | 22 bytes             | $beta.contract_id                  |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | entrypoint            | Variable             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    
    
    Smart_rollup_originate (tag 200)
    ================================
    
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | Name                            | Size                 | Contents                                                  |
    +=================================+======================+===========================================================+
    | Tag                             | 1 byte               | unsigned 8-bit integer                                    |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | source                          | 21 bytes             | $public_key_hash                                          |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | fee                             | Determined from data | $N.t                                                      |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | counter                         | Determined from data | $N.t                                                      |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | gas_limit                       | Determined from data | $N.t                                                      |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | storage_limit                   | Determined from data | $N.t                                                      |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | pvm_kind                        | 1 byte               | unsigned 8-bit integer encoding an enumeration (see X_29) |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | # bytes in next field           | 4 bytes              | unsigned 30-bit big-endian integer                        |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | kernel                          | Variable             | bytes                                                     |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | # bytes in next field           | 4 bytes              | unsigned 30-bit big-endian integer                        |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | parameters_ty                   | Variable             | bytes                                                     |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | ? presence of field "whitelist" | 1 byte               | boolean (0 for false, 255 for true)                       |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    | whitelist                       | Determined from data | $X_30                                                     |
    +---------------------------------+----------------------+-----------------------------------------------------------+
    
    
    Smart_rollup_add_messages (tag 201)
    ===================================
    
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | Tag                   | 1 byte               | unsigned 8-bit integer             |
    +-----------------------+----------------------+------------------------------------+
    | source                | 21 bytes             | $public_key_hash                   |
    +-----------------------+----------------------+------------------------------------+
    | fee                   | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | counter               | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | gas_limit             | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | storage_limit         | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | message               | Variable             | sequence of $X_4                   |
    +-----------------------+----------------------+------------------------------------+
    
    
    Smart_rollup_cement (tag 202)
    =============================
    
    +---------------+----------------------+------------------------+
    | Name          | Size                 | Contents               |
    +===============+======================+========================+
    | Tag           | 1 byte               | unsigned 8-bit integer |
    +---------------+----------------------+------------------------+
    | source        | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    | fee           | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | counter       | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | gas_limit     | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | storage_limit | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | rollup        | 20 bytes             | bytes                  |
    +---------------+----------------------+------------------------+
    
    
    Smart_rollup_publish (tag 203)
    ==============================
    
    +---------------+----------------------+------------------------+
    | Name          | Size                 | Contents               |
    +===============+======================+========================+
    | Tag           | 1 byte               | unsigned 8-bit integer |
    +---------------+----------------------+------------------------+
    | source        | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    | fee           | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | counter       | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | gas_limit     | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | storage_limit | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | rollup        | 20 bytes             | bytes                  |
    +---------------+----------------------+------------------------+
    | commitment    | 76 bytes             | $X_27                  |
    +---------------+----------------------+------------------------+
    
    
    Smart_rollup_refute (tag 204)
    =============================
    
    +---------------+----------------------+------------------------+
    | Name          | Size                 | Contents               |
    +===============+======================+========================+
    | Tag           | 1 byte               | unsigned 8-bit integer |
    +---------------+----------------------+------------------------+
    | source        | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    | fee           | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | counter       | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | gas_limit     | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | storage_limit | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | rollup        | 20 bytes             | bytes                  |
    +---------------+----------------------+------------------------+
    | opponent      | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    | refutation    | Determined from data | $X_26                  |
    +---------------+----------------------+------------------------+
    
    
    Smart_rollup_timeout (tag 205)
    ==============================
    
    +---------------+----------------------+------------------------+
    | Name          | Size                 | Contents               |
    +===============+======================+========================+
    | Tag           | 1 byte               | unsigned 8-bit integer |
    +---------------+----------------------+------------------------+
    | source        | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    | fee           | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | counter       | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | gas_limit     | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | storage_limit | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | rollup        | 20 bytes             | bytes                  |
    +---------------+----------------------+------------------------+
    | stakers       | 42 bytes             | $X_20                  |
    +---------------+----------------------+------------------------+
    
    
    Smart_rollup_execute_outbox_message (tag 206)
    =============================================
    
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | Tag                   | 1 byte               | unsigned 8-bit integer             |
    +-----------------------+----------------------+------------------------------------+
    | source                | 21 bytes             | $public_key_hash                   |
    +-----------------------+----------------------+------------------------------------+
    | fee                   | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | counter               | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | gas_limit             | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | storage_limit         | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | rollup                | 20 bytes             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    | cemented_commitment   | 32 bytes             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | output_proof          | Variable             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    
    
    Smart_rollup_recover_bond (tag 207)
    ===================================
    
    +---------------+----------------------+------------------------+
    | Name          | Size                 | Contents               |
    +===============+======================+========================+
    | Tag           | 1 byte               | unsigned 8-bit integer |
    +---------------+----------------------+------------------------+
    | source        | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    | fee           | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | counter       | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | gas_limit     | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | storage_limit | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | rollup        | 20 bytes             | bytes                  |
    +---------------+----------------------+------------------------+
    | staker        | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    
    
    Dal_publish_commitment (tag 230)
    ================================
    
    +---------------+----------------------+------------------------+
    | Name          | Size                 | Contents               |
    +===============+======================+========================+
    | Tag           | 1 byte               | unsigned 8-bit integer |
    +---------------+----------------------+------------------------+
    | source        | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    | fee           | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | counter       | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | gas_limit     | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | storage_limit | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | slot_header   | 145 bytes            | $X_19                  |
    +---------------+----------------------+------------------------+
    
    
    Zk_rollup_origination (tag 250)
    ===============================
    
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | Name                  | Size                 | Contents                                                                |
    +=======================+======================+=========================================================================+
    | Tag                   | 1 byte               | unsigned 8-bit integer                                                  |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | source                | 21 bytes             | $public_key_hash                                                        |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | fee                   | Determined from data | $N.t                                                                    |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | counter               | Determined from data | $N.t                                                                    |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | gas_limit             | Determined from data | $N.t                                                                    |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | storage_limit         | Determined from data | $N.t                                                                    |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer                                      |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | public_parameters     | Variable             | bytes                                                                   |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer                                      |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | circuits_info         | Variable             | sequence of $X_16                                                       |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer                                      |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | init_state            | Variable             | sequence of bytes                                                       |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    | nb_ops                | 4 bytes              | signed 31-bit big-endian integer in the range -1073741824 to 1073741823 |
    +-----------------------+----------------------+-------------------------------------------------------------------------+
    
    
    Zk_rollup_publish (tag 251)
    ===========================
    
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | Tag                   | 1 byte               | unsigned 8-bit integer             |
    +-----------------------+----------------------+------------------------------------+
    | source                | 21 bytes             | $public_key_hash                   |
    +-----------------------+----------------------+------------------------------------+
    | fee                   | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | counter               | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | gas_limit             | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | storage_limit         | Determined from data | $N.t                               |
    +-----------------------+----------------------+------------------------------------+
    | zk_rollup             | 20 bytes             | bytes                              |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | op                    | Variable             | sequence of $X_10                  |
    +-----------------------+----------------------+------------------------------------+
    
    
    Zk_rollup_update (tag 252)
    ==========================
    
    +---------------+----------------------+------------------------+
    | Name          | Size                 | Contents               |
    +===============+======================+========================+
    | Tag           | 1 byte               | unsigned 8-bit integer |
    +---------------+----------------------+------------------------+
    | source        | 21 bytes             | $public_key_hash       |
    +---------------+----------------------+------------------------+
    | fee           | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | counter       | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | gas_limit     | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | storage_limit | Determined from data | $N.t                   |
    +---------------+----------------------+------------------------+
    | zk_rollup     | 20 bytes             | bytes                  |
    +---------------+----------------------+------------------------+
    | update        | Determined from data | $X_2                   |
    +---------------+----------------------+------------------------+
    
    
    Signature_prefix (tag 255)
    ==========================
    
    +------------------+----------+------------------------+
    | Name             | Size     | Contents               |
    +==================+==========+========================+
    | Tag              | 1 byte   | unsigned 8-bit integer |
    +------------------+----------+------------------------+
    | signature_prefix | 33 bytes | $bls_signature_prefix  |
    +------------------+----------+------------------------+
    
    
    next_operation
    **************
    
    +-------------------------------+----------+----------------------------------------------------------------+
    | Name                          | Size     | Contents                                                       |
    +===============================+==========+================================================================+
    | # bytes in next field         | 4 bytes  | unsigned 30-bit big-endian integer                             |
    +-------------------------------+----------+----------------------------------------------------------------+
    | branch                        | 32 bytes | bytes                                                          |
    +-------------------------------+----------+----------------------------------------------------------------+
    | # bytes in next 2 fields      | 4 bytes  | unsigned 30-bit big-endian integer                             |
    +-------------------------------+----------+----------------------------------------------------------------+
    | contents_and_signature_prefix | Variable | sequence of $beta.operation.alpha.contents_or_signature_prefix |
    +-------------------------------+----------+----------------------------------------------------------------+
    | signature_suffix              | 64 bytes | bytes                                                          |
    +-------------------------------+----------+----------------------------------------------------------------+
    
    
    X_1
    ***
    
    +--------------------------+----------+------------------------------------+
    | Name                     | Size     | Contents                           |
    +==========================+==========+====================================+
    | # bytes in next 2 fields | 4 bytes  | unsigned 30-bit big-endian integer |
    +--------------------------+----------+------------------------------------+
    | # bytes in next field    | 4 bytes  | unsigned 30-bit big-endian integer |
    +--------------------------+----------+------------------------------------+
    | Unnamed field 0          | Variable | sequence of $next_operation        |
    +--------------------------+----------+------------------------------------+
    
    </pre>
    </div>
  <div id="POST_..--block_id--helpers--preapply--blockoutput.json" class="POST_..--block_id--helpers--preapply--block tabcontent">
    <pre>
    { "shell_header": $block_header.shell,
      "operations":
        [ { "applied":
              [ { /* An operation. The shell_header part indicates a block an
                     operation is meant to apply on top of. The proto part is
                     protocol-specific and appears as a binary blob. */
                  "hash": $Operation_hash,
                  "branch": $block_hash,
                  "data": /^([a-zA-Z0-9][a-zA-Z0-9])*$/ } ... ],
            "refused":
              [ { /* An operation. The shell_header part indicates a block an
                     operation is meant to apply on top of. The proto part is
                     protocol-specific and appears as a binary blob. */
                  "hash": $Operation_hash,
                  "branch": $block_hash,
                  "data": /^([a-zA-Z0-9][a-zA-Z0-9])*$/,
                  "error": $error } ... ],
            "outdated":
              [ { /* An operation. The shell_header part indicates a block an
                     operation is meant to apply on top of. The proto part is
                     protocol-specific and appears as a binary blob. */
                  "hash": $Operation_hash,
                  "branch": $block_hash,
                  "data": /^([a-zA-Z0-9][a-zA-Z0-9])*$/,
                  "error": $error } ... ],
            "branch_refused":
              [ { /* An operation. The shell_header part indicates a block an
                     operation is meant to apply on top of. The proto part is
                     protocol-specific and appears as a binary blob. */
                  "hash": $Operation_hash,
                  "branch": $block_hash,
                  "data": /^([a-zA-Z0-9][a-zA-Z0-9])*$/,
                  "error": $error } ... ],
            "branch_delayed":
              [ { /* An operation. The shell_header part indicates a block an
                     operation is meant to apply on top of. The proto part is
                     protocol-specific and appears as a binary blob. */
                  "hash": $Operation_hash,
                  "branch": $block_hash,
                  "data": /^([a-zA-Z0-9][a-zA-Z0-9])*$/,
                  "error": $error } ... ] } ... ] }
    $Context_hash:
      /* A hash of context (Base58Check-encoded) */
      $unistring
    $Operation_hash:
      /* A Tezos operation ID (Base58Check-encoded) */
      $unistring
    $Operation_list_list_hash:
      /* A list of list of operations (Base58Check-encoded) */
      $unistring
    $block_hash:
      /* A block identifier (Base58Check-encoded) */
      $unistring
    $block_header.shell:
      /* Shell header
         Block header's shell-related content. It contains information such as
         the block level, its predecessor and timestamp. */
      { "level": integer ∈ [-2^31-1, 2^31],
        "proto": integer ∈ [0, 255],
        "predecessor": $block_hash,
        "timestamp": $timestamp.protocol,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash": $Operation_list_list_hash,
        "fitness": $fitness,
        "context": $Context_hash }
    $error:
      /* The full list of errors is available with the global RPC `GET errors` */
      any
    $fitness:
      /* Block fitness
         The fitness, or score, of a block, that allow the Tezos to decide
         which chain is the best. A fitness value is a list of byte sequences.
         They are compared as follows: shortest lists are smaller; lists of the
         same length are compared according to the lexicographical order. */
      [ /^([a-zA-Z0-9][a-zA-Z0-9])*$/ ... ]
    $timestamp.protocol:
      /* A timestamp as seen by the protocol: second-level precision, epoch
         based. */
      $unistring
    $unistring:
      /* Universal string representation
         Either a plain UTF8 string, or a sequence of bytes for strings that
         contain invalid byte sequences. */
      string || { "invalid_utf8_string": [ integer ∈ [0, 255] ... ] }</pre>
    </div>
  <div id="POST_..--block_id--helpers--preapply--blockoutput.bin" class="POST_..--block_id--helpers--preapply--block tabcontent">
    <pre>
    +-----------------------+----------------------+------------------------------------+
    | Name                  | Size                 | Contents                           |
    +=======================+======================+====================================+
    | shell_header          | Determined from data | $block_header.shell                |
    +-----------------------+----------------------+------------------------------------+
    | # bytes in next field | 4 bytes              | unsigned 30-bit big-endian integer |
    +-----------------------+----------------------+------------------------------------+
    | operations            | Variable             | sequence of $X_0                   |
    +-----------------------+----------------------+------------------------------------+
    
    
    fitness.elem
    ************
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | bytes                              |
    +-----------------------+----------+------------------------------------+
    
    
    block_header.shell
    ******************
    
    +----------------------------+----------+------------------------------------+
    | Name                       | Size     | Contents                           |
    +============================+==========+====================================+
    | level                      | 4 bytes  | signed 32-bit big-endian integer   |
    +----------------------------+----------+------------------------------------+
    | proto                      | 1 byte   | unsigned 8-bit integer             |
    +----------------------------+----------+------------------------------------+
    | predecessor                | 32 bytes | bytes                              |
    +----------------------------+----------+------------------------------------+
    | timestamp                  | 8 bytes  | signed 64-bit big-endian integer   |
    +----------------------------+----------+------------------------------------+
    | validation_pass            | 1 byte   | unsigned 8-bit integer             |
    +----------------------------+----------+------------------------------------+
    | operations_hash            | 32 bytes | bytes                              |
    +----------------------------+----------+------------------------------------+
    | # bytes in field "fitness" | 4 bytes  | unsigned 30-bit big-endian integer |
    +----------------------------+----------+------------------------------------+
    | fitness                    | Variable | sequence of $fitness.elem          |
    +----------------------------+----------+------------------------------------+
    | context                    | 32 bytes | bytes                              |
    +----------------------------+----------+------------------------------------+
    
    
    X_1
    ***
    
    +--------------------------+----------+------------------------------------+
    | Name                     | Size     | Contents                           |
    +==========================+==========+====================================+
    | hash                     | 32 bytes | bytes                              |
    +--------------------------+----------+------------------------------------+
    | # bytes in next 2 fields | 4 bytes  | unsigned 30-bit big-endian integer |
    +--------------------------+----------+------------------------------------+
    | branch                   | 32 bytes | bytes                              |
    +--------------------------+----------+------------------------------------+
    | data                     | Variable | bytes                              |
    +--------------------------+----------+------------------------------------+
    
    
    X_2
    ***
    
    +--------------------------+----------+------------------------------------+
    | Name                     | Size     | Contents                           |
    +==========================+==========+====================================+
    | hash                     | 32 bytes | bytes                              |
    +--------------------------+----------+------------------------------------+
    | # bytes in next 2 fields | 4 bytes  | unsigned 30-bit big-endian integer |
    +--------------------------+----------+------------------------------------+
    | branch                   | 32 bytes | bytes                              |
    +--------------------------+----------+------------------------------------+
    | data                     | Variable | bytes                              |
    +--------------------------+----------+------------------------------------+
    | # bytes in field "error" | 4 bytes  | unsigned 30-bit big-endian integer |
    +--------------------------+----------+------------------------------------+
    | error                    | Variable | bytes                              |
    +--------------------------+----------+------------------------------------+
    
    
    X_0
    ***
    
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | applied               | Variable | sequence of $X_1                   |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | refused               | Variable | sequence of $X_2                   |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | outdated              | Variable | sequence of $X_2                   |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | branch_refused        | Variable | sequence of $X_2                   |
    +-----------------------+----------+------------------------------------+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | branch_delayed        | Variable | sequence of $X_2                   |
    +-----------------------+----------+------------------------------------+
    
    </pre>
    </div>
  


.. _GET_..--block_id--live_blocks :

**GET ../<block_id>/live_blocks**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--live_blocksdescr', 'GET_..--block_id--live_blocks')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--live_blocksoutput.json', 'GET_..--block_id--live_blocks')">Json output</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--live_blocksoutput.bin', 'GET_..--block_id--live_blocks')">Binary output</button>
    </div><div id="GET_..--block_id--live_blocksdescr" class="GET_..--block_id--live_blocks tabcontent">
            <p>
            List the ancestors of the given block which, if referred to as the branch in an operation header, are recent enough for that operation to be included in the current block.</p>
            </div>
  <div id="GET_..--block_id--live_blocksoutput.json" class="GET_..--block_id--live_blocks tabcontent">
    <pre>
    [ $block_hash ... ]
    $block_hash:
      /* A block identifier (Base58Check-encoded) */
      $unistring
    $unistring:
      /* Universal string representation
         Either a plain UTF8 string, or a sequence of bytes for strings that
         contain invalid byte sequences. */
      string || { "invalid_utf8_string": [ integer ∈ [0, 255] ... ] }</pre>
    </div>
  <div id="GET_..--block_id--live_blocksoutput.bin" class="GET_..--block_id--live_blocks tabcontent">
    <pre>
    +-----------------------+----------+------------------------------------+
    | Name                  | Size     | Contents                           |
    +=======================+==========+====================================+
    | # bytes in next field | 4 bytes  | unsigned 30-bit big-endian integer |
    +-----------------------+----------+------------------------------------+
    | Unnamed field 0       | Variable | sequence of bytes                  |
    +-----------------------+----------+------------------------------------+
    
    
    </pre>
    </div>
  



