

;
;        /!\ /!\ Do not modify this file /!\ /!\
;
; but the original template in `tezos-protocol-compiler`
;

; generated from src/lib_protocol_compiler/dune_protocol.template.v1

; This template is intended for protocols written after the Tezos
; project start using OCaml 4.12.
;
; template.v1 was introduced when we bumped the version of our OCaml
; dependency to 4.12.  This change to a newer OCaml version introduced
; new warnings that need to be ignored in the protocols written before
; the update (see dune_protocol.template.v0).

(rule
 (targets environment.ml)
 (action
  (write-file %{targets}
              "module Name = struct let name = \"alpha\" end
include Tezos_protocol_environment.MakeV4(Name)()
module CamlinternalFormatBasics = struct include CamlinternalFormatBasics end
")))

(rule
 (targets registerer.ml)
 (deps
   misc.mli misc.ml
   non_empty_string.mli non_empty_string.ml
   path_encoding.mli path_encoding.ml
   storage_description.mli storage_description.ml
   state_hash.mli state_hash.ml
   nonce_hash.mli nonce_hash.ml
   script_expr_hash.mli script_expr_hash.ml
   contract_hash.mli contract_hash.ml
   blinded_public_key_hash.mli blinded_public_key_hash.ml
   block_payload_hash.mli block_payload_hash.ml
   origination_nonce.mli origination_nonce.ml
   slot_repr.mli slot_repr.ml
   tez_repr.mli tez_repr.ml
   period_repr.mli period_repr.ml
   time_repr.mli time_repr.ml
   round_repr.mli round_repr.ml
   block_payload_repr.mli block_payload_repr.ml
   fixed_point_repr.mli fixed_point_repr.ml
   saturation_repr.mli saturation_repr.ml
   gas_limit_repr.mli gas_limit_repr.ml
   constants_repr.mli constants_repr.ml
   raw_level_repr.mli raw_level_repr.ml
   fitness_repr.mli fitness_repr.ml
   cycle_repr.mli cycle_repr.ml
   level_repr.mli level_repr.ml
   seed_repr.mli seed_repr.ml
   sampler.mli sampler.ml
   voting_period_repr.mli voting_period_repr.ml
   script_string_repr.mli script_string_repr.ml
   script_int_repr.mli script_int_repr.ml
   script_timestamp_repr.mli script_timestamp_repr.ml
   michelson_v1_primitives.mli michelson_v1_primitives.ml
   script_repr.mli script_repr.ml
   cache_memory_helpers.ml
   contract_repr.mli contract_repr.ml
   tx_rollup_repr.mli tx_rollup_repr.ml
   roll_repr_legacy.mli roll_repr_legacy.ml
   vote_repr.mli vote_repr.ml
   block_header_repr.mli block_header_repr.ml
   operation_repr.mli operation_repr.ml
   manager_repr.mli manager_repr.ml
   commitment_repr.mli commitment_repr.ml
   parameters_repr.mli parameters_repr.ml
   sapling_repr.ml
   lazy_storage_kind.mli lazy_storage_kind.ml
   receipt_repr.mli receipt_repr.ml
   migration_repr.mli migration_repr.ml
   raw_context_intf.ml
   raw_context.mli raw_context.ml
   storage_costs.mli storage_costs.ml
   storage_sigs.ml
   storage_functors.mli storage_functors.ml
   storage.mli storage.ml
   cache_repr.mli cache_repr.ml
   constants_storage.mli constants_storage.ml
   level_storage.mli level_storage.ml
   nonce_storage.mli nonce_storage.ml
   seed_storage.mli seed_storage.ml
   roll_storage_legacy.mli roll_storage_legacy.ml
   contract_manager_storage.mli contract_manager_storage.ml
   delegate_activation_storage.mli delegate_activation_storage.ml
   frozen_deposits_storage.mli frozen_deposits_storage.ml
   stake_storage.mli stake_storage.ml
   contract_delegate_storage.mli contract_delegate_storage.ml
   sapling_storage.ml
   lazy_storage_diff.mli lazy_storage_diff.ml
   contract_storage.mli contract_storage.ml
   commitment_storage.mli commitment_storage.ml
   token.mli token.ml
   delegate_storage.mli delegate_storage.ml
   bootstrap_storage.mli bootstrap_storage.ml
   voting_period_storage.mli voting_period_storage.ml
   vote_storage.mli vote_storage.ml
   fees_storage.mli fees_storage.ml
   ticket_storage.mli ticket_storage.ml
   liquidity_baking_repr.mli liquidity_baking_repr.ml
   liquidity_baking_cpmm.ml
   liquidity_baking_lqt.ml
   liquidity_baking_migration.mli liquidity_baking_migration.ml
   init_storage.mli init_storage.ml
   sapling_validator.ml
   global_constants_costs.mli global_constants_costs.ml
   global_constants_storage.mli global_constants_storage.ml
   tx_rollup_storage.mli tx_rollup_storage.ml
   alpha_context.mli alpha_context.ml
   carbonated_map_costs.mli carbonated_map_costs.ml
   carbonated_map.mli carbonated_map.ml
   local_gas_counter.ml
   gas_monad.mli gas_monad.ml
   script_tc_errors.ml
   script_ir_annot.mli script_ir_annot.ml
   script_typed_ir.mli script_typed_ir.ml
   script_typed_ir_size.mli script_typed_ir_size.ml
   script_typed_ir_size_costs.mli script_typed_ir_size_costs.ml
   michelson_v1_gas.mli michelson_v1_gas.ml
   script_list.mli script_list.ml
   script_comparable.mli script_comparable.ml
   script_set.mli script_set.ml
   script_map.mli script_map.ml
   script_tc_context.mli script_tc_context.ml
   script_ir_translator.mli script_ir_translator.ml
   script_cache.mli script_cache.ml
   script_tc_errors_registration.mli script_tc_errors_registration.ml
   ticket_costs.mli ticket_costs.ml
   ticket_scanner.mli ticket_scanner.ml
   ticket_balance_key.mli ticket_balance_key.ml
   script_interpreter_defs.ml
   script_interpreter.mli script_interpreter.ml
   baking.mli baking.ml
   amendment.mli amendment.ml
   apply_results.mli apply_results.ml
   apply.mli apply.ml
   services_registration.mli services_registration.ml
   constants_services.mli constants_services.ml
   sapling_services.ml
   contract_services.mli contract_services.ml
   delegate_services.mli delegate_services.ml
   voting_services.mli voting_services.ml
   tx_rollup_services.mli tx_rollup_services.ml
   alpha_services.mli alpha_services.ml
   main.mli main.ml
   (:src_dir TEZOS_PROTOCOL))
 (action
  (with-stdout-to %{targets}
                  (chdir %{workspace_root} (run %{bin:tezos-embedded-protocol-packer} "%{src_dir}" "alpha")))))

(rule
 (targets functor.ml)
 (deps
   misc.mli misc.ml
   non_empty_string.mli non_empty_string.ml
   path_encoding.mli path_encoding.ml
   storage_description.mli storage_description.ml
   state_hash.mli state_hash.ml
   nonce_hash.mli nonce_hash.ml
   script_expr_hash.mli script_expr_hash.ml
   contract_hash.mli contract_hash.ml
   blinded_public_key_hash.mli blinded_public_key_hash.ml
   block_payload_hash.mli block_payload_hash.ml
   origination_nonce.mli origination_nonce.ml
   slot_repr.mli slot_repr.ml
   tez_repr.mli tez_repr.ml
   period_repr.mli period_repr.ml
   time_repr.mli time_repr.ml
   round_repr.mli round_repr.ml
   block_payload_repr.mli block_payload_repr.ml
   fixed_point_repr.mli fixed_point_repr.ml
   saturation_repr.mli saturation_repr.ml
   gas_limit_repr.mli gas_limit_repr.ml
   constants_repr.mli constants_repr.ml
   raw_level_repr.mli raw_level_repr.ml
   fitness_repr.mli fitness_repr.ml
   cycle_repr.mli cycle_repr.ml
   level_repr.mli level_repr.ml
   seed_repr.mli seed_repr.ml
   sampler.mli sampler.ml
   voting_period_repr.mli voting_period_repr.ml
   script_string_repr.mli script_string_repr.ml
   script_int_repr.mli script_int_repr.ml
   script_timestamp_repr.mli script_timestamp_repr.ml
   michelson_v1_primitives.mli michelson_v1_primitives.ml
   script_repr.mli script_repr.ml
   cache_memory_helpers.ml
   contract_repr.mli contract_repr.ml
   tx_rollup_repr.mli tx_rollup_repr.ml
   roll_repr_legacy.mli roll_repr_legacy.ml
   vote_repr.mli vote_repr.ml
   block_header_repr.mli block_header_repr.ml
   operation_repr.mli operation_repr.ml
   manager_repr.mli manager_repr.ml
   commitment_repr.mli commitment_repr.ml
   parameters_repr.mli parameters_repr.ml
   sapling_repr.ml
   lazy_storage_kind.mli lazy_storage_kind.ml
   receipt_repr.mli receipt_repr.ml
   migration_repr.mli migration_repr.ml
   raw_context_intf.ml
   raw_context.mli raw_context.ml
   storage_costs.mli storage_costs.ml
   storage_sigs.ml
   storage_functors.mli storage_functors.ml
   storage.mli storage.ml
   cache_repr.mli cache_repr.ml
   constants_storage.mli constants_storage.ml
   level_storage.mli level_storage.ml
   nonce_storage.mli nonce_storage.ml
   seed_storage.mli seed_storage.ml
   roll_storage_legacy.mli roll_storage_legacy.ml
   contract_manager_storage.mli contract_manager_storage.ml
   delegate_activation_storage.mli delegate_activation_storage.ml
   frozen_deposits_storage.mli frozen_deposits_storage.ml
   stake_storage.mli stake_storage.ml
   contract_delegate_storage.mli contract_delegate_storage.ml
   sapling_storage.ml
   lazy_storage_diff.mli lazy_storage_diff.ml
   contract_storage.mli contract_storage.ml
   commitment_storage.mli commitment_storage.ml
   token.mli token.ml
   delegate_storage.mli delegate_storage.ml
   bootstrap_storage.mli bootstrap_storage.ml
   voting_period_storage.mli voting_period_storage.ml
   vote_storage.mli vote_storage.ml
   fees_storage.mli fees_storage.ml
   ticket_storage.mli ticket_storage.ml
   liquidity_baking_repr.mli liquidity_baking_repr.ml
   liquidity_baking_cpmm.ml
   liquidity_baking_lqt.ml
   liquidity_baking_migration.mli liquidity_baking_migration.ml
   init_storage.mli init_storage.ml
   sapling_validator.ml
   global_constants_costs.mli global_constants_costs.ml
   global_constants_storage.mli global_constants_storage.ml
   tx_rollup_storage.mli tx_rollup_storage.ml
   alpha_context.mli alpha_context.ml
   carbonated_map_costs.mli carbonated_map_costs.ml
   carbonated_map.mli carbonated_map.ml
   local_gas_counter.ml
   gas_monad.mli gas_monad.ml
   script_tc_errors.ml
   script_ir_annot.mli script_ir_annot.ml
   script_typed_ir.mli script_typed_ir.ml
   script_typed_ir_size.mli script_typed_ir_size.ml
   script_typed_ir_size_costs.mli script_typed_ir_size_costs.ml
   michelson_v1_gas.mli michelson_v1_gas.ml
   script_list.mli script_list.ml
   script_comparable.mli script_comparable.ml
   script_set.mli script_set.ml
   script_map.mli script_map.ml
   script_tc_context.mli script_tc_context.ml
   script_ir_translator.mli script_ir_translator.ml
   script_cache.mli script_cache.ml
   script_tc_errors_registration.mli script_tc_errors_registration.ml
   ticket_costs.mli ticket_costs.ml
   ticket_scanner.mli ticket_scanner.ml
   ticket_balance_key.mli ticket_balance_key.ml
   script_interpreter_defs.ml
   script_interpreter.mli script_interpreter.ml
   baking.mli baking.ml
   amendment.mli amendment.ml
   apply_results.mli apply_results.ml
   apply.mli apply.ml
   services_registration.mli services_registration.ml
   constants_services.mli constants_services.ml
   sapling_services.ml
   contract_services.mli contract_services.ml
   delegate_services.mli delegate_services.ml
   voting_services.mli voting_services.ml
   tx_rollup_services.mli tx_rollup_services.ml
   alpha_services.mli alpha_services.ml
   main.mli main.ml
   (:src_dir TEZOS_PROTOCOL))
 (action (with-stdout-to %{targets}
                         (chdir %{workspace_root}
                                (run %{bin:tezos-protocol-compiler.tezos-protocol-packer} %{src_dir})))))

(rule
 (targets protocol.ml)
 (deps
   misc.mli misc.ml
   non_empty_string.mli non_empty_string.ml
   path_encoding.mli path_encoding.ml
   storage_description.mli storage_description.ml
   state_hash.mli state_hash.ml
   nonce_hash.mli nonce_hash.ml
   script_expr_hash.mli script_expr_hash.ml
   contract_hash.mli contract_hash.ml
   blinded_public_key_hash.mli blinded_public_key_hash.ml
   block_payload_hash.mli block_payload_hash.ml
   origination_nonce.mli origination_nonce.ml
   slot_repr.mli slot_repr.ml
   tez_repr.mli tez_repr.ml
   period_repr.mli period_repr.ml
   time_repr.mli time_repr.ml
   round_repr.mli round_repr.ml
   block_payload_repr.mli block_payload_repr.ml
   fixed_point_repr.mli fixed_point_repr.ml
   saturation_repr.mli saturation_repr.ml
   gas_limit_repr.mli gas_limit_repr.ml
   constants_repr.mli constants_repr.ml
   raw_level_repr.mli raw_level_repr.ml
   fitness_repr.mli fitness_repr.ml
   cycle_repr.mli cycle_repr.ml
   level_repr.mli level_repr.ml
   seed_repr.mli seed_repr.ml
   sampler.mli sampler.ml
   voting_period_repr.mli voting_period_repr.ml
   script_string_repr.mli script_string_repr.ml
   script_int_repr.mli script_int_repr.ml
   script_timestamp_repr.mli script_timestamp_repr.ml
   michelson_v1_primitives.mli michelson_v1_primitives.ml
   script_repr.mli script_repr.ml
   cache_memory_helpers.ml
   contract_repr.mli contract_repr.ml
   tx_rollup_repr.mli tx_rollup_repr.ml
   roll_repr_legacy.mli roll_repr_legacy.ml
   vote_repr.mli vote_repr.ml
   block_header_repr.mli block_header_repr.ml
   operation_repr.mli operation_repr.ml
   manager_repr.mli manager_repr.ml
   commitment_repr.mli commitment_repr.ml
   parameters_repr.mli parameters_repr.ml
   sapling_repr.ml
   lazy_storage_kind.mli lazy_storage_kind.ml
   receipt_repr.mli receipt_repr.ml
   migration_repr.mli migration_repr.ml
   raw_context_intf.ml
   raw_context.mli raw_context.ml
   storage_costs.mli storage_costs.ml
   storage_sigs.ml
   storage_functors.mli storage_functors.ml
   storage.mli storage.ml
   cache_repr.mli cache_repr.ml
   constants_storage.mli constants_storage.ml
   level_storage.mli level_storage.ml
   nonce_storage.mli nonce_storage.ml
   seed_storage.mli seed_storage.ml
   roll_storage_legacy.mli roll_storage_legacy.ml
   contract_manager_storage.mli contract_manager_storage.ml
   delegate_activation_storage.mli delegate_activation_storage.ml
   frozen_deposits_storage.mli frozen_deposits_storage.ml
   stake_storage.mli stake_storage.ml
   contract_delegate_storage.mli contract_delegate_storage.ml
   sapling_storage.ml
   lazy_storage_diff.mli lazy_storage_diff.ml
   contract_storage.mli contract_storage.ml
   commitment_storage.mli commitment_storage.ml
   token.mli token.ml
   delegate_storage.mli delegate_storage.ml
   bootstrap_storage.mli bootstrap_storage.ml
   voting_period_storage.mli voting_period_storage.ml
   vote_storage.mli vote_storage.ml
   fees_storage.mli fees_storage.ml
   ticket_storage.mli ticket_storage.ml
   liquidity_baking_repr.mli liquidity_baking_repr.ml
   liquidity_baking_cpmm.ml
   liquidity_baking_lqt.ml
   liquidity_baking_migration.mli liquidity_baking_migration.ml
   init_storage.mli init_storage.ml
   sapling_validator.ml
   global_constants_costs.mli global_constants_costs.ml
   global_constants_storage.mli global_constants_storage.ml
   tx_rollup_storage.mli tx_rollup_storage.ml
   alpha_context.mli alpha_context.ml
   carbonated_map_costs.mli carbonated_map_costs.ml
   carbonated_map.mli carbonated_map.ml
   local_gas_counter.ml
   gas_monad.mli gas_monad.ml
   script_tc_errors.ml
   script_ir_annot.mli script_ir_annot.ml
   script_typed_ir.mli script_typed_ir.ml
   script_typed_ir_size.mli script_typed_ir_size.ml
   script_typed_ir_size_costs.mli script_typed_ir_size_costs.ml
   michelson_v1_gas.mli michelson_v1_gas.ml
   script_list.mli script_list.ml
   script_comparable.mli script_comparable.ml
   script_set.mli script_set.ml
   script_map.mli script_map.ml
   script_tc_context.mli script_tc_context.ml
   script_ir_translator.mli script_ir_translator.ml
   script_cache.mli script_cache.ml
   script_tc_errors_registration.mli script_tc_errors_registration.ml
   ticket_costs.mli ticket_costs.ml
   ticket_scanner.mli ticket_scanner.ml
   ticket_balance_key.mli ticket_balance_key.ml
   script_interpreter_defs.ml
   script_interpreter.mli script_interpreter.ml
   baking.mli baking.ml
   amendment.mli amendment.ml
   apply_results.mli apply_results.ml
   apply.mli apply.ml
   services_registration.mli services_registration.ml
   constants_services.mli constants_services.ml
   sapling_services.ml
   contract_services.mli contract_services.ml
   delegate_services.mli delegate_services.ml
   voting_services.mli voting_services.ml
   tx_rollup_services.mli tx_rollup_services.ml
   alpha_services.mli alpha_services.ml
   main.mli main.ml)
 (action
  (write-file %{targets}
    "module Environment = Tezos_protocol_environment_alpha.Environment
let hash = Tezos_crypto.Protocol_hash.of_b58check_exn \"ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK\"
let name = Environment.Name.name
include Tezos_raw_protocol_alpha
include Tezos_raw_protocol_alpha.Main
")))

(library
 (name tezos_protocol_environment_alpha)
 (instrumentation (backend bisect_ppx))
 (public_name tezos-protocol-alpha.environment)
 (library_flags (:standard -linkall))
 (libraries tezos-protocol-environment)
 (modules Environment))

(library
 (name tezos_raw_protocol_alpha)
 (instrumentation (backend bisect_ppx))
 (public_name tezos-protocol-alpha.raw)
 (libraries tezos_protocol_environment_alpha)
 (library_flags (:standard -linkall))
 (flags (:standard -nopervasives -nostdlib
                   -w +a-4-40..42-44-45-48
                   -warn-error +a
                   -open Tezos_protocol_environment_alpha__Environment
                   -open Pervasives
                   -open Error_monad))
 (modules
   Misc
   Non_empty_string
   Path_encoding
   Storage_description
   State_hash
   Nonce_hash
   Script_expr_hash
   Contract_hash
   Blinded_public_key_hash
   Block_payload_hash
   Origination_nonce
   Slot_repr
   Tez_repr
   Period_repr
   Time_repr
   Round_repr
   Block_payload_repr
   Fixed_point_repr
   Saturation_repr
   Gas_limit_repr
   Constants_repr
   Raw_level_repr
   Fitness_repr
   Cycle_repr
   Level_repr
   Seed_repr
   Sampler
   Voting_period_repr
   Script_string_repr
   Script_int_repr
   Script_timestamp_repr
   Michelson_v1_primitives
   Script_repr
   Cache_memory_helpers
   Contract_repr
   Tx_rollup_repr
   Roll_repr_legacy
   Vote_repr
   Block_header_repr
   Operation_repr
   Manager_repr
   Commitment_repr
   Parameters_repr
   Sapling_repr
   Lazy_storage_kind
   Receipt_repr
   Migration_repr
   Raw_context_intf
   Raw_context
   Storage_costs
   Storage_sigs
   Storage_functors
   Storage
   Cache_repr
   Constants_storage
   Level_storage
   Nonce_storage
   Seed_storage
   Roll_storage_legacy
   Contract_manager_storage
   Delegate_activation_storage
   Frozen_deposits_storage
   Stake_storage
   Contract_delegate_storage
   Sapling_storage
   Lazy_storage_diff
   Contract_storage
   Commitment_storage
   Token
   Delegate_storage
   Bootstrap_storage
   Voting_period_storage
   Vote_storage
   Fees_storage
   Ticket_storage
   Liquidity_baking_repr
   Liquidity_baking_cpmm
   Liquidity_baking_lqt
   Liquidity_baking_migration
   Init_storage
   Sapling_validator
   Global_constants_costs
   Global_constants_storage
   Tx_rollup_storage
   Alpha_context
   Carbonated_map_costs
   Carbonated_map
   Local_gas_counter
   Gas_monad
   Script_tc_errors
   Script_ir_annot
   Script_typed_ir
   Script_typed_ir_size
   Script_typed_ir_size_costs
   Michelson_v1_gas
   Script_list
   Script_comparable
   Script_set
   Script_map
   Script_tc_context
   Script_ir_translator
   Script_cache
   Script_tc_errors_registration
   Ticket_costs
   Ticket_scanner
   Ticket_balance_key
   Script_interpreter_defs
   Script_interpreter
   Baking
   Amendment
   Apply_results
   Apply
   Services_registration
   Constants_services
   Sapling_services
   Contract_services
   Delegate_services
   Voting_services
   Tx_rollup_services
   Alpha_services
   Main))

(install
 (section lib)
 (package tezos-protocol-alpha)
 (files (TEZOS_PROTOCOL as raw/TEZOS_PROTOCOL)))

(library
 (name tezos_protocol_alpha)
 (instrumentation (backend bisect_ppx))
 (public_name tezos-protocol-alpha)
 (libraries
      tezos-protocol-environment
      tezos-protocol-environment-sigs
      tezos_raw_protocol_alpha)
 (flags -w "+a-4-40..42-44-45-48"
        -warn-error "+a"
        -nopervasives)
 (modules Protocol))

(library
 (name tezos_protocol_alpha_functor)
 ; The instrumentation is removed as it can lead to a stack overflow
 ; https://gitlab.com/tezos/tezos/-/issues/1927
 ; (instrumentation (backend bisect_ppx))
 (public_name tezos-protocol-functor-alpha)
 (libraries
      tezos-protocol-environment
      tezos-protocol-environment-sigs
      tezos-protocol-alpha.raw)
 (flags -w "+a-4-40..42-44-45-48"
        -warn-error "+a"
        -nopervasives)
 (modules Functor))

(library
 (name tezos_embedded_protocol_alpha)
 (instrumentation (backend bisect_ppx))
 (public_name tezos-embedded-protocol-alpha)
 (library_flags (:standard -linkall))
 (libraries tezos-protocol-alpha
            tezos-protocol-updater
            tezos-protocol-environment)
 (flags (:standard -w +a-4-40..42-44-45-48
                   -warn-error +a))
 (modules Registerer))

(rule
 (alias runtest_compile_protocol)
 (deps
   misc.mli misc.ml
   non_empty_string.mli non_empty_string.ml
   path_encoding.mli path_encoding.ml
   storage_description.mli storage_description.ml
   state_hash.mli state_hash.ml
   nonce_hash.mli nonce_hash.ml
   script_expr_hash.mli script_expr_hash.ml
   contract_hash.mli contract_hash.ml
   blinded_public_key_hash.mli blinded_public_key_hash.ml
   block_payload_hash.mli block_payload_hash.ml
   origination_nonce.mli origination_nonce.ml
   slot_repr.mli slot_repr.ml
   tez_repr.mli tez_repr.ml
   period_repr.mli period_repr.ml
   time_repr.mli time_repr.ml
   round_repr.mli round_repr.ml
   block_payload_repr.mli block_payload_repr.ml
   fixed_point_repr.mli fixed_point_repr.ml
   saturation_repr.mli saturation_repr.ml
   gas_limit_repr.mli gas_limit_repr.ml
   constants_repr.mli constants_repr.ml
   raw_level_repr.mli raw_level_repr.ml
   fitness_repr.mli fitness_repr.ml
   cycle_repr.mli cycle_repr.ml
   level_repr.mli level_repr.ml
   seed_repr.mli seed_repr.ml
   sampler.mli sampler.ml
   voting_period_repr.mli voting_period_repr.ml
   script_string_repr.mli script_string_repr.ml
   script_int_repr.mli script_int_repr.ml
   script_timestamp_repr.mli script_timestamp_repr.ml
   michelson_v1_primitives.mli michelson_v1_primitives.ml
   script_repr.mli script_repr.ml
   cache_memory_helpers.ml
   contract_repr.mli contract_repr.ml
   tx_rollup_repr.mli tx_rollup_repr.ml
   roll_repr_legacy.mli roll_repr_legacy.ml
   vote_repr.mli vote_repr.ml
   block_header_repr.mli block_header_repr.ml
   operation_repr.mli operation_repr.ml
   manager_repr.mli manager_repr.ml
   commitment_repr.mli commitment_repr.ml
   parameters_repr.mli parameters_repr.ml
   sapling_repr.ml
   lazy_storage_kind.mli lazy_storage_kind.ml
   receipt_repr.mli receipt_repr.ml
   migration_repr.mli migration_repr.ml
   raw_context_intf.ml
   raw_context.mli raw_context.ml
   storage_costs.mli storage_costs.ml
   storage_sigs.ml
   storage_functors.mli storage_functors.ml
   storage.mli storage.ml
   cache_repr.mli cache_repr.ml
   constants_storage.mli constants_storage.ml
   level_storage.mli level_storage.ml
   nonce_storage.mli nonce_storage.ml
   seed_storage.mli seed_storage.ml
   roll_storage_legacy.mli roll_storage_legacy.ml
   contract_manager_storage.mli contract_manager_storage.ml
   delegate_activation_storage.mli delegate_activation_storage.ml
   frozen_deposits_storage.mli frozen_deposits_storage.ml
   stake_storage.mli stake_storage.ml
   contract_delegate_storage.mli contract_delegate_storage.ml
   sapling_storage.ml
   lazy_storage_diff.mli lazy_storage_diff.ml
   contract_storage.mli contract_storage.ml
   commitment_storage.mli commitment_storage.ml
   token.mli token.ml
   delegate_storage.mli delegate_storage.ml
   bootstrap_storage.mli bootstrap_storage.ml
   voting_period_storage.mli voting_period_storage.ml
   vote_storage.mli vote_storage.ml
   fees_storage.mli fees_storage.ml
   ticket_storage.mli ticket_storage.ml
   liquidity_baking_repr.mli liquidity_baking_repr.ml
   liquidity_baking_cpmm.ml
   liquidity_baking_lqt.ml
   liquidity_baking_migration.mli liquidity_baking_migration.ml
   init_storage.mli init_storage.ml
   sapling_validator.ml
   global_constants_costs.mli global_constants_costs.ml
   global_constants_storage.mli global_constants_storage.ml
   tx_rollup_storage.mli tx_rollup_storage.ml
   alpha_context.mli alpha_context.ml
   carbonated_map_costs.mli carbonated_map_costs.ml
   carbonated_map.mli carbonated_map.ml
   local_gas_counter.ml
   gas_monad.mli gas_monad.ml
   script_tc_errors.ml
   script_ir_annot.mli script_ir_annot.ml
   script_typed_ir.mli script_typed_ir.ml
   script_typed_ir_size.mli script_typed_ir_size.ml
   script_typed_ir_size_costs.mli script_typed_ir_size_costs.ml
   michelson_v1_gas.mli michelson_v1_gas.ml
   script_list.mli script_list.ml
   script_comparable.mli script_comparable.ml
   script_set.mli script_set.ml
   script_map.mli script_map.ml
   script_tc_context.mli script_tc_context.ml
   script_ir_translator.mli script_ir_translator.ml
   script_cache.mli script_cache.ml
   script_tc_errors_registration.mli script_tc_errors_registration.ml
   ticket_costs.mli ticket_costs.ml
   ticket_scanner.mli ticket_scanner.ml
   ticket_balance_key.mli ticket_balance_key.ml
   script_interpreter_defs.ml
   script_interpreter.mli script_interpreter.ml
   baking.mli baking.ml
   amendment.mli amendment.ml
   apply_results.mli apply_results.ml
   apply.mli apply.ml
   services_registration.mli services_registration.ml
   constants_services.mli constants_services.ml
   sapling_services.ml
   contract_services.mli contract_services.ml
   delegate_services.mli delegate_services.ml
   voting_services.mli voting_services.ml
   tx_rollup_services.mli tx_rollup_services.ml
   alpha_services.mli alpha_services.ml
   main.mli main.ml
  (:src_dir TEZOS_PROTOCOL))
 (action (run %{bin:tezos-protocol-compiler} -no-hash-check .)))

(rule
 (alias runtest_sandbox)
 (deps .tezos_protocol_alpha.objs/native/tezos_protocol_alpha.cmx)
 (action (progn)))

(rule
 (alias runtest)
 (package tezos-protocol-alpha)
 (deps (alias runtest_sandbox))
 (action (progn)))
