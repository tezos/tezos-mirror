

;
;        /!\ /!\ Do not modify this file /!\ /!\
;
; but the original template in `tezos-protocol-compiler`
;


(rule
 (targets environment.ml)
 (action
  (write-file %{targets}
              "module Name = struct let name = \"demo-counter\" end
include Tezos_protocol_environment.MakeV0(Name)()
module CamlinternalFormatBasics = struct include CamlinternalFormatBasics end
")))

(rule
 (targets registerer.ml)
 (deps
   error.mli error.ml
   proto_params.mli proto_params.ml
   header.mli header.ml
   state.mli state.ml
   proto_operation.mli proto_operation.ml
   receipt.mli receipt.ml
   apply.mli apply.ml
   services.mli services.ml
   main.mli main.ml
   (:src_dir TEZOS_PROTOCOL))
 (action
  (with-stdout-to %{targets}
                  (chdir %{workspace_root} (run %{bin:tezos-embedded-protocol-packer} "%{src_dir}" "demo_counter")))))

(rule
 (targets functor.ml)
 (deps
   error.mli error.ml
   proto_params.mli proto_params.ml
   header.mli header.ml
   state.mli state.ml
   proto_operation.mli proto_operation.ml
   receipt.mli receipt.ml
   apply.mli apply.ml
   services.mli services.ml
   main.mli main.ml
   (:src_dir TEZOS_PROTOCOL))
 (action (with-stdout-to %{targets}
                         (chdir %{workspace_root}
                                (run %{bin:tezos-protocol-compiler.tezos-protocol-packer} %{src_dir})))))

(rule
 (targets protocol.ml)
 (deps
   error.mli error.ml
   proto_params.mli proto_params.ml
   header.mli header.ml
   state.mli state.ml
   proto_operation.mli proto_operation.ml
   receipt.mli receipt.ml
   apply.mli apply.ml
   services.mli services.ml
   main.mli main.ml)
 (action
  (write-file %{targets}
    "module Environment = Tezos_protocol_environment_demo_counter.Environment
let hash = Tezos_crypto.Protocol_hash.of_b58check_exn \"ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT\"
let name = Environment.Name.name
include Tezos_raw_protocol_demo_counter
include Tezos_raw_protocol_demo_counter.Main
")))

(library
 (name tezos_protocol_environment_demo_counter)
 (public_name tezos-protocol-demo-counter.environment)
 (library_flags (:standard -linkall))
 (libraries tezos-protocol-environment)
 (modules Environment))

(library
 (name tezos_raw_protocol_demo_counter)
 (public_name tezos-protocol-demo-counter.raw)
 (libraries tezos_protocol_environment_demo_counter)
 (library_flags (:standard -linkall))
 (flags (:standard -nopervasives -nostdlib
                   -w +a-4-6-7-9-29-32-40..42-44-45-48
                   -warn-error -a+8
                   -open Tezos_protocol_environment_demo_counter__Environment
                   -open Pervasives
                   -open Error_monad))
 (modules
   Error
   Proto_params
   Header
   State
   Proto_operation
   Receipt
   Apply
   Services
   Main))

(install
 (section lib)
 (package tezos-protocol-demo-counter)
 (files (TEZOS_PROTOCOL as raw/TEZOS_PROTOCOL)))

(library
 (name tezos_protocol_demo_counter)
 (public_name tezos-protocol-demo-counter)
 (libraries
      tezos-protocol-environment
      tezos-protocol-environment-sigs
      tezos_raw_protocol_demo_counter)
 (flags -w "+a-4-6-7-9-29-40..42-44-45-48"
        -warn-error "-a+8"
        -nopervasives)
 (modules Protocol))

(library
 (name tezos_protocol_demo_counter_functor)
 (public_name tezos-protocol-demo-counter.functor)
 (libraries
      tezos-protocol-environment
      tezos-protocol-environment-sigs
      tezos_raw_protocol_demo_counter)
 (flags -w "+a-4-6-7-9-29-40..42-44-45-48"
        -warn-error "-a+8"
        -nopervasives)
 (modules Functor))

(library
 (name tezos_embedded_protocol_demo_counter)
 (public_name tezos-embedded-protocol-demo-counter)
 (library_flags (:standard -linkall))
 (libraries tezos-protocol-demo-counter
            tezos-protocol-updater
            tezos-protocol-environment)
 (flags (:standard -w +a-4-6-7-9-29-32-40..42-44-45-48
                   -warn-error -a+8))
 (modules Registerer))

(alias
 (name runtest_compile_protocol)
 (action (run %{bin:tezos-protocol-compiler} -no-hash-check .)))

(alias
 (name runtest_sandbox)
 (deps .tezos_protocol_demo_counter.objs/native/tezos_protocol_demo_counter.cmx))

(alias
 (name runtest)
 (package tezos-protocol-demo-counter)
 (deps (alias runtest_sandbox)))
