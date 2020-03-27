

;
;        /!\ /!\ Do not modify this file /!\ /!\
;
; but the original template in `tezos-protocol-compiler`
;


(rule
 (targets environment.ml)
 (action
  (write-file %{targets}
              "module Name = struct let name = \"000-Ps9mPmXa\" end
include Tezos_protocol_environment.MakeV0(Name)()
module CamlinternalFormatBasics = struct include CamlinternalFormatBasics end
")))

(rule
 (targets registerer.ml)
 (deps
   data.ml
   services.ml
   main.mli main.ml
   (:src_dir TEZOS_PROTOCOL))
 (action
  (with-stdout-to %{targets}
                  (chdir %{workspace_root} (run %{bin:tezos-embedded-protocol-packer} "%{src_dir}" "000_Ps9mPmXa")))))

(rule
 (targets functor.ml)
 (deps
   data.ml
   services.ml
   main.mli main.ml
   (:src_dir TEZOS_PROTOCOL))
 (action (with-stdout-to %{targets}
                         (chdir %{workspace_root}
                                (run %{bin:tezos-protocol-compiler.tezos-protocol-packer} %{src_dir})))))

(rule
 (targets protocol.ml)
 (deps
   data.ml
   services.ml
   main.mli main.ml)
 (action
  (write-file %{targets}
    "module Environment = Tezos_protocol_environment_000_Ps9mPmXa.Environment
let hash = Tezos_crypto.Protocol_hash.of_b58check_exn \"Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P\"
let name = Environment.Name.name
include Tezos_raw_protocol_000_Ps9mPmXa
include Tezos_raw_protocol_000_Ps9mPmXa.Main
")))

(library
 (name tezos_protocol_environment_000_Ps9mPmXa)
 (public_name tezos-protocol-000-Ps9mPmXa.environment)
 (library_flags (:standard -linkall))
 (libraries tezos-protocol-environment)
 (modules Environment))

(library
 (name tezos_raw_protocol_000_Ps9mPmXa)
 (public_name tezos-protocol-000-Ps9mPmXa.raw)
 (libraries tezos_protocol_environment_000_Ps9mPmXa)
 (library_flags (:standard -linkall))
 (flags (:standard -nopervasives -nostdlib
                   -w +a-4-6-7-9-29-32-40..42-44-45-48
                   -warn-error -a+8
                   -open Tezos_protocol_environment_000_Ps9mPmXa__Environment
                   -open Pervasives
                   -open Error_monad))
 (modules
   Data
   Services
   Main))

(install
 (section lib)
 (package tezos-protocol-000-Ps9mPmXa)
 (files (TEZOS_PROTOCOL as raw/TEZOS_PROTOCOL)))

(library
 (name tezos_protocol_000_Ps9mPmXa)
 (public_name tezos-protocol-000-Ps9mPmXa)
 (libraries
      tezos-protocol-environment
      tezos-protocol-environment-sigs
      tezos_raw_protocol_000_Ps9mPmXa)
 (flags -w "+a-4-6-7-9-29-40..42-44-45-48"
        -warn-error "-a+8"
        -nopervasives)
 (modules Protocol))

(library
 (name tezos_protocol_000_Ps9mPmXa_functor)
 (public_name tezos-protocol-000-Ps9mPmXa.functor)
 (libraries
      tezos-protocol-environment
      tezos-protocol-environment-sigs
      tezos_raw_protocol_000_Ps9mPmXa)
 (flags -w "+a-4-6-7-9-29-40..42-44-45-48"
        -warn-error "-a+8"
        -nopervasives)
 (modules Functor))

(library
 (name tezos_embedded_protocol_000_Ps9mPmXa)
 (public_name tezos-embedded-protocol-000-Ps9mPmXa)
 (library_flags (:standard -linkall))
 (libraries tezos-protocol-000-Ps9mPmXa
            tezos-protocol-updater
            tezos-protocol-environment)
 (flags (:standard -w +a-4-6-7-9-29-32-40..42-44-45-48
                   -warn-error -a+8))
 (modules Registerer))

(alias
 (name runtest_compile_protocol)
 (action (run %{bin:tezos-protocol-compiler}  .)))

(alias
 (name runtest_sandbox)
 (deps .tezos_protocol_000_Ps9mPmXa.objs/native/tezos_protocol_000_Ps9mPmXa.cmx))

(alias
 (name runtest)
 (package tezos-protocol-000-Ps9mPmXa)
 (deps (alias runtest_sandbox)))
