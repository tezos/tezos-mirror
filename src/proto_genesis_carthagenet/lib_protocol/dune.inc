

;
;        /!\ /!\ Do not modify this file /!\ /!\
;
; but the original template in `tezos-protocol-compiler`
;


(rule
 (targets environment.ml)
 (action
  (write-file %{targets}
              "module Name = struct let name = \"genesis-carthagenet\" end
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
                  (chdir %{workspace_root} (run %{bin:tezos-embedded-protocol-packer} "%{src_dir}" "genesis_carthagenet")))))

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
    "module Environment = Tezos_protocol_environment_genesis_carthagenet.Environment
let hash = Tezos_crypto.Protocol_hash.of_b58check_exn \"PtYuensgYBb3G3x1hLLbCmcav8ue8Kyd2khADcL5LsT5R1hcXex\"
let name = Environment.Name.name
include Tezos_raw_protocol_genesis_carthagenet
include Tezos_raw_protocol_genesis_carthagenet.Main
")))

(library
 (name tezos_protocol_environment_genesis_carthagenet)
 (public_name tezos-protocol-genesis-carthagenet.environment)
 (library_flags (:standard -linkall))
 (libraries tezos-protocol-environment)
 (modules Environment))

(library
 (name tezos_raw_protocol_genesis_carthagenet)
 (public_name tezos-protocol-genesis-carthagenet.raw)
 (libraries tezos_protocol_environment_genesis_carthagenet)
 (library_flags (:standard -linkall))
 (flags (:standard -nopervasives -nostdlib
                   -w +a-4-6-7-9-29-32-40..42-44-45-48
                   -warn-error -a+8
                   -open Tezos_protocol_environment_genesis_carthagenet__Environment
                   -open Pervasives
                   -open Error_monad))
 (modules
   Data
   Services
   Main))

(install
 (section lib)
 (package tezos-protocol-genesis-carthagenet)
 (files (TEZOS_PROTOCOL as raw/TEZOS_PROTOCOL)))

(library
 (name tezos_protocol_genesis_carthagenet)
 (public_name tezos-protocol-genesis-carthagenet)
 (libraries
      tezos-protocol-environment
      tezos-protocol-environment-sigs
      tezos_raw_protocol_genesis_carthagenet)
 (flags -w "+a-4-6-7-9-29-40..42-44-45-48"
        -warn-error "-a+8"
        -nopervasives)
 (modules Protocol))

(library
 (name tezos_protocol_genesis_carthagenet_functor)
 (public_name tezos-protocol-genesis-carthagenet.functor)
 (libraries
      tezos-protocol-environment
      tezos-protocol-environment-sigs
      tezos_raw_protocol_genesis_carthagenet)
 (flags -w "+a-4-6-7-9-29-40..42-44-45-48"
        -warn-error "-a+8"
        -nopervasives)
 (modules Functor))

(library
 (name tezos_embedded_protocol_genesis_carthagenet)
 (public_name tezos-embedded-protocol-genesis-carthagenet)
 (library_flags (:standard -linkall))
 (libraries tezos-protocol-genesis-carthagenet
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
 (deps .tezos_protocol_genesis_carthagenet.objs/native/tezos_protocol_genesis_carthagenet.cmx))

(alias
 (name runtest)
 (package tezos-protocol-genesis-carthagenet)
 (deps (alias runtest_sandbox)))
