

;
;        /!\ /!\ Do not modify this file /!\ /!\
;
; but the original template in `tezos-protocol-compiler`
;


(rule
 (targets environment.ml)
 (action
  (write-file %{targets}
              "include Tezos_protocol_environment_shell.MakeV1(struct let name = \"demo-noops\" end)()
      module CamlinternalFormatBasics = struct include CamlinternalFormatBasics end
")))

(rule
 (targets registerer.ml)
 (deps tezos_embedded_protocol_environment_demo_noops.cmxa
       (:src_dir TEZOS_PROTOCOL))
 (action
  (with-stdout-to %{targets}
                  (chdir %{workspace_root} (run %{bin:tezos-embedded-protocol-packer} "%{src_dir}" "demo_noops")))))


(rule
 (targets functor.ml)
 (deps main.mli main.ml
       (:src_dir TEZOS_PROTOCOL))
 (action (with-stdout-to %{targets}
                         (chdir %{workspace_root}
                                (run %{bin:tezos-protocol-compiler.tezos-protocol-packer} %{src_dir})))))

(library
 (name tezos_protocol_demo_noops)
 (public_name tezos-protocol-demo-noops)
 (libraries tezos-protocol-environment-sigs)
 (flags -w "+a-4-6-7-9-29-40..42-44-45-48"
        -warn-error "-a+8"
        -nopervasives)
 (modules Functor))

(library
 (name tezos_embedded_protocol_environment_demo_noops)
 (public_name tezos-embedded-protocol-demo-noops.environment)
 (library_flags (:standard -linkall))
 (libraries tezos-protocol-environment-shell)
 (modules Environment))

(library
 (name tezos_embedded_raw_protocol_demo_noops)
 (public_name tezos-embedded-protocol-demo-noops.raw)
 (libraries tezos_embedded_protocol_environment_demo_noops)
 (library_flags (:standard -linkall))
 (flags (:standard -nopervasives -nostdlib
                   -w +a-4-6-7-9-29-32-40..42-44-45-48
                   -warn-error -a+8
                   -open Tezos_embedded_protocol_environment_demo_noops__Environment
                   -open Pervasives
                   -open Error_monad))
 (modules Main))

(install
 (section lib)
 (package tezos-embedded-protocol-demo-noops)
 (files (TEZOS_PROTOCOL as raw/TEZOS_PROTOCOL)))

(library
 (name tezos_embedded_protocol_demo_noops)
 (public_name tezos-embedded-protocol-demo-noops)
 (library_flags (:standard -linkall))
 (libraries tezos_embedded_raw_protocol_demo_noops
            tezos-protocol-updater
            tezos-protocol-environment-shell)
 (flags (:standard -w +a-4-6-7-9-29-32-40..42-44-45-48
                   -warn-error -a+8))
 (modules Registerer))

(alias
 (name runtest_sandbox)
 (deps .tezos_protocol_demo_noops.objs/native/tezos_protocol_demo_noops.cmx))

