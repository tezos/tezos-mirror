(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let () =
  if Array.length Sys.argv <> 4 then (
    prerr_endline
      "Usage: regen_verifier_srs SRSU_G1_PATH SRSU_G2_PATH DEST_PATH" ;
    prerr_endline
      "Example: dune exec \
       ./src/tools/regen_verifier_srs/regen_verifier_srs.exe -- \\\n\
      \                   _opam/share/dal-trusted-setup/srsu_zcash_g1 \\\n\
      \                   _opam/share/dal-trusted-setup/srsu_zcash_g2 \\\n\
      \                   /tmp/verifier_srs.ml" ;
    exit 2) ;
  let srs_g1 = Sys.argv.(1) and srs_g2 = Sys.argv.(2) and dest = Sys.argv.(3) in
  (* call the helper; it returns (unit, _) result Lwt.t *)
  let res =
    Lwt_main.run
      (Tezos_crypto_dal.Srs.Internal_for_tests.print_verifier_srs_from_file
         ~srs_g1_path:srs_g1
         ~srs_g2_path:srs_g2
         ~dest_path:dest
         ())
  in
  match res with
  | Ok () ->
      Printf.printf "Wrote verifier SRS OCaml fragment to %s\n%!" dest ;
      exit 0
  | Error exn -> (
      match exn with
      | `Invalid_point point -> Format.eprintf "FAILED FOR POINT : %d" point
      | _ ->
          prerr_endline "Failed to generate verifier SRS fragment (see above)" ;
          exit 1)
