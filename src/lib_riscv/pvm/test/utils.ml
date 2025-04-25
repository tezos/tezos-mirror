(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Get a string pointing to the path of a RISC-V kernel which the RISC-V PVM
 * can interpret as an initial boot sector. The PVM will then load the kernel
 * directly. This is to bypass origination of large kernels, which is not
 * currently supported (RV-109) *)
let read_riscv_kernel (kernel_path : string) (checksum_path : string) =
  match String.split_on_char ' ' (read_file checksum_path) with
  | checksum :: _ -> Lwt.return ("kernel:" ^ kernel_path ^ ":" ^ checksum)
  | _ -> Lwt.fail_with "Kernel found"

let read_riscv_dummy_kernel () =
  read_riscv_kernel
    (project_root // "src/riscv/assets/riscv-dummy.elf")
    (project_root // "src/riscv/assets/riscv-dummy.elf.checksum")

let read_riscv_jstz_kernel () =
  read_riscv_kernel
    (project_root // "src/riscv/assets/jstz")
    (project_root // "src/riscv/assets/jstz.checksum")

let read_riscv_jstz_proof_first_step () =
  read_file (project_root // "src/riscv/lib/tests/expected/jstz/proof_initial")
