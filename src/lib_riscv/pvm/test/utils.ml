(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type kernel = Jstz | Etherlink

let kernel_name = function Jstz -> "jstz" | Etherlink -> "etherlink"

let kernel_path kernel =
  project_root // "src/riscv/assets" // kernel_name kernel

let checksum_path kernel = kernel_path kernel ^ ".checksum"

let proof_path kernel =
  project_root // "src/riscv/assets" // (kernel_name kernel ^ "_proof_initial")

(* Get a string pointing to the path of a RISC-V kernel which the RISC-V PVM
 * can interpret as an initial boot sector. The PVM will then load the kernel
 * directly. This is to bypass origination of large kernels, which is not
 * currently supported (RV-109) *)
let read_riscv_kernel_internal (kernel_path : string) (checksum_path : string) =
  match String.split_on_char ' ' (read_file checksum_path) with
  | checksum :: _ -> Lwt.return ("kernel:" ^ kernel_path ^ ":" ^ checksum)
  | _ -> Lwt.fail_with "Kernel found"

let read_riscv_dummy_kernel () =
  read_riscv_kernel_internal
    (project_root // "src/riscv/assets/riscv-dummy.elf")
    (project_root // "src/riscv/assets/riscv-dummy.elf.checksum")

let read_riscv_kernel kernel =
  read_riscv_kernel_internal (kernel_path kernel) (checksum_path kernel)

let read_riscv_proof_first_step kernel = read_file (proof_path kernel)
