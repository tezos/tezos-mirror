(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Checked-in kernels *)
type kernel = Jstz | Etherlink | Dummy

(** The name of the binary of a checked-in kernel *)
let kernel_name = function
  | Jstz -> "jstz"
  | Etherlink -> "etherlink"
  | Dummy -> "riscv-dummy.elf"

(** Get the paths to a checked-in kernel binay and its checksum *)
let asset_kernel_path (kernel : kernel) =
  let kernel = project_root // "src/riscv/assets" // kernel_name kernel in
  (kernel, kernel ^ ".checksum")

(** Get the path to the checked-in initial proof of a kernel *)
let proof_path (kernel : kernel) =
  project_root // "src/riscv/assets" // (kernel_name kernel ^ "_proof_initial")

(** Get the paths to a locally-built kernel binary and its checksum. *)
let riscv_kernel_path name =
  let in_kernels_dir = "../../kernels" // name in
  let at_project_root = project_root // name in

  (* First looks in the local dune build directory.
     If not built, it means we are running in CI. We use the artifact produced
     by the [oc.build_kernels] jobs instead, which is at the root. *)
  let kernel =
    if Sys.file_exists in_kernels_dir then in_kernels_dir else at_project_root
  in
  (kernel, kernel ^ ".checksum")

(* Get a string pointing to the path of a RISC-V kernel which the RISC-V PVM
 * can interpret as an initial boot sector. The PVM will then load the kernel
 * directly. This is to bypass origination of large kernels, which is not
 * currently supported (RV-109) *)
let read_riscv_kernel_internal ((kernel_path, checksum_path) : string * string)
    =
  match String.split_on_char ' ' (read_file checksum_path) with
  | checksum :: _ -> Lwt.return ("kernel:" ^ kernel_path ^ ":" ^ checksum)
  | _ -> Lwt.fail_with "Kernel found"

(** Get a boot sector for a checked-in kernel *)
let read_riscv_kernel (kernel : kernel) : string Lwt.t =
  read_riscv_kernel_internal (asset_kernel_path kernel)

(** Get the checked-in proof for the first step a kernel *)
let read_riscv_proof_first_step (kernel : kernel) : string =
  read_file (proof_path kernel)

(** Get a boot sector for a the echo kernel *)
let read_riscv_echo_kernel () : string Lwt.t =
  read_riscv_kernel_internal (riscv_kernel_path "riscv-echo")
