(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let test_dummy_kernel () =
  Tezt_risc_v_sandbox.run_kernel ~kernel:(project_root // "risc-v-dummy.elf")

let fold_dir_lwt ~f ~acc dirname =
  let open Unix in
  let d = opendir dirname in
  let rec loop acc =
    match readdir d with
    | "." | ".." -> loop acc
    | entry ->
        let* acc = f entry acc in
        loop acc
    | exception End_of_file ->
        closedir d ;
        Lwt.return acc
  in
  loop acc

(* We run the official riscv test suite, available here:
   https://github.com/riscv-software-src/riscv-tests

   The tests are split along the following units, corresponding to subcomponents of the CPU.*)
let riscv_test_units =
  ["mi"; "si"; "ua"; "uc"; "ud"; "uf"; "ui"; "um"; "mzicbo"; "ssvnapot"; "uzfh"]

let test_user_level_risc_v_unit_tests riscv_test_unit () =
  let directory = project_root // "tezt/tests/riscv-tests/generated" in
  let is_in_unit program =
    program =~ rex (sf "rv64%s.*-?-.*" riscv_test_unit)
  in
  let* kernels =
    fold_dir_lwt directory ~acc:[] ~f:(fun kernel acc -> return (kernel :: acc))
  in
  (* [fold_dir_lwt] doesn't list in an OS-specific way, we make it canonical. *)
  let kernels = List.sort String.compare kernels in
  Lwt_list.iter_s
    (fun kernel ->
      if is_in_unit kernel then
        Lwt.catch
          (fun () ->
            let* () =
              Tezt_risc_v_sandbox.run_kernel ~kernel:(directory // kernel)
            in
            Printf.ksprintf Regression.capture "%s: success" kernel ;
            Lwt.return_unit)
          (fun _exn ->
            Printf.ksprintf Regression.capture "%s: fail" kernel ;
            Lwt.return_unit)
      else Lwt.return_unit)
    kernels

let test_inline_asm () =
  let kernel =
    project_root // "src/risc_v/tests/inline_asm/rv64-inline-asm-tests"
  in
  Tezt_risc_v_sandbox.run_kernel ~kernel

let register () =
  Test.register
    ~__FILE__
    ~title:"Run the dummy kernel"
    ~tags:["riscv"; "sandbox"]
    ~uses:[Tezt_risc_v_sandbox.risc_v_sandbox]
    ~uses_client:false
    ~uses_admin_client:false
    test_dummy_kernel ;
  List.iter
    (fun test_unit ->
      Regression.register
        ~__FILE__
        ~title:(sf "Run risc-v unit tests (%s)" test_unit)
        ~tags:["riscv"; "sandbox"; "unit"; test_unit]
        ~uses:[Tezt_risc_v_sandbox.risc_v_sandbox]
        ~uses_client:false
        ~uses_admin_client:false
        (test_user_level_risc_v_unit_tests test_unit))
    riscv_test_units ;
  Test.register
    ~__FILE__
    ~title:"Run inline asm tests"
    ~tags:["riscv"; "sandbox"; "inline_asm"]
    ~uses:[Tezt_risc_v_sandbox.risc_v_sandbox]
    ~uses_client:false
    ~uses_admin_client:false
    test_inline_asm
