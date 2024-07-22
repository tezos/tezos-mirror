(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023-2024 TriliTech <contact@trili.tech>                    *)
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

(* Testing
   -------
   Component:    RISC-V Sandbox
   Requirements: make -C src/riscv build
   Invocation:   dune exec tezt/tests/main.exe -- --file riscv_sandbox.ml
   Subject:      RISC-V integration and unit tests
*)

let hermit_loader =
  Uses.make ~tag:"riscv" ~path:"src/riscv/assets/hermit-loader" ()

let dummy_kernel = Uses.make ~tag:"riscv" ~path:"src/riscv/riscv-dummy.elf" ()

let dummy_kernel_inbox =
  Uses.make
    ~tag:"riscv"
    ~path:"tezt/tests/riscv-tests/dummy-kernel-inbox.json"
    ()

let dummy_kernel_frozen =
  Uses.make ~tag:"riscv" ~path:"src/riscv/assets/riscv-dummy.elf" ()

let test_dummy_kernel () =
  Tezt_riscv_sandbox.run_pvm
    ~input:hermit_loader
    ~initrd:dummy_kernel
    ~inbox:dummy_kernel_inbox
    ()

let test_dummy_kernel_frozen () =
  Tezt_riscv_sandbox.run_pvm
    ~input:hermit_loader
    ~initrd:dummy_kernel_frozen
    ~inbox:dummy_kernel_inbox
    ~print_steps:true
    ()

let inline_asm_tests =
  Uses.make
    ~tag:"riscv"
    ~path:"src/riscv/tests/inline_asm/rv64-inline-asm-tests"
    ()

let test_inline_asm () = Tezt_riscv_sandbox.run_test ~input:inline_asm_tests ()

let register () =
  Regression.register
    ~__FILE__
    ~title:"Run the dummy kernel"
    ~tags:["riscv"; "sandbox"; "dummy"]
    ~uses:[Tezt_riscv_sandbox.riscv_sandbox; dummy_kernel; hermit_loader]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    test_dummy_kernel ;
  Regression.register
    ~__FILE__
    ~title:"Run the dummy kernel (frozen)"
    ~tags:["riscv"; "sandbox"; "dummy"]
    ~uses:[Tezt_riscv_sandbox.riscv_sandbox; dummy_kernel; hermit_loader]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    test_dummy_kernel_frozen ;
  Regression.register
    ~__FILE__
    ~title:"Run inline asm tests"
    ~tags:["riscv"; "sandbox"; "inline_asm"]
    ~uses:[Tezt_riscv_sandbox.riscv_sandbox; inline_asm_tests]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    test_inline_asm
