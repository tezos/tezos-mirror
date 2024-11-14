(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023-2024 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let riscv_sandbox =
  Uses.make ~tag:"riscv_sandbox" ~path:"./src/riscv/riscv-sandbox" ()

(* Tell Manifezt that [riscv_sandbox] itself depends on the full contents
   of the [src/riscv] directory. Manifezt doesn't know how to automatically
   infer non-OCaml dependency relationships. Also declare a dependency on
   the RISC-V test suite using the same tag to avoid warnings. *)
let _ = Uses.make ~tag:"riscv_sandbox" ~path:"./src/riscv/" ()

type vm_kind = Pvm | Test

let run ~kind ~input ?inbox ?(max_steps = Int64.max_int) ?initrd
    ?(print_steps = false) () =
  let process =
    Process.spawn
      ~hooks:Tezt_tezos.Tezos_regression.hooks
      (Uses.path riscv_sandbox)
      (["run"; "--input"; Uses.path input]
      @ Option.fold
          ~none:[]
          ~some:(fun initrd -> ["--initrd"; Uses.path initrd])
          initrd
      @ ["--max-steps"; Int64.to_string max_steps]
      @ (match inbox with
        | None -> []
        | Some inbox -> ["--inbox-file"; Uses.path inbox])
      @
      match kind with
      | Test -> ["--posix-exit-mode"; "machine"]
      | Pvm -> ["--pvm"] @ if print_steps then ["--print-steps"] else [])
  in
  Process.check process

let run_pvm = run ~kind:Pvm

let run_test = run ~kind:Test
