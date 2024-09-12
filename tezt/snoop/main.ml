(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* This module runs the scripts implemented in all other modules of this
   directory. *)

let run ~io_only_flag ?io_data_dir ?io_cache_dir proto =
  Lwt_main.run
    (let* () = Prepare_data.main proto in
     let* () =
       Perform_benchmarks.main ~io_only_flag ?io_data_dir ?io_cache_dir proto
     in
     let* () = Perform_inference.main () in
     Perform_codegen.main ())

let io_only =
  Clap.flag
    ~description:"Only run the IO benchmarks (io/READ and io/WRITE)."
    ~set_long:"io-only"
    false

let io_data_dir =
  Clap.optional_string
    ~long:"io-data-dir"
    ~description:"Path of the data dir for the io benchmarks."
    ()

let io_cache_dir =
  Clap.optional_string
    ~long:"io-cache-dir"
    ~description:"Path of the cache for the io benchmarks."
    ()

let () =
  Background.start (fun x -> raise x) ;
  run ~io_only_flag:io_only ?io_data_dir ?io_cache_dir Protocol.Alpha
