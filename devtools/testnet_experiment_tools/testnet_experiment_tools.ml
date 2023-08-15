(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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

open Tezt
open Tezt_tezos

let ensure_dir_exists dir =
  Lwt.catch
    (fun () ->
      let* () = Lwt_utils_unix.create_dir ~perm:0o744 dir in
      Lwt.return_unit)
    (fun exn ->
      Test.fail
        "Failed to create directory '%s': %s"
        dir
        (Printexc.to_string exn))

let default_number_of_bakers = 10

let bakers = "BAKERS"

let baker_alias n = Printf.sprintf "baker_%d" n

let number_of_bakers =
  Sys.getenv_opt bakers |> Option.map int_of_string
  |> Option.value ~default:default_number_of_bakers

let default_gen_keys_dir =
  let base_dir = Filename.temp_file ~temp_dir:"/tmp" "" "" in
  let _ = Lwt_unix.unlink base_dir in
  let _ = Lwt_unix.mkdir base_dir 0o700 in
  base_dir

let gen_keys_dir_name = "GEN_KEYS_DIR"

let gen_keys_dir =
  Sys.getenv_opt gen_keys_dir_name |> Option.value ~default:default_gen_keys_dir

let generate_baker_accounts n client =
  let rec generate_baker_account i =
    if i < 0 then Lwt.return_unit
    else
      let* _alias = Client.gen_keys ~alias:(baker_alias i) client in
      let* () = Lwt_io.printf "." in
      generate_baker_account (i - 1)
  in
  let* () = Lwt_io.printf "Generating accounts" in
  let* () = generate_baker_account (n - 1) in
  Lwt_io.printf "\n\n"

(* These tests can be run locally to generate the data needed to run a
   stresstest. *)
module Local = struct
  let generate_baker_accounts n () =
    let client_dir = gen_keys_dir in
    let* () =
      Lwt_io.printf
        "Keys will be saved in %s. You can change this by setting the \
         GEN_KEYS_DIR environment variable\n\n"
        default_gen_keys_dir
    in
    let* () =
      Lwt_io.printf
        "%d baker accounts will be generated. You can change this by setting \
         the BAKERS environment variable.\n\n"
        number_of_bakers
    in
    let client = Client.create ~base_dir:client_dir () in
    let* () = ensure_dir_exists client_dir in
    let* () = generate_baker_accounts n client in
    Lwt.return_unit

  let generate_network_configuration () = Test.fail "Not implemented"

  let generate_manager_operations () = Test.fail "Not implemented"
end

(* These tests must be run remotely by the nodes participating in
   a network that wants to be stresstested. *)
module Remote = struct
  let run_stresstest () = Test.fail "Not implemented"
end

let () =
  let open Tezt.Test in
  register
    ~__FILE__
    ~title:"Generate baker accounts"
    ~tags:["generate_baker_accounts"]
    (Local.generate_baker_accounts number_of_bakers) ;
  register
    ~__FILE__
    ~title:"Generate Network Configuration"
    ~tags:["generate_network_configuration"]
    Local.generate_network_configuration ;
  register
    ~__FILE__
    ~title:"Generate manager operations"
    ~tags:["generate_operations"]
    Local.generate_manager_operations ;
  register
    ~__FILE__
    ~title:"Run stresstest"
    ~tags:["run_stresstest"]
    Remote.run_stresstest ;
  Tezt.Test.run ()
