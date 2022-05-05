(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Persistence

let migrate_mockup ~(cctxt : Tezos_client_base.Client_context.full)
    ~protocol_hash ~next_protocol_hash =
  let open Lwt_result_syntax in
  let base_dir = cctxt#get_base_dir in
  let explain_will_not_work explain =
    let*! () =
      cctxt#error
        "@[<hv>Base directory %s %a@ This command will not work.@ Please \
         specify a correct mockup base directory.@]"
        base_dir
        explain
        ()
    in
    return_unit
  in
  let* base_dir_class = classify_base_dir base_dir in
  let* () =
    match base_dir_class with
    | Base_dir_does_not_exist ->
        explain_will_not_work (fun fmtr () ->
            Format.fprintf fmtr "does not exist.")
    | Base_dir_is_empty ->
        explain_will_not_work (fun fmtr () -> Format.fprintf fmtr "is empty.")
    | Base_dir_is_file ->
        explain_will_not_work (fun fmtr () -> Format.fprintf fmtr "is a file.")
    | Base_dir_is_nonempty ->
        explain_will_not_work (fun fmtr () ->
            Format.fprintf fmtr "is not a mockup base directory.")
    | Base_dir_is_mockup -> return_unit
  in
  let* ((module Current_mockup_env), registration_data) =
    get_mockup_context_from_disk ~base_dir ~protocol_hash cctxt
  in
  let* (module Next_mockup_env) =
    get_registered_mockup (Some next_protocol_hash) cctxt
  in
  let* {chain = chain_id; rpc_context; protocol_data} =
    Next_mockup_env.migrate registration_data
  in
  let* () =
    overwrite_mockup
      ~protocol_hash:next_protocol_hash
      ~chain_id
      ~rpc_context
      ~protocol_data
      ~base_dir
  in
  let*! () = cctxt#message "Migration successful." in
  return_unit
