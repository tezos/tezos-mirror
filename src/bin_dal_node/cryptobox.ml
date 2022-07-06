(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Tezos_crypto_dal

module Constants = struct
  let redundancy_factor = 2

  let slot_segment_size = 4096

  let slot_size = 1048576 (* 1Mb *)

  let shards_amount = 2048

  let trusted_setup_logarithm_size = 21
end

include Dal_cryptobox.Make (Constants)

exception Trusted_setup_not_found of string list

type error +=
  | Failed_to_load_trusted_setup of string
  | No_trusted_setup of string list

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.trusted_setup_loading_failed"
    ~title:"Trusted setup loading failed"
    ~description:"Trusted setup failed to load"
    ~pp:(fun ppf msg ->
      Format.fprintf ppf "Trusted setup failed to load: %s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function
      | Failed_to_load_trusted_setup parameter -> Some parameter | _ -> None)
    (fun parameter -> Failed_to_load_trusted_setup parameter) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.trusted_setup_not_found"
    ~title:"No trusted setup found"
    ~description:"No trusted setup found in the explored paths"
    ~pp:(fun ppf locations ->
      Format.fprintf
        ppf
        "@[<v>cannot find Trusted setup in any of:@,%a@]@."
        (Format.pp_print_list (fun fmt -> Format.fprintf fmt "- %s"))
        locations)
    Data_encoding.(obj1 (req "paths" (list string)))
    (function No_trusted_setup parameter -> Some parameter | _ -> None)
    (fun parameter -> No_trusted_setup parameter)

let find_trusted_setup ?(getenv_opt = Sys.getenv_opt) ?(getcwd = Sys.getcwd)
    ?(file_exists = Sys.file_exists) () =
  let ( // ) = Filename.concat in
  let env ?split name path =
    match getenv_opt name with
    | None -> []
    | Some value -> (
        match split with
        | None -> [Filename.concat value path]
        | Some char ->
            List.map (fun dir -> dir // path) (String.split_on_char char value))
  in
  let cwd path = try [getcwd () // path] with Sys_error _ -> [] in
  let candidate_directories =
    env "XDG_DATA_HOME" ".local/share/dal-trusted-setup"
    @ env ~split:':' "XDG_DATA_DIRS" "dal-trusted-setup"
    @ env "OPAM_SWITCH_PREFIX" "share/dal-trusted-setup"
    @ env "PWD" "_opam/share/dal-trusted-setup"
    @ cwd "_opam/share/dal-trusted-setup"
    @ env "HOME" ".dal-trusted-setup"
    @ env "HOME" ".local/share/dal-trusted-setup"
    @ env "HOMEBREW_PREFIX" "share/dal-trusted-setup"
    @ ["/usr/local/share/dal-trusted-setup"; "/usr/share/dal-trusted-setup"]
  in
  (* Files we are looking for. *)
  let srs_g1 = "srs_zcash_g1" in
  let srs_g2 = "srs_zcash_g2" in
  (* Find the first candidate directory that contains the expected files. *)
  let directory =
    let contains_trusted_setup_files directory =
      file_exists (directory // srs_g1) && file_exists (directory // srs_g2)
    in
    match List.find_opt contains_trusted_setup_files candidate_directories with
    | None -> raise (Trusted_setup_not_found candidate_directories)
    | Some directory -> directory
  in
  let srs_g1_file = directory // srs_g1 in
  let srs_g2_file = directory // srs_g2 in
  {
    srs_g1_file;
    srs_g2_file;
    logarithm_size = Constants.trusted_setup_logarithm_size;
  }

let init_setup ~no_trusted_setup () =
  let open Lwt_result_syntax in
  let* trusted_setup =
    try
      let trusted_setup = find_trusted_setup () in
      return (`Files trusted_setup)
    with Trusted_setup_not_found candidate_directories ->
      if no_trusted_setup then return `Unsafe_for_test_only
      else fail [No_trusted_setup candidate_directories]
  in
  let*? trusted_setup =
    try Ok (build_trusted_setup_instance trusted_setup)
    with Invalid_argument msg -> Error [Failed_to_load_trusted_setup msg]
  in
  return trusted_setup
