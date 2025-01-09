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

type Error_monad.error += No_trusted_setup of string list

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"node.config.trusted_setup_not_found"
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

(* FIXME https://gitlab.com/tezos/tezos/-/issues/3410

   This function should be factored out with the one of sapling. *)

let trusted_setup_candidate_directories ?(getenv_opt = Sys.getenv_opt)
    ?(getcwd = Sys.getcwd) () =
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
  env "DAL_TRUSTED_SETUP" ""
  @ env "XDG_DATA_HOME" ".local/share/dal-trusted-setup"
  @ env ~split:':' "XDG_DATA_DIRS" "dal-trusted-setup"
  @ env "OPAM_SWITCH_PREFIX" "share/dal-trusted-setup"
  @ env "PWD" "_opam/share/dal-trusted-setup"
  @ cwd "_opam/share/dal-trusted-setup"
  @ env "HOME" ".dal-trusted-setup"
  @ env "HOME" ".local/share/dal-trusted-setup"
  @ env "HOMEBREW_PREFIX" "share/dal-trusted-setup"
  @ ["/usr/local/share/dal-trusted-setup"; "/usr/share/dal-trusted-setup"]

let trusted_setup_preferred_directory ?getenv_opt ?getcwd () =
  Stdlib.List.hd (trusted_setup_candidate_directories ?getenv_opt ?getcwd ())

let find_trusted_setup_files ?(getenv_opt = Sys.getenv_opt)
    ?(getcwd = Sys.getcwd) ?(file_exists = Sys.file_exists) () =
  let ( // ) = Filename.concat in
  let candidate_directories =
    trusted_setup_candidate_directories ~getenv_opt ~getcwd ()
  in
  (* Files we are looking for. *)
  let g1_file = "srsu_zcash_g1" in
  let g2_file = "srsu_zcash_g2" in
  (* Find the first candidate directory that contains the expected files. *)
  let contains_trusted_setup_files directory =
    file_exists (directory // g1_file) && file_exists (directory // g2_file)
  in
  match List.find_opt contains_trusted_setup_files candidate_directories with
  | None -> Error [No_trusted_setup candidate_directories]
  | Some directory ->
      let g1_path = directory // g1_file in
      let g2_path = directory // g2_file in
      Ok (g1_path, g2_path)
