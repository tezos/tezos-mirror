(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let sf = Printf.sprintf

module License = struct
  type t = MIT | MIT_SPDX | MIT_OS | MIT_SPDX_deprecated

  let known : t list = [MIT; MIT_SPDX; MIT_OS; MIT_SPDX_deprecated]

  let to_string : t -> string = function
    | MIT -> "MIT"
    | MIT_OS -> "MIT-OS"
    | MIT_SPDX -> "MIT-SPDX"
    | MIT_SPDX_deprecated -> "MIT-SPDX-deprecated"

  let pp : t -> string = function
    | MIT -> "MIT"
    | MIT_OS -> "MIT (Open Source License variant)"
    | MIT_SPDX -> "MIT (SPDX)"
    | MIT_SPDX_deprecated -> "MIT (SPDX, deprecated version)"

  let of_string (s : string) : t =
    match String.uppercase_ascii s with
    | "MIT" -> MIT
    | "MIT-OS" -> MIT_OS
    | "MIT-SPDX" -> MIT_SPDX
    | "MIT-SPDX-deprecated" -> MIT_SPDX_deprecated
    | _ -> failwith (sf "Unknown license: %s" s)
end

let with_open_in (file : string) (read_f : in_channel -> 'a) : 'a =
  let chan = open_in file in
  Fun.protect ~finally:(fun () -> close_in_noerr chan) (fun () -> read_f chan)

let read_header (in_ch : in_channel) : string list =
  let rec loop acc =
    match input_line in_ch with
    | (exception End_of_file) | "" -> List.rev acc
    | line -> loop (line :: acc)
  in
  loop []

(* Represents an error on zero-indexed line-number [line_no] *)
type error = {line_no : int option; message : string}

let error ?(line_no : int option) (message : string) : ('a, error) result =
  Result.Error {line_no; message}

(* The [Check] module stacks a reader monad inside an Result monad.

   A checker (a value of [Check.m]) reads a sequence of lines and
   returns a result. Checkers are combined similarly to parser
   combinators and are used below to build license header checkers. *)
module Check = struct
  (* A Checker's state [(n, ls)] is the set of currently unread lines
     [ls] and the 0-indexed line number of the first element of
     [ls]. *)
  type st = int * string list

  type 'a m = st -> (st * 'a, error) Result.t

  let ( let* ) (m : 'a m) (f : 'a -> 'b m) : 'b m =
   fun st -> Result.bind (m st) (fun (st, v) -> f v st)

  let return (x : 'a) : 'a m = fun st -> Result.ok (st, x)

  (* [run ls checker] runs [checker] over the lines [ls]. *)
  let run (lines : string list) (m : 'a m) : ('a, error) Result.t =
    match m (0, lines) with
    | Ok (_st, v) -> Result.Ok v
    | Error e -> Result.Error e

  (* return the next line and the corresponding state *)
  let next (st : st) : (string * st) option =
    match st with n, l :: ls -> Some (l, (n + 1, ls)) | _, [] -> None

  (* Produce an error for a given state.

     Note: if the error is associated with the line [l] obtained
     through [Some (l, st') = next st], then you should pass the
     original state [st] to [error_f] so that the error is associated
     with the correct line number. *)
  let error_f (line_no, _) fmt =
    Printf.ksprintf (fun message -> error ~line_no message) fmt

  (* [lines ls] checks that the next sequence of lines are exactly [ls].

     Note that [lines ls] consumes [List.length ls] lines of the state. *)
  let rec lines (ls : string list) : unit m =
   fun st ->
    match (next st, ls) with
    | Some (h, st'), l' :: ls' when h = l' -> lines ls' st'
    | Some (h, _), l' :: _ ->
        error_f st "Expected to find:\n> %S\nfound:\n> %S\n" l' h
    | None, l' :: _ ->
        error_f st "Expected to find: %s\nfound premature end of header" l'
    | _, [] -> Result.Ok (st, ())

  (* [check ~hint f] checks that the next line satisfies [f].

     Consumes one line of the state.

     If not, an error based on [~hint] is emitted. *)
  let check ~(hint : string) (f : string -> bool) : unit m =
   fun st ->
    match next st with
    | Some (h, st') when f h -> Result.Ok (st', ())
    | Some (h, _) -> error_f st "%s\nfound\n> %S" hint h
    | _ -> error_f st "%s\nfound premature end of header" hint

  (* [any c] greedily checks [c] zero or more times.

     Greedily consumes as many lines of the state as satisfy [check]. *)
  let rec any (check : unit m) : unit m =
   fun st ->
    match check st with
    | Result.Ok (st', ()) -> any check st'
    | Result.Error _ -> Result.Ok (st, ())

  (* [many c] greedily checks [c] one or more times.

     Greedily consumes as many lines of the state as satisfy [check].

     If none, an error is emitted. *)
  let many (check : unit m) : unit m =
    let* () = check in
    any check
end

(* [check_license ?additional_licenses license inc] checks that [inc] has [license].

   Returns [Result.Ok license] if [inc] has [license]. If
   [additional_licenses] the license of [inc] is checked against all
   licenses [license :: additional_licenses] and [Result.Ok l] is
   returned where [l] is the first license that matches.

   Returns [Result.Error e] if no license matches. *)
let check_license ?(additional_licenses : License.t list = [])
    (license : License.t) (in_channel : in_channel) :
    (License.t, error) Result.t =
  let header = lazy (read_header in_channel) in
  let open Check in
  let check_license license =
    let copyright_line =
      many
      @@ check ~hint:"Expected a line like: (* Copyright (c) ... *)"
      @@ fun line ->
      String.(
        starts_with ~prefix:"(* Copyright (c)" line
        && ends_with ~suffix:"*)" line
        && length line = 79)
    in
    let spdx_copyright_line =
      many
      @@ check
           ~hint:
             "Expected a line like:\n\
              > \"(* SPDX-FileCopyrightText: [year(s)] [Holder <email>] ... \
              *)\""
      @@ fun line ->
      String.(
        starts_with ~prefix:"(* SPDX-FileCopyrightText: " line
        && ends_with ~suffix:"*)" line
        && length line = 79)
    in
    let mit_like ~license_identifier_line =
      let* () =
        lines
          [
            {|(*****************************************************************************)|};
            {|(*                                                                           *)|};
          ]
      in
      let* () = lines [license_identifier_line] in
      let* () = copyright_line in
      let* () =
        lines
          [
            {|(*                                                                           *)|};
            {|(* Permission is hereby granted, free of charge, to any person obtaining a   *)|};
            {|(* copy of this software and associated documentation files (the "Software"),*)|};
            {|(* to deal in the Software without restriction, including without limitation *)|};
            {|(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)|};
            {|(* and/or sell copies of the Software, and to permit persons to whom the     *)|};
            {|(* Software is furnished to do so, subject to the following conditions:      *)|};
            {|(*                                                                           *)|};
            {|(* The above copyright notice and this permission notice shall be included   *)|};
            {|(* in all copies or substantial portions of the Software.                    *)|};
            {|(*                                                                           *)|};
            {|(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)|};
            {|(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)|};
            {|(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)|};
            {|(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)|};
            {|(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)|};
            {|(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)|};
            {|(* DEALINGS IN THE SOFTWARE.                                                 *)|};
            {|(*                                                                           *)|};
            {|(*****************************************************************************)|};
          ]
      in
      return ()
    in
    let mit_spdx_like ~copyright_line =
      let* () =
        lines
          [
            {|(*****************************************************************************)|};
            {|(*                                                                           *)|};
            {|(* SPDX-License-Identifier: MIT                                              *)|};
          ]
      in
      let* () = copyright_line in
      let* () =
        lines
          [
            {|(*                                                                           *)|};
            {|(*****************************************************************************)|};
          ]
      in
      return ()
    in
    let check_license_m = function
      | License.MIT ->
          mit_like
            ~license_identifier_line:
              {|(* MIT License                                                               *)|}
      | MIT_OS ->
          mit_like
            ~license_identifier_line:
              {|(* Open Source License                                                       *)|}
      | MIT_SPDX -> mit_spdx_like ~copyright_line:spdx_copyright_line
      | MIT_SPDX_deprecated -> mit_spdx_like ~copyright_line
    in
    run (Lazy.force header) @@ check_license_m license
  in
  match additional_licenses with
  | [] -> check_license license |> Result.map (Fun.const license)
  | _ ->
      let rec loop = function
        | license :: licenses -> (
            match check_license license with
            | Result.Ok () -> Result.Ok license
            | Result.Error _ -> loop licenses)
        | [] -> error ~line_no:0 "No license matched"
      in
      loop (license :: additional_licenses)

type cli_options = {
  mutable licenses : License.t list;
      (** Set of licenses to check for. By default, checks any of [License.known] *)
  mutable verbose : bool;
      (** Enables verbose output, including error locations. *)
}

let () =
  let cli_options = {licenses = []; verbose = false} in
  let args_spec =
    let open Arg in
    [
      ( "--licenses",
        String
          (fun license_str ->
            cli_options.licenses <-
              String.split_on_char ',' license_str |> List.map License.of_string),
        "<LICENSES> Comma-separated list of licenses to check for. By default, \
         checks for any known license. Known licenses are: "
        ^ (List.map License.to_string License.known |> String.concat ", ")
        ^ "." );
      ( "--verbose",
        Unit (fun () -> cli_options.verbose <- true),
        " Verbose output." );
    ]
    @ List.map
        (fun license ->
          ( "--" ^ String.lowercase_ascii (License.to_string license),
            Unit
              (fun () ->
                cli_options.licenses <- license :: cli_options.licenses),
            " Adds '" ^ License.pp license
            ^ "' to the set of licenses to check for." ))
        License.known
    |> align
  in
  let usage_msg =
    Printf.sprintf
      {|Usage: %s [options] <PATH+>

Verifies the license header of each source file specified in PATH+.
Only supports OCaml .ml(i) files currently.

Example:

       %s --mit-spdx --mit my_module.ml my_module.mli

verifies that the files `my_module.ml` and `my_module.mli` have either MIT
SPDX or MIT license headers. Exits with code 0 and no output if all the specified
source files have one of the expected license headers.

Options are:|}
      Sys.argv.(0)
      Sys.argv.(0)
  in
  let paths = ref [] in
  Arg.(parse args_spec (fun s -> paths := s :: !paths) usage_msg) ;
  match List.rev !paths with
  | [] ->
      Arg.usage args_spec usage_msg ;
      exit 1
  | paths ->
      let license, additional_licenses =
        match cli_options.licenses with
        | license :: additional_licenses -> (license, additional_licenses)
        | [] -> List.(hd License.known, tl License.known)
      in
      let all_path_ok =
        List.fold_left
          (fun res path ->
            let path_ok =
              match
                with_open_in path (check_license ~additional_licenses license)
              with
              | Result.Ok license ->
                  if cli_options.verbose then
                    Printf.printf "%s: %s\n%!" path (License.pp license) ;
                  true
              | Result.Error {line_no; message} ->
                  Printf.eprintf
                    "%s: does not have a correct license header.\n"
                    path ;
                  if cli_options.verbose then
                    Printf.eprintf
                      "Line %s: %s\n"
                      (match line_no with
                      (* The first line is line 0, hence the addition *)
                      | Some n -> string_of_int (n + 1)
                      | None -> "?")
                      message ;
                  false
            in
            path_ok && res)
          true
          paths
      in
      if not all_path_ok then exit 1
