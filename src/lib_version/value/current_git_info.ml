(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* When we (or the CI) run "git archive", git substitutes the dollar-format part
   because this file is marked as "export-subst" in ".gitattributes".

   To know whether we are in a Git repository or in an archive, we check whether
   the string was substituted. Thanks to this, we know whether we should get the
   hash from Generated_git_info (not available in archives) or not. *)

(* The $Format string is substituted by git with attributes and export-subst *)
let raw_commit_hash = "$Format:%H$"

let commit_hash =
  if String.equal raw_commit_hash ("$Format" ^ ":%H$") then
    Generated_git_info.commit_hash
  else raw_commit_hash

let abbreviated_commit_hash =
  if String.length commit_hash >= 8 then String.sub commit_hash 0 8
  else commit_hash

(* The $Format string is substituted by git with attributes and export-subst *)
let raw_committer_date = "$Format:%ci$"

let committer_date =
  if String.equal raw_committer_date ("$Format" ^ ":%ci$") then
    Generated_git_info.committer_date
  else raw_committer_date

let octez_version = Generated_git_info.git_describe_octez

let octez_evm_node_version = Generated_git_info.git_describe_octez_evm_node
