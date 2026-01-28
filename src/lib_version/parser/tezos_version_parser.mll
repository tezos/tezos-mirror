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

{
  (* for the doc of this structure refer to the file version.mli *)
  type additional_info =
    | Dev
    | Beta of int
    | Beta_dev of int
    | RC of int
    | RC_dev of int
    | Release [@@deriving show]

  type product = Octez | Octez_evm_node | Octez_smart_rollup_node

  let pp_product ppf = function
  | Octez -> Format.fprintf ppf "Octez"
  | Octez_evm_node -> Format.fprintf ppf "Octez_evm_node"
  | Octez_smart_rollup_node -> Format.fprintf ppf "Octez_smart_rollup_node"

  let product_of_string = function
  | "octez" -> Octez
  | "octez-evm-node" -> Octez_evm_node
  | "octez-smart-rollup-node" -> Octez_smart_rollup_node
  | _ -> assert false

  type t = {
    product: product;
    major : int;
    minor : int;
    build: int;
    additional_info : additional_info} [@@deriving show]

  let default = { product = Octez; major = 0 ; minor = 0 ; build = 0 ; additional_info = Dev }

}

let num = ['0'-'9']+
let hexa = ['0'-'9' 'A'-'F' 'a'-'f']+
let product = ("octez" | "octez-evm-node" | "octez-smart-rollup-node")

(* This function is used to parse multiple formats:
   - the result of git describe (of the form TAG or TAG-COMMITS-HASH);
   - the --node-version-allowed argument of the agnostic baker (of the form TAG or TAG:HASH).
   TODO: instead of mixing the two, have --node-version-allowed be parsed
   by using [String.split_on_char ':'], and *then* use this parser.

   Some tests in test_parser.ml also parse with "+dev",
   which makes it look like this parser is expected to also be able
   to parse version numbers, except that those tests also test
   versions with "-rc" instead of "~rc", so they are not real version numbers,
   but "pseudo" version numbers.
   TODO: find out where we actually need to parse version numbers outside of tests,
   and why we're not expecting tildes. *)
rule parse_git_describe_or_node_version_allowed_exn = parse
  | (* Prefix that identifies the tag as a version tag. *)
    (product as product) '-' 'v'?
    (* Version number. *)
    (num as major) ('.' (num as minor))? ".0"?
    (* Additional information for versions that are not actually releases. *)
    ('-' ("rc" | "beta" as extra_kind) (num as extra_num))?
    (* Build number. *)
    ('-' (num as build))?
    (* What follows depends on what we are parsing. *)
    (
      (* When parsing --node-version-allowed, we expect the following. *)
      ':' (hexa as commit_hash)
      (* When parsing git describe, we expect the following (or nothing). *)
    | '-' (num as additional_commits) "-g" (hexa as commit_hash)
      (* When parsing pseudo version numbers, we can find the following. *)
    | ("+dev" as plus_dev)
    )?
    (* There must be nothing after that. *)
    eof
    {
      (* Convert strings. *)
      let product = product_of_string product in
      let major = int_of_string major in
      let minor = Option.map int_of_string minor |> Option.value ~default:0 in
      let build = Option.map int_of_string build |> Option.value ~default:0 in
      let additional_info_without_dev =
        match extra_kind, extra_num with
          | Some _, None | None, Some _ ->
              (* Cannot happen because both [extra_kind] and [extra_num]
                 are below the same [?]. *)
              assert false
          | None, None ->
              Release
          | Some "rc", Some num ->
              RC (int_of_string num)
          | Some "beta", Some num ->
              Beta (int_of_string num)
          | Some _, Some _ ->
              (* Cannot happen because the regexp only accepts "rc" and "beta". *)
              assert false
      in
      let additional_info =
        match additional_commits, plus_dev with
          | None, None ->
              additional_info_without_dev
          | _ ->
              match additional_info_without_dev with
                | Release -> Dev
                | RC n -> RC_dev n
                | Beta n -> Beta_dev n
                | Dev | RC_dev _ | Beta_dev _ as x -> x (* not supposed to happen *)
      in
      Some ({ product; major; minor; build; additional_info }, commit_hash)
    }
  | _ { None }

{
  let version_commit lexbuf =
    try
      parse_git_describe_or_node_version_allowed_exn lexbuf
    with Failure _ ->
      (* Raised by [int_of_string] for integers that are too large. *)
      None

  let version_tag lexbuf =
    Option.map fst (version_commit lexbuf)
}
