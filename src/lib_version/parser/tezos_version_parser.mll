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

  type product = Octez | Octez_evm_node

  let pp_product ppf = function
  | Octez -> Format.fprintf ppf "Octez"
  | Octez_evm_node -> Format.fprintf ppf "Octez_evm_node"

  type t = {
    product: product;
    major : int;
    minor : int;
    additional_info : additional_info} [@@deriving show]

  let int s = int_of_string_opt s |> Option.value ~default: 0

  let default = { product = Octez; major = 0 ; minor = 0 ; additional_info = Dev }

}

let num = ['0'-'9']+
let hexa = ['0'-'9' 'A'-'F' 'a'-'f']+

rule version_tag = parse
  | ("octez" | "octez-evm-node" as product) "-" 'v'? (num as major) '.' (num as minor) ".0"?
      {
        let product = match product with 
          | "octez-evm-node" -> Octez_evm_node
          | "octez" -> Octez
          | _ -> (* this case cannot happen, see pattern above *)  
                 assert false
        in
        Some {
        product;
        major = int major;
        minor = int minor;
        additional_info = extra lexbuf }
      }
  | _ | eof
      { None }

and extra = parse
  | "-rc" (num as rc) eof
      { (RC (int rc)) }
  | "-rc" (num as rc) _
      { (RC_dev (int rc)) }
  | "-beta" (num as beta) eof
      { (Beta (int beta)) }
  | "-beta" (num as beta) _
      { (Beta_dev (int beta)) }
  | eof
      { Release }
  | _
      { Dev }

and version_commit = parse 
  | ("octez" | "octez-evm-node" as product) "-" 'v'? (num as major) '.' (num as minor) ".0"?
      {
        let product = match product with 
          | "octez-evm-node" -> Octez_evm_node
          | "octez" -> Octez
          | _ -> (* this case cannot happen, see pattern above *)  
                 assert false
        in
        let extra = extra_noeof lexbuf in
        match extra with
        | None -> None
        | Some additional_info ->
          (let commit = commit lexbuf in
          match commit with
          | Some commit ->
            Some (
              {
                product;
                major = int major;
                minor = int minor;
                additional_info;
              },
            commit)
          | _ -> None)
      }
  | _ | eof
      { None }

(* This rule is similar to rule extra, but can be followed by a commit hash *)
and extra_noeof = parse
  | "-rc" (num as rc) (eof | ':')
      { Some (RC (int rc)) }
  | "-rc" (num as rc) "+dev" (eof | ':')
      { Some (RC_dev (int rc)) }
  | "-beta" (num as beta) (eof | ':')
      { Some (Beta (int beta)) }
  | "-beta" (num as beta) "+dev" (eof | ':')
      { Some (Beta_dev (int beta)) }
  | "+dev" (eof | ':')
      { Some Dev }
  | (eof | ':')
      { Some Release }
  | _
      { None }

and commit = parse
  | (hexa as hash) eof
      { let l = String.length hash in
        if l >= 8 && l <= 40
        then Some (Some (String.lowercase_ascii hash))
        else None
      }
  | eof
      { Some None }
  | _
      { None }
