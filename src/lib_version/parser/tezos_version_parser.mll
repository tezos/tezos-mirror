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

  type t = {
    major : int;
    minor : int;
    additional_info : additional_info} [@@deriving show]

  let int s = int_of_string_opt s |> Option.value ~default: 0

  let default = { major = 0 ; minor = 0 ; additional_info = Dev }
}

let num = ['0'-'9']+

rule version_tag = parse
  | 'v'? (num as major) '.' (num as minor) ".0"?
      { Some {
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
