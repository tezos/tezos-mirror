(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

module Address = struct
  module Entity = struct
    include Tezos_crypto.Hashed.Smart_rollup_address

    let of_source s =
      let open Lwt_result_syntax in
      match of_b58check_opt s with
      | Some address -> return address
      | None -> tzfail @@ error_of_fmt "bad smart rollup address notation"

    let to_source s = Lwt_result.return (to_b58check s)

    let name = "smart rollup"
  end

  include Client_aliases.Alias (Entity)

  let parse cctxt s =
    Client_aliases.parse_alternatives
      [
        ("text", fun text -> of_source text);
        ("alias", fun alias -> find cctxt alias);
      ]
      s

  let parameter () = Tezos_clic.parameter ~autocomplete parse

  let param ?(name = "smart rollup address")
      ?(desc = "the address of the targeted smart rollup") next =
    let desc =
      String.concat
        "\n"
        [
          desc;
          "Can be a literal or an alias (autodetected in order).\n\
           Use 'alias:name' or 'text:literal' to force.";
        ]
    in
    Tezos_clic.param ~name ~desc (parameter ()) next
end
