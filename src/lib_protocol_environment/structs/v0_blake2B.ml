(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

include Tezos_crypto.Blake2B

module Make (Register : sig
  val register_encoding :
    prefix:string ->
    length:int ->
    to_raw:('a -> string) ->
    of_raw:(string -> 'a option) ->
    wrap:('a -> Tezos_crypto.Base58.data) ->
    'a Tezos_crypto.Base58.encoding
end)
(Name : PrefixedName) =
struct
  include Tezos_crypto.Blake2B.Make (Register) (Name)

  module Set = struct
    include Stdlib.Set.Make (struct
      type nonrec t = t

      let compare = compare
    end)

    let encoding =
      Data_encoding.conv
        elements
        (fun l -> List.fold_left (fun m x -> add x m) empty l)
        Data_encoding.(list encoding)
  end

  module Map = struct
    include Stdlib.Map.Make (struct
      type nonrec t = t

      let compare = compare
    end)

    let encoding arg_encoding =
      Data_encoding.conv
        bindings
        (fun l -> List.fold_left (fun m (k, v) -> add k v m) empty l)
        Data_encoding.(list (tup2 encoding arg_encoding))
  end
end
