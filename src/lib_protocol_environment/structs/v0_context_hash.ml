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

include Tezos_crypto.Context_hash

module Set = struct
  include Stdlib.Set.Make (Tezos_crypto.Context_hash)

  let encoding =
    Data_encoding.conv
      elements
      (fun l -> List.fold_left (fun m x -> add x m) empty l)
      Data_encoding.(list Tezos_crypto.Context_hash.encoding)
end

module Map = struct
  include Stdlib.Map.Make (Tezos_crypto.Context_hash)

  let encoding arg_encoding =
    Data_encoding.conv
      bindings
      (fun l -> List.fold_left (fun m (k, v) -> add k v m) empty l)
      Data_encoding.(
        list (tup2 Tezos_crypto.Context_hash.encoding arg_encoding))
end

module Table = struct
  include Stdlib.Hashtbl.MakeSeeded (struct
    include Tezos_crypto.Context_hash

    let hash = Stdlib.Hashtbl.seeded_hash
  end)

  let encoding arg_encoding =
    Data_encoding.conv
      (fun h -> fold (fun k v l -> (k, v) :: l) h [])
      (fun l ->
        let h = create (List.length l) in
        List.iter (fun (k, v) -> add h k v) l ;
        h)
      Data_encoding.(
        list (tup2 Tezos_crypto.Context_hash.encoding arg_encoding))
end
