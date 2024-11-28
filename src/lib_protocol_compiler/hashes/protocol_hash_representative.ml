(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Maps protocol hashes to their representative. *)
module Representatives = Map.Make (Protocol_hash)

let register_protocol_hash computed_hash declared_hash map =
  Representatives.add
    (Protocol_hash.of_b58check_exn computed_hash)
    (Protocol_hash.of_b58check_exn declared_hash)
    map

let representatives =
  Representatives.empty
  (* Protocol used in tests only, see `tezt/test/injection.ml`. *)
  |> register_protocol_hash
       "Pry4stD6qN1ZagUX6YCHvMxA1xvSjARkdt8bhs86j74JGLoLDKN"
       "Ps8MVx2JuQaFrXpbuwSeBvXmi1xraGmXuJZHULEbPBY7mzFchxz"
  (* Known protocols. *)
  |> register_protocol_hash
       "PsERsg9nX1wo2X5jqVCzKJvoczwtbF9NtJ1XkvR8cnvhokv1EJc"
       "PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV"
  |> register_protocol_hash
       "PtMph9rT4iQxKFDvxYAcKqcQ8j45oY6uCpzedB98LfWMARxQQSW"
       "PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx"
  |> register_protocol_hash
       "PsgP5GoLQ57n2yBXG1uu6oKQf1YGYmW7w498E7S81MqheYRc5Wo"
       "Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A"
  |> register_protocol_hash
       "PssV7reTFUwrtfhfKSzAQ5MwVpfg35QNBiBBNeH4AvoAStqBBfH"
       "PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY"
  |> register_protocol_hash
       "PrqoTUFUrorf5LAt7F28SXDScpmS5CR2KuVJvzmrQtUJJZVuUaK"
       "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg"
  |> register_protocol_hash
       "Psad9fKoPh8bNtaB4vhwsnYBAeKhf5EXrQbmo7q1ozkQgGNQ3qb"
       "PtLimaPtLMwfNinJi9rCfDPWea8dFgTZ1MeJ9f1m2SRic6ayiwW"
  |> register_protocol_hash
       "PsE988pGzcDaLWsDYXKj8tZTYryfns8xQzFF58T6MHfw1HwJhbZ"
       "PtMumbai2TmsJHNGRkD8v8YDbtao7BLUC3wjASn1inAKLFCjaH1"
  |> register_protocol_hash
       "PsMmVdraV63nbGuMG1FGM2WCragcWRsb85T3duL7LEL87PH7RFU"
       "PtNairobiyssHuh87hEhfVBGCVrK3WnS8Z2FT4ymB5tAa4r1nQf"
  |> register_protocol_hash
       "Pse1yoTfQcn7CAnVpRpAQmKy88otrsdj1FJZb5RbqNbQHAbU94h"
       "ProxfordYmVfjWnRcgjWH36fW6PArwqykTFzotUxRs6gmTcZDuH"
  |> register_protocol_hash
       "PsvYZTuJyMerq4EF2yT8SETMR1D7fAqzwERDkh9B8T2cjQh5zph"
       "PtParisBxoLz5gzMmn3d9WBQNoPSZakgnkMC2VNuQ3KXfUtUQeZ"
  |> register_protocol_hash
       "PtJdFR5xNLSufXi5LmHKwstm7Dd8SDj7PEmfKmvz5B9hHs6oEtB"
       "PsParisCZo7KAh1Z1smVd9ZMZ1HHn5gkzbM94V3PLCpknFWhUAi"
  |> register_protocol_hash
       "PtSAFF8wPqJcqwFoJfQoKgQBef7b7YNAgW1LjKMdhmigC7BumiX"
       "PsQuebecnLByd3JwTiGadoG4nGWi3HYiLXUjkibeFV8dCFeVMUg"

(* Resolve is not transitive on purpose: there is no reason a protocol hash is
   represented by another protocol hash that has itself a representative. *)
let resolve_repr hash =
  Representatives.find_opt hash representatives |> Option.value ~default:hash

let equivalent protocol_hash protocol_hash' =
  let protocol_hash = resolve_repr protocol_hash in
  let protocol_hash' = resolve_repr protocol_hash' in
  Protocol_hash.equal protocol_hash protocol_hash'
