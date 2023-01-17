(** The chain starts from a genesis block associated to a seed protocol *)
type t = {
  time : Time.Protocol.t;
  block : Tezos_crypto.Hashed.Block_hash.t;
  protocol : Tezos_crypto.Hashed.Protocol_hash.t;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {time; block; protocol} -> (time, block, protocol))
    (fun (time, block, protocol) -> {time; block; protocol})
    (obj3
       (req "timestamp" Time.Protocol.encoding)
       (req "block" Tezos_crypto.Hashed.Block_hash.encoding)
       (req "protocol" Tezos_crypto.Hashed.Protocol_hash.encoding))

let equal g1 g2 =
  let {time = t1; block = b1; protocol = p1} = g1 in
  let {time = t2; block = b2; protocol = p2} = g2 in
  Time.Protocol.equal t1 t2
  && Tezos_crypto.Hashed.Block_hash.equal b1 b2
  && Tezos_crypto.Hashed.Protocol_hash.equal p1 p2

let pp ppf genesis =
  Data_encoding.Json.pp ppf (Data_encoding.Json.construct encoding genesis)

module Parameters = struct
  type t = {context_key : string; values : Data_encoding.json}

  let encoding =
    let open Data_encoding in
    conv
      (fun {context_key; values} -> (context_key, values))
      (fun (context_key, values) -> {context_key; values})
      (obj2 (dft "context_key" string "sandbox_parameter") (req "values" json))
end
