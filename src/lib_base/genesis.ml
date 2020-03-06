(** The chain starts from a genesis block associated to a seed protocol *)
type t = {
  time : Time.Protocol.t;
  block : Block_hash.t;
  protocol : Protocol_hash.t;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {time; block; protocol} -> (time, block, protocol))
    (fun (time, block, protocol) -> {time; block; protocol})
    (obj3
       (req "timestamp" Time.Protocol.encoding)
       (req "block" Block_hash.encoding)
       (req "protocol" Protocol_hash.encoding))

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
