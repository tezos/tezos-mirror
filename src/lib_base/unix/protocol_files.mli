open Error_monad

val read_dir :
  string -> (Tezos_crypto.Protocol_hash.t option * Protocol.t) tzresult Lwt.t

val write_dir :
  string ->
  ?hash:Tezos_crypto.Protocol_hash.t ->
  Protocol.t ->
  unit tzresult Lwt.t
