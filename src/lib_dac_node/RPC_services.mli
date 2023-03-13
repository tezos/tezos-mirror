val post_store_preimage :
  Dac_plugin.t ->
  ( [`POST],
    'a,
    'a,
    unit,
    Bytes.t * Pagination_scheme.t,
    Dac_plugin.hash * Bytes.t )
  Tezos_rpc.Service.service

val get_verify_signature :
  ([`GET], unit, unit, string option, unit, bool) Tezos_rpc.Service.service

val get_preimage :
  Dac_plugin.t ->
  ( [`GET],
    'a,
    'a * Dac_plugin.hash,
    unit,
    unit,
    Bytes.t )
  Tezos_rpc.Service.service

val put_dac_member_signature :
  Dac_plugin.t ->
  ([`PUT], 'a, 'a, unit, Signature_repr.t, unit) Tezos_rpc.Service.service

val get_certificate :
  Dac_plugin.t ->
  ( [`GET],
    'a,
    'a * Dac_plugin.hash,
    unit,
    unit,
    Certificate_repr.t option )
  Tezos_rpc.Service.service

module Coordinator : sig
  val post_preimage :
    Dac_plugin.t ->
    ([`POST], 'a, 'a, unit, Bytes.t, Dac_plugin.hash) Tezos_rpc.Service.service
end
