(** The chain starts from a genesis block associated to a seed protocol *)
type t = {
  time : Time.Protocol.t;
  block : Block_hash.t;
  protocol : Protocol_hash.t;
}

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

module Parameters : sig
  (** Parameters for the genesis protocol.

      [context_key] is the key, in the context, which leads to the genesis parameters.
      For [proto_genesis] for instance, this is ["sandbox_parameter"].

      [values] are the actual parameters. For [proto_genesis] for instance, it should
      be a JSON object with one field: ["genesis_pubkey"] containing the activator key. *)
  type t = {context_key : string; values : Data_encoding.json}

  val encoding : t Data_encoding.t
end
