(** The chain starts from a genesis block associated to a seed protocol *)
type t = {
  time : Time.Protocol.t;
  block : Block_hash.t;
  protocol : Protocol_hash.t;
}

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit
