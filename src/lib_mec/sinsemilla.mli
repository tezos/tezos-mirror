open Pallas

exception Bottom

module MakeSinsemilla : functor
  (Params : sig
     val iv : Affine.t

     val generators : Affine.t array

     val chunk_size : int
   end)
  -> sig
  (** Can raise [Bottom] *)
  val hash_exn : Iterator.Bit.t -> Affine.Base.t

  val hash_opt : Iterator.Bit.t -> Affine.Base.t option
end

module Zcash : sig
  (** [hash_exn initial_value iterator] is the instantiation of Sinsemilla with
      the Zcash generators and a chunk size of 10
  *)
  val hash_exn : Affine.t -> Iterator.Bit.t -> Affine.Base.t

  (** [hash_opt initial_value iterator] is the instantiation of Sinsemilla with
      the Zcash generators and a chunk size of 10
  *)
  val hash_opt : Affine.t -> Iterator.Bit.t -> Affine.Base.t option
end
