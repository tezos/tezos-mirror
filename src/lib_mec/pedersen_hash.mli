module MakePedersenHash : functor
  (Ec : Ec_sig.BASE)
  (Params : sig
     val generators : Ec.t list

     val chunks_per_generator : int
   end)
  -> sig
  val hash : Iterator.Bit.t -> Ec.t
end

module Zcash : sig
  val hash : Iterator.Bit.t -> Jubjub.AffineEdwards.t
end
