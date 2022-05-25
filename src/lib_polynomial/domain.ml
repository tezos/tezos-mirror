module Fr = Bls12_381.Fr

module Stubs = struct
  type fr = Fr.t

  type fr_array = Carray.Stubs.fr_array

  (** [compute_domain res n g] computes [[one; g; ..; g^{n-1}]] for a given
  blst_fr element [g]

  - requires: [n] is even and [0 < n <= size res]
  - ensures: [res[i] = g^i] for [i = 0..(n-1)] *)
  external compute_domain : fr_array -> int -> fr -> unit
    = "caml_polynomial_compute_domain_stubs"
    [@@noalloc]
end

module Domain_impl = struct
  type scalar = Bls12_381.Fr.t

  type t = Carray.t

  let of_carray : Carray.t -> t = fun x -> x

  let to_carray : t -> Carray.t = fun x -> x

  let of_array = Carray.of_array

  let to_array = Carray.to_array

  let length = Carray.length

  let get = Carray.get

  let create log root_of_unity =
    let n = 1 lsl log in
    let domain = Carray.allocate n in
    Stubs.compute_domain domain n root_of_unity ;
    (domain, n)

  let build ~log =
    let module Fr_g = Fr_generation.Make (Fr) in
    let root_u = Fr_g.root_of_unity log in
    create log root_u

  let subgroup ~log d =
    let n = 1 lsl log in
    let dom = Array.init n (fun _ -> Bls12_381.Fr.(copy zero)) in
    for i = 0 to n - 1 do
      dom.(i) <- Carray.get d (i * (length d / n))
    done ;
    Carray.of_array dom
end

module type Domain_sig = sig
  type scalar

  type t

  val to_array : t -> scalar array

  (** [length p] returns the length of a given array [p] *)
  val length : t -> int

  (** [get p i] returns the [i]-th element of a given array [p] *)
  val get : t -> int -> scalar

  (** [build log] computes [[one; g; ..; g^{n-1}]] where [g] is a primitive
    [n]-th root of unity and [n = 2^log] *)
  val build : log:int -> t

  val subgroup : log:int -> t -> t
end

module type Domain_unsafe_sig = sig
  include Domain_sig

  val to_carray : t -> Carray.t

  val of_carray : Carray.t -> t

  val to_array : t -> scalar array

  val of_array : scalar array -> t
end

module Domain_unsafe : Domain_unsafe_sig with type scalar = Bls12_381.Fr.t =
  Domain_impl

include (
  Domain_unsafe :
    Domain_sig
      with type t = Domain_unsafe.t
       and type scalar = Domain_unsafe.scalar)
