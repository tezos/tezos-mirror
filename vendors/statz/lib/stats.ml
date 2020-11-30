open Structures

(* Generative probability *)
type 'a gen = unit -> 'a

(* Empirical probability *)
type 'a emp = 'a array

module Floatarray = Stdlib.Float.Array

(* Finitely supported function *)
module type Fin_fun = sig
  type t

  module O : Ordered with type t = t

  module M : Map.S with type key = t

  val support : t array

  val weights : Floatarray.t

  val weightmap : float M.t
end

type 'a fin_fun = (module Fin_fun with type t = 'a)

type 'a fin_den = 'a fin_fun

(* A finitely supported probability is normalized. *)
type 'a fin_prb = 'a fin_fun

let rec compare_array cmp arr1 arr2 =
  let len1 = Array.length arr1 in
  let len2 = Array.length arr2 in
  let c = Int.compare len1 len2 in
  if c <> 0 then c else compare_array_loop cmp arr1 arr2 len1 0

and compare_array_loop cmp arr1 arr2 len i =
  if i = len then 0
  else
    let c = cmp arr1.(i) arr2.(i) in
    if c <> 0 then c else compare_array_loop cmp arr1 arr2 len (i + 1)

let rec compare_floatarray arr1 arr2 =
  let len1 = Floatarray.length arr1 in
  let len2 = Floatarray.length arr2 in
  let c = Stdlib.Int.compare len1 len2 in
  if c <> 0 then c else compare_farray_loop arr1 arr2 len1 0

and compare_farray_loop arr1 arr2 len i =
  if i = len then 0
  else
    let c =
      Stdlib.Float.compare
        (Floatarray.unsafe_get arr1 i)
        (Floatarray.unsafe_get arr2 i)
    in
    if c <> 0 then c else compare_farray_loop arr1 arr2 len (i + 1)

let compare_prb : type a. a fin_prb -> a fin_prb -> int =
 fun (module Prob1) (module Prob2) ->
  let c = compare_floatarray Prob1.weights Prob2.weights in
  if c <> 0 then c
  else compare_array Prob1.O.compare Prob1.support Prob2.support

(* Sampling *)
let sample_gen : 'a gen -> 'a = fun f -> f ()

let sample_emp : 'a emp -> 'a =
 fun data ->
  let len = Random.int (Array.length data) in
  data.(len)

let sample_prb : type a. a fin_prb -> a =
  fun (type t) (module P : Fin_fun with type t = t) ->
   let r = Random.float 1.0 in
   let rec loop (i : int) cumu =
     let cumu = cumu +. Floatarray.unsafe_get P.weights i in
     if r <= cumu then P.support.(i) else loop (i + 1) cumu
   in
   loop 0 0.0

(* Mapping *)
let map_gen : ('a -> 'b) -> 'a gen -> 'b gen = fun f prob () -> f (prob ())

let map_emp : ('a -> 'b) -> 'a emp -> 'b emp = Array.map

let generative ~sampler = sampler

let density (type t) (module O : Ordered with type t = t)
    (elements : (t * float) list) : t fin_den =
  let (points, weights) = List.split elements in
  ( module struct
    type nonrec t = t

    module O = O
    module M = Map.Make (O)

    let support = Array.of_list points

    let weights = Floatarray.of_list weights

    let weightmap =
      List.fold_left (fun map (x, w) -> M.add x w map) M.empty elements
  end )

let total_mass (type t) ((module D) : t fin_den) : float =
  let mass = ref 0.0 in
  for i = 0 to Floatarray.length D.weights - 1 do
    mass := !mass +. Floatarray.unsafe_get D.weights i
  done ;
  !mass

let normalize (type t) ((module D) : t fin_den) : t fin_prb =
  let mass = total_mass (module D) in
  let imass = 1.0 /. mass in
  ( module struct
    include D

    let weights = Floatarray.map (fun x -> x *. imass) weights

    let weightmap = M.map (fun x -> x *. imass) weightmap
  end )

let empirical_of_generative ~nsamples sampler =
  Array.init nsamples (fun _ -> sampler ())

let empirical_of_raw_data (x : 'a array) = x

let subsample ~n sampler : 'a gen =
  let counter = ref 0 in
  let rec loop () =
    let res = sample_gen sampler in
    incr counter ;
    if !counter mod n = 0 then res else loop ()
  in
  loop

let sorted_array_to_mset (type t) (module O : Ordered with type t = t)
    (arr : t array) =
  let (prev, prev_acc) =
    Array.fold_left
      (fun (prev, prev_acc) elt ->
        match prev with
        | None -> (Some (elt, 1), prev_acc)
        | Some (prev_elt, prev_count) ->
            let c = O.compare prev_elt elt in
            if c = 0 then (Some (prev_elt, prev_count + 1), prev_acc)
            else (Some (elt, 1), (prev_elt, prev_count) :: prev_acc))
      (None, [])
      arr
  in
  match prev with
  | None -> failwith "sorted_array_to_mset: empty array"
  | Some last_occ -> List.rev (last_occ :: prev_acc)

let fin_prb_of_empirical (type t) (module O : Ordered with type t = t)
    (p : t emp) : t fin_den =
  Array.sort O.compare p ;
  let counts = sorted_array_to_mset (module O) p in
  let counts = List.map (fun (x, count) -> (x, float count)) counts in
  normalize (density (module O) counts)

let uniform (type t) (module O : Ordered with type t = t) (arr : t array) :
    t fin_prb =
  let len = Array.length arr in
  if len = 0 then failwith "uniform: empty array"
  else
    let prb = 1.0 /. float len in
    ( module struct
      type nonrec t = t

      module O = O
      module M = Map.Make (O)

      let support = arr

      let weights = Floatarray.make len prb

      let weightmap = Array.fold_left (fun map x -> M.add x prb map) M.empty arr
    end )

let eval_prb (type t) ((module P) : t fin_prb) (x : t) : float =
  match P.M.find_opt x P.weightmap with None -> 0.0 | Some w -> w

let int_compare (i1 : int) (i2 : int) =
  if i1 < i2 then -1 else if i1 = i2 then 0 else 1

let empirical_float_random ~(nsamples : int) ~(samplers : float gen array) :
    float emp array =
  let length = Array.length samplers in
  let random =
    Array.init length (fun i ->
        let key = Random.bits () in
        let smp = samplers.(i) in
        (i, key, smp))
  in
  Array.sort (fun (_, k1, _) (_, k2, _) -> int_compare k1 k2) random ;
  let samplers = Array.map (fun (_, _, s) -> s) random in
  let samples = Array.init length (fun _ -> Array.make nsamples 0.0) in
  for i = 0 to nsamples - 1 do
    for j = 0 to length - 1 do
      samples.(j).(i) <- samplers.(j) ()
    done
  done ;
  (* invert random perm *)
  let samples =
    Array.map2 (fun (i, _, _) samples -> (i, samples)) random samples
  in
  Array.sort (fun (i1, _) (i2, _) -> int_compare i1 i2) samples ;
  Array.map (fun (_, arr) -> arr) samples

let truncate (type elt) (module O : Ordered with type t = elt)
    (distribution : elt emp) (p : float) =
  Array.sort O.compare distribution ;
  let len = Array.length distribution in
  let plen = int_of_float (p *. float len) in
  Array.sub distribution 0 plen

let quantile (type elt) (module O : Ordered with type t = elt)
    (distribution : elt emp) (p : float) =
  Array.sort O.compare distribution ;
  let len = Array.length distribution in
  let plen = int_of_float (p *. float len) in
  distribution.(plen)

let mean (type elt) (module L : Linear with type t = elt)
    (distribution : elt emp) =
  let ilen = 1.0 /. float (Array.length distribution) in
  let sum = Array.fold_left L.( + ) L.zero distribution in
  L.(ilen * sum)

let variance (distribution : float emp) =
  let mean = mean (module Float) distribution in
  let ilen = 1.0 /. float (Array.length distribution) in
  let sum =
    Array.fold_left
      (fun acc elt -> acc +. ((elt -. mean) ** 2.0))
      0.0
      distribution
  in
  ilen *. sum

let remove_outliers ~nsigmas (dist : float emp) =
  let mean = mean (module Float) dist in
  let var = variance dist in
  let std = sqrt var in
  let delta = std *. nsigmas in
  dist |> Array.to_list
  |> List.filter (fun x -> abs_float (x -. mean) <= delta)
  |> Array.of_list

let raw_data_empirical distribution = `Empirical distribution

let raw_data_density (type t) ((module D) : t fin_den) =
  let den =
    List.combine (Array.to_list D.support) (Floatarray.to_list D.weights)
  in
  `Density den

let raw_data_probability (type t) ((module D) : t fin_prb) =
  let den =
    List.combine (Array.to_list D.support) (Floatarray.to_list D.weights)
  in
  `Probability den

let pp_fin_fun :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a fin_den -> unit =
 fun kf f den ->
  let (`Density l) = raw_data_density den in
  Format.fprintf
    f
    "@[<h>%a@]"
    (Format.pp_print_list (fun elt_fmt (elt, pr) ->
         Format.fprintf elt_fmt "(%a, %f);@;" kf elt pr))
    l

let proj_ordered : 'a fin_fun -> (module Ordered with type t = 'a) =
  fun (type t) ((module F) : t fin_fun) -> (module F.O : Ordered with type t = t)

let inv_loge_2 = 1. /. log 2.

let log2 x = log x *. inv_loge_2

let shannon_entropy ((module Prob) : float fin_prb) =
  let weights = Prob.weights in
  let acc = ref 0.0 in
  for i = 0 to Floatarray.length weights - 1 do
    let pi = Floatarray.unsafe_get weights i in
    acc := !acc +. (pi *. log2 pi)
  done ;
  ~-. (!acc)

let coin ~bias : bool fin_prb =
  if bias < 0.0 || bias > 1.0 then failwith "Stats.coin: invalid bias"
  else density (module Bool) [(true, bias); (false, 1. -. bias)]

let exponential ~rate : float gen =
 fun () ->
  let u = Random.float 1.0 in
  ~-.(log u) /. rate

let box_muller : mean:float -> std:float -> (float * float) gen =
  let rec reject_loop () =
    let u = Random.float 2.0 -. 1.0 in
    let v = Random.float 2.0 -. 1.0 in
    let s = (u *. u) +. (v *. v) in
    if s = 0.0 || s >= 1.0 then reject_loop ()
    else
      let weight = sqrt (-2. *. log s /. s) in
      let variate1 = u *. weight in
      let variate2 = v *. weight in
      (variate1, variate2)
  in
  fun ~mean ~std () ->
    let (v1, v2) = reject_loop () in
    (mean +. (std *. v1), mean +. (std *. v2))

type gaussgen_state = Fresh | Last of float

let gaussian ~mean ~std : float gen =
  let state = ref Fresh in
  let gen = box_muller ~mean ~std in
  fun () ->
    match !state with
    | Fresh ->
        let (x1, x2) = sample_gen gen in
        state := Last x2 ;
        x1
    | Last x ->
        state := Fresh ;
        x

let bincoeff n k =
  let n = float n in
  let rec loop i acc =
    if i = k + 1 then acc
    else
      let fi = float i in
      loop (i + 1) (acc *. (n +. 1. -. fi) /. fi)
  in
  loop 1 1.0

let binomial (coin : bool fin_prb) n =
  let p = eval_prb coin true in
  let not_p = eval_prb coin false in
  let elements =
    let fn = float n in
    List.init n (fun k ->
        let fk = float k in
        (k, bincoeff n k *. (p ** fk) *. (not_p ** (fn -. fk))))
  in
  density (module Int) elements
