open Numerics.Sigs
open Result

exception Series_error of error

type 'a elt = Float_elt : float elt | Complex_elt : Complex.t elt

let float_elt = Float_elt

let complex_elt = Complex_elt

type (_, _) representation =
  | Dense :
      { elt : 'e elt; vec : ('e, 'a) vector_impl }
      -> ('e, 'a) representation
  | Fun_index : ('e, int -> 'e) representation
  | Fun_time : ('e, float -> 'e) representation

let dense_float64 = Dense { elt = Float_elt; vec = Numerics.Float64.vec }

let dense_complex64 = Dense { elt = Complex_elt; vec = Numerics.Complex64.vec }

let fun_index = Fun_index

let fun_time = Fun_time

type error +=
  | Grid_dimensions_mismatch : 'a Grid.t -> error
  | Incompatible_representations :
      (_, _) representation * (_, _) representation
      -> error

type ('g, 'elt, 'data) t =
  { grid : 'g Grid.t; data : 'data; repr : ('elt, 'data) representation }

(* -------------------------------------------------------------------------- *)
(* Getters *)

let grid : type g elt arr. (g, elt, arr) t -> g Grid.t = fun { grid; _ } -> grid

let data : type g elt arr. (g, elt, arr) t -> arr = fun { data; _ } -> data

let length : type g e a. (g, e, a) t -> int = fun { grid; _ } -> Grid.count grid

(* -------------------------------------------------------------------------- *)
(* Helpers *)

let index_to_time : type g e. g Grid.t -> (int -> e) -> float -> e =
 fun grid f (t : float) ->
  match Grid.index_from_real grid t with Error e -> fail e | Ok i -> f i

let time_to_index : type g e. g Grid.t -> (float -> e) -> int -> e =
 fun grid f (i : int) ->
  match Grid.real_from_index grid i with Error e -> fail e | Ok t -> f t

(* -------------------------------------------------------------------------- *)

let make : type g e a. (e, a) representation -> g Grid.t -> a -> (g, e, a) t =
 fun repr grid data ->
  match repr with
  | Dense { vec; _ } ->
      let module Vector = (val vec) in
      if Grid.count grid <> Vector.length data then
        raise (Series_error (Grid_dimensions_mismatch grid)) ;
      { grid; data; repr }
  | Fun_index -> { grid; data; repr }
  | Fun_time -> { grid; data; repr }

let discretise :
    type e a g. (e, a) representation -> (float -> e) -> g Grid.t -> (g, e, a) t
    =
 fun repr f grid ->
  match repr with
  | Dense { vec; _ } ->
      let module Vector = (val vec) in
      let data =
        Vector.init (Grid.count grid) (fun i ->
            match Grid.real_from_index grid i with
            | Error e -> fail e
            | Ok t -> f t)
      in
      { data; grid; repr }
  | Fun_index -> { data = time_to_index grid f; grid; repr }
  | Fun_time -> { data = f; grid; repr }

let discretise_i :
    type e a g. (e, a) representation -> (int -> e) -> g Grid.t -> (g, e, a) t =
 fun repr f grid ->
  match repr with
  | Dense { vec; _ } ->
      let module Vector = (val vec) in
      let data = Vector.init (Grid.count grid) f in
      { data; grid; repr }
  | Fun_index -> { data = f; grid; repr }
  | Fun_time -> { data = index_to_time grid f; grid; repr }

let eval_t : type g e a. (g, e, a) t -> float -> e =
 fun series time ->
  match series.repr with
  | Dense { vec; _ } -> (
      let module Vector = (val vec) in
      match Grid.index_from_real (grid series) time with
      | Error e -> fail e
      | Ok i -> Vector.get (data series) i )
  | Fun_index ->
      let f = index_to_time series.grid series.data in
      f time
  | Fun_time ->
      let f = data series in
      f time

let eval_i : type g e a. (g, e, a) t -> int -> e =
 fun series index ->
  match series.repr with
  | Dense { vec; _ } ->
      let module Vector = (val vec) in
      Vector.get (data series) index
  | Fun_index ->
      let f = series.data in
      f index
  | Fun_time ->
      let f = time_to_index series.grid series.data in
      f index

let map :
    type g e1 a1 e2 a2.
    (e2, a2) representation -> (e1 -> e2) -> (g, e1, a1) t -> (g, e2, a2) t =
 fun repr f series ->
  match series.repr with
  | Dense { vec = (module V1); _ } -> (
      let data = data series in
      let len = V1.length data in
      match repr with
      | Dense { vec = (module V2); _ } ->
          let res = V2.create len in
          for i = 0 to len - 1 do
            V2.unsafe_set res i (f (V1.unsafe_get data i))
          done ;
          make repr (grid series) res
      | Fun_time ->
          let f = index_to_time (grid series) (fun i -> f (V1.get data i)) in
          make repr (grid series) f
      | Fun_index -> make repr (grid series) (fun i -> f (V1.get data i)) )
  | Fun_time ->
      let s = data series in
      discretise repr (fun t -> f (s t)) (grid series)
  | Fun_index ->
      let s = data series in
      discretise_i repr (fun i -> f (s i)) (grid series)

let lift2 :
    type g e a.
    (a -> a -> a) -> (g, e, a) t -> (g, e, a) t -> (g, e, a) t result =
 fun f s1 s2 ->
  if not (Grid.equal s1.grid s2.grid) then
    Error [Grid_dimensions_mismatch s1.grid; Grid_dimensions_mismatch s2.grid]
  else
    let repr1 = s1.repr in
    let repr2 = s2.repr in
    match repr1 with
    | Dense { elt = _elt1; vec = _vec1 } -> (
        match repr2 with
        | Dense { elt = _elt2; vec = _vec2 } ->
            (* we know there is only one dense data representation
               per element type. *)
            Ok { grid = s1.grid; data = f s1.data s2.data; repr = s1.repr }
        | _ -> Error [Incompatible_representations (repr1, repr2)] )
    | Fun_index -> (
        match repr2 with
        | Fun_index ->
            Ok { grid = s1.grid; data = f s1.data s2.data; repr = s1.repr }
        | _ -> Error [Incompatible_representations (repr1, repr2)] )
    | Fun_time -> (
        match repr2 with
        | Fun_time ->
            Ok { grid = s1.grid; data = f s1.data s2.data; repr = s1.repr }
        | _ -> Error [Incompatible_representations (repr1, repr2)] )

(* WIP *)
(* let increments : type g a. (g, float, a) t -> (g, float, a) t =
 *   fun series ->
 *   let _new_grid : g grid =
 *     match grid series with
 *     | Regular { step ; start ; count } ->
 *       Grid.regular ~step ~start ~count:(count - 1)
 *     | Explicit { times } ->
 *       let times =
 *         Array.init (Array.length times - 1) (fun i -> times.(i)) in
 *       Grid.explicit ~times
 *   in
 *   assert false *)
