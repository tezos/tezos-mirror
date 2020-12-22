open Result

type regular = Regular

type explicit = Explicit

let _ = Regular

let _ = Explicit

type 'a t =
  | Regular : { step : float; start : int; count : int } -> regular t
  (* [start, count] are measured in [step] *)
  | Explicit : { reals : float array } -> explicit t

type search_outcome = Below | Inside of int | Above

type interval =
  | Real_interval of { start : float; stop : float }
  | Index_interval of { start : int; stop : int }

type error +=
  | Invalid_real : float -> error
  | Invalid_index : int -> error
  | Invalid_interval : interval -> error

(* -------------------------------------------------------------------------- *)
(* Helpers *)

let rec index_from_real_array (arr : float array) (t : float) (start : int)
    (stop : int) : search_outcome =
  let len = stop - start in
  if len = 1 then
    if t < arr.(start) then Below
    else if t > arr.(stop - 1) then Above
    else Inside start
  else
    let mid = start + (len / 2) in
    if t < arr.(mid) then index_from_real_array arr t start mid
    else index_from_real_array arr t mid stop

(* -------------------------------------------------------------------------- *)

let equal : type a. a t -> a t -> bool =
 fun g1 g2 ->
  match (g1, g2) with
  | ( Regular { step; start; count },
      Regular { step = step'; start = start'; count = count' } ) ->
      step = step' && start = start' && count = count'
  | (Explicit { reals }, Explicit { reals = reals' }) -> reals = reals'

let regular ~start ~step ~count = Regular { start; step; count }

let round_to_even i = if i mod 2 = 0 then i else i + 1

let sampling ~freq ~start ~stop =
  if start > stop then Error [Invalid_interval (Real_interval { start; stop })]
  else if freq < 0.0 then Error [Invalid_real freq]
  else
    let duration = stop -. start in
    let step = 1.0 /. freq in
    if duration < step then
      Error [Invalid_interval (Real_interval { start; stop }); Invalid_real freq]
    else
      let count = round_to_even (truncate (duration *. freq)) in
      let start = truncate (start *. freq) in
      Ok (Regular { start; step; count })

let explicit ~reals = Explicit { reals }

let to_explicit (Regular { start; step; count }) =
  let reals = Array.init count (fun i -> float (start + i) *. step) in
  explicit ~reals

let index_from_real (type a) (grid : a t) (t : float) : int result =
  match grid with
  | Regular { start; step; count } ->
      let t0 = float start *. step in
      let i = int_of_float ((t -. t0) /. step) in
      if i < 0 || i >= count then Error [Invalid_real t] else Ok i
  | Explicit { reals } -> (
      match index_from_real_array reals t 0 (Array.length reals - 1) with
      | Above | Below -> Error [Invalid_real t]
      | Inside i -> Ok i )

let real_from_index (type a) (grid : a t) (i : int) : float result =
  match grid with
  | Regular { start; step; count } ->
      if i < 0 || i > count then Error [Invalid_index i]
      else Ok (step *. float (start + i))
  | Explicit { reals } ->
      if i < 0 || i >= Array.length reals then Error [Invalid_index i]
      else Ok reals.(i)

let start (x : regular t) = match x with Regular { start; _ } -> start

let step (x : regular t) = match x with Regular { step; _ } -> step

let count : type a. a t -> int =
 fun grid ->
  match grid with
  | Regular { count; _ } -> count
  | Explicit { reals } -> Array.length reals

let t0 : type a. a t -> float =
 fun grid ->
  match grid with
  | Regular { step; start; _ } -> step *. float start
  | Explicit { reals } ->
      let last = Array.length reals - 1 in
      reals.(last) -. reals.(0)

let extent : type a. a t -> float =
 fun grid ->
  match grid with
  | Regular { step; count; _ } -> float count *. step
  | Explicit { reals } ->
      let last = Array.length reals - 1 in
      reals.(last) -. reals.(0)

let fold_regular f init (g : regular t) =
  match g with
  | Regular { step; start; count } ->
      let stop = start + count - 1 in
      let rec loop acc i =
        if i > stop then acc
        else
          let t = float i *. step in
          let acc = f acc t in
          loop acc (i + 1)
      in
      loop init start

(* let acc = ref init in
 * for i = start to start + count - 1 do
 *   let t = float i *. step in
 *   acc := f !acc t
 * done ;
 * !acc *)

let fold_explicit f init (g : explicit t) =
  match g with
  | Explicit { reals } ->
      let stop = Array.length reals - 1 in
      let rec loop acc i =
        if i > stop then acc
        else
          let t = reals.(i) in
          let acc = f acc t in
          loop acc (i + 1)
      in
      loop init 0

(* let acc = ref init in
 * for i = 0 to Array.length reals - 1 do
 *   let t = reals.(i) in
 *   acc := f !acc t
 * done ;
 * !acc *)

let fold (type g) f init (grid : g t) =
  match grid with
  | Regular _ -> fold_regular f init grid
  | Explicit _ -> fold_explicit f init grid

let fold_increments_regular f init (grid : regular t) =
  match grid with
  | Regular { step; start; count } ->
      let stop = start + count - 2 in
      let rec loop i acc =
        if i > stop then acc
        else
          let acc = f acc step in
          loop (i + 1) acc
      in
      loop start init

(* let acc = ref init in
 * for _i = start to start + count - 2 do
 *   acc := f !acc step
 * done ;
 * !acc *)

let fold_increments_explicit f init (grid : explicit t) =
  match grid with
  | Explicit { reals } ->
      let stop = Array.length reals - 1 in
      let rec loop i acc =
        if i > stop then acc
        else
          let dt = reals.(i) -. reals.(i - 1) in
          let acc = f acc dt in
          loop (i + 1) acc
      in
      loop 1 init

(* let acc = ref init in
 * for i = 1 to Array.length reals - 1 do
 *   let dt = reals.(i) -. reals.(i - 1) in
 *   acc := f !acc dt
 * done ;
 * !acc *)

let fold_increments (type g) f init (grid : g t) =
  match grid with
  | Regular _ -> fold_increments_regular f init grid
  | Explicit _ -> fold_increments_explicit f init grid

let inter_regular (grid : regular t) (itv : interval) : regular t result =
  match grid with
  | Regular { step; start = _; count } -> (
      match itv with
      | Real_interval { start; stop } ->
          index_from_real grid start >>= fun start_index ->
          index_from_real grid stop >>= fun stop_index ->
          let new_count = stop_index - start_index + 1 in
          if new_count >= count || new_count < 0 then
            Error [Invalid_interval itv]
          else Ok (Regular { step; start = start_index; count })
      | Index_interval { start; stop } ->
          let new_count = stop - start + 1 in
          if new_count >= count || new_count < 0 then
            Error [Invalid_interval itv]
          else Ok (Regular { step; start; count }) )

let inter_explicit (grid : explicit t) (itv : interval) : explicit t result =
  match grid with
  | Explicit { reals } -> (
      let count = Array.length reals in
      match itv with
      | Real_interval { start; stop } ->
          index_from_real grid start >>= fun start_index ->
          index_from_real grid stop >>= fun stop_index ->
          let new_count = stop_index - start_index + 1 in
          if new_count >= count || new_count < 0 then
            Error [Invalid_interval itv]
          else
            let reals = Array.sub reals start_index new_count in
            Ok (Explicit { reals })
      | Index_interval { start; stop } ->
          let new_count = stop - start + 1 in
          if new_count >= count || new_count < 0 then
            Error [Invalid_interval itv]
          else
            let reals = Array.sub reals start new_count in
            Ok (Explicit { reals }) )

let inter (type g) (grid : g t) (itv : interval) : g t result =
  match grid with
  | Regular _ -> inter_regular grid itv
  | Explicit _ -> inter_explicit grid itv
