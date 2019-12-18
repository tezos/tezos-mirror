open Numerics

type error = Incompatible_sampling_grid : 'a Grid.t * 'a Grid.t -> error

exception Fourier_error of error

type time_signal = (Grid.regular, float, Float64.Vec.t) Series.t

type freq_signal = (Grid.regular, Complex.t, Complex64.Vec.t) Series.t

(* -------------------------------------------------------------------------- *)
(* Helpers *)

let log2 = log 2.

(* [minpow2 l] returns the smallest 2^n which is [>= l]. Works when
   [l <= 2^53]. *)
let minpow2 l = truncate (2. ** ceil (log (float l) /. log2))

(* -------------------------------------------------------------------------- *)
(* fft code *)

let fft : time_signal -> freq_signal =
 fun series ->
  let dim = Series.length series in
  let rdata = Series.data series in
  let cdata = Numerics.Fft.rfft rdata in
  match Series.grid series with
  | Regular { step; _ } ->
      let freq = 1.0 /. step in
      let resl = freq /. float dim in
      let count = Complex64.Vec.length cdata in
      let freq_grid = Grid.regular ~start:0 ~step:resl ~count in
      Series.make Series.dense_complex64 freq_grid cdata

let ifft : ?t0:float -> freq_signal -> time_signal =
 fun ?(t0 = 0.0) series ->
  let cdata = Series.data series in
  let rdata = Numerics.Fft.irfft cdata in
  match Series.grid series with
  | Regular { step = freq_resl; _ } ->
      let time_step = 1.0 /. freq_resl in
      let time_start = int_of_float (t0 /. time_step) in
      let time_grid =
        Grid.regular
          ~start:time_start
          ~step:time_step
          ~count:(Float64.Vec.length rdata)
      in
      Series.make Series.dense_float64 time_grid rdata

let align (s1 : time_signal) (s2 : time_signal) =
  let (Regular { start = ti1; step = dt1; _ } as grid1) = Series.grid s1 in
  let (Regular { start = ti2; step = dt2; _ } as grid2) = Series.grid s2 in
  if dt1 <> dt2 then
    raise (Fourier_error (Incompatible_sampling_grid (grid1, grid2))) ;
  if ti1 = ti2 then (s1, s2)
  else
    let step = dt1 in
    let data1 = Series.data s1 in
    let data2 = Series.data s2 in
    if ti1 < ti2 then
      let delta = ti2 - ti1 in
      let data2 = Float64.Vec.(concat (create delta) data2) in
      let count = Float64.Vec.length data2 in
      let grid = Grid.regular ~step ~start:ti1 ~count in
      (s1, Series.make Series.dense_float64 grid data2)
    else
      let delta = ti1 - ti2 in
      let data1 = Float64.Vec.(concat (create delta) data1) in
      let count = Float64.Vec.length data1 in
      let grid = Grid.regular ~step ~start:ti2 ~count in
      (Series.make Series.dense_float64 grid data1, s2)

let convolution (s1 : time_signal) (s2 : time_signal) =
  ( if Grid.step (Series.grid s1) <> Grid.step (Series.grid s2) then
    let msg =
      "convolution: inputs have incompatible sampling grids. Aborting."
    in
    failwith msg ) ;
  let (s1, s2) = align s1 s2 in
  let (Regular { step = step1; start = start1; count = count1 } as grid1) =
    Series.grid s1
  in
  let (Regular { step = step2; start = start2; count = count2 } as grid2) =
    Series.grid s2
  in
  let (fft1, fft2) =
    (* Pad the signals s.t. their number of samples is the smallest power of 2 (for efficiency)
       above the sum of the length of the signals (to store all delays) *)
    let target = minpow2 (count1 + count2 - 1) in
    let s1 =
      let data =
        Float64.Vec.concat
          (Series.data s1)
          (Float64.Vec.create (target - count1))
      in
      let count = Float64.Vec.length data in
      let grid = Grid.regular ~start:start1 ~step:step1 ~count in
      Series.make Series.dense_float64 grid data
    in
    let s2 =
      let data =
        Float64.Vec.concat
          (Series.data s2)
          (Float64.Vec.create (target - count2))
      in
      let count = Float64.Vec.length data in
      let grid = Grid.regular ~start:start2 ~step:step2 ~count in
      Series.make Series.dense_float64 grid data
    in
    (fft s1, fft s2)
  in
  let prod = Complex64.Vec.mul (Series.data fft1) (Series.data fft2) in
  (* we put dummy nan values into the signal - these should not get out of this function anyway. *)
  let fsignal =
    Series.make
      Series.dense_complex64
      (Grid.regular ~start:0 ~step:nan ~count:(Complex64.Vec.length prod))
      prod
  in
  (* Freq.({ data = prod; ti = nan; freq = hertz nan; period = nan; resl = hertz nan }) in *)
  let tsignal = ifft fsignal in
  let data =
    let tdata = Series.data tsignal in
    let len = Series.length s1 + Series.length s2 - 1 in
    Bigarray.Array1.sub tdata 0 len
  in
  let start = Grid.start grid2 - (Grid.start grid1 + Grid.count grid1) in
  Series.make
    Series.dense_float64
    (Grid.regular
       ~start
       ~step:(Grid.step grid1)
       ~count:(Float64.Vec.length data))
    data

let reverse : time_signal -> time_signal =
 fun signal ->
  let data = Float64.Vec.copy (Series.data signal) in
  let count = Float64.Vec.length data in
  for i = 0 to (count / 2) - 1 do
    let tmp = Float64.Vec.unsafe_get data i in
    let j = count - i - 1 in
    let x = Float64.Vec.unsafe_get data j in
    Float64.Vec.unsafe_set data i x ;
    Float64.Vec.unsafe_set data (count - i - 1) tmp
  done ;
  let grid = Series.grid signal in
  Series.make Series.dense_float64 grid data

let cross_correlation s1 s2 = convolution (reverse s1) s2
