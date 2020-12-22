open Pyplot
open StaTz
open Numerics

let signal =
  Series.discretise
    Series.dense_float64
    (fun x -> sin (x *. 100.0))
    (Grid.regular ~start:0 ~step:0.01 ~count:1000)

let fft = Fourier.fft signal

let norm2 = Series.map Series.dense_float64 Complex.norm2 fft

let to_vectors (s : (Grid.regular, float, _) Series.t) =
  let data = Series.data s in
  let grid = Series.grid s in
  let grid = Grid.to_explicit grid in
  match grid with
  | Grid.Explicit { reals } ->
      let dim = Array.length reals in
      let xs = Matrix.init ~lines:dim ~cols:1 ~f:(fun l _ -> reals.(l)) in
      let ys =
        Matrix.init ~lines:dim ~cols:1 ~f:(fun l _ ->
            Float64.Vec.unsafe_get data l)
      in
      (xs, ys)

let _ =
  Plot.init () ;
  Plot.(
    run
      ~nrows:1
      ~ncols:1
      ( subplot_2d
          ~row:0
          ~col:0
          (let (xs, ys) = to_vectors signal in
           let (xs', ys') = to_vectors norm2 in
           Axis.(
             line_2d ~xs ~ys ~legend:None ~line:None >>= fun () ->
             line_2d ~xs:xs' ~legend:None ~ys:ys' ~line:None))
      >>= fun () -> show () ))

let _ =
  Plot.init () ;
  let grid = Grid.regular ~start:0 ~step:0.1 ~count:4000 in
  let brownian = Sde.brownian 0.0 grid in
  let sample = Stats.sample_gen brownian in
  let fft = Fourier.fft sample in
  let norm2 = Series.map Series.dense_float64 Complex.norm2 fft in
  Plot.(
    run
      ~nrows:2
      ~ncols:1
      ( subplot_2d
          ~row:0
          ~col:0
          Axis.(
            let (xs, ys) = to_vectors sample in
            line_2d ~xs ~ys ~legend:None ~line:None)
      >>= fun () ->
        subplot_2d
          ~row:1
          ~col:0
          Axis.(
            let (xs, ys) = to_vectors norm2 in
            line_2d ~xs ~ys ~legend:None ~line:None)
        >>= fun () -> show () ))
