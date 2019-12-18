open StaTz
open Structures
open Stats
open Pyplot

(* Testing MH *)

module MH_test (X : sig end) = struct
  module Partition (M : Metric) = struct
    type t = { cells : M.t array }

    type cell = { partition : t; pt : int }

    module Cell_ordered = struct
      type t = cell

      let compare c1 c2 = Stdlib.compare c1.pt c2.pt
    end

    let make (points : M.t array) : t = { cells = points }

    let cell (partition : t) (p : M.t) : cell =
      let rec loop (i : int) nearest dist =
        if i >= Array.length partition.cells then nearest
        else
          let dist' = M.dist p partition.cells.(i) in
          if dist' < dist then loop (i + 1) i dist'
          else loop (i + 1) nearest dist
      in
      let index = loop 1 0 (M.dist p partition.cells.(0)) in
      { partition; pt = index }

    let extend_density (partition : t) (den : M.t fin_den) : cell fin_den =
      let (`Density den) = raw_data_density den in
      let den = List.map (fun (p, weight) -> (cell partition p, weight)) den in
      density (module Cell_ordered) den
  end

  module Reals : Metric with type t = float = struct
    type t = float

    let dist (x : t) (y : t) = abs_float (y -. x)
  end

  module Part = Partition (Reals)

  module Cells_ordered = struct
    type t = Part.cell

    let compare (c1 : t) (c2 : t) = Stdlib.compare c1.pt c2.pt
  end

  module Test = struct
    type t = Part.cell

    let pp fmtr (cell : t) =
      match cell with
      | { partition; pt } ->
          let (low, high) =
            if pt = Array.length partition.cells - 1 then
              (partition.cells.(pt - 2), partition.cells.(pt - 1))
            else (partition.cells.(pt), partition.cells.(pt + 1))
          in
          Format.fprintf fmtr "[%f, %f]" low high

    let float_compare (x : float) (y : float) =
      if x < y then -1 else if x = y then 0 else 1

    let compare = Cells_ordered.compare

    (* -- *)

    let float_points =
      let pts = Array.init 2000 (fun _ -> -10. +. Random.float 20.0) in
      let npts = Array.map (fun x -> ~-.x) pts in
      let line = Array.concat [npts; pts] in
      Array.sort float_compare line ;
      line

    let part = Part.make float_points

    let alpha = 3.0

    let proposal_gen x =
      generative ~sampler:(fun () ->
          let delta = Random.float alpha in
          if Random.bool () then Part.cell part (x +. delta)
          else Part.cell part (x -. delta))

    let proposal (cell : t) =
      let pt = part.cells.(cell.pt) in
      let gen = proposal_gen pt in
      let emp = empirical_of_generative ~nsamples:1000 gen in
      fin_prb_of_empirical (module Cells_ordered) emp

    let pi = acos ~-.1.0

    let gaussian_density mean variance =
      let normalizer = 1.0 /. (sqrt @@ (2.0 *. pi *. variance)) in
      fun x ->
        let delta = (x -. mean) ** 2.0 in
        normalizer *. (exp @@ ~-.(delta /. (2.0 *. variance)))

    (* The target unnormalize density *)
    let weight (c : Part.cell) =
      let open Part in
      let x = part.cells.(c.pt) in
      (sin x ** 2.0)
      *. ((sin @@ (2.0 *. x)) ** 2.0)
      *. gaussian_density 0.0 1.0 x

    let log_weight c = log (weight c)
  end

  module Sampler = MH.Make (Test)

  (* let _ = Printexc.register_printer
   *     (function
   *       | Sampler.Null_backward_flow(x1, x2) ->
   *           let { Part.partition = part1 ; pt = pt1 } = x1 in
   *           let cell1 = string_of_float part1.Part.cells.(pt1) in
   *           let { Part.partition = part2 ; pt = pt2 } = x2 in
   *           let cell2 = string_of_float part2.Part.cells.(pt2) in
   *           Some (Printf.sprintf "Null backward flow from %d = %s to %d = %s" pt1 cell1 pt2 cell2)
   *       | _ -> None) *)

  let _ =
    let mcmc =
      Sampler.mcmc
        ~verbosity:`Trace
        ~initial:(Part.cell Test.part 0.0)
        ~burn_in:3000
    in
    let empirical = empirical_of_generative ~nsamples:8000 mcmc in
    let (`Empirical data) = raw_data_empirical empirical in
    let float_data =
      Array.map (fun { Part.pt; _ } -> Test.part.cells.(pt)) data
    in
    Plot.init () ;
    Plot.(
      run
        ~nrows:1
        ~ncols:1
        ( subplot_2d
            ~row:0
            ~col:0
            (let data =
               Matrix.init
                 ~lines:(Array.length float_data)
                 ~cols:1
                 ~f:(fun i _ -> float_data.(i))
             in
             Plot.(
               histogram
               @@ Histogram
                    { data;
                      options =
                        Some
                          { bins = Some (Bins_num 30);
                            range = Some (-10., 10.)
                          };
                      axes = Dim2Axes { xaxis = "samples"; yaxis = "frequency" };
                      title = "empirical"
                    }))
        >>= fun () -> show () ))
end

module Test = MH_test ()
