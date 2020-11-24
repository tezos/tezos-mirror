let _ =
  {| This test cannot be run on the CI, and is hence deactivated. |}
(*
open Pyplot

let () =
  if Array.length Sys.argv = 1 then
    Plot.init ()
  else
    Plot.init ~interpreter:Sys.argv.(1) ()

let pi = acos ~-. 1.

let gaussian mean std =
  let u1 = Random.float 1. in
  let u2 = Random.float 1. in
  mean +. std *. (sqrt (~-. 2. *. log u1)) *. (cos (2. *. pi *. u2))

let gaussian_vector mean std dim =
  Matrix.init ~lines:1 ~cols:dim ~f:(fun _ _ -> gaussian mean std)

let scatter_2d =
  let xs = gaussian_vector 4.0 1.0 1000 in
  let ys = gaussian_vector 3.0 8.0 1000 in
  Plot.Axis.(
    scatter_2d ~xs ~ys ~color:Plot.Red ~shape:Plot.Dot >>= fun _ ->
    set_xlabel "dim x" >>= fun _ ->
    set_ylabel "dim y" >>= fun _ ->
    set_title "2d scatter plot")

let scatter_3d =
  let xs = gaussian_vector 4.0 1.0 1000 in
  let ys = gaussian_vector 3.0 8.0 1000 in
  let zs = Matrix.init ~lines:1 ~cols:1000 ~f:(fun _l col ->
               let x = Matrix.get xs 0 col in
               let y = Matrix.get ys 0 col in
               (gaussian 0.0 3.0) *. x *. y
             ) in
  Plot.Axis.(
    scatter_3d ~xs ~ys ~zs ~color:Plot.(Rgb { r = 0.0 ; g = 0.0 ; b = 0.0}) ~shape:Plot.Triangle >>= fun _ ->
    set_xlabel "dim x" >>= fun _ ->
    set_ylabel "dim y" >>= fun _ ->
    set_zlabel "dim z" >>= fun _ ->
    set_title "3d scatter plot")

let hist =
  (* Warning, Histogram expects line-formatted data *)
  let h = Matrix.init ~lines:1000 ~cols:1 ~f:(fun _ _ ->
              gaussian 0.0 1.0
            ) in
  Plot.Axis.(
    histogram_1d ~h ~opts:{ bins = Some Bins_auto ; range = None } >>= fun _ ->
    set_xlabel "samples" >>= fun _ ->
    set_ylabel "frequency")

let plot_scatter_2d_alone =
  Plot.(run ~nrows:1 ~ncols:1 (subplot_2d ~row:0 ~col:0 scatter_2d >>= fun _ ->
                               savefig ~filename:"scatter2d.png"  ~dpi:300 ~quality:95

  ))

let plot_scatter_2d_and_scatter_3d_alone =
  Plot.(run ~nrows:1 ~ncols:2 (
            subplot_3d ~row:0 ~col:0 scatter_3d >>= fun _ ->
            subplot_2d ~row:0 ~col:1 scatter_2d >>= fun _ ->
            show () >>= fun _ ->
            savefig ~filename:"scatter2d_and_3d.png"  ~dpi:300 ~quality:95
  ))

let all =
  Plot.(run ~nrows:2 ~ncols:2 (
            subplot_3d ~row:0 ~col:0 scatter_3d >>= fun _ ->
            subplot_2d ~row:0 ~col:1 scatter_2d >>= fun _ ->
            subplot_2d ~row:1 ~col:0 hist >>= fun _ ->
            show () >>= fun _ ->
            savefig ~filename:"all.png"  ~dpi:300 ~quality:95
  ))

let several_scatters_in_one_plot =
  let xs = gaussian_vector 4.0 1.0 50 in
  let ys = gaussian_vector 3.0 8.0 50 in
  let xs' = gaussian_vector 10.0 1.0 50 in
  let ys' = gaussian_vector 3.0 1.0 50 in
  Plot.(run ~nrows:1 ~ncols:1 (
            subplot_2d ~row:0 ~col:0
              (scatter (Scatter
                          { data = [ Dim2Scatter { xs ; ys }, (Red, Circle) ;
                                     Dim2Scatter { xs = xs' ; ys = ys' }, (Green, Square)
                                   ] ;
                            axes = Dim2Axes { xaxis = "xaxis" ; yaxis = "yaxis" } ;
                            title = "scatter"
                          })) >>= fun _ ->
            show ()))

let line_plots =
  let xs = Matrix.init ~lines:30 ~cols:1 ~f:(fun l _c -> float l) in
  let ys1 = Matrix.init ~lines:30 ~cols:1 ~f:(fun l _c -> gaussian (float l) 1.0) in
  let ys2 = Matrix.init ~lines:30 ~cols:1 ~f:(fun l _c -> gaussian (float l) 2.0) in
  let ys3 = Matrix.init ~lines:30 ~cols:1 ~f:(fun l _c -> gaussian (float l) 3.0) in
  let ys4 = Matrix.init ~lines:30 ~cols:1 ~f:(fun l _c -> gaussian (float l) 4.0) in
  Plot.(run ~nrows:1 ~ncols:1 (
      subplot_2d ~row:0 ~col:0 Axis.(
          line_2d ~xs ~ys:ys1 ~legend:(Some "legend 1") ~line:None >>= fun () ->
          line_2d ~xs ~ys:ys2 ~legend:(Some "legend 2") ~line:(Some { linestyle = Line_dashed ;
                                            dot_style = Some Square ;
                                            color = Blue }) >>= fun () ->
          line_2d ~xs ~ys:ys3 ~legend:(Some "legend 3") ~line:(Some { linestyle = Line_nothing ;
                                            dot_style = Some Triangle ;
                                            color = Green }) >>= fun () ->
          line_2d ~xs ~ys:ys4 ~legend:(Some "legend 4") ~line:(Some { linestyle = Line_dotted ;
                                                                    dot_style = Some Triangle ;
                                                                    color = Red }) >>= fun () ->
          set_xlabel "x axis" >>= fun () ->
          set_ylabel "y axis" >>= fun () ->
          set_title "2d plot"
        ) >>= fun () ->
      suptitle ~title:"Interesting title" ~fontsize:(Some 16) >>= fun () ->
      show ()
    ))
*)
