(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type color = Red | Green | Blue | Rgb of {r : float; g : float; b : float}

type shape = Square | Dot | Circle | Triangle

type style = color * shape

type linestyle =
  | Line_solid
  | Line_dashed
  | Line_dashdot
  | Line_dotted
  | Line_nothing

type line = {linestyle : linestyle; dot_style : shape option; color : color}

(* An element of type scatter_data represents a bunch of
   n-dimensional points where n is the length of the list.
   All elements of the list should be column vectors with
   equal dimensions. *)
type dim2

type dim3

(* It is required for all [scatter_data] to have the same dimensionality
   (2d and 3d cannot be mixed). *)
type _ scatter_data =
  | Dim2Scatter : {xs : Matrix.t; ys : Matrix.t} -> dim2 scatter_data
  | Dim3Scatter : {
      xs : Matrix.t;
      ys : Matrix.t;
      zs : Matrix.t;
    }
      -> dim3 scatter_data

type _ axes =
  | Dim2Axes : {xaxis : string; yaxis : string} -> dim2 axes
  | Dim3Axes : {xaxis : string; yaxis : string; zaxis : string} -> dim3 axes

type 'dim scatter =
  | Scatter : {
      data : ('dim scatter_data * style) list;
      axes : 'dim axes;
      title : string;
    }
      -> 'dim scatter

type histogram =
  | Histogram of {
      data : Matrix.t;
      options : histogram_options option;
      axes : dim2 axes;
      title : string;
    }

and histogram_options = {
  bins : bins_options option;
  range : (float * float) option;
}

and bins_options =
  | Bins_num of int
  | Bins_edges of float list
  | Bins_auto
  | Bins_fd
  | Bins_sturges
  | Bins_doane
  | Bins_scott
  | Bins_stone
  | Bins_rice
  | Bins_sqrt

(* -------------------------------------------------------------------------- *)
(* Style helpers *)

let python_of_color = function
  | Red ->
      Py.String.of_string "r"
  | Green ->
      Py.String.of_string "g"
  | Blue ->
      Py.String.of_string "b"
  | Rgb {r; g; b} ->
      assert (0. <= r && r <= 1.0) ;
      assert (0. <= g && g <= 1.0) ;
      assert (0. <= b && b <= 1.0) ;
      Py.Tuple.of_array
        [|Py.Float.of_float r; Py.Float.of_float g; Py.Float.of_float b|]

let python_of_shape = function
  | Square ->
      Py.String.of_string "s"
  | Dot ->
      Py.String.of_string "."
  | Circle ->
      Py.String.of_string "o"
  | Triangle ->
      Py.String.of_string "v"

(* -------------------------------------------------------------------------- *)
(* Helpers *)

let ( --> ) pyobj field = Py.Module.get_function pyobj field

let ( -->? ) pyobj field = Py.Module.get_function_with_keywords pyobj field

(* Using a Reader monad to thread a matplotlib "axis" object throughout. *)
module Axis : sig
  type ('a, 'k) t

  val return : 'a -> ('a, 'k) t

  val ( let* ) : ('a, 'k) t -> ('a -> ('b, 'k) t) -> ('b, 'k) t

  val run : ('a, 'k) t -> Pytypes.pyobject -> 'a

  val set_xlabel : string -> (unit, 'k) t

  val set_ylabel : string -> (unit, 'k) t

  val set_zlabel : string -> (unit, 'k) t

  val set_title : string -> (unit, 'k) t

  val set_ylim : ymin:float option -> ymax:float option -> (unit, 'k) t

  val histogram_1d : h:Matrix.t -> opts:histogram_options -> (unit, dim2) t

  val scatter_2d :
    xs:Matrix.t -> ys:Matrix.t -> color:color -> shape:shape -> (unit, dim2) t

  val scatter_3d :
    xs:Matrix.t ->
    ys:Matrix.t ->
    zs:Matrix.t ->
    color:color ->
    shape:shape ->
    (unit, dim3) t

  val line_2d :
    xs:Matrix.t ->
    ys:Matrix.t ->
    legend:string option ->
    line:line option ->
    (unit, dim2) t

  val legend : unit -> (unit, 'k) t
end = struct
  type ('a, 'k) t = ax:Pytypes.pyobject -> 'a

  let return : 'a -> ('a, 'k) t = fun x ~ax -> ignore ax ; x

  let ( let* ) : 'k. ('a, 'k) t -> ('a -> ('b, 'k) t) -> ('b, 'k) t =
    fun (type dim) (m : ('a, dim) t) (f : 'a -> ('b, dim) t) ->
     let res ~ax =
       let r = m ~ax in
       f r ~ax
     in
     (res : ('b, dim) t)

  let run m ax = m ~ax

  let set_xlabel : string -> (unit, 'k) t =
   fun xlabel ~ax ->
    ignore
      (Pytools.handle_python_error "In set_xlabel" (fun () ->
           (ax --> "set_xlabel") [|Py.String.of_string xlabel|]))

  let set_ylabel : string -> (unit, 'k) t =
   fun ylabel ~ax ->
    ignore
      (Pytools.handle_python_error "In set_ylabel" (fun () ->
           (ax --> "set_ylabel") [|Py.String.of_string ylabel|]))

  let set_zlabel : string -> (unit, [< `ThreeD]) t =
   fun zlabel ~ax ->
    ignore
      (Pytools.handle_python_error "In set_zlabel" (fun () ->
           (ax --> "set_zlabel") [|Py.String.of_string zlabel|]))

  let set_title : string -> (unit, 'k) t =
   fun zlabel ~ax ->
    ignore
      (Pytools.handle_python_error "In set_title" (fun () ->
           (ax --> "set_title") [|Py.String.of_string zlabel|]))

  let set_ylim : ymin:float option -> ymax:float option -> (unit, 'k) t =
   fun ~ymin ~ymax ~ax ->
    match (ymin, ymax) with
    | (None, None) ->
        invalid_arg "set_ylim: both arguments equal to None"
    | _ ->
        let ymin_arg =
          match ymin with
          | None ->
              []
          | Some y ->
              [("ymin", Py.Float.of_float y)]
        in
        let ymax_arg =
          match ymax with
          | None ->
              []
          | Some y ->
              [("ymax", Py.Float.of_float y)]
        in
        let args = ymin_arg @ ymax_arg in
        ignore
          (Pytools.handle_python_error "In set_autoscaley_on" (fun () ->
               (ax --> "set_autoscaley_on") [|Py.Bool.f|])) ;
        ignore
          (Pytools.handle_python_error "In set_ylim" (fun () ->
               (ax -->? "set_ylim") [||] args))

  let histogram_1d : h:Matrix.t -> opts:histogram_options -> (unit, dim2) t =
   fun ~(h : Matrix.t) ~opts ~ax ->
    let options =
      let bin_opts =
        match opts.bins with
        | None ->
            []
        | Some bins_options ->
            let opt_value =
              match bins_options with
              | Bins_num i ->
                  Py.Int.of_int i
              | Bins_edges e ->
                  let edges = List.map (fun x -> Py.Float.of_float x) e in
                  Py.List.of_list edges
              | Bins_auto ->
                  Py.String.of_string "auto"
              | Bins_fd ->
                  Py.String.of_string "fd"
              | Bins_doane ->
                  Py.String.of_string "doane"
              | Bins_scott ->
                  Py.String.of_string "scott"
              | Bins_stone ->
                  Py.String.of_string "stone"
              | Bins_sturges ->
                  Py.String.of_string "sturges"
              | Bins_rice ->
                  Py.String.of_string "rice"
              | Bins_sqrt ->
                  Py.String.of_string "sqrt"
            in
            [("bins", opt_value)]
      in
      let range_opts =
        match opts.range with
        | None ->
            []
        | Some (lo, hi) ->
            let lo = Py.Float.of_float lo in
            let hi = Py.Float.of_float hi in
            [("range", Py.Tuple.of_list [lo; hi])]
      in
      range_opts @ bin_opts
    in
    ignore
      (Pytools.handle_python_error "In hist" (fun () ->
           (ax -->? "hist") [|Matrix.to_numpy h|] options))

  let scatter_2d ~xs ~ys ~color ~shape : (unit, dim2) t =
   fun ~ax ->
    let color = ("c", python_of_color color) in
    let shape = ("marker", python_of_shape shape) in
    let keywords =
      [("x", Matrix.to_numpy xs); ("y", Matrix.to_numpy ys); color; shape]
    in
    ignore
      (Pytools.handle_python_error "In scatter_2d" (fun () ->
           (ax -->? "scatter") [||] keywords))

  let scatter_3d ~xs ~ys ~zs ~color ~shape : (unit, dim3) t =
   fun ~ax ->
    let color = ("c", python_of_color color) in
    let shape = ("marker", python_of_shape shape) in
    let keywords =
      [ ("xs", Matrix.to_numpy xs);
        ("ys", Matrix.to_numpy ys);
        ("zs", Matrix.to_numpy zs);
        color;
        shape ]
    in
    ignore
      (Pytools.handle_python_error "In scatter_3d" (fun () ->
           (ax -->? "scatter") [||] keywords))

  let line_2d ~xs ~ys ~legend ~line ~ax =
    let keywords =
      match line with
      | None ->
          []
      | Some {linestyle; dot_style; color} ->
          let color = [("c", python_of_color color)] in
          let shape =
            match dot_style with
            | None ->
                []
            | Some shape ->
                [("marker", python_of_shape shape)]
          in
          let line =
            let linestyle_str =
              match linestyle with
              | Line_solid ->
                  "solid"
              | Line_dashed ->
                  "dashed"
              | Line_dashdot ->
                  "dashdot"
              | Line_dotted ->
                  "dotted"
              | Line_nothing ->
                  "None"
            in
            [("linestyle", Py.String.of_string linestyle_str)]
          in
          color @ shape @ line
    in
    let keywords =
      match legend with
      | None ->
          keywords
      | Some legend ->
          ("label", Py.String.of_string legend) :: keywords
    in
    ignore
      (Pytools.handle_python_error "In line_2d" (fun () ->
           (ax -->? "plot") [|Matrix.to_numpy xs; Matrix.to_numpy ys|] keywords))

  let legend () : (unit, _) t =
   fun ~ax ->
    ignore
      (Pytools.handle_python_error "legend" (fun () -> (ax --> "legend") [||]))
end

type matplotlib_state = {nrows : int; ncols : int; fig : Pytypes.pyobject}

type 'a t = matplotlib_state -> 'a

let return : 'a -> 'a t = fun x _ -> x

let ( let* ) (m : 'a t) (f : 'a -> 'b t) : 'b t = fun state -> f (m state) state

let run ?figsize ~nrows ~ncols (plot : 'a t) =
  let (xfig, yfig) =
    match figsize with
    | None ->
        let x = float ncols *. 7.0 in
        let y = 8.5 in
        (x, y)
    | Some (x, y) ->
        (x, y)
  in
  Pytools.handle_python_error "In run" (fun () ->
      let _ = Pymodules.mpl_toolkits () in
      (* Coping with Maplotlib's side effects. *)
      let fig =
        (Pymodules.pyplot () -->? "figure")
          [||]
          [ ( "figsize",
              Py.Tuple.of_tuple2
                (Py.Float.of_float xfig, Py.Float.of_float yfig) ) ]
      in
      let res = plot {nrows; ncols; fig} in
      ignore @@ (Pymodules.pyplot () --> "close") [|fig|] ;
      res)

(* [row] and [col] range resp. in [0..nrows-1] and [0..ncols-1]. *)
let subplot_2d : row:int -> col:int -> (unit, dim2) Axis.t -> unit t =
 fun ~row ~col axis {nrows; ncols; fig} ->
  let index = 1 + (row * ncols) + col in
  ignore
  @@ Pytools.handle_python_error "In subplot_2d"
  @@ fun () ->
  let axis_object =
    Py.Module.get_function_with_keywords
      fig
      "add_subplot"
      [|Py.Int.of_int nrows; Py.Int.of_int ncols; Py.Int.of_int index|]
      []
  in
  Axis.run axis axis_object

(* [row] and [col] range resp. in [0..nrows-1] and [0..ncols-1]. *)
let subplot_3d : row:int -> col:int -> (unit, dim3) Axis.t -> unit t =
 fun ~row ~col axis {nrows; ncols; fig} ->
  let index = 1 + (row * ncols) + col in
  ignore
    (Pytools.handle_python_error "In subplot_3d" (fun () ->
         let axis_object =
           Py.Module.get_function_with_keywords
             fig
             "add_subplot"
             [|Py.Int.of_int nrows; Py.Int.of_int ncols; Py.Int.of_int index|]
             [("projection", Py.String.of_string "3d")]
         in
         Axis.run axis axis_object))

let rec perform_scatter_2d (data : (dim2 scatter_data * style) list) =
  match data with
  | [] ->
      Axis.return ()
  | (scatter_data, style) :: more -> (
      let (color, shape) = style in
      match scatter_data with
      | Dim2Scatter {xs; ys} ->
          Axis.(
            let* () = perform_scatter_2d more in
            scatter_2d ~xs ~ys ~color ~shape) )

let rec perform_scatter_3d (data : (dim3 scatter_data * style) list) =
  match data with
  | [] ->
      Axis.return ()
  | (scatter_data, style) :: more -> (
      let (color, shape) = style in
      match scatter_data with
      | Dim3Scatter {xs; ys; zs} ->
          Axis.(
            let* () = perform_scatter_3d more in
            scatter_3d ~xs ~ys ~zs ~color ~shape) )

let scatter (type dim) (s : dim scatter) : (unit, dim) Axis.t =
  match s with
  | Scatter {data; axes; title} -> (
    match axes with
    | Dim2Axes {xaxis; yaxis} ->
        Axis.(
          let* () = set_xlabel xaxis in
          let* () = set_ylabel yaxis in
          let* () = set_title title in
          perform_scatter_2d data)
    | Dim3Axes {xaxis; yaxis; zaxis} ->
        Axis.(
          let* () = set_xlabel xaxis in
          let* () = set_ylabel yaxis in
          let* () = set_zlabel zaxis in
          let* () = set_title title in
          perform_scatter_3d data)
    )

let histogram (h : histogram) : (unit, dim2) Axis.t =
  match h with
  | Histogram {data; options; axes = Dim2Axes {xaxis; yaxis}; title} -> (
      Axis.(
        let* () = set_xlabel xaxis in
        let* () = set_ylabel yaxis in
        let* () = set_title title in
        match options with
        | None ->
            histogram_1d ~h:data ~opts:{bins = None; range = None}
        | Some opts ->
            histogram_1d ~h:data ~opts) )

let suptitle ~(title : string) ~(fontsize : int option) : unit t =
  let opt_args =
    match fontsize with
    | None ->
        []
    | Some size ->
        [("fontsize", Py.Int.of_int size)]
  in
  fun {fig; _} ->
    ignore ((fig -->? "suptitle") [|Py.String.of_string title|] opt_args)

let savefig : filename:string -> dpi:int -> quality:int -> unit t =
 fun ~filename ~dpi ~quality {fig; _} ->
  if quality < 1 || quality > 95 || dpi < 0 then
    Stdlib.failwith "savefig: invalid parameters"
  else
    ignore
      ((fig -->? "savefig")
         [|Py.String.of_string filename|]
         [("dpi", Py.Int.of_int dpi); ("quality", Py.Int.of_int quality)])

let show () _ =
  ignore (Py.Module.get_function (Pymodules.pyplot ()) "legend" [||]) ;
  ignore (Py.Module.get_function (Pymodules.pyplot ()) "show" [||])

let init ?interpreter ?version () =
  if not (Py.is_initialized ()) then (
    Printf.eprintf "pyml-pyplot: initializing python... " ;
    Py.initialize ?interpreter ?version () ;
    Printf.eprintf "pyml-pyplot: done.\n%!" ) ;
  if not (Py.is_initialized ()) then (
    Printf.printf "pyml-pyplot: can't initialize Python, exiting" ;
    exit 1 )
