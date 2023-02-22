(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2021 Ilias Garnier. <igarnier@protonmail.com>               *)
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

(*
   Module copied from https://github.com/igarnier/prbnmcn-gnuplot
*)

(* Small interface to gnuplot *)

let unit_interval x = x >= 0.0 && x <= 1.

type color = {r : float; g : float; b : float}

module Color = struct
  type t = color

  let rgb r g b =
    if not (unit_interval r && unit_interval g && unit_interval b) then
      invalid_arg "rgb" ;
    {r; g; b}

  let red = {r = 1.0; g = 0.0; b = 0.0}

  let green = {r = 0.0; g = 1.0; b = 0.0}

  let blue = {r = 0.0; g = 0.0; b = 1.0}

  let black = {r = 0.0; g = 0.0; b = 0.0}
end

type pointtype =
  | Dot
  | Plus
  | Cross
  | Star
  | Square
  | Box
  | Circle
  | Disk
  | Delta
  | Delta_solid

module Pointtype = struct
  type t = pointtype

  let square = Square

  let dot = Dot

  let circle = Circle

  let delta = Delta

  let delta_solid = Delta_solid

  let plus = Plus

  let cross = Cross

  let star = Star

  let box = Box

  let disk = Disk
end

type fill_style =
  | Empty
  | Solid of {density : float; transparent : bool}
  | Pattern of {code : int; transparent : bool}

type border_style = Noborder

type fill = {
  fill_style : fill_style;
  border : border_style;
  fill_color : Color.t option;
}

module Fill = struct
  type t = fill

  let default = {fill_style = Empty; border = Noborder; fill_color = None}

  let set_empty fill = {fill with fill_style = Empty}

  let set_solid ~density ~transparent fill =
    if not (unit_interval density) then invalid_arg "set_solid" ;
    {fill with fill_style = Solid {density; transparent}}

  let set_pattern ~code ~transparent fill =
    {fill with fill_style = Pattern {code; transparent}}

  let set_color color fill = {fill with fill_color = Some color}
end

type style = {shape_type : shape_type; color : Color.t option}

and shape_type =
  | Point of {ptyp : Pointtype.t option; psize : float option}
  | Circle of {fill : Fill.t option; radius : float}

module Style = struct
  type t = style

  let default = {shape_type = Point {ptyp = None; psize = None}; color = None}

  let set_point ?ptyp ?psize style =
    {style with shape_type = Point {ptyp; psize}}

  let set_circle ?fill ~radius style =
    {style with shape_type = Circle {fill; radius}}

  let set_color color style = {style with color = Some color}
end

type tics_position =
  | Tics_increment of float
  | Tics of {start : float; incr : float; stop : float option}

type tics = {
  border : bool;
  mirror : bool;
  in_ : bool;
  rotate_by : float option;
  position : tics_position option;
  logscale : bool;
}

module Tics = struct
  type t = tics

  let default =
    {
      border = true;
      mirror = true;
      in_ = true;
      rotate_by = None;
      position = None;
      logscale = false;
    }

  let set_border border tics = {tics with border}

  let set_mirror mirror tics = {tics with mirror}

  let set_outwards inwards tics = {tics with in_ = not inwards}

  let set_rotate ~degrees tics = {tics with rotate_by = Some degrees}

  let set_position ?start ?incr ?stop tics =
    match (start, incr, stop) with
    | None, None, None -> {tics with position = None}
    | None, Some incr, None -> {tics with position = Some (Tics_increment incr)}
    | Some start, Some incr, stop ->
        {tics with position = Some (Tics {start; incr; stop})}
    | _ -> invalid_arg "Tics.set_position"

  let set_logscale logscale tics = {tics with logscale}
end

type r1 = {x : float} [@@unboxed]

type r2 = {x : float; y : float}

type r3 = {x : float; y : float; z : float}

type r4 = {w : float; x : float; y : float; z : float}

let r1 x = {x}

let r2 x y = {x; y}

let r3 x y z = {x; y; z}

let r4 w x y z = {w; x; y; z}

let tup_r2 (x, y) = {x; y}

let tup_r3 (x, y, z) = {x; y; z}

module Data = struct
  (* This could be refined later into something smarter (allowing to replot the same
     data without having to duplicate it in the script for instance) *)
  type 'a t = 'a Seq.t

  let of_seq s = s

  let of_array = Array.to_seq

  let of_list = List.to_seq
end

type histogram_options = {
  color : color option;
  bins : int option;
  binwidth : float option;
}

type 'data with_metadata = {data : 'data; style : style; legend : string option}

type _ axes =
  | Dim2_axes : {
      xaxis : string;
      yaxis : string;
      xtics : Tics.t;
      ytics : Tics.t;
    }
      -> r2 axes
  | Dim3_axes : {
      xaxis : string;
      yaxis : string;
      zaxis : string;
      xtics : Tics.t;
      ytics : Tics.t;
      ztics : Tics.t;
    }
      -> r3 axes

type _ spec =
  | Scatter : {
      data : 'dim Data.t with_metadata;
      error_bars : r2 Seq.t option;
          (** Error bars for 2d point plots, always none for 3d plots *)
    }
      -> 'dim spec
  | Histogram : {
      data : r1 Seq.t;
      options : histogram_options;
      legend : string option;
    }
      -> r2 spec
  | Line : {
      data : 'dim Data.t with_metadata;
      with_points : bool;
      error_bars : r2 Seq.t option;
          (** Error bars for 2d line plots, always none for 3d plots *)
    }
      -> 'dim spec
  | Boxes : {
      data : (string * float) Data.t with_metadata;
      box_width : float option;
      fill : Fill.t;
    }
      -> r2 spec

(*
  - error bars: with scatter and with lines
  - box: only with lines
*)
type plot =
  | Plot : {
      axes : 'dim axes;
      plots : 'dim spec list;
      title : string option;
    }
      -> plot

module Scatter = struct
  let points_2d ~points ?(style = Style.default) ?legend ?error_bars () =
    Scatter {data = {data = points; style; legend}; error_bars}

  let points_3d ~points ?(style = Style.default) ?legend () =
    Scatter {data = {data = points; style; legend}; error_bars = None}
end

module Line = struct
  let compute_with_points style legend with_points =
    match (with_points, style.shape_type) with
    | None, Point {ptyp = Some _; psize = _} -> true
    | None, _ -> false
    | Some false, Point {ptyp = Some _; psize = _} ->
        let legend = Option.value ~default:"none" legend in
        let msg =
          Printf.sprintf
            "line_2d: style inconsistent with with_points = false (legend = \
             \"%s\")"
            legend
        in
        invalid_arg msg
    | Some false, _ -> false
    | Some true, _ -> true

  let line_2d ~points ?(style = Style.default) ?legend ?with_points ?error_bars
      () =
    let with_points = compute_with_points style legend with_points in
    Line {data = {data = points; style; legend}; with_points; error_bars}

  let line_3d ~points ?(style = Style.default) ?legend ?with_points () =
    let with_points = compute_with_points style legend with_points in
    Line {data = {data = points; style; legend}; with_points; error_bars = None}

  let to_2d (ys : r1 Seq.t) : r2 Seq.t =
    let c = ref 0.0 in
    Seq.map
      (fun ({x = y} : r1) ->
        let x = !c in
        c := x +. 1. ;
        {x; y})
      ys

  let line ~(points : r1 Seq.t) ?style ?legend ?with_points ?error_bars () =
    let points = to_2d points in
    line_2d ~points ?style ?legend ?with_points ?error_bars ()
end

module Histogram = struct
  type options = histogram_options = {
    color : color option;
    bins : int option;
    binwidth : float option;
  }

  let hist ~points ?color ?bins ?binwidth ?legend () =
    match (bins, binwidth) with
    | None, None ->
        let bins = Some 20 in
        Histogram {data = points; options = {color; bins; binwidth}; legend}
    | _ -> Histogram {data = points; options = {color; bins; binwidth}; legend}
end

module Bar = struct
  (* type bars = r1 Data.t * Fill.t
   *
   * let bars data ?(fill = Fill.default) = (data, fill)
   *
   * let boxes ~xtics ~bars ?(boxwidth = 0.5) ?legend () =
   *   if bars = [] then invalid_arg "boxes: empty data" ;
   *   if xtics = [] then invalid_arg "boxes: empty xtics" ;
   *   let xtics_count = List.length xtics in
   *   List.iter
   *     (fun data ->
   *       if List.(length (of_seq data)) <> xtics_count then
   *         invalid_arg "boxes: data length inconsistent with xtics")
   *     bars *)

  let simple data ?fill ?box_width ?legend () =
    let default =
      Fill.(
        default |> set_color Color.red |> set_pattern ~code:1 ~transparent:false)
    in
    let fill = Option.value ~default fill in
    Boxes {data = {data; style = Style.default; legend}; fill; box_width}
end

module Axes = struct
  let _2 ~xaxis ~yaxis ~xtics ~ytics = Dim2_axes {xaxis; yaxis; xtics; ytics}

  let _3 ~xaxis ~yaxis ~zaxis ~xtics ~ytics ~ztics =
    Dim3_axes {xaxis; yaxis; zaxis; xtics; ytics; ztics}
end

let plot2 ~xaxis ~yaxis ?xtics ?ytics ?title plots =
  let xtics = Option.value ~default:Tics.default xtics in
  let ytics = Option.value ~default:Tics.default ytics in
  Plot {axes = Axes._2 ~xaxis ~yaxis ~xtics ~ytics; plots; title}

let plot3 ~xaxis ~yaxis ~zaxis ?xtics ?ytics ?ztics ?title plots =
  let xtics = Option.value ~default:Tics.default xtics in
  let ytics = Option.value ~default:Tics.default ytics in
  let ztics = Option.value ~default:Tics.default ztics in
  Plot {axes = Axes._3 ~xaxis ~yaxis ~zaxis ~xtics ~ytics ~ztics; plots; title}

(* ------------------------- *)
(* Producing gnuplot scripts *)

let sf = Printf.sprintf

let concat l = String.concat "\n" l

module GP_data : sig
  type t = private string

  val _1d : data_name:string -> r1 Seq.t -> t

  val _2d : data_name:string -> r2 Seq.t -> t

  val _3d : data_name:string -> r3 Seq.t -> t

  val _4d : data_name:string -> r4 Seq.t -> t

  val _s3d : data_name:string -> (string * r3) Seq.t -> t
end = struct
  type t = string

  let _1d ~data_name points =
    let acc = ref [] in
    Seq.iter (fun ({x} : r1) -> acc := string_of_float x :: !acc) points ;
    concat ([sf "$%s << EOD" data_name] @ List.rev !acc @ [sf "EOD"])

  let _2d ~data_name points =
    let acc = ref [] in
    Seq.iter (fun ({x; y} : r2) -> acc := sf "%f %f" x y :: !acc) points ;
    concat ([sf "$%s << EOD" data_name] @ List.rev !acc @ [sf "EOD"])

  let _3d ~data_name points =
    let acc = ref [] in
    Seq.iter (fun ({x; y; z} : r3) -> acc := sf "%f %f %f" x y z :: !acc) points ;
    concat ([sf "$%s << EOD" data_name] @ List.rev !acc @ [sf "EOD"])

  let _4d ~data_name points =
    let acc = ref [] in
    Seq.iter
      (fun ({w; x; y; z} : r4) -> acc := sf "%f %f %f %f" w x y z :: !acc)
      points ;
    concat ([sf "$%s << EOD" data_name] @ List.rev !acc @ [sf "EOD"])

  let _s3d ~data_name points =
    let acc = ref [] in
    Seq.iter
      (fun ((s, {x; y; z}) : string * r3) ->
        acc := sf "%s %f %f %f" s x y z :: !acc)
      points ;
    concat ([sf "$%s << EOD" data_name] @ List.rev !acc @ [sf "EOD"])
end

module GP_style : sig
  type t = private string

  val linecolor : color -> t

  val fillcolor : color -> t [@@ocaml.warning "-32"]

  val fill : fill -> t

  val render : style -> t
end = struct
  type t = string

  let ptcode s =
    match s with
    | Dot -> 0
    | Plus -> 1
    | Cross -> 2
    | Star -> 3
    | Square -> 4
    | Box -> 5
    | Disk -> 6
    | Circle -> 7
    | Delta -> 8
    | Delta_solid -> 9

  (* UI from the 70's *)
  (* Note: this mapping should depend on the terminal type ... *)
  let pointtype (s : pointtype option) (sz : float option) =
    match (s, sz) with
    | None, None -> ""
    | None, Some sz -> sf "pointsize %f" sz
    | Some pt, None -> sf "pointtype %d" (ptcode pt)
    | Some pt, Some sz -> sf "pointtype %d pointsize %f" (ptcode pt) sz

  let linecolor {r; g; b} =
    let r = int_of_float (255. *. r) in
    let g = int_of_float (255. *. g) in
    let b = int_of_float (255. *. b) in
    sf "lc rgb '0x%02X%02X%02X'" r g b

  let fillcolor {r; g; b} =
    let r = int_of_float (255. *. r) in
    let g = int_of_float (255. *. g) in
    let b = int_of_float (255. *. b) in
    sf "fillcolor rgb '0x%02X%02X%02X'" r g b

  let fill (f : fill) =
    let fill_style_str =
      match f.fill_style with
      | Empty -> sf "fs empty"
      | Solid {density; transparent = true} ->
          sf "fs transparent solid %f" density
      | Solid {density; transparent = false} -> sf "fs solid %f" density
      | Pattern {code; transparent = true} ->
          sf "fs transparent pattern %d" code
      | Pattern {code; transparent = false} -> sf "fs pattern %d" code
    in
    let fill_color_str = Option.fold ~none:"" ~some:fillcolor f.fill_color in
    String.concat " " [fill_style_str; fill_color_str]

  let render (style : style) =
    let clr_str = Option.fold ~none:"" ~some:linecolor style.color in
    match style.shape_type with
    | Point {ptyp; psize} ->
        let pointtype_str = pointtype ptyp psize in
        String.concat " " [pointtype_str; clr_str]
    | Circle {fill = None; _} -> clr_str
    | Circle {fill = Some fillspec; _} ->
        let fill_str = fill fillspec in
        let clr_str = Option.fold ~none:"" ~some:linecolor style.color in
        String.concat " " [clr_str; fill_str]
end

module GP_subcommand : sig
  type t = private string

  val scatter_2d : data_name:string -> style -> legend_opt:string option -> t

  val y_error_bars : data_name:string -> style -> legend_opt:string option -> t

  val y_error_lines : data_name:string -> style -> legend_opt:string option -> t

  val scatter_3d : data_name:string -> style -> legend_opt:string option -> t

  val lines_2d : data_name:string -> style -> legend_opt:string option -> t

  val lines_3d : data_name:string -> style -> legend_opt:string option -> t

  val linespoints_2d :
    data_name:string -> style -> legend_opt:string option -> t

  val linespoints_3d :
    data_name:string -> style -> legend_opt:string option -> t

  val histogram :
    data_name:string -> histogram_options -> legend_opt:string option -> t

  val boxes : data_name:string -> legend_opt:string option -> fill:Fill.t -> t
end = struct
  type t = string

  let print_legend legend_opt =
    Option.fold ~none:"notitle" ~some:(fun x -> sf "title '%s'" x) legend_opt

  let scatter_2d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    match s.shape_type with
    | Point _ ->
        sf
          "$%s using 1:2 %s with points %s"
          data_name
          (print_legend legend_opt)
          sty
    | Circle {radius; _} ->
        sf
          "$%s using 1:2:(%f) %s with circles %s"
          data_name
          radius
          (print_legend legend_opt)
          sty

  let y_error_bars ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf
      "$%s using 1:2:3:4 %s with yerrorbars %s"
      data_name
      (print_legend legend_opt)
      sty

  let y_error_lines ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf
      "$%s using 1:2:3:4 %s with yerrorlines %s"
      data_name
      (print_legend legend_opt)
      sty

  let scatter_3d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    match s.shape_type with
    | Point _ ->
        sf
          "$%s using 1:2:3 %s with points %s"
          data_name
          (print_legend legend_opt)
          sty
    | Circle {radius; _} ->
        sf
          "$%s using 1:2:3:(%f) %s with circles %s"
          data_name
          radius
          (print_legend legend_opt)
          sty

  let lines_2d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf "$%s using 1:2 %s with lines %s" data_name (print_legend legend_opt) sty

  let lines_3d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf
      "$%s using 1:2:3 %s with lines %s"
      data_name
      (print_legend legend_opt)
      sty

  let linespoints_2d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf
      "$%s using 1:2 %s with linespoints %s"
      data_name
      (print_legend legend_opt)
      sty

  let linespoints_3d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf
      "$%s using 1:2:3 %s with linespoints %s"
      data_name
      (print_legend legend_opt)
      sty

  let hist_preprocessing_text {bins; binwidth; _} =
    match (bins, binwidth) with
    | None, None -> ""
    | Some nbins, None -> sf "bins=%d" nbins
    | None, Some width -> sf "bins binwidth=%f" width
    | Some nbins, Some width -> sf "bins=%d binwidth=%f" nbins width

  let histogram ~data_name preprocessing_opts ~legend_opt =
    sf
      "$%s using 1 %s %s with boxes %s"
      data_name
      (print_legend legend_opt)
      (hist_preprocessing_text preprocessing_opts)
      (Option.fold
         ~none:""
         ~some:(fun c -> (GP_style.linecolor c :> t))
         preprocessing_opts.color)

  let boxes ~data_name ~legend_opt ~fill =
    sf
      "$%s using 2:3:4:xtic(1) %s with boxes %s"
      data_name
      (print_legend legend_opt)
      (GP_style.fill fill :> string)
end

module GP_command : sig
  type t = private string

  val plot : GP_subcommand.t list -> t

  val splot : GP_subcommand.t list -> t
end = struct
  type t = string

  let concat_subcommands plots =
    List.map (fun (x : GP_subcommand.t) -> (x :> string)) plots
    |> String.concat ", "

  let splot plots = sf "splot %s" (concat_subcommands plots)

  let plot plots = sf "plot %s" (concat_subcommands plots)
end

module GP_script : sig
  type t = private string

  val render : plot -> t

  val render_matrix : title:string -> plot option array array -> t
end = struct
  type t = string

  let set_xlabel l = sf "set xlabel \"%s\"" l

  let set_ylabel l = sf "set ylabel \"%s\"" l

  let set_zlabel l = sf "set zlabel \"%s\"" l

  let set_title t = sf "set title \"%s\"" t

  let tics_spec ~with_logscale (tics : Tics.t) =
    let {border; mirror; in_; rotate_by; position; logscale} = tics in
    let border = if border then "border" else "axis" in
    let mirror = if mirror then "mirror" else "nomirror" in
    let in_ = if in_ then "in" else "out" in
    let rotate_by =
      match rotate_by with
      | None -> "norotate"
      | Some degrees -> sf "rotate by %f" degrees
    in
    let position =
      match position with
      | None -> "autofreq"
      | Some (Tics_increment incr) -> string_of_float incr
      | Some (Tics {start; incr; stop}) -> (
          let pp_float x =
            (* prevent parsing errors by gnuplot, see doc for [xtics series] *)
            if x < 0.0 then sf "0-%f" (-.x) else string_of_float x
          in
          match stop with
          | None -> sf "%s, %s" (pp_float start) (pp_float incr)
          | Some stop -> sf "%s, %s, %f" (pp_float start) (pp_float incr) stop)
    in
    let logscale =
      if with_logscale then if logscale then "logscale" else "nologscale"
      else ""
    in
    sf "%s %s %s %s %s %s" border mirror in_ rotate_by position logscale

  let branch_version_check version script_passed script_failed =
    concat
      ([sf "if (GPVAL_VERSION >= %s) {" version]
      @ script_passed @ ["} else {"] @ script_failed @ ["}"])

  let set_tics tics_kind (tics : Tics.t) =
    branch_version_check
      "5.2"
      [sf "set %s %s" tics_kind (tics_spec ~with_logscale:true tics)]
      [sf "set %s %s" tics_kind (tics_spec ~with_logscale:false tics)]

  let set_xtics (tics : Tics.t) = set_tics "xtics" tics

  let set_ytics (tics : Tics.t) = set_tics "ytics" tics

  let set_ztics (tics : Tics.t) = set_tics "ztics" tics

  let namegen =
    let x = ref (-1) in
    fun () ->
      incr x ;
      sf "name_%d" !x

  let set_fixed_z_range data =
    let fulldata = namegen () in
    concat
      ([sf "set print $%s" fulldata]
      @ List.map (fun x -> sf "print $%s" x) data
      @ ["set print"]
      @ [sf "stats $%s using 3 nooutput" fulldata]
      @ ["set zrange [STATS_min:STATS_max]"])

  let rec seq_map2 (seq1 : 'a Seq.t) (seq2 : 'b Seq.t) f : _ Seq.t =
   fun () ->
    match (seq1 (), seq2 ()) with
    | Nil, Nil -> Nil
    | Cons (x, tl1), Cons (y, tl2) -> Cons (f x y, seq_map2 tl1 tl2 f)
    | _ -> invalid_arg "seq_map2: uneven length"

  let make_error_bars (data : r2 Seq.t) (error_bars : r2 Seq.t) =
    seq_map2 data error_bars (fun ({x; y} : r2) ({x = y1; y = y2} : r2) ->
        r4 x y (y +. y1) (y -. y2))

  let scatter (type dim) (axes : dim axes)
      ({data; style; legend} : dim Data.t with_metadata)
      (error_bars : r2 Seq.t option) =
    match axes with
    | Dim2_axes _ -> (
        let data_name = namegen () in
        match error_bars with
        | None ->
            let data_block = GP_data._2d ~data_name data in
            let command_chunk =
              GP_subcommand.scatter_2d ~data_name style ~legend_opt:legend
            in
            (`Data data_block, `Command command_chunk, data_name)
        | Some error_bars ->
            let data_with_errors =
              GP_data._4d ~data_name (make_error_bars data error_bars)
            in
            let command_chunk =
              GP_subcommand.y_error_bars ~data_name style ~legend_opt:legend
            in
            (`Data data_with_errors, `Command command_chunk, data_name))
    | Dim3_axes _ ->
        let data_name = namegen () in
        let data_block = GP_data._3d ~data_name data in
        let command_chunk =
          GP_subcommand.scatter_3d ~data_name style ~legend_opt:legend
        in
        (`Data data_block, `Command command_chunk, data_name)

  let line (type dim) (axes : dim axes)
      ({data; style; legend} : dim Data.t with_metadata) with_points error_bars
      =
    match axes with
    | Dim2_axes _ -> (
        let data_name = namegen () in
        match error_bars with
        | None ->
            let data_block = GP_data._2d ~data_name data in
            let command_chunk =
              if with_points then
                GP_subcommand.linespoints_2d ~data_name style ~legend_opt:legend
              else GP_subcommand.lines_2d ~data_name style ~legend_opt:legend
            in
            (`Data data_block, `Command command_chunk, data_name)
        | Some error_bars ->
            (* assert (not with_points) ; *)
            let data_with_errors =
              GP_data._4d ~data_name (make_error_bars data error_bars)
            in
            let command_chunk =
              GP_subcommand.y_error_lines ~data_name style ~legend_opt:legend
            in
            (`Data data_with_errors, `Command command_chunk, data_name))
    | Dim3_axes _ ->
        let data_name = namegen () in
        let data_block = GP_data._3d ~data_name data in
        let command_chunk =
          if with_points then
            GP_subcommand.linespoints_3d ~data_name style ~legend_opt:legend
          else GP_subcommand.lines_3d ~data_name style ~legend_opt:legend
        in
        (`Data data_block, `Command command_chunk, data_name)

  let histogram (data : r1 Seq.t) (options : histogram_options) legend_opt =
    let data_name = namegen () in
    let data_block = GP_data._1d ~data_name data in
    let command_chunk =
      GP_subcommand.histogram ~data_name options ~legend_opt
    in
    (`Data data_block, `Command command_chunk, data_name)

  let boxes (data : (string * float) Data.t with_metadata) box_width fill =
    let ys = List.of_seq data.data in
    let len = List.length ys in
    let xs = Stdlib.List.init len (fun i -> float_of_int (1 + i)) in
    let data_name = namegen () in
    let data_block =
      let box_width = match box_width with None -> 1. | Some w -> w in
      GP_data._s3d
        ~data_name
        (List.to_seq
           (Stdlib.List.map2
              (fun x (s, y) -> ("\"" ^ s ^ "\"", r3 x y box_width))
              xs
              ys))
    in
    let command_chunk =
      GP_subcommand.boxes ~data_name ~legend_opt:data.legend ~fill
    in
    (`Data data_block, `Command command_chunk, data_name)

  let rec spec_list :
      type dim.
      dim axes ->
      dim spec list ->
      GP_data.t list ->
      GP_subcommand.t list ->
      string list ->
      GP_data.t list * GP_subcommand.t list * string list =
    fun (type dim)
        (axes : dim axes)
        (specs : dim spec list)
        data_acc
        cmd_acc
        name_acc ->
     match specs with
     | [] -> (List.rev data_acc, List.rev cmd_acc, List.rev name_acc)
     | Scatter {data; error_bars} :: tl ->
         let `Data data_block, `Command command_chunk, data_name =
           scatter axes data error_bars
         in
         spec_list
           axes
           tl
           (data_block :: data_acc)
           (command_chunk :: cmd_acc)
           (data_name :: name_acc)
     | Histogram {data; options; legend} :: tl ->
         let `Data data_block, `Command command_chunk, data_name =
           histogram data options legend
         in
         spec_list
           axes
           tl
           (data_block :: data_acc)
           (command_chunk :: cmd_acc)
           (data_name :: name_acc)
     | Line {data; with_points; error_bars} :: tl ->
         let `Data data_block, `Command command_chunk, data_name =
           line axes data with_points error_bars
         in
         spec_list
           axes
           tl
           (data_block :: data_acc)
           (command_chunk :: cmd_acc)
           (data_name :: name_acc)
     | Boxes {data; box_width; fill} :: tl ->
         let `Data data_block, `Command command_chunk, data_name =
           boxes data box_width fill
         in
         spec_list
           axes
           tl
           (data_block :: data_acc)
           (command_chunk :: cmd_acc)
           (data_name :: name_acc)

  let render ?save (Plot {axes; plots; title}) =
    let title_cmd =
      match title with None -> set_title "" | Some title -> set_title title
    in
    let save_cmd =
      match save with None -> [] | Some savefile -> [sf "save %s" savefile]
    in
    match axes with
    | Dim2_axes {xaxis; yaxis; xtics; ytics} ->
        let all_data, cmds, _all_data_names = spec_list axes plots [] [] [] in
        let all_data = (all_data :> string list) in
        let cmd = (GP_command.plot cmds :> string) in
        concat
          ([
             set_xlabel xaxis;
             set_ylabel yaxis;
             set_xtics xtics;
             set_ytics ytics;
             concat all_data;
             title_cmd;
             cmd;
           ]
          @ save_cmd)
    | Dim3_axes {xaxis; yaxis; zaxis; xtics; ytics; ztics} ->
        let all_data, cmds, all_data_names = spec_list axes plots [] [] [] in
        let all_data = (all_data :> string list) in
        let cmd = (GP_command.splot cmds :> string) in
        concat
          ([
             set_xlabel xaxis;
             set_ylabel yaxis;
             set_zlabel zaxis;
             set_xtics xtics;
             set_ytics ytics;
             set_ztics ztics;
             concat all_data;
             title_cmd;
             set_fixed_z_range all_data_names;
             cmd;
           ]
          @ save_cmd)

  let save_file i j = sf "ARG0.'save_%d_%d.option'" i j

  let multiplot ~title ~matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let size = rows * cols in
    let plots =
      List.flatten (Array.to_list ((Array.map Array.to_list) matrix))
    in
    concat
      ([
         sf
           "set multiplot layout %d, %d rowsfirst downwards title '%s'"
           rows
           cols
           title;
       ]
      @ plots
      @ [sf "unset multiplot"]
      @ [sf "N = %d" size]
      @ ["array plots[N]"]
      @ Stdlib.List.init size (fun c ->
            let i = c mod rows in
            let j = c / rows in
            sf "plots[%d] = %s" (i + 1) (save_file i j))
      @ ["multi = 1"] @ ["c = 1"]
      (* Macro for plotting *)
      @ [
          sf
            "PLOT = \"if (multi == 1) {min = 1; max = N; rn = %d; cn = %d} \
             else {min = c; max = c; rn = 1; cn = 1}; set multiplot layout rn, \
             cn rowsfirst downwards title ''; do for [i=min:max] { load \
             plots[i]; if (multi == 0) {clear; replot}}; unset multiplot\""
            rows
            cols;
        ]
      (* Key bindings to navigate the plots *)
      (* Ctrl+Left/Right: previous/next plot*)
      @ [
          "bind 'ctrl-Right' 'if (multi == 1) { multi = 0 } else { c = (c % N) \
           + 1 };'.PLOT";
        ]
      @ [
          "bind 'ctrl-Left' 'if (multi == 1) { multi = 0 } else { c = ((c + N \
           - 2) % N) + 1 };'.PLOT";
        ]
      (* Ctrl+Down: Show multiplot *)
      @ ["bind 'ctrl-Down' 'if (multi == 0) { multi = 1 };'.PLOT"]
      (* Ctrl+Up: Show single plot *)
      @ ["bind 'ctrl-Up' 'if (multi == 1) { multi = 0 };'.PLOT"])

  let render_matrix ~title plots =
    let plot_matrix =
      Array.mapi
        (fun i ->
          Array.mapi (fun j -> function
            | None -> "set multiplot next"
            | Some p -> render ~save:(save_file i j) p))
        plots
    in
    multiplot ~title ~matrix:plot_matrix

  let render = render ?save:None
end

(* Plot targets *)
type target =
  | Pdf_target of {cm_size : (float * float) option; pdf_file : string}
  | Png_target of {pixel_size : (int * int) option; png_file : string}
  | X11_target
  | Qt_target of {pixel_size : (int * int) option}

let pdf ?cm_size ~pdf_file () = Pdf_target {cm_size; pdf_file}

let png ?pixel_size ~png_file () = Png_target {pixel_size; png_file}

let x11 = X11_target

let qt ?pixel_size () = Qt_target {pixel_size}

module GP_run = struct
  let set_target (t : target) =
    let print_pixel_size pixel_size =
      Option.fold ~none:"" ~some:(fun (x, y) -> sf "size %d, %d" x y) pixel_size
    in
    match t with
    | Png_target {pixel_size; png_file} ->
        concat
          [
            sf "set terminal pngcairo %s" (print_pixel_size pixel_size);
            sf "set output '%s'" png_file;
          ]
    | Pdf_target {cm_size; pdf_file} ->
        concat
          [
            sf
              "set terminal pdf %s"
              (Option.fold
                 ~none:""
                 ~some:(fun (x, y) -> sf "size %fcm, %fcm" x y)
                 cm_size);
            sf "set output '%s'" pdf_file;
          ]
    | X11_target -> "set terminal x11 noreplotonresize"
    | Qt_target {pixel_size} ->
        sf "set terminal qt %s" (print_pixel_size pixel_size)

  let is_target_interactive (t : target) =
    match t with
    | Png_target _ | Pdf_target _ -> false
    | X11_target | Qt_target _ -> true

  let make_script ~matrix_mode ~target ~(plot : GP_script.t) =
    concat
      ([set_target target; (plot :> string)]
      @ (if is_target_interactive target then ["pause mouse close"] else [])
      @ if matrix_mode then ["do for [i=1:N]{system 'rm '.plots[i]}"] else [])

  let write_script ~filename ~matrix_mode ~target ~(plot : GP_script.t) =
    match open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o666 filename with
    | exception _ ->
        Format.eprintf
          "write_script: could not open file %s, exiting@."
          filename ;
        exit 1
    | oc ->
        let script = make_script ~matrix_mode ~target ~plot in
        output_string oc script ;
        close_out oc

  let run_script ?(path = "gnuplot") ?(detach = false) ~matrix_mode ~target
      ~(plot : GP_script.t) () =
    let name, oc = Filename.open_temp_file ~perms:0o666 "gnuplot" ".gp" in
    let full_command =
      concat
        ([set_target target; (plot :> string)]
        @ (if is_target_interactive target then ["pause mouse close"] else [])
        @ if matrix_mode then ["do for [i=1:N]{system 'rm '.plots[i]}"] else []
        )
    in
    output_string oc full_command ;
    close_out oc ;
    match Unix.fork () with
    | 0 -> (
        match Unix.system (sf "%s %s 2>/dev/null" path name) with
        | WEXITED 0 ->
            Unix.unlink name ;
            exit 0
        | _ ->
            Unix.unlink name ;
            Format.eprintf "run_script: call to gnuplot failed, exiting@." ;
            exit 1)
    | pid -> if not detach then ignore @@ Unix.waitpid [] pid else ()

  let write_and_run_script ?(path = "gnuplot") ?(detach = false) ~filename
      ~matrix_mode ~target ~(plot : GP_script.t) () =
    match open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o666 filename with
    | exception _ ->
        Format.eprintf
          "write_script: could not open file %s, exiting@."
          filename ;
        exit 1
    | oc -> (
        let script = make_script ~matrix_mode ~target ~plot in
        output_string oc script ;
        close_out oc ;
        match Unix.fork () with
        | 0 -> (
            match Unix.system (sf "%s %s 2>/dev/null" path filename) with
            | WEXITED 0 -> exit 0
            | _ ->
                Format.eprintf
                  "run_script: call to gnuplot failed, exiting (wrote script \
                   to %s)@."
                  filename ;
                exit 1)
        | pid -> if not detach then ignore @@ Unix.waitpid [] pid else ())
end

type action =
  | Exec
  | Save_to of string
  | Exec_and_save_to of string
  | Exec_detach

let exec = Exec

let exec_detach = Exec_detach

let save_to filename = Save_to filename

let exec_and_save_to filename = Exec_and_save_to filename

let write_plot ~filename ~target ~plot =
  let plot = GP_script.render plot in
  GP_run.write_script ~filename ~matrix_mode:false ~target ~plot

let run_plot ?path ?detach ~target ~plot () =
  let plot = GP_script.render plot in
  GP_run.run_script ?path ?detach ~matrix_mode:false ~target ~plot ()

let write_and_run_plot ?path ~filename ~target ~plot () =
  let plot = GP_script.render plot in
  GP_run.write_and_run_script
    ?path
    ~filename
    ~matrix_mode:false
    ~target
    ~plot
    ()

let write_matrix ~filename ~title ~target ~plots =
  let plot = GP_script.render_matrix ~title plots in
  GP_run.write_script ~filename ~matrix_mode:true ~target ~plot

let run_matrix ?path ?detach ~title ~target ~plots () =
  let plot = GP_script.render_matrix ~title plots in
  GP_run.run_script ?path ?detach ~matrix_mode:true ~target ~plot ()

let write_and_run_matrix ?path ~title ~filename ~target ~plots () =
  let plot = GP_script.render_matrix ~title plots in
  GP_run.write_and_run_script ?path ~filename ~matrix_mode:true ~target ~plot ()

let run ?path ~target action plot =
  match action with
  | Exec -> run_plot ?path ~plot ~target ()
  | Save_to filename -> write_plot ~filename ~target ~plot
  | Exec_and_save_to filename ->
      write_and_run_plot ?path ~filename ~target ~plot ()
  | Exec_detach -> run_plot ?path ~detach:true ~plot ~target ()

let run_matrix ?path ~target ?(title = "") action plots =
  match action with
  | Exec -> run_matrix ?path ~title ~plots ~target ()
  | Save_to filename -> write_matrix ~filename ~title ~target ~plots
  | Exec_and_save_to filename ->
      write_and_run_matrix ?path ~title ~filename ~target ~plots ()
  | Exec_detach -> run_matrix ?path ~detach:true ~title ~plots ~target ()

let get_targets ?(path = "gnuplot") () =
  let for_reading_by_parent, for_writing_by_child =
    Unix.pipe ~cloexec:false ()
  in
  match Unix.fork () with
  | 0 ->
      let _ = Unix.dup2 ~cloexec:false for_writing_by_child Unix.stdout in
      let _ = Unix.dup2 ~cloexec:false for_writing_by_child Unix.stderr in
      Unix.execvp path [|path; "-e"; "print(GPVAL_TERMINALS); quit"|]
  | child_pid ->
      Unix.close for_writing_by_child ;
      let rec waitloop () =
        match Unix.waitpid [] child_pid with
        | exception Unix.Unix_error (Unix.EINTR, _, _) ->
            (* [waitpid] gets interrupted, probably because some other child of
               the parent process terminated while waiting (?) *)
            waitloop ()
        | _, WEXITED 0 ->
            let ic = Unix.in_channel_of_descr for_reading_by_parent in
            let buf = Buffer.create 1024 in
            (try
               while true do
                 Buffer.add_channel buf ic 512
               done
             with End_of_file -> ()) ;
            close_in ic ;
            let file = Buffer.contents buf in
            Option.some (String.split_on_char ' ' (String.trim file))
        | _ ->
            Format.eprintf "Child process terminated abnormally@." ;
            None
      in
      waitloop ()
