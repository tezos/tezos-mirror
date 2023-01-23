(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2021 Ilias Garnier                                          *)
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

(** [prbnmcn-gnuplot]: a self-contained interface to Gnuplot. *)

(** {1 Introduction}

    We give an introduction to the library by giving some examples.
    These examples can be run by performing

    {[> dune exec ./test/example.exe]}

    in the root of the library.

    Let's assume throughout we've access to some sampler for the standard normal
    distribution:

    {[
      val normal : Random.State.t -> float
    ]}

    {2 Data: 1d, 2d, 3d}

    The library encapsulates 1d, 2d and 3d data points using the types
    {!type-Plot.r1}, {!type-Plot.r2} and {!type-Plot.r3}.  Similarly named
    functions allow to inject floating-point values into these types.

    As an example, we can write samplers for 2d and 3d normal variates with
    specified means:

    {[
      (** [st] is the random state *)
      let normal2 (mu_x, mu_y)
        st = Plot.r2 (normal st +. mu_x) (normal st +. mu_y)
    ]}

    {2 Scatter plot example}

    Let's plot some clouds of points using [normal2].
    For 2d scatter plots, we use {!Plot.Scatter.points_2d}.
    We need to feed this function with a value of type [r2 Plot.Data.t].
    For instance, the following function specifies a 2d scatter plot with 100 random
    normally distributed points with mean [mu].

    {[
      let scatter ~mu st =
        Plot.Scatter.points_2d
          ~points:(Plot.Data.of_array (Array.init 100 (fun _ -> normal2 mu st)))
          ()
    ]}

    Let's produce a plot with two clouds around [(3, 2)] and
    [(10, 8)] and display it using the {!Plot.plot2} function.

    {[
      let st = Random.State.make_self_init ()

      let scatter2d_example =
        Plot.plot2
          ~xaxis:"x"
          ~yaxis:"y"
          ~title:"Clouds"
          [scatter ~mu:(3., 2.) st; scatter ~mu:(10., 8.) st]
    ]}

    Finally, we can produce the plot and save it, say to a .png file.

    {[
      let () =
        let target = Plot.png ~pixel_size:(1024, 1024) ~png_file:"scatter.png" () in
        Plot.(run ~plot:scatter2d_example ~target exec)
    ]}

    3d plots work similarly, using the {!Plot.Scatter.points_3d} function.

    {2 Line plot example}

    The library also handles line plots, through the functions exposed in
    {!Plot.Line}. The API works similarly. Functions must be discretized
    before being plotted: for the
    sake of the example, let's introduce the following function.

    {[
      let discretize f =
        Array.init 100 (fun i ->
            let x = float_of_int i *. 0.1 in
            let y = f x in
            Plot.r2 x y)
    ]}

    We prepare a fancy plot for the discretized sine function as follows.

    {[
      let sine_with_points =
        let open Plot in
        Line.line_2d
          ~points:(Data.of_array (discretize sin))
          ~style:
            Style.(default |> set_color Color.red |> set_point ~ptyp:Pointtype.box)
          ~with_points:true
          ()
    ]}

    We display it similarly as in the scatter plot case.
    {[
      let line2d_example =
        Plot.plot2 ~xaxis:"x" ~yaxis:"y" ~title:"Lines" [sine_with_points]

      let () =
        let target = Plot.png ~pixel_size:(1024, 1024) ~png_file:"lines.png" () in
        Plot.(run ~plot:line2d_example ~target exec)
    ]}

    {2 Histogram example}

    Finally, the library allows to prepare histograms.

    {[
      let gaussian =
        let open Plot in
        Histogram.hist
          ~points:(Data.of_array (Array.init 100 (fun _ -> r1 @@ normal st)))
          ()

      let histogram_example =
        Plot.plot2 ~xaxis:"x" ~yaxis:"freq" ~title:"Histogram" [gaussian]

      let () =
        let target = Plot.png ~pixel_size:(1024, 1024) ~png_file:"histogram.png" () in
        Plot.(run ~plot:histogram_example ~target exec)
    ]}

    The binning is performed
    automatically by Gnuplot: currently, the API does not provide any functionality
    for manually specifed box plots.
*)

(** {1 API} *)

(** {2 Raw data} *)

(** [r1, r2, r3] encode elements of [R^n] for [n] equal to 1, 2 or 3. *)
type r1

type r2

type r3

(** [r1, r2, r3] are injections into their respective types. *)
val r1 : float -> r1

val r2 : float -> float -> r2

val r3 : float -> float -> float -> r3

(** [tup_r2, tup_r3] proceed on tuples. *)
val tup_r2 : float * float -> r2

val tup_r3 : float * float * float -> r3

(** {2 Colors, styles, etc} *)

(** Colors *)
module Color : sig
  type t

  val red : t

  val blue : t

  val green : t

  val black : t

  (** [rgb r g b] specifies a color.

      @raise Invalid_arg if [r], [g], or [b] do not belong to the [0,1] interval. *)
  val rgb : float -> float -> float -> t
end

(** Pointtypes *)
module Pointtype : sig
  type t

  val dot : t

  val square : t

  val box : t

  val plus : t

  val cross : t

  val star : t

  val circle : t

  val disk : t

  val delta : t

  val delta_solid : t
end

(** Filling shapes. *)
module Fill : sig
  type t

  (** The default Gnuplot fill style. *)
  val default : t

  (** Sets the fill style to be "empty" (ie no fill). *)
  val set_empty : t -> t

  (** Sets the fill style to be "solid" with prescribed density and transparency flag. *)
  val set_solid : density:float -> transparent:bool -> t -> t

  (** Sets the fill style to use the pattern specified by [code].
      [code] corresponds to a Gnuplot pattern code (see Gnuplot doc).
      [transparent] sets the transparency flag. *)
  val set_pattern : code:int -> transparent:bool -> t -> t

  (** Sets the fill color. *)
  val set_color : Color.t -> t -> t
end

(** Styles *)
module Style : sig
  type t

  (** The default plotting style corresponds to using points with the Gnuplot
      defaults for size and color. *)
  val default : t

  (** Sets the plotting style to use points.
      - The optional argument [?ptyp] specifies the Gnuplot pointtype.
      - The optional argument [?psize] specifies the point size. *)
  val set_point : ?ptyp:Pointtype.t -> ?psize:float -> t -> t

  (** Sets the plotting style to use circles.
      - The argument [radius] specifies the circle radius.
      - The optional argument [?fill] specifies how to fill the circle and defaults
      to {!Fill.default}. *)
  val set_circle : ?fill:Fill.t -> radius:float -> t -> t

  val set_color : Color.t -> t -> t
end

(** Tics. See the gnuplot documentation for the [xtics] command for the meaning
    of these options. *)
module Tics : sig
  type t

  val default : t

  val set_border : bool -> t -> t

  val set_mirror : bool -> t -> t

  val set_outwards : bool -> t -> t

  val set_rotate : degrees:float -> t -> t

  val set_position : ?start:float -> ?incr:float -> ?stop:float -> t -> t

  val set_logscale : bool -> t -> t
end

(** {2 Datasets} *)

(** Datasets *)
module Data : sig
  (** ['dim t] is the type of datasets of type ['dim]. *)
  type 'dim t

  (** Import a dataset from a [Seq.t] *)
  val of_seq : 'dim Seq.t -> 'dim t

  (** Import a dataset from an [array] *)
  val of_array : 'dim array -> 'dim t

  (** Import a dataset from a [list] *)
  val of_list : 'dim list -> 'dim t
end

(** {2 Plot specifications} *)

(** ['dim spec] is the type of a declarative plot specification for data of type ['dim]. *)
type 'dim spec

(** {3 Scatter plots} *)

(** Preparing scatter plots. *)
module Scatter : sig
  (** [points_2d ~points ~style ?legend ()] creates a 2d scatter plot using [style] to
       select color and point shape.
      - The optional argument [?style] defaults to {!Style.default}
      - The optional argument [?legend] defaults to [""].
      - The optional argument [?error_bars], if present, is a sequence of [r2] elements
        such that each element [(y1, y2)] is displayed as a vertical error bar.
        The error bar associated to the element [(x, y)] then correspond to the segment
        starting at [(x, y + y1)] and ending at [(x, y - y2)].

      @raise Invalid_argument if the length of [error_bars], if present, is not equal to
             that of [points]. *)
  val points_2d :
    points:r2 Data.t ->
    ?style:Style.t ->
    ?legend:string ->
    ?error_bars:r2 Data.t ->
    unit ->
    r2 spec

  (** [points_3d ~points ~style ?legend] creates a 3d scatter plot. See {!points_2d} for the
      meaning of other options. *)
  val points_3d :
    points:r3 Data.t -> ?style:Style.t -> ?legend:string -> unit -> r3 spec
end

(** {3 Line plots} *)

(** Preparing line plots. *)
module Line : sig
  (** [line_2d ~points ?style ?legend ?with_points ?error_bars ()] creates a 2d line plot.
      - The optional argument [?style] defaults to {!Style.default}
      - The optional argument [?legend] defaults to [""].
      - The optional argument [?with_points] set to [true] will display additional symbols
        (specified through the [style]) at the positions corresponding to [points].
        This argument is ignored if [?error_bars] is passed.
      - The optional argument [?error_bars], if present, is a sequence of [r2] elements
        such that each element [(y1, y2)] is displayed as a vertical error bar.
        The error bar associated to the element [(x, y)] then correspond to the segment
        starting at [(x, y + y1)] and ending at [(x, y - y2)].

      @raise Invalid_argument if the length of [error_bars], if present, is not equal to
             that of [points].
   *)
  val line_2d :
    points:r2 Data.t ->
    ?style:Style.t ->
    ?legend:string ->
    ?with_points:bool ->
    ?error_bars:r2 Data.t ->
    unit ->
    r2 spec

  (** [line_3d ~points ~style ~legend] creates a 3d line plot. See {!line_2d} for
      the meaning of the other options. *)
  val line_3d :
    points:r3 Data.t ->
    ?style:Style.t ->
    ?legend:string ->
    ?with_points:bool ->
    unit ->
    r3 spec

  (** [line ~points ?style ?legend ?with_points ?error_bars ()] creates a 2d line plot.
      [points] is a sequence of [y] coordinates y{_1}, y{_2}, y{_3} ...,
      which is interpreted as the sequence (1, y{_1}), (2, y{_2}), (3, y{_3})
      and then passed to {!line_2d}. See the documentation of {!line_2d} for the
      meaning of the other options. *)
  val line :
    points:r1 Data.t ->
    ?style:Style.t ->
    ?legend:string ->
    ?with_points:bool ->
    ?error_bars:r2 Data.t ->
    unit ->
    r2 spec
end

(** {3 Histograms} *)

(** Preparing histograms.  *)
module Histogram : sig
  (** [hist ~points ?color ?bins ?binwidth ?legend] creates an histogram out of [points].
      - The optional argument [?color] specifies the color of the histogram.
      - The optional argument [?bins] specifies the number of bins to be used
        when constructing the histogram.
      - The optional argument [?binwidth] specifies the width of each bin: the bigger,
        the coarser the histogram.
      - The optional argument [?legend] specifies the legend attached to
        the histogram.
      If neither the [?bins] or [?binwidth] parameter is specified,
      this command will let [?bins] default to [20] and will let [?binwidth] be
      automatically computed by Gnuplot. *)
  val hist :
    points:r1 Data.t ->
    ?color:Color.t ->
    ?bins:int ->
    ?binwidth:float ->
    ?legend:string ->
    unit ->
    r2 spec
end

(** {4 Bar plots} *)
module Bar : sig
  (** A value of type [bars] correspond to some bar heights together with a fill style.  *)
  (* type bars *)

  (* (\** [bars data ?fill ()] create some bars. *\)
   * val bars : r1 Data.t -> ?fill:Fill.t -> unit -> bars *)

  val simple :
    (string * float) Data.t ->
    ?fill:Fill.t ->
    ?box_width:float ->
    ?legend:string ->
    unit ->
    r2 spec
end

(** {2 Plot rendering} *)

(** A [plot] consists of
    - a choice of [axes] (2d or 3d, with a name for each axis)
    - a list of plot [spec]s consistent with the dimension of the [axes]
    - a title *)
type plot

(** [plot2 ~xaxis ~yaxis ?xtics ?ytics ?title specs] construct a 2d plot. The plots [specs]
    are stacked together in the same figure. All the plots share the same
    2d axis system, with names [xaxis] and [yaxis]. *)
val plot2 :
  xaxis:string ->
  yaxis:string ->
  ?xtics:Tics.t ->
  ?ytics:Tics.t ->
  ?title:string ->
  r2 spec list ->
  plot

(** [plot3 ~xaxis ~yaxis ~zaxis ~title specs] construct a 3d plot. The plots [specs]
    are stacked together in the same figure. All the plots share the same
    3d axis system, with names [xaxis], [yaxis] and [zaxis]. *)
val plot3 :
  xaxis:string ->
  yaxis:string ->
  zaxis:string ->
  ?xtics:Tics.t ->
  ?ytics:Tics.t ->
  ?ztics:Tics.t ->
  ?title:string ->
  r3 spec list ->
  plot

(** Plot target. Note that the [Qt] and [X11] targets do not work well
    with multiple ("matrix") plots on some tiling window managers or generally when resizing
    windows. In this case, it is recommended to use the [Pdf] or [Png] targets
    and use a third-party visualizer. *)
type target

(** [pdf ?cm_size ~pdf_file ()] specifies that the plot should be written to
    the file [pdf_file] using the pdf format.
    The optional argument [?cm_size] specifies the width and height of the figure in [cm].
    If not specified, Gnuplot will select the aspect ratio and size automatically. *)
val pdf : ?cm_size:float * float -> pdf_file:string -> unit -> target

(** [png ?pixel_size ~png_file ()] specifies that the plot should be written to
    the file [png_file] using the PNG format.
    The optional argument [?pixel_size] specifies the width and height of the figure in [pixels].
    If not specified, Gnuplot will select the aspect ratio and size automatically. *)
val png : ?pixel_size:int * int -> png_file:string -> unit -> target

(** The X11 target. This target is interactive (it will open a window). *)
val x11 : target

(** The qt target. This target is interactive (it will open a window).
    The optional argument [?pixel_size] specifies the width and height of the figure in [cm].
    If not specified, Gnuplot will select the aspect ratio and size automatically. *)
val qt : ?pixel_size:int * int -> unit -> target

(** [get_targets ()] returns a list of all available targets (called 'terminals' by
    gnuplot). Returns [None] if the list of targets could not be obtained. *)
val get_targets : ?path:string -> unit -> string list option

(** The type of actions to perform with a plot. *)
type action

(** [exec] executes the plot *)
val exec : action

(** [exec_detach] executes the plot but doesn't block the program *)
val exec_detach : action

(** [save_to filename] saves the script to [filename] (does not execute the script) *)
val save_to : string -> action

(** [exec_and_save_to filename] executes the plot and also saves the script [filename] *)
val exec_and_save_to : string -> action

(** [run ?path ~plot ~target action] performs [action] on [plot] using [target].
    - The optional argument [?path] defaults to ["gnuplot"].

    For instance, to run some [plot] to the [qt] backend, one would perform the call:
    {[
      run ~plot ~target:(qt ()) exec
    ]} *)
val run : ?path:string -> target:target -> action -> plot -> unit

(** [run_matrix ?path ~plots ~target action] performs [action] on the plot matrix [plots] using [target].
    - The optional argument [?path] defaults to ["gnuplot"].
    - The optional argument [?title] specifies a title for the collection of sub-plots. It defaults to [""]. *)
val run_matrix :
  ?path:string ->
  target:target ->
  ?title:string ->
  action ->
  plot option array array ->
  unit
