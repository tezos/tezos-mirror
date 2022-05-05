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

type dim2

type dim3

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

(** Plots are called "Axes" in matplotlib. For now we provide 2d & 3d scatterplots and
    histograms. We abstract monadically over matplotlib's statefulness. *)
module Axis : sig
  type ('a, 'k) t

  val return : 'a -> ('a, 'k) t

  val ( let* ) : ('a, 'k) t -> ('a -> ('b, 'k) t) -> ('b, 'k) t

  val run : ('a, 'k) t -> Pytypes.pyobject -> 'a

  (** Set the x-axis label for the current plot. *)
  val set_xlabel : string -> (unit, 'k) t

  (** Set the y-axis label for the current plot. *)
  val set_ylabel : string -> (unit, 'k) t

  (** Set the z-axis label for the current plot. *)
  val set_zlabel : string -> (unit, 'k) t

  (** Set the title for the current plot. *)
  val set_title : string -> (unit, 'k) t

  (** Override the range of the y axis manually. *)
  val set_ylim : ymin:float option -> ymax:float option -> (unit, 'k) t

  (** Plots a histogram. The matrix [h] contains columns(h) histogram.
      I.e. data for each histograms are stored as column vectors. *)
  val histogram_1d : h:Matrix.t -> opts:histogram_options -> (unit, dim2) t

  (** 2d scatter plot. [xs] stores the x-components of the points and [ys] the y-components
      of the points. [xs] and [ys] should be row vectors of the same dimensions.
      [color] specifies the color used for the plot. [shape] specifies the shape
      of each point in the scatter plot. *)
  val scatter_2d :
    xs:Matrix.t -> ys:Matrix.t -> color:color -> shape:shape -> (unit, dim2) t

  (** See [scatter_2d]. *)
  val scatter_3d :
    xs:Matrix.t ->
    ys:Matrix.t ->
    zs:Matrix.t ->
    color:color ->
    shape:shape ->
    (unit, dim3) t

  (** 2d line plot. [xs] stores the x-components and [ys] the y-components.
      [line] is an optional style, by default a red solid line will be drawn. *)
  val line_2d :
    xs:Matrix.t ->
    ys:Matrix.t ->
    legend:string option ->
    line:line option ->
    (unit, dim2) t

  (** Activates displaying legends defined up to call time for plots in current axis. *)
  val legend : unit -> (unit, 'k) t
end

(** The type of layouts of plots. This specifies how a plot should be displayed.
    matplotlib allows to compose plots together as subplots. We provide
    another monadic interface to abstract over matplotlib's statefulness.

    We can't use the same monad as in the [Axis] module because the corresponding
    states are handled distinctly in matplotlib. *)
type 'a t

val return : 'a -> 'a t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

(** Performs a layout in a matrix of [nrows]x[ncols] subplots. *)
val run : ?figsize:float * float -> nrows:int -> ncols:int -> 'a t -> 'a

(** Add a 2d subplot at the prescribde position in the ambient subplot matrix. *)
val subplot_2d : row:int -> col:int -> (unit, dim2) Axis.t -> unit t

(** Add a 3d subplot at the prescribde position in the ambient subplot matrix. *)
val subplot_3d : row:int -> col:int -> (unit, dim3) Axis.t -> unit t

(** Declarative helper to produce a 2d or 3d scatterplot. *)
val scatter : 'a scatter -> (unit, 'a) Axis.t

(** Declarative helper to produce a histogram. *)
val histogram : histogram -> (unit, dim2) Axis.t

(** Sets title for whole figure. *)
val suptitle : title:string -> fontsize:int option -> unit t

(** Saves the current layout to a file. *)
val savefig : filename:string -> dpi:int -> quality:int -> unit t

(** Displays the current layout on the screen. *)
val show : unit -> unit t

(** Initializes python. This function has to be called first. *)
val init : ?interpreter:string -> ?version:int -> unit -> unit
