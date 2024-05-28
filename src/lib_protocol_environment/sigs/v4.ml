(* This file was automatically generated, do not edit.*)
(* Edit file v4.in.ml instead. *)
# 1 "v4.in.ml"
module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : sig
# 1 "v4/pervasives.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The OCaml Standard library.

    This module is automatically opened at the beginning of each
    compilation. All components of this module can therefore be
    referred by their short name, without prefixing them by [Stdlib].

    It particular, it provides the basic operations over the built-in
    types (numbers, booleans, byte sequences, strings, exceptions,
    references, lists, arrays, input-output channels, ...) and the
    {{!modules}standard library modules}.
*)

(** {1 Exceptions} *)

external raise : exn -> 'a = "%raise"
(** Raise the given exception value *)

external raise_notrace : exn -> 'a = "%raise_notrace"
(** A faster version [raise] which does not record the backtrace.
    @since 4.02.0
*)

val invalid_arg : string -> 'a
(** Raise exception [Invalid_argument] with the given string. *)

val failwith : string -> 'a
(** Raise exception [Failure] with the given string. *)

exception Exit
(** The [Exit] exception is not raised by any library function.  It is
    provided for use in your programs. *)

(** {1 Boolean operations} *)

external not : bool -> bool = "%boolnot"
(** The boolean negation. *)

external ( && ) : bool -> bool -> bool = "%sequand"
(** The boolean 'and'. Evaluation is sequential, left-to-right:
   in [e1 && e2], [e1] is evaluated first, and if it returns [false],
   [e2] is not evaluated at all.
   Right-associative operator,  see {!Ocaml_operators} for more information.
*)

external ( || ) : bool -> bool -> bool = "%sequor"
(** The boolean 'or'. Evaluation is sequential, left-to-right:
   in [e1 || e2], [e1] is evaluated first, and if it returns [true],
   [e2] is not evaluated at all.
   Right-associative operator,  see {!Ocaml_operators} for more information.
*)

(** {1 Debugging} *)

external __LOC__ : string = "%loc_LOC"
(** [__LOC__] returns the location at which this expression appears in
    the file currently being parsed by the compiler, with the standard
    error format of OCaml: "File %S, line %d, characters %d-%d".
    @since 4.02.0
*)

external __FILE__ : string = "%loc_FILE"
(** [__FILE__] returns the name of the file currently being
    parsed by the compiler.
    @since 4.02.0
*)

external __LINE__ : int = "%loc_LINE"
(** [__LINE__] returns the line number at which this expression
    appears in the file currently being parsed by the compiler.
    @since 4.02.0
*)

external __MODULE__ : string = "%loc_MODULE"
(** [__MODULE__] returns the module name of the file being
    parsed by the compiler.
    @since 4.02.0
*)

external __POS__ : string * int * int * int = "%loc_POS"
(** [__POS__] returns a tuple [(file,lnum,cnum,enum)], corresponding
    to the location at which this expression appears in the file
    currently being parsed by the compiler. [file] is the current
    filename, [lnum] the line number, [cnum] the character position in
    the line and [enum] the last character position in the line.
    @since 4.02.0
 *)

external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
(** [__LOC_OF__ expr] returns a pair [(loc, expr)] where [loc] is the
    location of [expr] in the file currently being parsed by the
    compiler, with the standard error format of OCaml: "File %S, line
    %d, characters %d-%d".
    @since 4.02.0
*)

external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
(** [__LINE_OF__ expr] returns a pair [(line, expr)], where [line] is the
    line number at which the expression [expr] appears in the file
    currently being parsed by the compiler.
    @since 4.02.0
 *)

external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"
(** [__POS_OF__ expr] returns a pair [(loc,expr)], where [loc] is a
    tuple [(file,lnum,cnum,enum)] corresponding to the location at
    which the expression [expr] appears in the file currently being
    parsed by the compiler. [file] is the current filename, [lnum] the
    line number, [cnum] the character position in the line and [enum]
    the last character position in the line.
    @since 4.02.0
 *)

(** {1 Composition operators} *)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
(** Reverse-application operator: [x |> f |> g] is exactly equivalent
 to [g (f (x))].
 Left-associative operator, see {!Ocaml_operators} for more information.
 @since 4.01
*)

external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
(** Application operator: [g @@ f @@ x] is exactly equivalent to
 [g (f (x))].
 Right-associative operator, see {!Ocaml_operators} for more information.
 @since 4.01
*)

(** {1 Integer arithmetic} *)

(** Integers are [Sys.int_size] bits wide.
    All operations are taken modulo 2{^[Sys.int_size]}.
    They do not fail on overflow. *)

external ( ~- ) : int -> int = "%negint"
(** Unary negation. You can also write [- e] instead of [~- e].
    Unary operator, see {!Ocaml_operators} for more information.
*)


external ( ~+ ) : int -> int = "%identity"
(** Unary addition. You can also write [+ e] instead of [~+ e].
    Unary operator, see {!Ocaml_operators} for more information.
    @since 3.12.0
*)

external succ : int -> int = "%succint"
(** [succ x] is [x + 1]. *)

external pred : int -> int = "%predint"
(** [pred x] is [x - 1]. *)

external ( + ) : int -> int -> int = "%addint"
(** Integer addition.
    Left-associative operator, see {!Ocaml_operators} for more information.
*)

external ( - ) : int -> int -> int = "%subint"
(** Integer subtraction.
    Left-associative operator, , see {!Ocaml_operators} for more information.
*)

external ( * ) : int -> int -> int = "%mulint"
(** Integer multiplication.
    Left-associative operator, see {!Ocaml_operators} for more information.
*)

external ( / ) : int -> int -> int = "%divint"
(** Integer division.
   Raise [Division_by_zero] if the second argument is 0.
   Integer division rounds the real quotient of its arguments towards zero.
   More precisely, if [x >= 0] and [y > 0], [x / y] is the greatest integer
   less than or equal to the real quotient of [x] by [y].  Moreover,
   [(- x) / y = x / (- y) = - (x / y)].
   Left-associative operator, see {!Ocaml_operators} for more information.
*)

external ( mod ) : int -> int -> int = "%modint"
(** Integer remainder.  If [y] is not zero, the result
   of [x mod y] satisfies the following properties:
   [x = (x / y) * y + x mod y] and
   [abs(x mod y) <= abs(y) - 1].
   If [y = 0], [x mod y] raises [Division_by_zero].
   Note that [x mod y] is negative only if [x < 0].
   Raise [Division_by_zero] if [y] is zero.
   Left-associative operator, see {!Ocaml_operators} for more information.
*)

val abs : int -> int
(** Return the absolute value of the argument.  Note that this may be
  negative if the argument is [min_int]. *)

val max_int : int
(** The greatest representable integer. *)

val min_int : int
(** The smallest representable integer. *)


(** {2 Bitwise operations} *)

external ( land ) : int -> int -> int = "%andint"
(** Bitwise logical and.
    Left-associative operator, see {!Ocaml_operators} for more information.
*)

external ( lor ) : int -> int -> int = "%orint"
(** Bitwise logical or.
    Left-associative operator, see {!Ocaml_operators} for more information.
*)

external ( lxor ) : int -> int -> int = "%xorint"
(** Bitwise logical exclusive or.
    Left-associative operator, see {!Ocaml_operators} for more information.
*)

val lnot : int -> int
(** Bitwise logical negation. *)

external ( lsl ) : int -> int -> int = "%lslint"
(** [n lsl m] shifts [n] to the left by [m] bits.
    The result is unspecified if [m < 0] or [m > Sys.int_size].
    Right-associative operator, see {!Ocaml_operators} for more information.
*)

external ( lsr ) : int -> int -> int = "%lsrint"
(** [n lsr m] shifts [n] to the right by [m] bits.
    This is a logical shift: zeroes are inserted regardless of
    the sign of [n].
    The result is unspecified if [m < 0] or [m > Sys.int_size].
    Right-associative operator, see {!Ocaml_operators} for more information.
*)

external ( asr ) : int -> int -> int = "%asrint"
(** [n asr m] shifts [n] to the right by [m] bits.
    This is an arithmetic shift: the sign bit of [n] is replicated.
    The result is unspecified if [m < 0] or [m > Sys.int_size].
    Right-associative operator, see {!Ocaml_operators} for more information.
*)

(** {1 String operations}

   More string operations are provided in module {!String}.
*)

val ( ^ ) : string -> string -> string
(** String concatenation.
    Right-associative operator, see {!Ocaml_operators} for more information.
*)

(** {1 Character operations}

   More character operations are provided in module {!Char}.
*)

external int_of_char : char -> int = "%identity"
(** Return the ASCII code of the argument. *)

val char_of_int : int -> char
(** Return the character with the given ASCII code.
   Raise [Invalid_argument "char_of_int"] if the argument is
   outside the range 0--255. *)


(** {1 Unit operations} *)

external ignore : 'a -> unit = "%ignore"
(** Discard the value of its argument and return [()].
   For instance, [ignore(f x)] discards the result of
   the side-effecting function [f].  It is equivalent to
   [f x; ()], except that the latter may generate a
   compiler warning; writing [ignore(f x)] instead
   avoids the warning. *)


(** {1 String conversion functions} *)

val string_of_bool : bool -> string
(** Return the string representation of a boolean. As the returned values
   may be shared, the user should not modify them directly.
*)

val bool_of_string_opt: string -> bool option
(** Convert the given string to a boolean.

   Return [None] if the string is not ["true"] or ["false"].
   @since 4.05
*)

val string_of_int : int -> string
(** Return the string representation of an integer, in decimal. *)

val int_of_string_opt: string -> int option
(** Convert the given string to an integer.
   The string is read in decimal (by default, or if the string
   begins with [0u]), in hexadecimal (if it begins with [0x] or
   [0X]), in octal (if it begins with [0o] or [0O]), or in binary
   (if it begins with [0b] or [0B]).

   The [0u] prefix reads the input as an unsigned integer in the range
   [[0, 2*max_int+1]].  If the input exceeds {!max_int}
   it is converted to the signed integer
   [min_int + input - max_int - 1].

   The [_] (underscore) character can appear anywhere in the string
   and is ignored.

   Return [None] if the given string is not a valid representation of an
   integer, or if the integer represented exceeds the range of integers
   representable in type [int].
   @since 4.05
*)

(** {1 Pair operations} *)

external fst : 'a * 'b -> 'a = "%field0"
(** Return the first component of a pair. *)

external snd : 'a * 'b -> 'b = "%field1"
(** Return the second component of a pair. *)


(** {1 List operations}

   More list operations are provided in module {!List}.
*)

val ( @ ) : 'a list -> 'a list -> 'a list
(** List concatenation.  Not tail-recursive (length of the first argument).
  Right-associative operator, see {!Ocaml_operators} for more information.
*)

(** {1 References} *)

type 'a ref = { mutable contents : 'a }
(** The type of references (mutable indirection cells) containing
   a value of type ['a]. *)

external ref : 'a -> 'a ref = "%makemutable"
(** Return a fresh reference containing the given value. *)

external ( ! ) : 'a ref -> 'a = "%field0"
(** [!r] returns the current contents of reference [r].
   Equivalent to [fun r -> r.contents].
   Unary operator, see {!Ocaml_operators} for more information.
*)

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
(** [r := a] stores the value of [a] in reference [r].
   Equivalent to [fun r v -> r.contents <- v].
   Right-associative operator, see {!Ocaml_operators} for more information.
*)

external incr : int ref -> unit = "%incr"
(** Increment the integer contained in the given reference.
   Equivalent to [fun r -> r := succ !r]. *)

external decr : int ref -> unit = "%decr"
(** Decrement the integer contained in the given reference.
   Equivalent to [fun r -> r := pred !r]. *)

(** {1 Result type} *)

(** @since 4.03.0 *)
type ('a,'b) result = Ok of 'a | Error of 'b

(** {1 Operations on format strings} *)

(** Format strings are character strings with special lexical conventions
  that defines the functionality of formatted input/output functions. Format
  strings are used to read data with formatted input functions from module
  {!Scanf} and to print data with formatted output functions from modules
  {!Printf} and {!Format}.

  Format strings are made of three kinds of entities:
  - {e conversions specifications}, introduced by the special character ['%']
    followed by one or more characters specifying what kind of argument to
    read or print,
  - {e formatting indications}, introduced by the special character ['@']
    followed by one or more characters specifying how to read or print the
    argument,
  - {e plain characters} that are regular characters with usual lexical
    conventions. Plain characters specify string literals to be read in the
    input or printed in the output.

  There is an additional lexical rule to escape the special characters ['%']
  and ['@'] in format strings: if a special character follows a ['%']
  character, it is treated as a plain character. In other words, ["%%"] is
  considered as a plain ['%'] and ["%@"] as a plain ['@'].

  For more information about conversion specifications and formatting
  indications available, read the documentation of modules {!Scanf},
  {!Printf} and {!Format}.
*)

(** Format strings have a general and highly polymorphic type
    [('a, 'b, 'c, 'd, 'e, 'f) format6].
    The two simplified types, [format] and [format4] below are
    included for backward compatibility with earlier releases of
    OCaml.

    The meaning of format string type parameters is as follows:

    - ['a] is the type of the parameters of the format for formatted output
      functions ([printf]-style functions);
      ['a] is the type of the values read by the format for formatted input
      functions ([scanf]-style functions).

    - ['b] is the type of input source for formatted input functions and the
      type of output target for formatted output functions.
      For [printf]-style functions from module {!Printf}, ['b] is typically
      [out_channel];
      for [printf]-style functions from module {!Format}, ['b] is typically
      {!Format.formatter};
      for [scanf]-style functions from module {!Scanf}, ['b] is typically
      {!Scanf.Scanning.in_channel}.

      Type argument ['b] is also the type of the first argument given to
      user's defined printing functions for [%a] and [%t] conversions,
      and user's defined reading functions for [%r] conversion.

    - ['c] is the type of the result of the [%a] and [%t] printing
      functions, and also the type of the argument transmitted to the
      first argument of [kprintf]-style functions or to the
      [kscanf]-style functions.

    - ['d] is the type of parameters for the [scanf]-style functions.

    - ['e] is the type of the receiver function for the [scanf]-style functions.

    - ['f] is the final result type of a formatted input/output function
      invocation: for the [printf]-style functions, it is typically [unit];
      for the [scanf]-style functions, it is typically the result type of the
      receiver function.
*)

type ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

val string_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
(** Converts a format string into a string. *)

external format_of_string :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"
(** [format_of_string s] returns a format string read from the string
    literal [s].
    Note: [format_of_string] can not convert a string argument that is not a
    literal. If you need this functionality, use the more general
    {!Scanf.format_from_string} function.
*)

val ( ^^ ) :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('f, 'b, 'c, 'e, 'g, 'h) format6 ->
  ('a, 'b, 'c, 'd, 'g, 'h) format6
(** [f1 ^^ f2] catenates format strings [f1] and [f2]. The result is a
  format string that behaves as the concatenation of format strings [f1] and
  [f2]: in case of formatted output, it accepts arguments from [f1], then
  arguments from [f2]; in case of formatted input, it returns results from
  [f1], then results from [f2].
  Right-associative operator, see {!Ocaml_operators} for more information.
*)
end
# 6 "v4.in.ml"


  open Pervasives

  module String : sig
# 1 "v4/string.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** String operations.

  A string is an immutable data structure that contains a
  fixed-length sequence of (single-byte) characters. Each character
  can be accessed in constant time through its index.

  Given a string [s] of length [l], we can access each of the [l]
  characters of [s] via its index in the sequence. Indexes start at
  [0], and we will call an index valid in [s] if it falls within the
  range [[0...l-1]] (inclusive). A position is the point between two
  characters or at the beginning or end of the string.  We call a
  position valid in [s] if it falls within the range [[0...l]]
  (inclusive). Note that the character at index [n] is between
  positions [n] and [n+1].

  Two parameters [start] and [len] are said to designate a valid
  substring of [s] if [len >= 0] and [start] and [start+len] are
  valid positions in [s].

  Note: OCaml strings used to be modifiable in place, for instance via
  the {!String.set} and {!String.blit} functions described below. This
  usage is only possible when the compiler is put in "unsafe-string"
  mode by giving the [-unsafe-string] command-line option. This
  compatibility mode makes the types [string] and [bytes] (see module
  {!Bytes}) interchangeable so that functions expecting byte sequences
  can also accept strings as arguments and modify them.

  The distinction between [bytes] and [string] was introduced in OCaml
  4.02, and the "unsafe-string" compatibility mode was the default
  until OCaml 4.05. Starting with 4.06, the compatibility mode is
  opt-in; we intend to remove the option in the future.
*)

external length : string -> int = "%string_length"
(** Return the length (number of characters) of the given string. *)

external get : string -> int -> char = "%string_safe_get"
(** [String.get s n] returns the character at index [n] in string [s].
   You can also write [s.[n]] instead of [String.get s n].

   Raise [Invalid_argument] if [n] not a valid index in [s]. *)


val make : int -> char -> string
(** [String.make n c] returns a fresh string of length [n],
   filled with the character [c].

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val init : int -> (int -> char) -> string
(** [String.init n f] returns a string of length [n], with character
    [i] initialized to the result of [f i] (called in increasing
    index order).

    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}.

    @since 4.02.0
*)

val sub : string -> int -> int -> string
(** [String.sub s start len] returns a fresh string of length [len],
   containing the substring of [s] that starts at position [start] and
   has length [len].

   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid substring of [s]. *)

val blit : string -> int -> bytes -> int -> int -> unit
(** Same as {!Bytes.blit_string}. *)

val concat : string -> string list -> string
(** [String.concat sep sl] concatenates the list of strings [sl],
    inserting the separator string [sep] between each.

    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_string_length} bytes. *)

val iter : (char -> unit) -> string -> unit
(** [String.iter f s] applies function [f] in turn to all
   the characters of [s].  It is equivalent to
   [f s.[0]; f s.[1]; ...; f s.[String.length s - 1]; ()]. *)

val iteri : (int -> char -> unit) -> string -> unit
(** Same as {!String.iter}, but the
   function is applied to the index of the element as first argument
   (counting from 0), and the character itself as second argument.
   @since 4.00.0 *)

val map : (char -> char) -> string -> string
(** [String.map f s] applies function [f] in turn to all the
    characters of [s] (in increasing index order) and stores the
    results in a new string that is returned.
    @since 4.00.0 *)

val mapi : (int -> char -> char) -> string -> string
(** [String.mapi f s] calls [f] with each character of [s] and its
    index (in increasing index order) and stores the results in a new
    string that is returned.
    @since 4.02.0 *)

val trim : string -> string
(** Return a copy of the argument, without leading and trailing
   whitespace.  The characters regarded as whitespace are: [' '],
   ['\012'], ['\n'], ['\r'], and ['\t'].  If there is neither leading nor
   trailing whitespace character in the argument, return the original
   string itself, not a copy.
   @since 4.00.0 *)

val escaped : string -> string
(** Return a copy of the argument, with special characters
    represented by escape sequences, following the lexical
    conventions of OCaml.
    All characters outside the ASCII printable range (32..126) are
    escaped, as well as backslash and double-quote.

    If there is no special character in the argument that needs
    escaping, return the original string itself, not a copy.

    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_string_length} bytes.

    The function {!Scanf.unescaped} is a left inverse of [escaped],
    i.e. [Scanf.unescaped (escaped s) = s] for any string [s] (unless
    [escape s] fails). *)

val index_opt: string -> char -> int option
(** [String.index_opt s c] returns the index of the first
    occurrence of character [c] in string [s], or
    [None] if [c] does not occur in [s].
    @since 4.05 *)

val rindex_opt: string -> char -> int option
(** [String.rindex_opt s c] returns the index of the last occurrence
    of character [c] in string [s], or [None] if [c] does not occur in
    [s].
    @since 4.05 *)

val index_from_opt: string -> int -> char -> int option
(** [String.index_from_opt s i c] returns the index of the
    first occurrence of character [c] in string [s] after position [i]
    or [None] if [c] does not occur in [s] after position [i].

    [String.index_opt s c] is equivalent to [String.index_from_opt s 0 c].
    Raise [Invalid_argument] if [i] is not a valid position in [s].

    @since 4.05
*)

val rindex_from_opt: string -> int -> char -> int option
(** [String.rindex_from_opt s i c] returns the index of the
   last occurrence of character [c] in string [s] before position [i+1]
   or [None] if [c] does not occur in [s] before position [i+1].

   [String.rindex_opt s c] is equivalent to
   [String.rindex_from_opt s (String.length s - 1) c].

   Raise [Invalid_argument] if [i+1] is not a valid position in [s].

    @since 4.05
*)

val contains : string -> char -> bool
(** [String.contains s c] tests if character [c]
   appears in the string [s]. *)

val contains_from : string -> int -> char -> bool
(** [String.contains_from s start c] tests if character [c]
   appears in [s] after position [start].
   [String.contains s c] is equivalent to
   [String.contains_from s 0 c].

   Raise [Invalid_argument] if [start] is not a valid position in [s]. *)

val rcontains_from : string -> int -> char -> bool
(** [String.rcontains_from s stop c] tests if character [c]
   appears in [s] before position [stop+1].

   Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
   position in [s]. *)

val uppercase_ascii : string -> string
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, using the US-ASCII character set.
   @since 4.03.0 *)

val lowercase_ascii : string -> string
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, using the US-ASCII character set.
   @since 4.03.0 *)

val capitalize_ascii : string -> string
(** Return a copy of the argument, with the first character set to uppercase,
   using the US-ASCII character set.
   @since 4.03.0 *)

val uncapitalize_ascii : string -> string
(** Return a copy of the argument, with the first character set to lowercase,
   using the US-ASCII character set.
   @since 4.03.0 *)

type t = string
(** An alias for the type of strings. *)

val compare: t -> t -> int
(** The comparison function for strings, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [String] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for strings.
    @since 4.03.0 *)

val split_on_char: char -> string -> string list
(** [String.split_on_char sep s] returns the list of all (possibly empty)
    substrings of [s] that are delimited by the [sep] character.

    The function's output is specified by the following invariants:

    - The list is not empty.
    - Concatenating its elements using [sep] as a separator returns a
      string equal to the input ([String.concat (String.make 1 sep)
      (String.split_on_char sep s) = s]).
    - No string in the result contains the [sep] character.

    @since 4.04.0
*)
end
# 10 "v4.in.ml"


  module Char : sig
# 1 "v4/char.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Character operations. *)

external code : char -> int = "%identity"
(** Return the ASCII code of the argument. *)

val chr : int -> char
(** Return the character with the given ASCII code.
   Raise [Invalid_argument "Char.chr"] if the argument is
   outside the range 0--255. *)

val escaped : char -> string
(** Return a string representing the given character,
    with special characters escaped following the lexical conventions
    of OCaml.
    All characters outside the ASCII printable range (32..126) are
    escaped, as well as backslash, double-quote, and single-quote. *)

val lowercase_ascii : char -> char
(** Convert the given character to its equivalent lowercase character,
   using the US-ASCII character set.
   @since 4.03.0 *)

val uppercase_ascii : char -> char
(** Convert the given character to its equivalent uppercase character,
   using the US-ASCII character set.
   @since 4.03.0 *)

type t = char
(** An alias for the type of characters. *)

val compare: t -> t -> int
(** The comparison function for characters, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for chars.
    @since 4.03.0 *)
end
# 12 "v4.in.ml"


  module Bytes : sig
# 1 "v4/bytes.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Byte sequence operations.

   A byte sequence is a mutable data structure that contains a
   fixed-length sequence of bytes. Each byte can be indexed in
   constant time for reading or writing.

   Given a byte sequence [s] of length [l], we can access each of the
   [l] bytes of [s] via its index in the sequence. Indexes start at
   [0], and we will call an index valid in [s] if it falls within the
   range [[0...l-1]] (inclusive). A position is the point between two
   bytes or at the beginning or end of the sequence.  We call a
   position valid in [s] if it falls within the range [[0...l]]
   (inclusive). Note that the byte at index [n] is between positions
   [n] and [n+1].

   Two parameters [start] and [len] are said to designate a valid
   range of [s] if [len >= 0] and [start] and [start+len] are valid
   positions in [s].

   Byte sequences can be modified in place, for instance via the [set]
   and [blit] functions described below.  See also strings (module
   {!String}), which are almost the same data structure, but cannot be
   modified in place.

   Bytes are represented by the OCaml type [char].

   @since 4.02.0
 *)

external length : bytes -> int = "%bytes_length"
(** Return the length (number of bytes) of the argument. *)

external get : bytes -> int -> char = "%bytes_safe_get"
(** [get s n] returns the byte at index [n] in argument [s].

    Raise [Invalid_argument] if [n] is not a valid index in [s]. *)

external set : bytes -> int -> char -> unit = "%bytes_safe_set"
(** [set s n c] modifies [s] in place, replacing the byte at index [n]
    with [c].

    Raise [Invalid_argument] if [n] is not a valid index in [s]. *)

val make : int -> char -> bytes
(** [make n c] returns a new byte sequence of length [n], filled with
    the byte [c].

    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val init : int -> (int -> char) -> bytes
(** [Bytes.init n f] returns a fresh byte sequence of length [n], with
    character [i] initialized to the result of [f i] (in increasing
    index order).

    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val empty : bytes
(** A byte sequence of size 0. *)

val copy : bytes -> bytes
(** Return a new byte sequence that contains the same bytes as the
    argument. *)

val of_string : string -> bytes
(** Return a new byte sequence that contains the same bytes as the
    given string. *)

val to_string : bytes -> string
(** Return a new string that contains the same bytes as the given byte
    sequence. *)

val sub : bytes -> int -> int -> bytes
(** [sub s start len] returns a new byte sequence of length [len],
    containing the subsequence of [s] that starts at position [start]
    and has length [len].

    Raise [Invalid_argument] if [start] and [len] do not designate a
    valid range of [s]. *)

val sub_string : bytes -> int -> int -> string
(** Same as [sub] but return a string instead of a byte sequence. *)

val extend : bytes -> int -> int -> bytes
(** [extend s left right] returns a new byte sequence that contains
    the bytes of [s], with [left] uninitialized bytes prepended and
    [right] uninitialized bytes appended to it. If [left] or [right]
    is negative, then bytes are removed (instead of appended) from
    the corresponding side of [s].

    Raise [Invalid_argument] if the result length is negative or
    longer than {!Sys.max_string_length} bytes. *)

val fill : bytes -> int -> int -> char -> unit
(** [fill s start len c] modifies [s] in place, replacing [len]
    characters with [c], starting at [start].

    Raise [Invalid_argument] if [start] and [len] do not designate a
    valid range of [s]. *)

val blit : bytes -> int -> bytes -> int -> int -> unit
(** [blit src srcoff dst dstoff len] copies [len] bytes from sequence
    [src], starting at index [srcoff], to sequence [dst], starting at
    index [dstoff]. It works correctly even if [src] and [dst] are the
    same byte sequence, and the source and destination intervals
    overlap.

    Raise [Invalid_argument] if [srcoff] and [len] do not
    designate a valid range of [src], or if [dstoff] and [len]
    do not designate a valid range of [dst]. *)

val blit_string : string -> int -> bytes -> int -> int -> unit
(** [blit src srcoff dst dstoff len] copies [len] bytes from string
    [src], starting at index [srcoff], to byte sequence [dst],
    starting at index [dstoff].

    Raise [Invalid_argument] if [srcoff] and [len] do not
    designate a valid range of [src], or if [dstoff] and [len]
    do not designate a valid range of [dst]. *)

val concat : bytes -> bytes list -> bytes
(** [concat sep sl] concatenates the list of byte sequences [sl],
    inserting the separator byte sequence [sep] between each, and
    returns the result as a new byte sequence.

    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_string_length} bytes. *)

val cat : bytes -> bytes -> bytes
(** [cat s1 s2] concatenates [s1] and [s2] and returns the result
     as new byte sequence.

    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_string_length} bytes. *)

val iter : (char -> unit) -> bytes -> unit
(** [iter f s] applies function [f] in turn to all the bytes of [s].
    It is equivalent to [f (get s 0); f (get s 1); ...; f (get s
    (length s - 1)); ()]. *)

val iteri : (int -> char -> unit) -> bytes -> unit
(** Same as {!Bytes.iter}, but the function is applied to the index of
    the byte as first argument and the byte itself as second
    argument. *)

val map : (char -> char) -> bytes -> bytes
(** [map f s] applies function [f] in turn to all the bytes of [s]
    (in increasing index order) and stores the resulting bytes in
    a new sequence that is returned as the result. *)

val mapi : (int -> char -> char) -> bytes -> bytes
(** [mapi f s] calls [f] with each character of [s] and its
    index (in increasing index order) and stores the resulting bytes
    in a new sequence that is returned as the result. *)

val trim : bytes -> bytes
(** Return a copy of the argument, without leading and trailing
    whitespace. The bytes regarded as whitespace are the ASCII
    characters [' '], ['\012'], ['\n'], ['\r'], and ['\t']. *)

val escaped : bytes -> bytes
(** Return a copy of the argument, with special characters represented
    by escape sequences, following the lexical conventions of OCaml.
    All characters outside the ASCII printable range (32..126) are
    escaped, as well as backslash and double-quote.

    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_string_length} bytes. *)

val index_opt: bytes -> char -> int option
(** [index_opt s c] returns the index of the first occurrence of byte [c]
    in [s] or [None] if [c] does not occur in [s].
    @since 4.05 *)

val rindex_opt: bytes -> char -> int option
(** [rindex_opt s c] returns the index of the last occurrence of byte [c]
    in [s] or [None] if [c] does not occur in [s].
    @since 4.05 *)

val index_from_opt: bytes -> int -> char -> int option
(** [index_from _opts i c] returns the index of the first occurrence of
    byte [c] in [s] after position [i] or [None] if [c] does not occur in [s]
    after position [i].
    [Bytes.index_opt s c] is equivalent to [Bytes.index_from_opt s 0 c].

    Raise [Invalid_argument] if [i] is not a valid position in [s].
    @since 4.05 *)

val rindex_from_opt: bytes -> int -> char -> int option
(** [rindex_from_opt s i c] returns the index of the last occurrence
    of byte [c] in [s] before position [i+1] or [None] if [c] does not
    occur in [s] before position [i+1].  [rindex_opt s c] is equivalent to
    [rindex_from s (Bytes.length s - 1) c].

    Raise [Invalid_argument] if [i+1] is not a valid position in [s].
    @since 4.05 *)

val contains : bytes -> char -> bool
(** [contains s c] tests if byte [c] appears in [s]. *)

val contains_from : bytes -> int -> char -> bool
(** [contains_from s start c] tests if byte [c] appears in [s] after
    position [start].  [contains s c] is equivalent to [contains_from
    s 0 c].

    Raise [Invalid_argument] if [start] is not a valid position in [s]. *)

val rcontains_from : bytes -> int -> char -> bool
(** [rcontains_from s stop c] tests if byte [c] appears in [s] before
    position [stop+1].

    Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
    position in [s]. *)

val uppercase_ascii : bytes -> bytes
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, using the US-ASCII character set.
   @since 4.03.0 *)

val lowercase_ascii : bytes -> bytes
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, using the US-ASCII character set.
   @since 4.03.0 *)

val capitalize_ascii : bytes -> bytes
(** Return a copy of the argument, with the first character set to uppercase,
   using the US-ASCII character set.
   @since 4.03.0 *)

val uncapitalize_ascii : bytes -> bytes
(** Return a copy of the argument, with the first character set to lowercase,
   using the US-ASCII character set.
   @since 4.03.0 *)

type t = bytes
(** An alias for the type of byte sequences. *)

val compare: t -> t -> int
(** The comparison function for byte sequences, with the same
    specification as {!Stdlib.compare}.  Along with the type [t],
    this function [compare] allows the module [Bytes] to be passed as
    argument to the functors {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equality function for byte sequences.
    @since 4.03.0 *)
end
# 14 "v4.in.ml"


  module Int32 : sig
# 1 "v4/int32.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** 32-bit integers.

   This module provides operations on the type [int32]
   of signed 32-bit integers.  Unlike the built-in [int] type,
   the type [int32] is guaranteed to be exactly 32-bit wide on all
   platforms.  All arithmetic operations over [int32] are taken
   modulo 2{^32}.

   Performance notice: values of type [int32] occupy more memory
   space than values of type [int], and arithmetic operations on
   [int32] are generally slower than those on [int].  Use [int32]
   only when the application requires exact 32-bit arithmetic.

    Literals for 32-bit integers are suffixed by l:
    {[
      let zero: int32 = 0l
      let one: int32 = 1l
      let m_one: int32 = -1l
    ]}
*)

val zero : int32
(** The 32-bit integer 0. *)

val one : int32
(** The 32-bit integer 1. *)

val minus_one : int32
(** The 32-bit integer -1. *)

external neg : int32 -> int32 = "%int32_neg"
(** Unary negation. *)

external add : int32 -> int32 -> int32 = "%int32_add"
(** Addition. *)

external sub : int32 -> int32 -> int32 = "%int32_sub"
(** Subtraction. *)

external mul : int32 -> int32 -> int32 = "%int32_mul"
(** Multiplication. *)

external div : int32 -> int32 -> int32 = "%int32_div"
(** Integer division.  Raise [Division_by_zero] if the second
   argument is zero.  This division rounds the real quotient of
   its arguments towards zero, as specified for {!Stdlib.(/)}. *)

external rem : int32 -> int32 -> int32 = "%int32_mod"
(** Integer remainder.  If [y] is not zero, the result
   of [Int32.rem x y] satisfies the following property:
   [x = Int32.add (Int32.mul (Int32.div x y) y) (Int32.rem x y)].
   If [y = 0], [Int32.rem x y] raises [Division_by_zero]. *)

val succ : int32 -> int32
(** Successor.  [Int32.succ x] is [Int32.add x Int32.one]. *)

val pred : int32 -> int32
(** Predecessor.  [Int32.pred x] is [Int32.sub x Int32.one]. *)

val abs : int32 -> int32
(** Return the absolute value of its argument. *)

val max_int : int32
(** The greatest representable 32-bit integer, 2{^31} - 1. *)

val min_int : int32
(** The smallest representable 32-bit integer, -2{^31}. *)


external logand : int32 -> int32 -> int32 = "%int32_and"
(** Bitwise logical and. *)

external logor : int32 -> int32 -> int32 = "%int32_or"
(** Bitwise logical or. *)

external logxor : int32 -> int32 -> int32 = "%int32_xor"
(** Bitwise logical exclusive or. *)

val lognot : int32 -> int32
(** Bitwise logical negation. *)

external shift_left : int32 -> int -> int32 = "%int32_lsl"
(** [Int32.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= 32]. *)

external shift_right : int32 -> int -> int32 = "%int32_asr"
(** [Int32.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= 32]. *)

external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"
(** [Int32.shift_right_logical x y] shifts [x] to the right by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= 32]. *)

external of_int : int -> int32 = "%int32_of_int"
(** Convert the given integer (type [int]) to a 32-bit integer
    (type [int32]). On 64-bit platforms, the argument is taken
    modulo 2{^32}. *)

external to_int : int32 -> int = "%int32_to_int"
(** Convert the given 32-bit integer (type [int32]) to an
   integer (type [int]).  On 32-bit platforms, the 32-bit integer
   is taken modulo 2{^31}, i.e. the high-order bit is lost
   during the conversion.  On 64-bit platforms, the conversion
   is exact. *)

val of_string_opt: string -> int32 option
(** Same as [of_string], but return [None] instead of raising.
    @since 4.05 *)


val to_string : int32 -> string
(** Return the string representation of its argument, in signed decimal. *)

type t = int32
(** An alias for the type of 32-bit integers. *)

val compare: t -> t -> int
(** The comparison function for 32-bit integers, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Int32] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for int32s.
    @since 4.03.0 *)
end
# 16 "v4.in.ml"


  module Int64 : sig
# 1 "v4/int64.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** 64-bit integers.

   This module provides operations on the type [int64] of
   signed 64-bit integers.  Unlike the built-in [int] type,
   the type [int64] is guaranteed to be exactly 64-bit wide on all
   platforms.  All arithmetic operations over [int64] are taken
   modulo 2{^64}

   Performance notice: values of type [int64] occupy more memory
   space than values of type [int], and arithmetic operations on
   [int64] are generally slower than those on [int].  Use [int64]
   only when the application requires exact 64-bit arithmetic.

    Literals for 64-bit integers are suffixed by L:
    {[
      let zero: int64 = 0L
      let one: int64 = 1L
      let m_one: int64 = -1L
    ]}
*)

val zero : int64
(** The 64-bit integer 0. *)

val one : int64
(** The 64-bit integer 1. *)

val minus_one : int64
(** The 64-bit integer -1. *)

external neg : int64 -> int64 = "%int64_neg"
(** Unary negation. *)

external add : int64 -> int64 -> int64 = "%int64_add"
(** Addition. *)

external sub : int64 -> int64 -> int64 = "%int64_sub"
(** Subtraction. *)

external mul : int64 -> int64 -> int64 = "%int64_mul"
(** Multiplication. *)

external div : int64 -> int64 -> int64 = "%int64_div"
(** Integer division.  Raise [Division_by_zero] if the second
   argument is zero.  This division rounds the real quotient of
   its arguments towards zero, as specified for {!Stdlib.(/)}. *)

external rem : int64 -> int64 -> int64 = "%int64_mod"
(** Integer remainder.  If [y] is not zero, the result
   of [Int64.rem x y] satisfies the following property:
   [x = Int64.add (Int64.mul (Int64.div x y) y) (Int64.rem x y)].
   If [y = 0], [Int64.rem x y] raises [Division_by_zero]. *)

val succ : int64 -> int64
(** Successor.  [Int64.succ x] is [Int64.add x Int64.one]. *)

val pred : int64 -> int64
(** Predecessor.  [Int64.pred x] is [Int64.sub x Int64.one]. *)

val abs : int64 -> int64
(** Return the absolute value of its argument. *)

val max_int : int64
(** The greatest representable 64-bit integer, 2{^63} - 1. *)

val min_int : int64
(** The smallest representable 64-bit integer, -2{^63}. *)

external logand : int64 -> int64 -> int64 = "%int64_and"
(** Bitwise logical and. *)

external logor : int64 -> int64 -> int64 = "%int64_or"
(** Bitwise logical or. *)

external logxor : int64 -> int64 -> int64 = "%int64_xor"
(** Bitwise logical exclusive or. *)

val lognot : int64 -> int64
(** Bitwise logical negation. *)

external shift_left : int64 -> int -> int64 = "%int64_lsl"
(** [Int64.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= 64]. *)

external shift_right : int64 -> int -> int64 = "%int64_asr"
(** [Int64.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= 64]. *)

external shift_right_logical : int64 -> int -> int64 = "%int64_lsr"
(** [Int64.shift_right_logical x y] shifts [x] to the right by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= 64]. *)

external of_int : int -> int64 = "%int64_of_int"
(** Convert the given integer (type [int]) to a 64-bit integer
    (type [int64]). *)

external to_int : int64 -> int = "%int64_to_int"
(** Convert the given 64-bit integer (type [int64]) to an
   integer (type [int]).  On 64-bit platforms, the 64-bit integer
   is taken modulo 2{^63}, i.e. the high-order bit is lost
   during the conversion.  On 32-bit platforms, the 64-bit integer
   is taken modulo 2{^31}, i.e. the top 33 bits are lost
   during the conversion. *)

external of_int32 : int32 -> int64 = "%int64_of_int32"
(** Convert the given 32-bit integer (type [int32])
   to a 64-bit integer (type [int64]). *)

external to_int32 : int64 -> int32 = "%int64_to_int32"
(** Convert the given 64-bit integer (type [int64]) to a
   32-bit integer (type [int32]). The 64-bit integer
   is taken modulo 2{^32}, i.e. the top 32 bits are lost
   during the conversion.  *)

val of_string_opt: string -> int64 option
(** Same as [of_string], but return [None] instead of raising.
    @since 4.05 *)

val to_string : int64 -> string
(** Return the string representation of its argument, in decimal. *)

type t = int64
(** An alias for the type of 64-bit integers. *)

val compare: t -> t -> int
(** The comparison function for 64-bit integers, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Int64] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for int64s.
    @since 4.03.0 *)
end
# 18 "v4.in.ml"


  module Format : sig
# 1 "v4/format.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Pretty-printing.

   This module implements a pretty-printing facility to format values
   within {{!boxes}'pretty-printing boxes'} and {{!tags}'semantic tags'}
   combined with a set of {{!fpp}printf-like functions}.
   The pretty-printer splits lines at specified {{!breaks}break hints},
   and indents lines according to the box structure.
   Similarly, {{!tags}semantic tags} can be used to decouple text
   presentation from its contents.

   This pretty-printing facility is implemented as an overlay on top of
   abstract {{!section:formatter}formatters} which provide basic output
   functions.
   Some formatters are predefined, notably:
   - {!std_formatter} outputs to {{!Stdlib.stdout}stdout}
   - {!err_formatter} outputs to {{!Stdlib.stderr}stderr}

   Most functions in the {!Format} module come in two variants:
   a short version that operates on {!std_formatter} and the
   generic version prefixed by [pp_] that takes a formatter
   as its first argument.

   More formatters can be created with {!formatter_of_out_channel},
   {!formatter_of_buffer}, {!formatter_of_symbolic_output_buffer}
   or using {{!section:formatter}custom formatters}.

*)

(** {1 Introduction}
   For a gentle introduction to the basics of pretty-printing using
   [Format], read
   {{:http://caml.inria.fr/resources/doc/guides/format.en.html}
    http://caml.inria.fr/resources/doc/guides/format.en.html}.

   You may consider this module as providing an extension to the
   [printf] facility to provide automatic line splitting. The addition of
   pretty-printing annotations to your regular [printf] format strings gives
   you fancy indentation and line breaks.
   Pretty-printing annotations are described below in the documentation of
   the function {!Format.fprintf}.

   You may also use the explicit pretty-printing box management and printing
   functions provided by this module. This style is more basic but more
   verbose than the concise [fprintf] format strings.

   For instance, the sequence
   [open_box 0; print_string "x ="; print_space ();
    print_int 1; close_box (); print_newline ()]
   that prints [x = 1] within a pretty-printing box, can be
   abbreviated as [printf "@[%s@ %i@]@." "x =" 1], or even shorter
   [printf "@[x =@ %i@]@." 1].

   Rule of thumb for casual users of this library:
 - use simple pretty-printing boxes (as obtained by [open_box 0]);
 - use simple break hints as obtained by [print_cut ()] that outputs a
   simple break hint, or by [print_space ()] that outputs a space
   indicating a break hint;
 - once a pretty-printing box is open, display its material with basic
   printing functions (e. g. [print_int] and [print_string]);
 - when the material for a pretty-printing box has been printed, call
   [close_box ()] to close the box;
 - at the end of pretty-printing, flush the pretty-printer to display all
   the remaining material, e.g. evaluate [print_newline ()].

   The behavior of pretty-printing commands is unspecified
   if there is no open pretty-printing box. Each box opened by
   one of the [open_] functions below must be closed using [close_box]
   for proper formatting. Otherwise, some of the material printed in the
   boxes may not be output, or may be formatted incorrectly.

   In case of interactive use, each phrase is executed in the initial state
   of the standard pretty-printer: after each phrase execution, the
   interactive system closes all open pretty-printing boxes, flushes all
   pending text, and resets the standard pretty-printer.

   Warning: mixing calls to pretty-printing functions of this module with
   calls to {!Stdlib} low level output functions is error prone.

   The pretty-printing functions output material that is delayed in the
   pretty-printer queue and stacks in order to compute proper line
   splitting. In contrast, basic I/O output functions write directly in
   their output device. As a consequence, the output of a basic I/O function
   may appear before the output of a pretty-printing function that has been
   called before. For instance,
   [
    Stdlib.print_string "<";
    Format.print_string "PRETTY";
    Stdlib.print_string ">";
    Format.print_string "TEXT";
   ]
   leads to output [<>PRETTYTEXT].

*)

type formatter
(** Abstract data corresponding to a pretty-printer (also called a
    formatter) and all its machinery. See also {!section:formatter}. *)

(** {1:boxes Pretty-printing boxes} *)

(** The pretty-printing engine uses the concepts of pretty-printing box and
  break hint to drive indentation and line splitting behavior of the
  pretty-printer.

  Each different pretty-printing box kind introduces a specific line splitting
  policy:

  - within an {e horizontal} box, break hints never split the line (but the
    line may be split in a box nested deeper),
  - within a {e vertical} box, break hints always split the line,
  - within an {e horizontal/vertical} box, if the box fits on the current line
    then break hints never split the line, otherwise break hint always split
    the line,
  - within a {e compacting} box, a break hint never splits the line,
    unless there is no more room on the current line.

  Note that line splitting policy is box specific: the policy of a box does
  not rule the policy of inner boxes. For instance, if a vertical box is
  nested in an horizontal box, all break hints within the vertical box will
  split the line.
*)

val pp_open_box : formatter -> int -> unit
(** [pp_open_box ppf d] opens a new compacting pretty-printing box with
    offset [d] in the formatter [ppf].

   Within this box, the pretty-printer prints as much as possible material on
   every line.

   A break hint splits the line if there is no more room on the line to
   print the remainder of the box.

   Within this box, the pretty-printer emphasizes the box structure: a break
   hint also splits the line if the splitting ``moves to the left''
   (i.e. the new line gets an indentation smaller than the one of the current
   line).

   This box is the general purpose pretty-printing box.

   If the pretty-printer splits the line in the box, offset [d] is added to
   the current indentation.
*)


val pp_close_box : formatter -> unit -> unit
(** Closes the most recently open pretty-printing box. *)

val pp_open_hbox : formatter -> unit -> unit
(** [pp_open_hbox ppf ()] opens a new 'horizontal' pretty-printing box.

  This box prints material on a single line.

  Break hints in a horizontal box never split the line.
  (Line splitting may still occur inside boxes nested deeper).
*)

val pp_open_vbox : formatter -> int -> unit
(** [pp_open_vbox ppf d] opens a new 'vertical' pretty-printing box
  with offset [d].

  This box prints material on as many lines as break hints in the box.

  Every break hint in a vertical box splits the line.

  If the pretty-printer splits the line in the box, [d] is added to the
  current indentation.
*)

val pp_open_hvbox : formatter -> int -> unit
(** [pp_open_hvbox ppf d] opens a new 'horizontal/vertical' pretty-printing box
  with offset [d].

  This box behaves as an horizontal box if it fits on a single line,
  otherwise it behaves as a vertical box.

  If the pretty-printer splits the line in the box, [d] is added to the
  current indentation.
*)

val pp_open_hovbox : formatter -> int -> unit
(** [pp_open_hovbox ppf d] opens a new 'horizontal-or-vertical'
  pretty-printing box with offset [d].

  This box prints material as much as possible on every line.

  A break hint splits the line if there is no more room on the line to
  print the remainder of the box.

  If the pretty-printer splits the line in the box, [d] is added to the
  current indentation.
*)

(** {1 Formatting functions} *)

val pp_print_string : formatter -> string -> unit
(** [pp_print_string ppf s] prints [s] in the current pretty-printing box. *)

val pp_print_as : formatter -> int -> string -> unit
(** [pp_print_as ppf len s] prints [s] in the current pretty-printing box.
  The pretty-printer formats [s] as if it were of length [len].
*)

val pp_print_int : formatter -> int -> unit
(** Print an integer in the current pretty-printing box. *)

val pp_print_char : formatter -> char -> unit
(** Print a character in the current pretty-printing box. *)

val pp_print_bool : formatter -> bool -> unit
(** Print a boolean in the current pretty-printing box. *)

(** {1:breaks Break hints} *)

(** A 'break hint' tells the pretty-printer to output some space or split the
  line whichever way is more appropriate to the current pretty-printing box
  splitting rules.

  Break hints are used to separate printing items and are mandatory to let
  the pretty-printer correctly split lines and indent items.

  Simple break hints are:
  - the 'space': output a space or split the line if appropriate,
  - the 'cut': split the line if appropriate.

  Note: the notions of space and line splitting are abstract for the
  pretty-printing engine, since those notions can be completely redefined
  by the programmer.
  However, in the pretty-printer default setting, ``output a space'' simply
  means printing a space character (ASCII code 32) and ``split the line''
  means printing a newline character (ASCII code 10).
*)

val pp_print_space : formatter -> unit -> unit
(** [pp_print_space ppf ()] emits a 'space' break hint:
  the pretty-printer may split the line at this point,
  otherwise it prints one space.

  [pp_print_space ppf ()] is equivalent to [pp_print_break ppf 1 0].
*)

val pp_print_cut : formatter -> unit -> unit
(** [pp_print_cut ppf ()] emits a 'cut' break hint:
  the pretty-printer may split the line at this point,
  otherwise it prints nothing.

  [pp_print_cut ppf ()] is equivalent to [pp_print_break ppf 0 0].
*)

val pp_print_break : formatter -> int -> int -> unit
(** [pp_print_break ppf nspaces offset] emits a 'full' break hint:
  the pretty-printer may split the line at this point,
  otherwise it prints [nspaces] spaces.

  If the pretty-printer splits the line, [offset] is added to
  the current indentation.
*)

val pp_print_custom_break :
  formatter ->
  fits:(string * int * string) ->
  breaks:(string * int * string) ->
  unit
(** [pp_print_custom_break ppf ~fits:(s1, n, s2) ~breaks:(s3, m, s4)] emits a
   custom break hint: the pretty-printer may split the line at this point.

   If it does not split the line, then the [s1] is emitted, then [n] spaces,
   then [s2].

   If it splits the line, then it emits the [s3] string, then an indent
   (according to the box rules), then an offset of [m] spaces, then the [s4]
   string.

   While [n] and [m] are handled by [formatter_out_functions.out_indent], the
   strings will be handled by [formatter_out_functions.out_string]. This allows
   for a custom formatter that handles indentation distinctly, for example,
   outputs [<br/>] tags or [&nbsp;] entities.

   The custom break is useful if you want to change which visible
   (non-whitespace) characters are printed in case of break or no break. For
   example, when printing a list [ [a; b; c] ], you might want to add a
   trailing semicolon when it is printed vertically:

   {[
[
  a;
  b;
  c;
]
   ]}

   You can do this as follows:
   {[
printf "@[<v 0>[@;<0 2>@[<v 0>a;@,b;@,c@]%t]@]@\n"
  (pp_print_custom_break ~fits:("", 0, "") ~breaks:(";", 0, ""))
   ]}

  @since 4.08.0
*)

val pp_force_newline : formatter -> unit -> unit
(** Force a new line in the current pretty-printing box.

  The pretty-printer must split the line at this point,

  Not the normal way of pretty-printing, since imperative line splitting may
  interfere with current line counters and box size calculation.
  Using break hints within an enclosing vertical box is a better
  alternative.
*)

val pp_print_if_newline : formatter -> unit -> unit
(** Execute the next formatting command if the preceding line
  has just been split. Otherwise, ignore the next formatting
  command.
*)

(** {1 Pretty-printing termination} *)

val pp_print_flush : formatter -> unit -> unit
(** End of pretty-printing: resets the pretty-printer to initial state.

  All open pretty-printing boxes are closed, all pending text is printed.
  In addition, the pretty-printer low level output device is flushed to
  ensure that all pending text is really displayed.

  Note: never use [print_flush] in the normal course of a pretty-printing
  routine, since the pretty-printer uses a complex buffering machinery to
  properly indent the output; manually flushing those buffers at random
  would conflict with the pretty-printer strategy and result to poor
  rendering.

  Only consider using [print_flush] when displaying all pending material is
  mandatory (for instance in case of interactive use when you want the user
  to read some text) and when resetting the pretty-printer state will not
  disturb further pretty-printing.

  Warning: If the output device of the pretty-printer is an output channel,
  repeated calls to [print_flush] means repeated calls to {!Stdlib.flush}
  to flush the out channel; these explicit flush calls could foil the
  buffering strategy of output channels and could dramatically impact
  efficiency.
*)

val pp_print_newline : formatter -> unit -> unit
(** End of pretty-printing: resets the pretty-printer to initial state.

  All open pretty-printing boxes are closed, all pending text is printed.

  Equivalent to {!print_flush} followed by a new line.
  See corresponding words of caution for {!print_flush}.

  Note: this is not the normal way to output a new line;
  the preferred method is using break hints within a vertical pretty-printing
  box.
*)

(** {1 Margin} *)

val pp_set_margin : formatter -> int -> unit
(** [pp_set_margin ppf d] sets the right margin to [d] (in characters):
  the pretty-printer splits lines that overflow the right margin according to
  the break hints given.
  Nothing happens if [d] is smaller than 2.
  If [d] is too large, the right margin is set to the maximum
  admissible value (which is greater than [10 ^ 9]).
  If [d] is less than the current maximum indentation limit, the
  maximum indentation limit is decreased while trying to preserve
  a minimal ratio [max_indent/margin>=50%] and if possible
  the current difference [margin - max_indent].

  See also {!pp_set_geometry}.
*)

val pp_get_margin : formatter -> unit -> int
(** Returns the position of the right margin. *)

(** {1 Maximum indentation limit} *)

val pp_set_max_indent : formatter -> int -> unit
(** [pp_set_max_indent ppf d] sets the maximum indentation limit of lines
  to [d] (in characters):
  once this limit is reached, new pretty-printing boxes are rejected to the
  left, unless the enclosing box fully fits on the current line.
  As an illustration,
  {[ set_margin 10; set_max_indent 5; printf "@[123456@[7@]89A@]@." ]}
  yields
  {[
    123456
    789A
  ]}
  because the nested box ["@[7@]"] is opened after the maximum indentation
  limit ([7>5]) and its parent box does not fit on the current line.
  Either decreasing the length of the parent box to make it fit on a line:
  {[ printf "@[123456@[7@]89@]@." ]}
  or opening an intermediary box before the maximum indentation limit which
  fits on the current line
  {[ printf "@[123@[456@[7@]89@]A@]@." ]}
  avoids the rejection to the left of the inner boxes and print respectively
  ["123456789"] and ["123456789A"] .
  Note also that vertical boxes never fit on a line whereas horizontal boxes
  always fully fit on the current line.

  Nothing happens if [d] is smaller than 2.

  If [d] is too large, the limit is set to the maximum
  admissible value (which is greater than [10 ^ 9]).

  If [d] is greater or equal than the current margin, it is ignored,
  and the current maximum indentation limit is kept.

  See also {!pp_set_geometry}.
*)

val pp_get_max_indent : formatter -> unit -> int
(** Return the maximum indentation limit (in characters). *)

(** {1 Maximum formatting depth} *)

(** The maximum formatting depth is the maximum number of pretty-printing
  boxes simultaneously open.

  Material inside boxes nested deeper is printed as an ellipsis (more
  precisely as the text returned by {!get_ellipsis_text} [()]).
*)

val pp_set_max_boxes : formatter -> int -> unit
(** [pp_set_max_boxes ppf max] sets the maximum number of pretty-printing
    boxes simultaneously open.

  Material inside boxes nested deeper is printed as an ellipsis (more
  precisely as the text returned by {!get_ellipsis_text} [()]).

  Nothing happens if [max] is smaller than 2.
*)

val pp_get_max_boxes : formatter -> unit -> int
(** Returns the maximum number of pretty-printing boxes allowed before
  ellipsis.
*)

val pp_over_max_boxes : formatter -> unit -> bool
(** Tests if the maximum number of pretty-printing boxes allowed have already
  been opened.
*)

(** {1 Tabulation boxes} *)

(**

  A {e tabulation box} prints material on lines divided into cells of fixed
  length. A tabulation box provides a simple way to display vertical columns
  of left adjusted text.

  This box features command [set_tab] to define cell boundaries, and command
  [print_tab] to move from cell to cell and split the line when there is no
  more cells to print on the line.

  Note: printing within tabulation box is line directed, so arbitrary line
  splitting inside a tabulation box leads to poor rendering. Yet, controlled
  use of tabulation boxes allows simple printing of columns within
  module {!Format}.
*)

val pp_open_tbox : formatter -> unit -> unit
(** [open_tbox ()] opens a new tabulation box.

  This box prints lines separated into cells of fixed width.

  Inside a tabulation box, special {e tabulation markers} defines points of
  interest on the line (for instance to delimit cell boundaries).
  Function {!Format.set_tab} sets a tabulation marker at insertion point.

  A tabulation box features specific {e tabulation breaks} to move to next
  tabulation marker or split the line. Function {!Format.print_tbreak} prints
  a tabulation break.
*)

val pp_close_tbox : formatter -> unit -> unit
(** Closes the most recently opened tabulation box. *)

val pp_set_tab : formatter -> unit -> unit
(** Sets a tabulation marker at current insertion point. *)

val pp_print_tab : formatter -> unit -> unit
(** [print_tab ()] emits a 'next' tabulation break hint: if not already set on
  a tabulation marker, the insertion point moves to the first tabulation
  marker on the right, or the pretty-printer splits the line and insertion
  point moves to the leftmost tabulation marker.

  It is equivalent to [print_tbreak 0 0]. *)

val pp_print_tbreak : formatter -> int -> int -> unit
(** [print_tbreak nspaces offset] emits a 'full' tabulation break hint.

  If not already set on a tabulation marker, the insertion point moves to the
  first tabulation marker on the right and the pretty-printer prints
  [nspaces] spaces.

  If there is no next tabulation marker on the right, the pretty-printer
  splits the line at this point, then insertion point moves to the leftmost
  tabulation marker of the box.

  If the pretty-printer splits the line, [offset] is added to
  the current indentation.
*)

(** {1 Ellipsis} *)

val pp_set_ellipsis_text : formatter -> string -> unit
(** Set the text of the ellipsis printed when too many pretty-printing boxes
  are open (a single dot, [.], by default).
*)

val pp_get_ellipsis_text : formatter -> unit -> string
(** Return the text of the ellipsis. *)

(** {1 Convenience formatting functions.} *)

val pp_print_list:
  ?pp_sep:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) -> (formatter -> 'a list -> unit)
(** [pp_print_list ?pp_sep pp_v ppf l] prints items of list [l],
  using [pp_v] to print each item, and calling [pp_sep]
  between items ([pp_sep] defaults to {!pp_print_cut}.
  Does nothing on empty lists.

  @since 4.02.0
*)

val pp_print_text : formatter -> string -> unit
(** [pp_print_text ppf s] prints [s] with spaces and newlines respectively
  printed using {!pp_print_space} and {!pp_force_newline}.

  @since 4.02.0
*)

val pp_print_option :
  ?none:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) -> (formatter -> 'a option -> unit)
(** [pp_print_option ?none pp_v ppf o] prints [o] on [ppf]
    using [pp_v] if [o] is [Some v] and [none] if it is [None]. [none]
    prints nothing by default.

    @since 4.08 *)

val pp_print_result :
  ok:(formatter -> 'a -> unit) -> error:(formatter -> 'e -> unit) ->
  formatter -> ('a, 'e) result -> unit
(** [pp_print_result ~ok ~error ppf r] prints [r] on [ppf] using
    [ok] if [r] is [Ok _] and [error] if [r] is [Error _].

    @since 4.08 *)

(** {1:fpp Formatted pretty-printing} *)

(**
  Module [Format] provides a complete set of [printf] like functions for
  pretty-printing using format string specifications.

  Specific annotations may be added in the format strings to give
  pretty-printing commands to the pretty-printing engine.

  Those annotations are introduced in the format strings using the [@]
  character. For instance, [@ ] means a space break, [@,] means a cut,
  [@\[] opens a new box, and [@\]] closes the last open box.

*)

val fprintf : formatter -> ('a, formatter, unit) format -> 'a

(** [fprintf ff fmt arg1 ... argN] formats the arguments [arg1] to [argN]
  according to the format string [fmt], and outputs the resulting string on
  the formatter [ff].

  The format string [fmt] is a character string which contains three types of
  objects: plain characters and conversion specifications as specified in
  the {!Printf} module, and pretty-printing indications specific to the
  [Format] module.

  The pretty-printing indication characters are introduced by
  a [@] character, and their meanings are:
  - [@\[]: open a pretty-printing box. The type and offset of the
    box may be optionally specified with the following syntax:
    the [<] character, followed by an optional box type indication,
    then an optional integer offset, and the closing [>] character.
    Pretty-printing box type is one of [h], [v], [hv], [b], or [hov].
    '[h]' stands for an 'horizontal' pretty-printing box,
    '[v]' stands for a 'vertical' pretty-printing box,
    '[hv]' stands for an 'horizontal/vertical' pretty-printing box,
    '[b]' stands for an 'horizontal-or-vertical' pretty-printing box
    demonstrating indentation,
    '[hov]' stands a simple 'horizontal-or-vertical' pretty-printing box.
    For instance, [@\[<hov 2>] opens an 'horizontal-or-vertical'
    pretty-printing box with indentation 2 as obtained with [open_hovbox 2].
    For more details about pretty-printing boxes, see the various box opening
    functions [open_*box].
  - [@\]]: close the most recently opened pretty-printing box.
  - [@,]: output a 'cut' break hint, as with [print_cut ()].
  - [@ ]: output a 'space' break hint, as with [print_space ()].
  - [@;]: output a 'full' break hint as with [print_break]. The
    [nspaces] and [offset] parameters of the break hint may be
    optionally specified with the following syntax:
    the [<] character, followed by an integer [nspaces] value,
    then an integer [offset], and a closing [>] character.
    If no parameters are provided, the good break defaults to a
    'space' break hint.
  - [@.]: flush the pretty-printer and split the line, as with
    [print_newline ()].
  - [@<n>]: print the following item as if it were of length [n].
    Hence, [printf "@<0>%s" arg] prints [arg] as a zero length string.
    If [@<n>] is not followed by a conversion specification,
    then the following character of the format is printed as if
    it were of length [n].
  - [@\{]: open a semantic tag. The name of the tag may be optionally
    specified with the following syntax:
    the [<] character, followed by an optional string
    specification, and the closing [>] character. The string
    specification is any character string that does not contain the
    closing character ['>']. If omitted, the tag name defaults to the
    empty string.
    For more details about semantic tags, see the functions {!open_stag} and
    {!close_stag}.
  - [@\}]: close the most recently opened semantic tag.
  - [@?]: flush the pretty-printer as with [print_flush ()].
    This is equivalent to the conversion [%!].
  - [@\n]: force a newline, as with [force_newline ()], not the normal way
    of pretty-printing, you should prefer using break hints inside a vertical
    pretty-printing box.

  Note: To prevent the interpretation of a [@] character as a
  pretty-printing indication, escape it with a [%] character.
  Old quotation mode [@@] is deprecated since it is not compatible with
  formatted input interpretation of character ['@'].

  Example: [printf "@[%s@ %d@]@." "x =" 1] is equivalent to
  [open_box (); print_string "x ="; print_space ();
   print_int 1; close_box (); print_newline ()].
  It prints [x = 1] within a pretty-printing 'horizontal-or-vertical' box.

*)

val sprintf : ('a, unit, string) format -> 'a
(** Same as [printf] above, but instead of printing on a formatter,
  returns a string containing the result of formatting the arguments.
  Note that the pretty-printer queue is flushed at the end of {e each
  call} to [sprintf].

  In case of multiple and related calls to [sprintf] to output
  material on a single string, you should consider using [fprintf]
  with the predefined formatter [str_formatter] and call
  [flush_str_formatter ()] to get the final result.

  Alternatively, you can use [Format.fprintf] with a formatter writing to a
  buffer of your own: flushing the formatter and the buffer at the end of
  pretty-printing returns the desired string.
*)

val asprintf : ('a, formatter, unit, string) format4 -> 'a
(** Same as [printf] above, but instead of printing on a formatter,
  returns a string containing the result of formatting the arguments.
  The type of [asprintf] is general enough to interact nicely with [%a]
  conversions.

  @since 4.01.0
*)

val dprintf :
  ('a, formatter, unit, formatter -> unit) format4 -> 'a
(** Same as {!fprintf}, except the formatter is the last argument.
  [dprintf "..." a b c] is a function of type
  [formatter -> unit] which can be given to a format specifier [%t].

  This can be used as a replacement for {!asprintf} to delay
  formatting decisions. Using the string returned by {!asprintf} in a
  formatting context forces formatting decisions to be taken in
  isolation, and the final string may be created
  prematurely. {!dprintf} allows delay of formatting decisions until
  the final formatting context is known.
  For example:
{[
  let t = Format.dprintf "%i@ %i@ %i" 1 2 3 in
  ...
  Format.printf "@[<v>%t@]" t
]}

  @since 4.08.0
*)


val ifprintf : formatter -> ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but does not print anything.
  Useful to ignore some material when conditionally printing.

  @since 3.10.0
*)

(** Formatted Pretty-Printing with continuations. *)

val kfprintf :
  (formatter -> 'a) -> formatter ->
  ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [fprintf] above, but instead of returning immediately,
  passes the formatter to its first argument at the end of printing. *)

val kdprintf :
  ((formatter -> unit) -> 'a) ->
  ('b, formatter, unit, 'a) format4 -> 'b
(** Same as {!dprintf} above, but instead of returning immediately,
  passes the suspended printer to its first argument at the end of printing.

  @since 4.08.0
*)

val ikfprintf :
  (formatter -> 'a) -> formatter ->
  ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [kfprintf] above, but does not print anything.
  Useful to ignore some material when conditionally printing.

  @since 3.12.0
*)

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
(** Same as [sprintf] above, but instead of returning the string,
  passes it to the first argument. *)

val kasprintf : (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [asprintf] above, but instead of returning the string,
  passes it to the first argument.

  @since 4.03
*)
end
# 20 "v4.in.ml"


  module Logging : sig
# 1 "v4/logging.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Logging levels.  See [docs/developer/guidelines.rst] for their meaning *)
type level = Debug | Info | Notice | Warning | Error | Fatal

(** Logs a message. It is the shell's responsibility to manage the actual
    logging.

    Even though logging may involve system calls, formatting, or other work, the
    shell guarantees that calling this function doesn't transfer control over
    another promise. Consequently, the performance of this function can be
    considered predictable from the point of view of gas-consumption.

    Note that the function call has predictable performance, but that it is the
    caller's responsibility to ensure that argument evaluation has predictable
    performance too. E.g., [log Notice "%s" (Format.asprint …)] may spend time
    formatting the argument string. *)
val log : level -> ('a, Format.formatter, unit, unit) format4 -> 'a

(** Same as [log] but more efficient with a simpler interface. *)
val log_string : level -> string -> unit
end
# 22 "v4.in.ml"


  module Hex : sig
# 1 "v4/hex.mli"
(*
 * Copyright (c) 2015 Trevor Summers Smith <trevorsummerssmith@gmail.com>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Hexadecimal encoding.

    [Hex] defines hexadecimal encodings for {{!char}characters},
    {{!string}strings} and {{!cstruct}Cstruct.t} buffers. *)

type t = [`Hex of string]
(** The type var hexadecimal values. *)

(** {1:char Characters} *)

val of_char: char -> char * char
(** [of_char c] is the the hexadecimal encoding of the character
    [c]. *)

val to_char: char -> char -> char option
(** [to_char x y] is the character corresponding to the [xy]
    hexadecimal encoding.

    Returns [None] if [x] or [y] are not in the ranges ['0'..'9'],
    ['a'..'f'], or ['A'..'F']. *)

(** {1:string Strings} *)

val of_string: ?ignore:char list -> string -> t
(** [of_string s] is the hexadecimal representation of the binary
    string [s]. If [ignore] is set, skip the characters in the list
    when converting. Eg [of_string ~ignore:[' '] "a f"]. The default
    value of [ignore] is [[]]). *)

val to_string: t -> string option
(** [to_string t] is the binary string [s] such that [of_string s] is
    [t].

    Returns [None] if [t] contains a character that is not in the range ['0'..'9'],
    ['a'..'f'], or ['A'..'F']. *)

(** {1:byte Bytes} *)

val of_bytes: ?ignore:char list -> bytes -> t
(** [of_bytes s] is the hexadecimal representation of the binary
    string [s]. If [ignore] is set, skip the characters in the list
    when converting. Eg [of_bytes ~ignore:[' '] "a f"]. The default
    value of [ignore] is [[]]). *)

val to_bytes: t -> bytes option
(** [to_bytes t] is the binary string [s] such that [of_bytes s] is
    [t].

    Returns [None] if [t] contains a character that is not in the range ['0'..'9'],
    ['a'..'f'], or ['A'..'F']. *)

(** {1 Debugging} *)

val hexdump_s: ?print_row_numbers:bool -> ?print_chars:bool -> t -> string
(** Same as [hexdump] except returns a string. *)

(** {1 Pretty printing} *)

val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
(** [pp fmt t] will output a human-readable hex representation of [t]
    to the formatter [fmt]. *)

val show : t -> string
(** [show t] will return a human-readable hex representation of [t] as
    a string. *)
end
# 24 "v4.in.ml"


  module Z : sig
# 1 "v4/z.mli"
(**
   Integers.

   This modules provides arbitrary-precision integers.
   Small integers internally use a regular OCaml [int].
   When numbers grow too large, we switch transparently to GMP numbers
   ([mpn] numbers fully allocated on the OCaml heap).

   This interface is rather similar to that of [Int32] and [Int64],
   with some additional functions provided natively by GMP
   (GCD, square root, pop-count, etc.).


   This file is part of the Zarith library
   http://forge.ocamlcore.org/projects/zarith .
   It is distributed under LGPL 2 licensing, with static linking exception.
   See the LICENSE file included in the distribution.

   Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
   Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
   a joint laboratory by:
   CNRS (Centre national de la recherche scientifique, France),
   ENS (École normale supérieure, Paris, France),
   INRIA Rocquencourt (Institut national de recherche en informatique, France).

 *)


(** {1 Toplevel} *)

(** For an optimal experience with the [ocaml] interactive toplevel,
    the magic commands are:

    {[
    #load "zarith.cma";;
    #install_printer Z.pp_print;;
    ]}

    Alternatively, using the new [Zarith_top] toplevel module, simply:
    {[
    #require "zarith.top";;
    ]}
*)



(** {1 Types} *)

type t
(** Type of integers of arbitrary length. *)

exception Overflow
(** Raised by conversion functions when the value cannot be represented in
    the destination type.
 *)

(** {1 Construction} *)

val zero: t
(** The number 0. *)

val one: t
(** The number 1. *)

val minus_one: t
(** The number -1. *)

external of_int: int -> t = "%identity"
(** Converts from a base integer. *)

external of_int32: int32 -> t = "ml_z_of_int32"
(** Converts from a 32-bit integer. *)

external of_int64: int64 -> t = "ml_z_of_int64"
(** Converts from a 64-bit integer. *)

val of_string: string -> t
(** Converts a string to an integer.
    An optional [-] prefix indicates a negative number, while a [+]
    prefix is ignored.
    An optional prefix [0x], [0o], or [0b] (following the optional [-]
    or [+] prefix) indicates that the number is,
    represented, in hexadecimal, octal, or binary, respectively.
    Otherwise, base 10 is assumed.
    (Unlike C, a lone [0] prefix does not denote octal.)
    Raises an [Invalid_argument] exception if the string is not a
    syntactically correct representation of an integer.
 *)

val of_substring : string -> pos:int -> len:int -> t
(** [of_substring s ~pos ~len] is the same as [of_string (String.sub s
    pos len)]
 *)

val of_string_base: int -> string -> t
(** Parses a number represented as a string in the specified base,
    with optional [-] or [+] prefix.
    The base must be between 2 and 16.
 *)

external of_substring_base
  : int -> string -> pos:int -> len:int -> t
  = "ml_z_of_substring_base"
(** [of_substring_base base s ~pos ~len] is the same as [of_string_base
    base (String.sub s pos len)]
*)


(** {1 Basic arithmetic operations} *)

val succ: t -> t
(** Returns its argument plus one. *)

val pred: t -> t
(** Returns its argument minus one. *)

val abs: t -> t
(** Absolute value. *)

val neg: t -> t
(** Unary negation. *)

val add: t -> t -> t
(** Addition. *)

val sub: t -> t -> t
(** Subtraction. *)

val mul: t -> t -> t
(** Multiplication. *)

val div: t -> t -> t
(** Integer division. The result is truncated towards zero
    and obeys the rule of signs.
    Raises [Division_by_zero] if the divisor (second argument) is 0.
 *)

val rem: t -> t -> t
(** Integer remainder. Can raise a [Division_by_zero].
    The result of [rem a b] has the sign of [a], and its absolute value is
    strictly smaller than the absolute value of [b].
    The result satisfies the equality [a = b * div a b + rem a b].
 *)

external div_rem: t -> t -> (t * t) = "ml_z_div_rem"
(** Computes both the integer quotient and the remainder.
    [div_rem a b] is equal to [(div a b, rem a b)].
    Raises [Division_by_zero] if [b = 0].
 *)

external cdiv: t -> t -> t = "ml_z_cdiv"
(** Integer division with rounding towards +oo (ceiling).
    Can raise a [Division_by_zero].
 *)

external fdiv: t -> t -> t = "ml_z_fdiv"
(** Integer division with rounding towards -oo (floor).
    Can raise a [Division_by_zero].
 *)

val ediv_rem: t -> t -> (t * t)
(** Euclidean division and remainder.  [ediv_rem a b] returns a pair [(q, r)]
    such that [a = b * q + r] and [0 <= r < |b|].
    Raises [Division_by_zero] if [b = 0].
 *)

val ediv: t -> t -> t
(** Euclidean division. [ediv a b] is equal to [fst (ediv_rem a b)].
    The result satisfies [0 <= a - b * ediv a b < |b|].
    Raises [Division_by_zero] if [b = 0].
 *)

val erem: t -> t -> t
(** Euclidean remainder.  [erem a b] is equal to [snd (ediv_rem a b)].
    The result satisfies [0 <= erem a b < |b|] and
    [a = b * ediv a b + erem a b].  Raises [Division_by_zero] if [b = 0].
 *)

val divexact: t -> t -> t
(** [divexact a b] divides [a] by [b], only producing correct result when the
    division is exact, i.e., when [b] evenly divides [a].
    It should be faster than general division.
    Can raise a [Division_by_zero].
*)

external divisible: t -> t -> bool = "ml_z_divisible"
(** [divisible a b] returns [true] if [a] is exactly divisible by [b].
    Unlike the other division functions, [b = 0] is accepted 
    (only 0 is considered divisible by 0).
*)

external congruent: t -> t -> t -> bool = "ml_z_congruent"
(** [congruent a b c] returns [true] if [a] is congruent to [b] modulo [c].
    Unlike the other division functions, [c = 0] is accepted 
    (only equal numbers are considered equal congruent 0).
*)




(** {1 Bit-level operations} *)

(** For all bit-level operations, negative numbers are considered in 2's
    complement representation, starting with a virtual infinite number of
    1s.
 *)

val logand: t -> t -> t
(** Bitwise logical and. *)
    
val logor: t -> t -> t
(** Bitwise logical or. *)
    
val logxor: t -> t -> t
(** Bitwise logical exclusive or. *)

val lognot: t -> t
(** Bitwise logical negation. 
    The identity [lognot a]=[-a-1] always hold.
 *)

val shift_left: t -> int -> t
(** Shifts to the left. 
    Equivalent to a multiplication by a power of 2.
    The second argument must be nonnegative.
 *)

val shift_right: t -> int -> t
(** Shifts to the right. 
    This is an arithmetic shift, 
    equivalent to a division by a power of 2 with rounding towards -oo.
    The second argument must be nonnegative.
 *)

val shift_right_trunc: t -> int -> t
(** Shifts to the right, rounding towards 0.
    This is equivalent to a division by a power of 2, with truncation.
    The second argument must be nonnegative.
 *)

external numbits: t -> int = "ml_z_numbits" [@@noalloc]
(** Returns the number of significant bits in the given number.
    If [x] is zero, [numbits x] returns 0.  Otherwise,
    [numbits x] returns a positive integer [n] such that
    [2^{n-1} <= |x| < 2^n].  Note that [numbits] is defined
    for negative arguments, and that [numbits (-x) = numbits x]. *)

external trailing_zeros: t -> int = "ml_z_trailing_zeros" [@@noalloc]
(** Returns the number of trailing 0 bits in the given number.
    If [x] is zero, [trailing_zeros x] returns [max_int].
    Otherwise, [trailing_zeros x] returns a nonnegative integer [n]
    which is the largest [n] such that [2^n] divides [x] evenly.
    Note that [trailing_zeros] is defined for negative arguments,
    and that [trailing_zeros (-x) = trailing_zeros x]. *)

val testbit: t -> int -> bool
(** [testbit x n] return the value of bit number [n] in [x]:
    [true] if the bit is 1, [false] if the bit is 0.
    Bits are numbered from 0.  Raise [Invalid_argument] if [n]
    is negative. *)

external popcount: t -> int = "ml_z_popcount"
(** Counts the number of bits set.
    Raises [Overflow] for negative arguments, as those have an infinite
    number of bits set.
 *)

external hamdist: t -> t -> int = "ml_z_hamdist"
(** Counts the number of different bits.
    Raises [Overflow] if the arguments have different signs
    (in which case the distance is infinite).
 *)

(** {1 Conversions} *)

(** Note that, when converting to an integer type that cannot represent the
    converted value, an [Overflow] exception is raised.
 *)

val to_int: t -> int
(** Converts to a base integer. May raise an [Overflow]. *)

external to_int32: t -> int32 = "ml_z_to_int32"
(** Converts to a 32-bit integer. May raise [Overflow]. *)

external to_int64: t -> int64 = "ml_z_to_int64"
(** Converts to a 64-bit integer. May raise [Overflow]. *)

val to_string: t -> string
(** Gives a human-readable, decimal string representation of the argument. *)

external format: string -> t -> string = "ml_z_format"
(** Gives a string representation of the argument in the specified
    printf-like format.
    The general specification has the following form:

    [% \[flags\] \[width\] type]

    Where the type actually indicates the base:

    - [i], [d], [u]: decimal
    - [b]: binary
    - [o]: octal
    - [x]: lowercase hexadecimal
    - [X]: uppercase hexadecimal

    Supported flags are:

    - [+]: prefix positive numbers with a [+] sign
    - space: prefix positive numbers with a space
    - [-]: left-justify (default is right justification)
    - [0]: pad with zeroes (instead of spaces)
    - [#]: alternate formatting (actually, simply output a literal-like prefix: [0x], [0b], [0o])

    Unlike the classic [printf], all numbers are signed (even hexadecimal ones),
    there is no precision field, and characters that are not part of the format
    are simply ignored (and not copied in the output).
 *)

external fits_int: t -> bool = "ml_z_fits_int" [@@noalloc]
(** Whether the argument fits in a regular [int]. *)

external fits_int32: t -> bool = "ml_z_fits_int32" [@@noalloc]
(** Whether the argument fits in an [int32]. *)

external fits_int64: t -> bool = "ml_z_fits_int64" [@@noalloc]
(** Whether the argument fits in an [int64]. *)


(** {1 Printing} *)

val pp_print: Format.formatter -> t -> unit
(** Prints the argument on the specified formatter.
    Can be used as [%a] format printer in [Format.printf] and as
    argument to [#install_printer] in the top-level.
 *)


(** {1 Ordering} *)

external compare: t -> t -> int = "ml_z_compare" [@@noalloc]
(** Comparison.  [compare x y] returns 0 if [x] equals [y],
    -1 if [x] is smaller than [y], and 1 if [x] is greater than [y].

    Note that Pervasive.compare can be used to compare reliably two integers
    only on OCaml 3.12.1 and later versions.
 *)

external equal: t -> t -> bool = "ml_z_equal" [@@noalloc]
(** Equality test. *)

val leq: t -> t -> bool
(** Less than or equal. *)

val geq: t -> t -> bool
(** Greater than or equal. *)

val lt: t -> t -> bool
(** Less than (and not equal). *)

val gt: t -> t -> bool
(** Greater than (and not equal). *)

external sign: t -> int = "ml_z_sign" [@@noalloc]
(** Returns -1, 0, or 1 when the argument is respectively negative, null, or
    positive.
 *)

val min: t -> t -> t
(** Returns the minimum of its arguments. *)

val max: t -> t -> t
(** Returns the maximum of its arguments. *)

val is_even: t -> bool
(** Returns true if the argument is even (divisible by 2), false if odd. *)

val is_odd: t -> bool
(** Returns true if the argument is odd, false if even. *)

(** {1 Powers} *)

external pow: t -> int -> t = "ml_z_pow"
(** [pow base exp] raises [base] to the [exp] power.
    [exp] must be nonnegative.
    Note that only exponents fitting in a machine integer are supported, as
    larger exponents would surely make the result's size overflow the
    address space.
 *)

external sqrt: t -> t = "ml_z_sqrt"
(** Returns the square root. The result is truncated (rounded down
    to an integer).
    Raises an [Invalid_argument] on negative arguments.
 *)

external sqrt_rem: t -> (t * t) = "ml_z_sqrt_rem"
(** Returns the square root truncated, and the remainder.
    Raises an [Invalid_argument] on negative arguments.
 *)

external root: t -> int -> t = "ml_z_root"
(** [root x n] computes the [n]-th root of [x].
    [n] must be positive and, if [n] is even, then [x] must be nonnegative.
    Otherwise, an [Invalid_argument] is raised.
 *)

external rootrem: t -> int -> t * t = "ml_z_rootrem"
(** [rootrem x n] computes the [n]-th root of [x] and the remainder
    [x-root**n].
    [n] must be positive and, if [n] is even, then [x] must be nonnegative.
    Otherwise, an [Invalid_argument] is raised.
 *)

external perfect_power: t -> bool = "ml_z_perfect_power"
(** True if the argument has the form [a^b], with [b>1] *)

external perfect_square: t -> bool = "ml_z_perfect_square"
(** True if the argument has the form [a^2]. *)

val log2: t -> int
(** Returns the base-2 logarithm of its argument, rounded down to
    an integer.  If [x] is positive, [log2 x] returns the largest [n]
    such that [2^n <= x].  If [x] is negative or zero, [log2 x] raise
    the [Invalid_argument] exception. *)

val log2up: t -> int
(** Returns the base-2 logarithm of its argument, rounded up to
    an integer.  If [x] is positive, [log2up x] returns the smallest [n]
    such that [x <= 2^n].  If [x] is negative or zero, [log2up x] raise
    the [Invalid_argument] exception. *)

(** {1 Representation} *)

external size: t -> int = "ml_z_size" [@@noalloc]
(** Returns the number of machine words used to represent the number. *)

external to_bits: t -> string = "ml_z_to_bits"
(** Returns a binary representation of the argument.
    The string result should be interpreted as a sequence of bytes,
    corresponding to the binary representation of the absolute value of
    the argument in little endian ordering.
    The sign is not stored in the string.
 *)

external of_bits: string -> t = "ml_z_of_bits"
(** Constructs a number from a binary string representation.
    The string is interpreted as a sequence of bytes in little endian order,
    and the result is always positive.
    We have the identity: [of_bits (to_bits x) = abs x].
    However, we can have [to_bits (of_bits s) <> s] due to the presence of
    trailing zeros in s.
 *)
end
# 26 "v4.in.ml"


  module Lwt : sig
# 1 "v4/lwt.mli"
(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)


(** {2 Fundamentals} *)

(** {3 Promises} *)

type +'a t
(** Promises for values of type ['a].

    A {b promise} is a memory cell that is always in one of three {b states}:

    - {e fulfilled}, and containing one value of type ['a],
    - {e rejected}, and containing one exception, or
    - {e pending}, in which case it may become fulfilled or rejected later.

    A {e resolved} promise is one that is either fulfilled or rejected, i.e. not
    pending. Once a promise is resolved, its content cannot change. So, promises
    are {e write-once references}. The only possible state changes are (1) from
    pending to fulfilled and (2) from pending to rejected.

    Promises are typically “read” by attaching {b callbacks} to them. The most
    basic functions for that are {!Lwt.bind}, which attaches a callback that is
    called when a promise becomes fulfilled, and {!Lwt.catch}, for rejection.

    Promise variables of this type, ['a Lwt.t], are actually {b read-only} in
    Lwt. Separate {e resolvers} of type ['a ]{!Lwt.u} are used to write to them.
    Promises and their resolvers are created together by calling {!Lwt.wait}.
    There is one exception to this: most promises can be {e canceled} by calling
    {!Lwt.cancel}, without going through a resolver. *)

val return : 'a -> 'a t
(** [Lwt.return v] creates a new {{: #TYPEt} promise} that is {e already
    fulfilled} with value [v].

    This is needed to satisfy the type system in some cases. For example, in a
    [match] expression where one case evaluates to a promise, the other cases
    have to evaluate to promises as well:

{[
match need_input with
| true -> Lwt_io.(read_line stdin)   (* Has type string Lwt.t... *)
| false -> Lwt.return ""             (* ...so wrap empty string in a promise. *)
]}

    Another typical usage is in {{: #VALbind} [let%lwt]}. The expression after
    the “[in]” has to evaluate to a promise. So, if you compute an ordinary
    value instead, you have to wrap it:

{[
let%lwt line = Lwt_io.(read_line stdin) in
Lwt.return (line ^ ".")
]} *)


(** {3 Callbacks} *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [Lwt.bind p_1 f] makes it so that [f] will run when [p_1] is {{: #TYPEt}
    {e fulfilled}}.

    When [p_1] is fulfilled with value [v_1], the callback [f] is called with
    that same value [v_1]. Eventually, after perhaps starting some I/O or other
    computation, [f] returns promise [p_2].

    [Lwt.bind] itself returns immediately. It only attaches the callback [f] to
    [p_1] – it does not wait for [p_2]. {e What} [Lwt.bind] returns is yet a
    third promise, [p_3]. Roughly speaking, fulfillment of [p_3] represents both
    [p_1] and [p_2] becoming fulfilled, one after the other.

    A minimal example of this is an echo program:

{[
let () =
  let p_3 =
    Lwt.bind
      Lwt_io.(read_line stdin)
      (fun line -> Lwt_io.printl line)
  in
  Lwt_main.run p_3

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    Rejection of [p_1] and [p_2], and raising an exception in [f], are all
    forwarded to rejection of [p_3].

    {b Precise behavior}

    [Lwt.bind] returns a promise [p_3] immediately. [p_3] starts out pending,
    and is resolved as follows:

    - The first condition to wait for is that [p_1] becomes resolved. It does
      not matter whether [p_1] is already resolved when [Lwt.bind] is called, or
      becomes resolved later – the rest of the behavior is the same.
    - If and when [p_1] becomes resolved, it will, by definition, be either
      fulfilled or rejected.
    - If [p_1] is rejected, [p_3] is rejected with the same exception.
    - If [p_1] is fulfilled, with value [v], [f] is applied to [v].
    - [f] may finish by returning the promise [p_2], or raising an exception.
    - If [f] raises an exception, [p_3] is rejected with that exception.
    - Finally, the remaining case is when [f] returns [p_2]. From that point on,
      [p_3] is effectively made into a reference to [p_2]. This means they have
      the same state, undergo the same state changes, and performing any
      operation on one is equivalent to performing it on the other.

    {b Syntactic sugar}

    [Lwt.bind] is almost never written directly, because sequences of [Lwt.bind]
    result in growing indentation and many parentheses:

{[
let () =
  Lwt_main.run begin
    Lwt.bind Lwt_io.(read_line stdin) (fun line ->
      Lwt.bind (Lwt_unix.sleep 1.) (fun () ->
        Lwt_io.printf "One second ago, you entered %s\n" line))
  end

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    The recommended way to write [Lwt.bind] is using the [let%lwt] syntactic
    sugar:

{[
let () =
  Lwt_main.run begin
    let%lwt line = Lwt_io.(read_line stdin) in
    let%lwt () = Lwt_unix.sleep 1. in
    Lwt_io.printf "One second ago, you entered %s\n" line
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    This uses the Lwt {{: Ppx_lwt.html} PPX} (preprocessor). Note that we had to
    add package [lwt_ppx] to the command line for building this program. We will
    do that throughout this manual.

    Another way to write [Lwt.bind], that you may encounter while reading code,
    is with the [>>=] operator:

{[
open Lwt.Infix

let () =
  Lwt_main.run begin
    Lwt_io.(read_line stdin) >>= fun line ->
    Lwt_unix.sleep 1. >>= fun () ->
    Lwt_io.printf "One second ago, you entered %s\n" line
  end

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    The [>>=] operator comes from the module {!Lwt.Infix}, which is why we
    opened it at the beginning of the program.

    See also {!Lwt.map}. *)



(** {2 Convenience} *)

(** {3 Callback helpers} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [Lwt.map f p_1] is similar to {!Lwt.bind}[ p_1 f], but [f] is not expected
    to return a promise.

    This function is more convenient that {!Lwt.bind} when [f] inherently does
    not return a promise. An example is [Stdlib.int_of_string]:

{[
let read_int : unit -> int Lwt.t = fun () ->
  Lwt.map
    int_of_string
    Lwt_io.(read_line stdin)

let () =
  Lwt_main.run begin
    let%lwt number = read_int () in
    Lwt_io.printf "%i\n" number
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    By comparison, the {!Lwt.bind} version is more awkward:

{[
let read_int : unit -> int Lwt.t = fun () ->
  Lwt.bind
    Lwt_io.(read_line stdin)
    (fun line -> Lwt.return (int_of_string line))
]}

    As with {!Lwt.bind}, sequences of calls to [Lwt.map] result in excessive
    indentation and parentheses. The recommended syntactic sugar for avoiding
    this is the {{: #VAL(>|=)} [>|=]} operator, which comes from module
    [Lwt.Infix]:

{[
open Lwt.Infix

let read_int : unit -> int Lwt.t = fun () ->
  Lwt_io.(read_line stdin) >|= int_of_string
]}

    The detailed operation follows. For consistency with the promises in
    {!Lwt.bind}, the {e two} promises involved are named [p_1] and [p_3]:

    - [p_1] is the promise passed to [Lwt.map].
    - [p_3] is the promise returned by [Lwt.map].

    [Lwt.map] returns a promise [p_3]. [p_3] starts out pending. It is resolved
    as follows:

    - [p_1] may be, or become, resolved. In that case, by definition, it will
      become fulfilled or rejected. Fulfillment is the interesting case, but the
      behavior on rejection is simpler, so we focus on rejection first.
    - When [p_1] becomes rejected, [p_3] is rejected with the same exception.
    - When [p_1] instead becomes fulfilled, call the value it is fulfilled with
      [v].
    - [f v] is applied. If this finishes, it may either return another value, or
      raise an exception.
    - If [f v] returns another value [v'], [p_3] is fulfilled with [v'].
    - If [f v] raises exception [exn], [p_3] is rejected with [exn]. *)


(** {3 Pre-allocated promises} *)

val return_unit : unit t
(** [Lwt.return_unit] is defined as {!Lwt.return}[ ()], but this definition is
    evaluated only once, during initialization of module [Lwt], at the beginning
    of your program.

    This means the promise is allocated only once. By contrast, each time
    {!Lwt.return}[ ()] is evaluated, it allocates a new promise.

    It is recommended to use [Lwt.return_unit] only where you know the
    allocations caused by an instance of {!Lwt.return}[ ()] are a performance
    bottleneck. Generally, the cost of I/O tends to dominate the cost of
    {!Lwt.return}[ ()] anyway.

    In future Lwt, we hope to perform this optimization, of using a single,
    pre-allocated promise, automatically, wherever {!Lwt.return}[ ()] is
    written. *)

val return_none : (_ option) t
(** [Lwt.return_none] is like {!Lwt.return_unit}, but for
    {!Lwt.return}[ None]. *)

val return_nil : (_ list) t
(** [Lwt.return_nil] is like {!Lwt.return_unit}, but for {!Lwt.return}[ []]. *)

val return_true : bool t
(** [Lwt.return_true] is like {!Lwt.return_unit}, but for
    {!Lwt.return}[ true]. *)

val return_false : bool t
(** [Lwt.return_false] is like {!Lwt.return_unit}, but for
    {!Lwt.return}[ false]. *)
end
# 28 "v4.in.ml"


  module Data_encoding : sig
# 1 "v4/data_encoding.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** In memory JSON data *)
type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]

type json_schema

type 'a t

type 'a encoding = 'a t

val classify : 'a encoding -> [`Fixed of int | `Dynamic | `Variable]

val splitted : json:'a encoding -> binary:'a encoding -> 'a encoding

val null : unit encoding

val empty : unit encoding

val unit : unit encoding

val constant : string -> unit encoding

val int8 : int encoding

val uint8 : int encoding

val int16 : int encoding

val uint16 : int encoding

val int31 : int encoding

val int32 : int32 encoding

val int64 : int64 encoding

val n : Z.t encoding

val z : Z.t encoding

val bool : bool encoding

val string : string encoding

val bytes : bytes encoding

val option : 'a encoding -> 'a option encoding

val string_enum : (string * 'a) list -> 'a encoding

module Fixed : sig
  val string : int -> string encoding

  val bytes : int -> bytes encoding

  val add_padding : 'a encoding -> int -> 'a encoding
end

module Variable : sig
  val string : string encoding

  val bytes : bytes encoding

  val array : ?max_length:int -> 'a encoding -> 'a array encoding

  val list : ?max_length:int -> 'a encoding -> 'a list encoding
end

module Bounded : sig
  val string : int -> string encoding

  val bytes : int -> bytes encoding
end

val dynamic_size :
  ?kind:[`Uint30 | `Uint16 | `Uint8] -> 'a encoding -> 'a encoding

val json : json encoding

val json_schema : json_schema encoding

type 'a field

val req :
  ?title:string -> ?description:string -> string -> 't encoding -> 't field

val opt :
  ?title:string ->
  ?description:string ->
  string ->
  't encoding ->
  't option field

val varopt :
  ?title:string ->
  ?description:string ->
  string ->
  't encoding ->
  't option field

val dft :
  ?title:string ->
  ?description:string ->
  string ->
  't encoding ->
  't ->
  't field

val obj1 : 'f1 field -> 'f1 encoding

val obj2 : 'f1 field -> 'f2 field -> ('f1 * 'f2) encoding

val obj3 : 'f1 field -> 'f2 field -> 'f3 field -> ('f1 * 'f2 * 'f3) encoding

val obj4 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  ('f1 * 'f2 * 'f3 * 'f4) encoding

val obj5 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding

val obj6 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding

val obj7 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  'f7 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding

val obj8 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  'f7 field ->
  'f8 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding

val obj9 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  'f7 field ->
  'f8 field ->
  'f9 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding

val obj10 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  'f7 field ->
  'f8 field ->
  'f9 field ->
  'f10 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

val tup1 : 'f1 encoding -> 'f1 encoding

val tup2 : 'f1 encoding -> 'f2 encoding -> ('f1 * 'f2) encoding

val tup3 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> ('f1 * 'f2 * 'f3) encoding

val tup4 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  'f4 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4) encoding

val tup5 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  'f4 encoding ->
  'f5 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding

val tup6 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  'f4 encoding ->
  'f5 encoding ->
  'f6 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding

val tup7 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  'f4 encoding ->
  'f5 encoding ->
  'f6 encoding ->
  'f7 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding

val tup8 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  'f4 encoding ->
  'f5 encoding ->
  'f6 encoding ->
  'f7 encoding ->
  'f8 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding

val tup9 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  'f4 encoding ->
  'f5 encoding ->
  'f6 encoding ->
  'f7 encoding ->
  'f8 encoding ->
  'f9 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding

val tup10 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  'f4 encoding ->
  'f5 encoding ->
  'f6 encoding ->
  'f7 encoding ->
  'f8 encoding ->
  'f9 encoding ->
  'f10 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

val merge_objs : 'o1 encoding -> 'o2 encoding -> ('o1 * 'o2) encoding

val merge_tups : 'a1 encoding -> 'a2 encoding -> ('a1 * 'a2) encoding

val array : ?max_length:int -> 'a encoding -> 'a array encoding

val list : ?max_length:int -> 'a encoding -> 'a list encoding

val assoc : 'a encoding -> (string * 'a) list encoding

type case_tag = Tag of int | Json_only

type 't case

val case :
  title:string ->
  ?description:string ->
  case_tag ->
  'a encoding ->
  ('t -> 'a option) ->
  ('a -> 't) ->
  't case

type match_result

type 'a matching_function = 'a -> match_result

(* [tag_size] is not declared in the upstream library, instead, the expanded
   polymorphic-variant type-expression is used as is. We include it in the
   protocol environment to help coq-of-ocaml process the files. *)
type tag_size = [`Uint8 | `Uint16]

val matched : ?tag_size:tag_size -> int -> 'a encoding -> 'a -> match_result

val matching :
  ?tag_size:tag_size -> 't matching_function -> 't case list -> 't encoding

val union : ?tag_size:tag_size -> 't case list -> 't encoding

val def :
  string -> ?title:string -> ?description:string -> 't encoding -> 't encoding

val conv :
  ('a -> 'b) -> ('b -> 'a) -> ?schema:json_schema -> 'b encoding -> 'a encoding

val conv_with_guard :
  ('a -> 'b) ->
  ('b -> ('a, string) result) ->
  ?schema:json_schema ->
  'b encoding ->
  'a encoding

val with_decoding_guard :
  ('a -> (unit, string) result) -> 'a encoding -> 'a encoding

val mu :
  string ->
  ?title:string ->
  ?description:string ->
  ('a encoding -> 'a encoding) ->
  'a encoding

type 'a lazy_t

val lazy_encoding : 'a encoding -> 'a lazy_t encoding

val force_decode : 'a lazy_t -> 'a option

val force_bytes : 'a lazy_t -> bytes

val make_lazy : 'a encoding -> 'a -> 'a lazy_t

val apply_lazy :
  fun_value:('a -> 'b) ->
  fun_bytes:(bytes -> 'b) ->
  fun_combine:('b -> 'b -> 'b) ->
  'a lazy_t ->
  'b

module Json : sig
  val schema : ?definitions_path:string -> 'a encoding -> json_schema

  val construct : 't encoding -> 't -> json

  val destruct : 't encoding -> json -> 't

  (** JSON Error *)

  type path = path_item list

  and path_item =
    [ `Field of string  (** A field in an object. *)
    | `Index of int  (** An index in an array. *)
    | `Star  (** Any / every field or index. *)
    | `Next  (** The next element after an array. *) ]

  (** Exception raised by destructors, with the location in the original
      JSON structure and the specific error. *)
  exception Cannot_destruct of (path * exn)

  (** Unexpected kind of data encountered (w/ the expectation). *)
  exception Unexpected of string * string

  (** Some {!union} couldn't be destructed, w/ the reasons for each {!type-case}. *)
  exception No_case_matched of exn list

  (** Array of unexpected size encountered  (w/ the expectation). *)
  exception Bad_array_size of int * int

  (** Missing field in an object. *)
  exception Missing_field of string

  (** Supernumerary field in an object. *)
  exception Unexpected_field of string

  val print_error :
    ?print_unknown:(Format.formatter -> exn -> unit) ->
    Format.formatter ->
    exn ->
    unit

  (** Helpers for writing encoders. *)
  val cannot_destruct : ('a, Format.formatter, unit, 'b) format4 -> 'a

  val wrap_error : ('a -> 'b) -> 'a -> 'b

  val pp : Format.formatter -> json -> unit
end

module Binary : sig
  val fixed_length : 'a encoding -> int option

  val maximum_length : 'a encoding -> int option

  val length : 'a encoding -> 'a -> int

  val to_bytes_opt : ?buffer_size:int -> 'a encoding -> 'a -> bytes option

  val to_bytes_exn : ?buffer_size:int -> 'a encoding -> 'a -> bytes

  val of_bytes_opt : 'a encoding -> bytes -> 'a option

  val to_string_opt : ?buffer_size:int -> 'a encoding -> 'a -> string option

  val to_string_exn : ?buffer_size:int -> 'a encoding -> 'a -> string

  val of_string_opt : 'a encoding -> string -> 'a option
end

(** [check_size size encoding] ensures that the binary encoding
    of a value will not be allowed to exceed [size] bytes. The reader
    and the writer fails otherwise. This function do not modify
    the JSON encoding. *)
val check_size : int -> 'a encoding -> 'a encoding
end
# 30 "v4.in.ml"


  module Raw_hashes : sig
# 1 "v4/raw_hashes.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

val blake2b : bytes -> bytes

val sha256 : bytes -> bytes

val sha512 : bytes -> bytes

val keccak256 : bytes -> bytes

val sha3_256 : bytes -> bytes

val sha3_512 : bytes -> bytes
end
# 32 "v4.in.ml"


  module Compare : sig
# 1 "v4/compare.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type t

  val ( = ) : t -> t -> bool

  val ( <> ) : t -> t -> bool

  val ( < ) : t -> t -> bool

  val ( <= ) : t -> t -> bool

  val ( >= ) : t -> t -> bool

  val ( > ) : t -> t -> bool

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val max : t -> t -> t

  val min : t -> t -> t
end

module Make (P : COMPARABLE) : S with type t := P.t

module Char : S with type t = char

module Bool : S with type t = bool

module Int : S with type t = int

module Int32 : S with type t = int32

module Uint32 : S with type t = int32

module Int64 : S with type t = int64

module Uint64 : S with type t = int64

module String : S with type t = string

module Bytes : S with type t = bytes

module Z : S with type t = Z.t

module List (P : COMPARABLE) : S with type t = P.t list

module Option (P : COMPARABLE) : S with type t = P.t option

module List_length_with : sig
  val ( = ) : 'a list -> int -> bool

  val ( <> ) : 'a list -> int -> bool

  val ( < ) : 'a list -> int -> bool

  val ( <= ) : 'a list -> int -> bool

  val ( >= ) : 'a list -> int -> bool

  val ( > ) : 'a list -> int -> bool

  val compare : 'a list -> int -> int

  val equal : 'a list -> int -> bool
end

module List_lengths : sig
  val ( = ) : 'a list -> 'b list -> bool

  val ( <> ) : 'a list -> 'b list -> bool

  val ( < ) : 'a list -> 'b list -> bool

  val ( <= ) : 'a list -> 'b list -> bool

  val ( >= ) : 'a list -> 'b list -> bool

  val ( > ) : 'a list -> 'b list -> bool

  val compare : 'a list -> 'b list -> int

  val equal : 'a list -> 'b list -> bool
end
end
# 34 "v4.in.ml"


  module Time : sig
# 1 "v4/time.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t

include Compare.S with type t := t

val add : t -> int64 -> t

val diff : t -> t -> int64

val of_seconds : int64 -> t

val to_seconds : t -> int64

val of_notation : string -> t option

val of_notation_exn : string -> t

val to_notation : t -> string

val encoding : t Data_encoding.t

val rfc_encoding : t Data_encoding.t

val pp_hum : Format.formatter -> t -> unit
end
# 36 "v4.in.ml"


  module TzEndian : sig
# 1 "v4/tzEndian.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

val get_int32 : bytes -> int -> int32

val get_int32_string : string -> int -> int32

val set_int32 : bytes -> int -> int32 -> unit

val set_int8 : bytes -> int -> int -> unit

val get_int8 : bytes -> int -> int

val get_int8_string : string -> int -> int

val set_int16 : bytes -> int -> int -> unit

val get_int16 : bytes -> int -> int

val get_int16_string : string -> int -> int

val set_int64 : bytes -> int -> int64 -> unit

val get_int64 : bytes -> int -> int64

val get_int64_string : string -> int -> int64

val get_uint8 : bytes -> int -> int

val get_uint8_string : string -> int -> int

val set_uint8 : bytes -> int -> int -> unit

val get_uint16 : bytes -> int -> int

val get_uint16_string : string -> int -> int

val set_uint16 : bytes -> int -> int -> unit
end
# 38 "v4.in.ml"


  module Bits : sig
# 1 "v4/bits.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Assuming [x >= 0], [numbits x] is the number of bits needed to
   represent [x]. This is also the unique [k] such that [2^{k - 1}
   <= x < 2^k] if [x > 0] and [0] otherwise. *)
val numbits : int -> int
end
# 40 "v4.in.ml"


  module Equality_witness : sig
# 1 "v4/equality_witness.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(**

   This module provides support for type equalities and runtime type identifiers.

   For two types [a] and [b], [(a, b) eq] is a witness that [a = b]. This is
   a standard generalized algebraic datatype on top of which type-level
   programming techniques can be implemented.

   Given a type [a], an inhabitant of [a t] is a dynamic identifier for [a].
   Identifiers can be compared for equality. They are also equipped with a
   hash function.

   WARNING: the hash function changes at every run. Therefore, the result
   of the hash function should never be stored.

   Notice that dynamic identifiers are not unique: two identifiers for [a]
   can have distinct hash and can be physically distinct. Hence, only [eq]
   can decide if two type identifiers correspond to the same type.

*)

(** A proof witness that two types are equal. *)
type (_, _) eq = Refl : ('a, 'a) eq

(** A dynamic representation for ['a]. *)
type 'a t

(** [make ()] is a dynamic representation for ['a]. A fresh identifier
   is returned each time [make ()] is evaluated. *)
val make : unit -> 'a t

(** [eq ida idb] returns a proof that [a = b] if [ida] and [idb]
   identify the same type. *)
val eq : 'a t -> 'b t -> ('a, 'b) eq option

(** [hash id] returns a hash for [id]. *)
val hash : 'a t -> int
end
# 42 "v4.in.ml"


  module FallbackArray : sig
# 1 "v4/fallbackArray.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(**

   This module implements arrays equipped with accessors that cannot
   raise exceptions. Reading out of the bounds of the arrays return a
   fallback value fixed at array construction time, writing out of the
   bounds of the arrays is ignored.

*)

(** The type for array containing values of type ['a]. *)
type 'a t

(** [make len v] builds an array [a] initialized [len] cells with
   [v]. The value [v] is the fallback value for [a]. *)
val make : int -> 'a -> 'a t

(** [fallback a] returns the fallback value for [a]. *)
val fallback : 'a t -> 'a

(** [length a] returns the length of [a]. *)
val length : 'a t -> int

(** [get a idx] returns the contents of the cell of index [idx] in
   [a]. If [idx] < 0 or [idx] >= [length a], [get a idx] =
   [fallback a]. *)
val get : 'a t -> int -> 'a

(** [set a idx value] updates the cell of index [idx] with [value].
    If [idx] < 0 or [idx] >= [length a], [a] is unchanged. *)
val set : 'a t -> int -> 'a -> unit

(** [iter f a] iterates [f] over the cells of [a] from the
   cell indexed [0] to the cell indexed [length a - 1]. *)
val iter : ('a -> unit) -> 'a t -> unit

(** [map f a] computes a new array obtained by applying [f] to each
   cell contents of [a]. Notice that the fallback value of the new
   array is [f (fallback a)]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [fold a init f] traverses [a] from the cell indexed [0] to the
   cell indexed [length a - 1] and transforms [accu] into [f accu x]
   where [x] is the content of the cell under focus. [accu] is
   [init] on the first iteration. *)
val fold : ('b -> 'a -> 'b) -> 'a t -> 'b -> 'b
end
# 44 "v4.in.ml"


  module Error_monad : sig
# 1 "v4/error_monad.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type error_category = [`Branch | `Temporary | `Permanent | `Outdated]

(** CORE : errors *)

type error = ..

val error_encoding : error Data_encoding.t

val pp : Format.formatter -> error -> unit

(** EXT : error registration/query *)

val register_error_kind :
  error_category ->
  id:string ->
  title:string ->
  description:string ->
  ?pp:(Format.formatter -> 'err -> unit) ->
  'err Data_encoding.t ->
  (error -> 'err option) ->
  ('err -> error) ->
  unit

val json_of_error : error -> Data_encoding.json

val error_of_json : Data_encoding.json -> error

type error_info = {
  category : error_category;
  id : string;
  title : string;
  description : string;
  schema : Data_encoding.json_schema;
}

val pp_info : Format.formatter -> error_info -> unit

(** Retrieves information of registered errors *)
val get_registered_errors : unit -> error_info list

(** MONAD : trace, monad, etc. *)

type 'err trace

type 'a tzresult = ('a, error trace) result

val make_trace_encoding : 'error Data_encoding.t -> 'error trace Data_encoding.t

val trace_encoding : error trace Data_encoding.t

val pp_trace : Format.formatter -> error trace -> unit

val result_encoding : 'a Data_encoding.t -> 'a tzresult Data_encoding.t

val ok : 'a -> ('a, 'trace) result

val return : 'a -> ('a, 'trace) result Lwt.t

val return_unit : (unit, 'trace) result Lwt.t

val return_none : ('a option, 'trace) result Lwt.t

val return_some : 'a -> ('a option, 'trace) result Lwt.t

val return_nil : ('a list, 'trace) result Lwt.t

val return_true : (bool, 'trace) result Lwt.t

val return_false : (bool, 'trace) result Lwt.t

val error : 'err -> ('a, 'err trace) result

val trace_of_error : 'err -> 'err trace

val fail : 'err -> ('a, 'err trace) result Lwt.t

val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

val ( >>? ) :
  ('a, 'trace) result -> ('a -> ('b, 'trace) result) -> ('b, 'trace) result

val ( >|? ) : ('a, 'trace) result -> ('a -> 'b) -> ('b, 'trace) result

val ( >>=? ) :
  ('a, 'trace) result Lwt.t ->
  ('a -> ('b, 'trace) result Lwt.t) ->
  ('b, 'trace) result Lwt.t

val ( >|=? ) :
  ('a, 'trace) result Lwt.t -> ('a -> 'b) -> ('b, 'trace) result Lwt.t

val ( >>?= ) :
  ('a, 'trace) result ->
  ('a -> ('b, 'trace) result Lwt.t) ->
  ('b, 'trace) result Lwt.t

val ( >|?= ) :
  ('a, 'trace) result -> ('a -> 'b Lwt.t) -> ('b, 'trace) result Lwt.t

val record_trace : 'err -> ('a, 'err trace) result -> ('a, 'err trace) result

val trace :
  'err -> ('b, 'err trace) result Lwt.t -> ('b, 'err trace) result Lwt.t

val record_trace_eval :
  (unit -> 'err) -> ('a, 'err trace) result -> ('a, 'err trace) result

val trace_eval :
  (unit -> 'err) ->
  ('b, 'err trace) result Lwt.t ->
  ('b, 'err trace) result Lwt.t

val error_unless : bool -> 'err -> (unit, 'err trace) result

val error_when : bool -> 'err -> (unit, 'err trace) result

val fail_unless : bool -> 'err -> (unit, 'err trace) result Lwt.t

val fail_when : bool -> 'err -> (unit, 'err trace) result Lwt.t

val unless :
  bool -> (unit -> (unit, 'trace) result Lwt.t) -> (unit, 'trace) result Lwt.t

val when_ :
  bool -> (unit -> (unit, 'trace) result Lwt.t) -> (unit, 'trace) result Lwt.t

val dont_wait :
  (exn -> unit) ->
  ('trace -> unit) ->
  (unit -> (unit, 'trace) result Lwt.t) ->
  unit

(** [catch f] executes [f] within a try-with block and wraps exceptions within
    a [tzresult]. [catch f] is equivalent to
    [try Ok (f ()) with e -> Error (error_of_exn e)].

    If [catch_only] is set, then only exceptions [e] such that [catch_only e] is
    [true] are caught.

    Whether [catch_only] is set or not, this function never catches
    non-deterministic runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system-exceptions such as {!Unix.Unix_error} and
    {!Sys_error}. *)
val catch : ?catch_only:(exn -> bool) -> (unit -> 'a) -> 'a tzresult

(** [catch_f f handler] is equivalent to [map_error (catch f) handler].
    In other words, it catches exceptions in [f ()] and either returns the
    value in an [Ok] or passes the exception to [handler] for the [Error].

    [catch_only] has the same use as with [catch]. The same restriction on
    catching non-deterministic runtime exceptions applies. *)
val catch_f :
  ?catch_only:(exn -> bool) -> (unit -> 'a) -> (exn -> error) -> 'a tzresult

(** [catch_s] is like [catch] but when [f] returns a promise. It is equivalent
    to

{[
Lwt.try_bind f
  (fun v -> Lwt.return (Ok v))
  (fun e -> Lwt.return (Error (error_of_exn e)))
]}

    If [catch_only] is set, then only exceptions [e] such that [catch_only e] is
    [true] are caught.

    Whether [catch_only] is set or not, this function never catches
    non-deterministic runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system-exceptions such as {!Unix.Unix_error} and
    {!Sys_error}. *)
val catch_s :
  ?catch_only:(exn -> bool) -> (unit -> 'a Lwt.t) -> 'a tzresult Lwt.t

(* Synchronisation *)

val join_e : (unit, 'err trace) result list -> (unit, 'err trace) result

val all_e : ('a, 'err trace) result list -> ('a list, 'err trace) result

val both_e :
  ('a, 'err trace) result ->
  ('b, 'err trace) result ->
  ('a * 'b, 'err trace) result

(**/**)

(* The protocol environment needs to know about shell's tzresult because they are
   used for in-protocol RPCs. Moreover, some light processing on these results
   is done in the protocol which requires the type to be concrete.

   The type is kept private because the environment is sole responsible for
   wrapping the protocol's errors into the shell's. *)

type shell_tztrace

type 'a shell_tzresult = ('a, shell_tztrace) result
end
# 46 "v4.in.ml"


  open Error_monad

  module Seq : sig
# 1 "v4/seq.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* From Lwtreslib *)

type 'a t = unit -> 'a node

and +'a node = Nil | Cons of 'a * 'a t

val empty : 'a t

val return : 'a -> 'a t

val cons : 'a -> 'a t -> 'a t

val append : 'a t -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val filter : ('a -> bool) -> 'a t -> 'a t

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val iter : ('a -> unit) -> 'a t -> unit

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t

(** {3 Lwtreslib-specific extensions} *)

(** [first s] is [None] if [s] is empty, it is [Some x] where [x] is the
    first element of [s] otherwise.

    Note that [first] forces the first element of the sequence, which can have
    side-effects or be computationally expensive. Consider, e.g., the case
    where [s = filter (fun …) s']: [first s] can force multiple of the values
    from [s']. *)
val first : 'a t -> 'a option

(** Similar to {!fold_left} but wraps the traversal in {!result}. The
    traversal is interrupted if one of the step returns an [Error _]. *)
val fold_left_e :
  ('a -> 'b -> ('a, 'trace) result) -> 'a -> 'b t -> ('a, 'trace) result

(** Similar to {!fold_left} but wraps the traversing in {!Lwt}. Each step of
    the traversal is started after the previous one has resolved. The
    traversal is interrupted if one of the promise is rejected. *)
val fold_left_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b t -> 'a Lwt.t

(** Similar to {!fold_left} but wraps the traversing in [result Lwt.t].
    Each step of the traversal is started after the previous one resolved. The
    traversal is interrupted if one of the step is rejected or is fulfilled
    with [Error _]. *)
val fold_left_es :
  ('a -> 'b -> ('a, 'trace) result Lwt.t) ->
  'a ->
  'b t ->
  ('a, 'trace) result Lwt.t

(** Similar to {!iter} but wraps the iteration in {!result}. The iteration
    is interrupted if one of the step returns an [Error _]. *)
val iter_e : ('a -> (unit, 'trace) result) -> 'a t -> (unit, 'trace) result

(** Similar to {!iter} but wraps the iteration in {!Lwt}. Each step
    of the iteration is started after the previous one resolved. The iteration
    is interrupted if one of the promise is rejected. *)
val iter_s : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

(** Similar to {!iter} but wraps the iteration in [result Lwt.t]. Each step
    of the iteration is started after the previous one resolved. The iteration
    is interrupted if one of the promise is rejected of fulfilled with an
    [Error _]. *)
val iter_es :
  ('a -> (unit, 'trace) result Lwt.t) -> 'a t -> (unit, 'trace) result Lwt.t

(** Similar to {!iter} but wraps the iteration in [result Lwt.t]. All the
    steps of the iteration are started concurrently. The promise [iter_ep]
    resolves once all the promises of the traversal resolve. At this point it
    either:
    - is rejected if at least one of the promises is, otherwise
    - is fulfilled with [Error _] if at least one of the promises is,
      otherwise
    - is fulfilled with [Ok ()] if all the promises are. *)
val iter_ep :
  ('a -> (unit, 'error Error_monad.trace) result Lwt.t) ->
  'a t ->
  (unit, 'error Error_monad.trace) result Lwt.t

(** Similar to {!iter} but wraps the iteration in {!Lwt}. All the
    steps of the iteration are started concurrently. The promise [iter_p f s]
    is resolved only once all the promises of the iteration are. At this point
    it is either fulfilled if all promises are, or rejected if at least one of
    them is. *)
val iter_p : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
end
# 50 "v4.in.ml"


  module List : sig
# 1 "v4/list.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** {1 List}

    A replacement for {!Stdlib.List} which:
    - replaces the exception-raising functions by exception-safe variants,
    - provides Lwt-, result- and Lwt-result-aware traversors.

    [List] is intended to shadow both {!Stdlib.List} and {!Lwt_list}. *)

(** {2 Basics}

    Checkout {!Lwtreslib} for an introduction to the naming and semantic
    convention of Lwtreslib. In a nutshell:
    - Stdlib functions that raise exceptions are replaced by safe variants
      (typically returning [option]).
    - The [_e] suffix is for result-aware traversors ("e" stands for "error"),
      [_s] and [_p] are for Lwt-aware, and [_es] and [_ep] are for
      Lwt-result-aware.
    - [_e], [_s], and [_es] traversors are {i fail-early}: they stop traversal
      as soon as a failure ([Error] or [Fail]) occurs; [_p] and [_ep]
      traversors are {i best-effort}: they only resolve once all of the
      intermediate promises have, even if a failure occurs. *)

(** {2 Double-traversal and combine}

    Note that double-list traversors ([iter2], [map2], etc., and also [combine])
    take an additional [when_different_lengths] parameter. This is to control
    the error that is returned when the two lists passed as arguments have
    different lengths.

    This mechanism is a replacement for {!Stdlib.List.iter2} (etc.) raising
    [Invalid_argument].

    Note that, as per the fail-early behaviour mentioned above, [_e], [_s], and
    [_es] traversors will have already processed the common-prefix before the
    error is returned.

    Because the best-effort behaviour of [_p] and [_ep] is unsatisfying for this
    failure case, double parallel traversors are omitted from this library.
    (Specifically, it is not obvious whether nor how the
    [when_different_lengths] error should be composed with the other errors.)

    To obtain a different behaviour for sequential traversors, or to process
    two lists in parallel, you can use {!combine} or any of the alternatives
    that handles the error differently: {!combine_drop},
    {!combine_with_leftovers}. Finally, the {!rev_combine} is provided to allow
    to avoid multiple-reversing.

    {3 Special considerations}

    Because they traverse the list from right-to-left, the {!fold_right2}
    function and all its variants fail with [when_different_lengths] before any
    of the processing starts. Whilst this is still within the fail-early
    behaviour, it may be surprising enough that it requires mentioning here.

    Because they may return early, {!for_all2} and {!exists2} and all their
    variants may return [Ok _] even though the arguments have different lengths.
*)

(** {3 Trivial values} *)

type 'a t = 'a list = [] | ( :: ) of 'a * 'a list

(** in-monad, preallocated nil *)

(** [nil] is [[]] *)
val nil : 'a list

(** [nil_e] is [Ok []] *)
val nil_e : ('a list, 'trace) result

(** [nil_s] is [Lwt.return_nil] *)
val nil_s : 'a list Lwt.t

(** [nil_es] is [Lwt.return (Ok [])] *)
val nil_es : ('a list, 'trace) result Lwt.t

(** {3 Safe wrappers}

    Shadowing unsafe functions to avoid all exceptions. *)

(** {4 Safe lookups, scans, retrievals}

    Return option rather than raise [Not_found], [Failure _], or
    [Invalid_argument _] *)

(** [hd xs] is the head (first element) of the list or [None] if the list is
    empty. *)
val hd : 'a list -> 'a option

(** [tl xs] is the tail of the list (the whole list except the first element)
    or [None] if the list is empty. *)
val tl : 'a list -> 'a list option

(** [nth xs n] is the [n]th element of the list or [None] if the list has
    fewer than [n] elements.

    [nth xs 0 = hd xs] *)
val nth : 'a list -> int -> 'a option

(** [nth_opt] is an alias for [nth] provided for backwards compatibility. *)
val nth_opt : 'a list -> int -> 'a option

(** [last x xs] is the last element of the list [xs] or [x] if [xs] is empty.

    The primary intended use for [last] is after destructing a list:
    [match l with | None -> … | Some x :: xs -> last x xs]
    but it can also be used for a default value:
    [last default_value_if_empty xs]. *)
val last : 'a -> 'a list -> 'a

(** [last_opt xs] is the last element of the list [xs] or [None] if the list
    [xs] is empty. *)
val last_opt : 'a list -> 'a option

(** [find predicate xs] is the first element [x] of the list [xs] such that
    [predicate x] is [true] or [None] if the list [xs] has no such element. *)
val find : ('a -> bool) -> 'a list -> 'a option

(** [find_opt] is an alias for [find] provided for backwards compatibility. *)
val find_opt : ('a -> bool) -> 'a list -> 'a option

(** [mem ~equal a l] is [true] iff there is an element [e] of [l] such that
    [equal a e]. *)
val mem : equal:('a -> 'a -> bool) -> 'a -> 'a list -> bool

(** [assoc ~equal k kvs] is [Some v] such that [(k', v)] is the first pair in
    the list such that [equal k' k] or [None] if the list contains no such
    pair. *)
val assoc : equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option

(** [assoc_opt] is an alias for [assoc] provided for backwards compatibility. *)
val assoc_opt : equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option

(** [assq k kvs] is the same as [assoc ~equal:Stdlib.( == ) k kvs]: it uses
    the physical equality. *)
val assq : 'a -> ('a * 'b) list -> 'b option

(** [assq_opt] is an alias for [assq] provided for backwards compatibility. *)
val assq_opt : 'a -> ('a * 'b) list -> 'b option

(** [mem_assoc ~equal k l] is equivalent to
    [Option.is_some @@ assoc ~equal k l]. *)
val mem_assoc : equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> bool

(** [mem_assq k l] is [mem_assoc ~equal:Stdlib.( == ) k l]. *)
val mem_assq : 'a -> ('a * 'b) list -> bool

(** [remove_assoc ~equal k l] is [l] without the first element [(k', _)] such
    that [equal k k']. *)
val remove_assoc :
  equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> ('a * 'b) list

(** [remove_assoq k l] is [remove_assoc ~equal:Stdlib.( == ) k l]. *)
val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list

(** {4 Initialisation} *)

(** [init ~when_negative_length n f] is [Error when_negative_length] if [n] is
    strictly negative and [Ok (Stdlib.List.init n f)] otherwise. *)
val init :
  when_negative_length:'trace ->
  int ->
  (int -> 'a) ->
  ('a list, 'trace) result

(** {4 Basic traversal} *)

val length : 'a list -> int

val rev : 'a list -> 'a list

val concat : 'a list list -> 'a list

val append : 'a list -> 'a list -> 'a list

val rev_append : 'a list -> 'a list -> 'a list

val flatten : 'a list list -> 'a list

(** {4 Double-list traversals}

    These safe-wrappers take an explicit value to handle the case of lists of
    unequal length.
*)

(** [combine ~when_different_lengths l1 l2] is either
    - [Error when_different_lengths] if [List.length l1 <> List.length l2]
    - a list of pairs of elements from [l1] and [l2]

    E.g., [combine ~when_different_lengths [] [] = Ok []]

    E.g., [combine ~when_different_lengths [1; 2] ['a'; 'b'] = Ok [(1,'a'); (2, 'b')]]

    E.g., [combine ~when_different_lengths:() [1] [] = Error ()]

    Note: [combine ~when_different_lengths l1 l2] is equivalent to
    [try Ok (Stdlib.List.combine l1 l2)
     with Invalid_argument _ -> when_different_lengths]

    The same equivalence almost holds for the other double traversors below.
    The notable difference is if the functions passed as argument to the
    traversors raise the [Invalid_argument _] exception. *)
val combine :
  when_different_lengths:'trace ->
  'a list ->
  'b list ->
  (('a * 'b) list, 'trace) result

(** [rev_combine ~when_different_lengths xs ys] is
    [rev (combine ~when_different_lengths xs ys)] but more efficient. *)
val rev_combine :
  when_different_lengths:'trace ->
  'a list ->
  'b list ->
  (('a * 'b) list, 'trace) result

val split : ('a * 'b) list -> 'a list * 'b list

val iter2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> unit) ->
  'a list ->
  'b list ->
  (unit, 'trace) result

val map2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c) ->
  'a list ->
  'b list ->
  ('c list, 'trace) result

val rev_map2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c) ->
  'a list ->
  'b list ->
  ('c list, 'trace) result

val fold_left2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c -> 'a) ->
  'a ->
  'b list ->
  'c list ->
  ('a, 'trace) result

(** This function is not tail-recursive *)
val fold_right2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c -> 'c) ->
  'a list ->
  'b list ->
  'c ->
  ('c, 'trace) result

(** [fold_left_map f a xs] is a combination of [fold_left] and [map] that maps
    over all elements of [xs] and threads an accumulator with initial value [a]
    through calls to [f]. *)
val fold_left_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list

val for_all2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> bool) ->
  'a list ->
  'b list ->
  (bool, 'trace) result

val exists2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> bool) ->
  'a list ->
  'b list ->
  (bool, 'trace) result

(** {3 Monad-aware variants}

    The functions below are strict extensions of the standard {!Stdlib.List}
    module. It is for result-, lwt- and lwt-result-aware variants. The meaning
    of the suffix is as described above, in {!Lwtreslib}, and in {!Sigs.Seq}. *)

(** {4 Initialisation variants}

    Note that for asynchronous variants ([_s], [_es], [_p], and [_ep]), if the
    length parameter is negative, then the promise is returned already
    fulfilled with [Error when_different_lengths]. *)

val init_e :
  when_negative_length:'trace ->
  int ->
  (int -> ('a, 'trace) result) ->
  ('a list, 'trace) result

val init_s :
  when_negative_length:'trace ->
  int ->
  (int -> 'a Lwt.t) ->
  ('a list, 'trace) result Lwt.t

val init_es :
  when_negative_length:'trace ->
  int ->
  (int -> ('a, 'trace) result Lwt.t) ->
  ('a list, 'trace) result Lwt.t

val init_p :
  when_negative_length:'trace ->
  int ->
  (int -> 'a Lwt.t) ->
  ('a list, 'trace) result Lwt.t

(** {4 Query variants} *)

val find_e :
  ('a -> (bool, 'trace) result) -> 'a list -> ('a option, 'trace) result

val find_s : ('a -> bool Lwt.t) -> 'a list -> 'a option Lwt.t

val find_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a option, 'trace) result Lwt.t

val filter : ('a -> bool) -> 'a list -> 'a list

(** [rev_filter f l] is [rev (filter f l)] but more efficient. *)
val rev_filter : ('a -> bool) -> 'a list -> 'a list

val rev_filter_some : 'a option list -> 'a list

val filter_some : 'a option list -> 'a list

val rev_filter_ok : ('a, 'b) result list -> 'a list

val filter_ok : ('a, 'b) result list -> 'a list

val rev_filter_error : ('a, 'b) result list -> 'b list

val filter_error : ('a, 'b) result list -> 'b list

val rev_filter_e :
  ('a -> (bool, 'trace) result) -> 'a list -> ('a list, 'trace) result

val filter_e :
  ('a -> (bool, 'trace) result) -> 'a list -> ('a list, 'trace) result

val rev_filter_s : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

val filter_s : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

val rev_filter_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a list, 'trace) result Lwt.t

val filter_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a list, 'trace) result Lwt.t

val filter_p : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

val rev_partition : ('a -> bool) -> 'a list -> 'a list * 'a list

val partition : ('a -> bool) -> 'a list -> 'a list * 'a list

val rev_partition_result : ('a, 'b) result list -> 'a list * 'b list

val partition_result : ('a, 'b) result list -> 'a list * 'b list

val rev_partition_e :
  ('a -> (bool, 'trace) result) ->
  'a list ->
  ('a list * 'a list, 'trace) result

val partition_e :
  ('a -> (bool, 'trace) result) ->
  'a list ->
  ('a list * 'a list, 'trace) result

val rev_partition_s :
  ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t

val partition_s : ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t

val rev_partition_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a list * 'a list, 'trace) result Lwt.t

val partition_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a list * 'a list, 'trace) result Lwt.t

val partition_p : ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t

(** {4 Traversal variants} *)
val iter : ('a -> unit) -> 'a list -> unit

val iter_e : ('a -> (unit, 'trace) result) -> 'a list -> (unit, 'trace) result

val iter_s : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t

val iter_es :
  ('a -> (unit, 'trace) result Lwt.t) ->
  'a list ->
  (unit, 'trace) result Lwt.t

val iter_p : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t

val iteri : (int -> 'a -> unit) -> 'a list -> unit

val iteri_e :
  (int -> 'a -> (unit, 'trace) result) -> 'a list -> (unit, 'trace) result

val iteri_s : (int -> 'a -> unit Lwt.t) -> 'a list -> unit Lwt.t

val iteri_es :
  (int -> 'a -> (unit, 'trace) result Lwt.t) ->
  'a list ->
  (unit, 'trace) result Lwt.t

val iteri_p : (int -> 'a -> unit Lwt.t) -> 'a list -> unit Lwt.t

val map : ('a -> 'b) -> 'a list -> 'b list

val map_e : ('a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

val map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val map_es :
  ('a -> ('b, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

val map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

val mapi_e :
  (int -> 'a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

val mapi_s : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val mapi_es :
  (int -> 'a -> ('b, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

val mapi_p : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val rev_map : ('a -> 'b) -> 'a list -> 'b list

val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

val rev_map_e :
  ('a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

val rev_map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val rev_map_es :
  ('a -> ('b, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

val rev_map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val rev_mapi_e :
  (int -> 'a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

val rev_mapi_s : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val rev_mapi_es :
  (int -> 'a -> ('b, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

val rev_mapi_p : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val rev_filter_map : ('a -> 'b option) -> 'a list -> 'b list

val rev_filter_map_e :
  ('a -> ('b option, 'trace) result) -> 'a list -> ('b list, 'trace) result

val filter_map_e :
  ('a -> ('b option, 'trace) result) -> 'a list -> ('b list, 'trace) result

val rev_filter_map_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

val filter_map : ('a -> 'b option) -> 'a list -> 'b list

val filter_map_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

val rev_filter_map_es :
  ('a -> ('b option, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

val filter_map_es :
  ('a -> ('b option, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

val filter_map_p : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

val concat_map : ('a -> 'b list) -> 'a list -> 'b list

val concat_map_s : ('a -> 'b list Lwt.t) -> 'a list -> 'b list Lwt.t

val concat_map_e :
  ('a -> ('b list, 'error) result) -> 'a list -> ('b list, 'error) result

val concat_map_es :
  ('a -> ('b list, 'error) result Lwt.t) ->
  'a list ->
  ('b list, 'error) result Lwt.t

val concat_map_p : ('a -> 'b list Lwt.t) -> 'a list -> 'b list Lwt.t

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val fold_left_e :
  ('a -> 'b -> ('a, 'trace) result) -> 'a -> 'b list -> ('a, 'trace) result

val fold_left_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b list -> 'a Lwt.t

val fold_left_es :
  ('a -> 'b -> ('a, 'trace) result Lwt.t) ->
  'a ->
  'b list ->
  ('a, 'trace) result Lwt.t

(** [fold_left_map_e f a xs] is a combination of [fold_left_e] and [map_e] that
    maps over all elements of [xs] and threads an accumulator with initial
    value [a] through calls to [f]. The list is traversed from left to right
    and the first encountered error is returned. *)
val fold_left_map_e :
  ('a -> 'b -> ('a * 'c, 'trace) result) ->
  'a ->
  'b list ->
  ('a * 'c list, 'trace) result

(** [fold_left_map_s f a xs] is a combination of [fold_left_s] and [map_s] that
    maps over all elements of [xs] and threads an accumulator with initial
    value [a] through calls to [f]. *)
val fold_left_map_s :
  ('a -> 'b -> ('a * 'c) Lwt.t) -> 'a -> 'b list -> ('a * 'c list) Lwt.t

(** [fold_left_map_es f a xs] is a combination of [fold_left_es] and [map_es] that
    maps over all elements of [xs] and threads an accumulator with initial
    value [a] through calls to [f]. The list is traversed from left to right
    and the first encountered error is returned. *)
val fold_left_map_es :
  ('a -> 'b -> ('a * 'c, 'trace) result Lwt.t) ->
  'a ->
  'b list ->
  ('a * 'c list, 'trace) result Lwt.t

val fold_left_i : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val fold_left_i_e :
  (int -> 'a -> 'b -> ('a, 'trace) result) ->
  'a ->
  'b list ->
  ('a, 'trace) result

val fold_left_i_s : (int -> 'a -> 'b -> 'a Lwt.t) -> 'a -> 'b list -> 'a Lwt.t

val fold_left_i_es :
  (int -> 'a -> 'b -> ('a, 'trace) result Lwt.t) ->
  'a ->
  'b list ->
  ('a, 'trace) result Lwt.t

(** This function is not tail-recursive *)
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

(** This function is not tail-recursive *)
val fold_right_e :
  ('a -> 'b -> ('b, 'trace) result) -> 'a list -> 'b -> ('b, 'trace) result

(** This function is not tail-recursive *)
val fold_right_s : ('a -> 'b -> 'b Lwt.t) -> 'a list -> 'b -> 'b Lwt.t

(** This function is not tail-recursive *)
val fold_right_es :
  ('a -> 'b -> ('b, 'trace) result Lwt.t) ->
  'a list ->
  'b ->
  ('b, 'trace) result Lwt.t

(** {4 Double-traversal variants}

    As mentioned above, there are no [_p] and [_ep] double-traversors. Use
    {!combine} (and variants) to circumvent this. *)

val iter2_e :
  when_different_lengths:'trace ->
  ('a -> 'b -> (unit, 'trace) result) ->
  'a list ->
  'b list ->
  (unit, 'trace) result

val iter2_s :
  when_different_lengths:'trace ->
  ('a -> 'b -> unit Lwt.t) ->
  'a list ->
  'b list ->
  (unit, 'trace) result Lwt.t

val iter2_es :
  when_different_lengths:'trace ->
  ('a -> 'b -> (unit, 'trace) result Lwt.t) ->
  'a list ->
  'b list ->
  (unit, 'trace) result Lwt.t

val map2_e :
  when_different_lengths:'trace ->
  ('a -> 'b -> ('c, 'trace) result) ->
  'a list ->
  'b list ->
  ('c list, 'trace) result

val map2_s :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c Lwt.t) ->
  'a list ->
  'b list ->
  ('c list, 'trace) result Lwt.t

val map2_es :
  when_different_lengths:'trace ->
  ('a -> 'b -> ('c, 'trace) result Lwt.t) ->
  'a list ->
  'b list ->
  ('c list, 'trace) result Lwt.t

val rev_map2_e :
  when_different_lengths:'trace ->
  ('a -> 'b -> ('c, 'trace) result) ->
  'a list ->
  'b list ->
  ('c list, 'trace) result

val rev_map2_s :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c Lwt.t) ->
  'a list ->
  'b list ->
  ('c list, 'trace) result Lwt.t

val rev_map2_es :
  when_different_lengths:'trace ->
  ('a -> 'b -> ('c, 'trace) result Lwt.t) ->
  'a list ->
  'b list ->
  ('c list, 'trace) result Lwt.t

val fold_left2_e :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c -> ('a, 'trace) result) ->
  'a ->
  'b list ->
  'c list ->
  ('a, 'trace) result

val fold_left2_s :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c -> 'a Lwt.t) ->
  'a ->
  'b list ->
  'c list ->
  ('a, 'trace) result Lwt.t

val fold_left2_es :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c -> ('a, 'trace) result Lwt.t) ->
  'a ->
  'b list ->
  'c list ->
  ('a, 'trace) result Lwt.t

(** This function is not tail-recursive *)
val fold_right2_e :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c -> ('c, 'trace) result) ->
  'a list ->
  'b list ->
  'c ->
  ('c, 'trace) result

(** This function is not tail-recursive *)
val fold_right2_s :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c -> 'c Lwt.t) ->
  'a list ->
  'b list ->
  'c ->
  ('c, 'trace) result Lwt.t

(** This function is not tail-recursive *)
val fold_right2_es :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c -> ('c, 'trace) result Lwt.t) ->
  'a list ->
  'b list ->
  'c ->
  ('c, 'trace) result Lwt.t

(** {4 Scanning variants} *)

val for_all : ('a -> bool) -> 'a list -> bool

val for_all_e :
  ('a -> (bool, 'trace) result) -> 'a list -> (bool, 'trace) result

val for_all_s : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

val for_all_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  (bool, 'trace) result Lwt.t

val for_all_p : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

val exists : ('a -> bool) -> 'a list -> bool

val exists_e :
  ('a -> (bool, 'trace) result) -> 'a list -> (bool, 'trace) result

val exists_s : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

val exists_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  (bool, 'trace) result Lwt.t

val exists_p : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

(** {4 Double-scanning variants}

    As mentioned above, there are no [_p] and [_ep] double-scanners. Use
    {!combine} (and variants) to circumvent this. *)

val for_all2_e :
  when_different_lengths:'trace ->
  ('a -> 'b -> (bool, 'trace) result) ->
  'a list ->
  'b list ->
  (bool, 'trace) result

val for_all2_s :
  when_different_lengths:'trace ->
  ('a -> 'b -> bool Lwt.t) ->
  'a list ->
  'b list ->
  (bool, 'trace) result Lwt.t

val for_all2_es :
  when_different_lengths:'trace ->
  ('a -> 'b -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  'b list ->
  (bool, 'trace) result Lwt.t

val exists2_e :
  when_different_lengths:'trace ->
  ('a -> 'b -> (bool, 'trace) result) ->
  'a list ->
  'b list ->
  (bool, 'trace) result

val exists2_s :
  when_different_lengths:'trace ->
  ('a -> 'b -> bool Lwt.t) ->
  'a list ->
  'b list ->
  (bool, 'trace) result Lwt.t

val exists2_es :
  when_different_lengths:'trace ->
  ('a -> 'b -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  'b list ->
  (bool, 'trace) result Lwt.t

(** {3 Combine variants}

    These are primarily intended to be used for preprocessing before applying
    a traversor to the resulting list of pairs. They give alternatives to the
    [when_different_lengths] mechanism of the immediate double-traversors
    above.

    In case the semantic of, say, [map2_es] was unsatisfying, one can use
    [map_es] on a [combine]-preprocessed pair of lists. The different variants
    of [combine] give different approaches to different-length handling. *)

(** [combine_drop ll lr] is a list [l] of pairs of elements taken from the
    common-length prefix of [ll] and [lr]. The suffix of whichever list is
    longer (if any) is dropped.

    More formally [nth l n] is:
    - [None] if [n >= min (length ll) (length lr)]
    - [Some (Option.get @@ nth ll n, Option.get @@ nth lr n)] otherwise
    *)
val combine_drop : 'a list -> 'b list -> ('a * 'b) list

(** A type like [result] but which is symmetric *)
type ('a, 'b) left_or_right_list = [`Left of 'a list | `Right of 'b list]

(** [combine_with_leftovers ll lr] is a tuple [(combined, leftover)]
    where [combined] is [combine_drop ll lr]
    and [leftover] is either [`Left lsuffix] or [`Right rsuffix] depending on
    which of [ll] or [lr] is longer. [leftover] is [None] if the two lists
    have the same length. *)
val combine_with_leftovers :
  'a list ->
  'b list ->
  ('a * 'b) list * ('a, 'b) left_or_right_list option

(** {3 compare / equal} *)

val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

val compare_lengths : 'a list -> 'b list -> int

val compare_length_with : 'a list -> int -> int

val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool

(** {3 Sorting} *)

val sort : ('a -> 'a -> int) -> 'a list -> 'a list

val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list

val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list

val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list

(** {3 conversion} *)

val to_seq : 'a list -> 'a Seq.t

val of_seq : 'a Seq.t -> 'a list

val init_ep :
  when_negative_length:'error ->
  int ->
  (int -> ('a, 'error Error_monad.trace) result Lwt.t) ->
  ('a list, 'error Error_monad.trace) result Lwt.t

val filter_ep :
  ('a -> (bool, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('a list, 'error Error_monad.trace) result Lwt.t

val partition_ep :
  ('a -> (bool, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('a list * 'a list, 'error Error_monad.trace) result Lwt.t

val iter_ep :
  ('a -> (unit, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  (unit, 'error Error_monad.trace) result Lwt.t

val iteri_ep :
  (int -> 'a -> (unit, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  (unit, 'error Error_monad.trace) result Lwt.t

val map_ep :
  ('a -> ('b, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error Error_monad.trace) result Lwt.t

val mapi_ep :
  (int -> 'a -> ('b, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error Error_monad.trace) result Lwt.t

val rev_map_ep :
  ('a -> ('b, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error Error_monad.trace) result Lwt.t

val rev_mapi_ep :
  (int -> 'a -> ('b, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error Error_monad.trace) result Lwt.t

val filter_map_ep :
  ('a -> ('b option, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error Error_monad.trace) result Lwt.t

val concat_map_ep :
  ('a -> ('b list, 'error trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error trace) result Lwt.t

val for_all_ep :
  ('a -> (bool, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  (bool, 'error Error_monad.trace) result Lwt.t

val exists_ep :
  ('a -> (bool, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  (bool, 'error Error_monad.trace) result Lwt.t
end
# 52 "v4.in.ml"


  module Set : sig
# 1 "v4/set.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* From Lwtreslib *)

module type S = sig
  type elt

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val disjoint : t -> t -> bool

  val diff : t -> t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val iter_e : (elt -> (unit, 'trace) result) -> t -> (unit, 'trace) result

  val iter_s : (elt -> unit Lwt.t) -> t -> unit Lwt.t

  val iter_p : (elt -> unit Lwt.t) -> t -> unit Lwt.t

  val iter_es :
    (elt -> (unit, 'trace) result Lwt.t) -> t -> (unit, 'trace) result Lwt.t

  val map : (elt -> elt) -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val fold_e :
    (elt -> 'a -> ('a, 'trace) result) -> t -> 'a -> ('a, 'trace) result

  val fold_s : (elt -> 'a -> 'a Lwt.t) -> t -> 'a -> 'a Lwt.t

  val fold_es :
    (elt -> 'a -> ('a, 'trace) result Lwt.t) ->
    t ->
    'a ->
    ('a, 'trace) result Lwt.t

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val partition : (elt -> bool) -> t -> t * t

  val cardinal : t -> int

  val elements : t -> elt list

  val min_elt : t -> elt option

  val max_elt : t -> elt option

  val choose : t -> elt option

  val split : elt -> t -> t * bool * t

  val find : elt -> t -> elt option

  val find_first : (elt -> bool) -> t -> elt option

  val find_last : (elt -> bool) -> t -> elt option

  val of_list : elt list -> t

  val to_seq_from : elt -> t -> elt Seq.t

  val to_seq : t -> elt Seq.t

  val add_seq : elt Seq.t -> t -> t

  val of_seq : elt Seq.t -> t

  val iter_ep :
    (elt -> (unit, 'error Error_monad.trace) result Lwt.t) ->
    t ->
    (unit, 'error Error_monad.trace) result Lwt.t
end

module Make (Ord : Compare.COMPARABLE) : S with type elt = Ord.t
end
# 54 "v4.in.ml"


  module Map : sig
# 1 "v4/map.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* From Lwtreslib *)

module type S = sig

  type key

  type +!'a t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val mem : key -> 'a t -> bool

  val add : key -> 'a -> 'a t -> 'a t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val singleton : key -> 'a -> 'a t

  val remove : key -> 'a t -> 'a t

  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  (** [iter_e f m] applies [f] to the bindings of [m] one by one in an
      unspecified order. If all the applications result in [Ok ()], then the
      result of the iteration is [Ok ()]. If any of the applications results in
      [Error e] then the iteration stops and the result of the iteration is
      [Error e]. *)
  val iter_e :
    (key -> 'a -> (unit, 'trace) result) -> 'a t -> (unit, 'trace) result

  val iter_s : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val iter_p : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  (** [iter_es f m] applies [f] to the bindings of [m] in an unspecified order,
      one after the other as the promises resolve. If all the applications
      result in [Ok ()], then the result of the iteration is [Ok ()]. If any of
      the applications results in [Error e] then the iteration stops and the
      result of the iteration is [Error e]. *)
  val iter_es :
    (key -> 'a -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    (unit, 'trace) result Lwt.t

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** [fold_e f m init] is
      [f k1 d1 init >>? fun acc -> f k2 d2 acc >>? fun acc -> …] where [kN] is
      the key bound to [dN] in [m]. *)
  val fold_e :
    (key -> 'a -> 'b -> ('b, 'trace) result) ->
    'a t ->
    'b ->
    ('b, 'trace) result

  val fold_s : (key -> 'a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

  (** [fold_es f m init] is
      [f k1 d1 init >>=? fun acc -> f k2 d2 acc >>=? fun acc -> …] where [kN] is
      the key bound to [dN] in [m]. *)
  val fold_es :
    (key -> 'a -> 'b -> ('b, 'trace) result Lwt.t) ->
    'a t ->
    'b ->
    ('b, 'trace) result Lwt.t

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t

  val cardinal : 'a t -> int

  val bindings : 'a t -> (key * 'a) list

  val min_binding : 'a t -> (key * 'a) option

  val max_binding : 'a t -> (key * 'a) option

  val choose : 'a t -> (key * 'a) option

  val split : key -> 'a t -> 'a t * 'a option * 'a t

  val find : key -> 'a t -> 'a option

  val find_first : (key -> bool) -> 'a t -> (key * 'a) option

  val find_last : (key -> bool) -> 'a t -> (key * 'a) option

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t

  val to_seq : 'a t -> (key * 'a) Seq.t

  val to_seq_from : key -> 'a t -> (key * 'a) Seq.t

  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t

  val of_seq : (key * 'a) Seq.t -> 'a t

  val iter_ep :
    (key -> 'a -> (unit, 'error Error_monad.trace) result Lwt.t) ->
    'a t ->
    (unit, 'error Error_monad.trace) result Lwt.t

end

module Make (Ord : Compare.COMPARABLE) : S with type key = Ord.t
end
# 56 "v4.in.ml"


  module Option : sig
# 1 "v4/option.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Signature from Lwtreslib's option module *)

type 'a t = 'a option = None | Some of 'a

val none : 'a option

val none_e : ('a option, 'trace) result

val none_s : 'a option Lwt.t

val none_es : ('a option, 'trace) result Lwt.t

val some : 'a -> 'a option

val some_unit : unit option

val some_nil : 'a list option

val some_e : 'a -> ('a option, 'trace) result

val some_s : 'a -> 'a option Lwt.t

val some_es : 'a -> ('a option, 'trace) result Lwt.t

val value : 'a option -> default:'a -> 'a

val value_e : 'a option -> error:'trace -> ('a, 'trace) result

val value_f : 'a option -> default:(unit -> 'a) -> 'a

val value_fe : 'a option -> error:(unit -> 'trace) -> ('a, 'trace) result

val bind : 'a option -> ('a -> 'b option) -> 'b option

val join : 'a option option -> 'a option

val either : 'a option -> 'a option -> 'a option

val map : ('a -> 'b) -> 'a option -> 'b option

val map_s : ('a -> 'b Lwt.t) -> 'a option -> 'b option Lwt.t

val map_e :
  ('a -> ('b, 'trace) result) -> 'a option -> ('b option, 'trace) result

val map_es :
  ('a -> ('b, 'trace) result Lwt.t) ->
  'a option ->
  ('b option, 'trace) result Lwt.t

val fold : none:'a -> some:('b -> 'a) -> 'b option -> 'a

val fold_s : none:'a -> some:('b -> 'a Lwt.t) -> 'b option -> 'a Lwt.t

val fold_f : none:(unit -> 'a) -> some:('b -> 'a) -> 'b option -> 'a

val iter : ('a -> unit) -> 'a option -> unit

val iter_s : ('a -> unit Lwt.t) -> 'a option -> unit Lwt.t

val iter_e :
  ('a -> (unit, 'trace) result) -> 'a option -> (unit, 'trace) result

val iter_es :
  ('a -> (unit, 'trace) result Lwt.t) ->
  'a option ->
  (unit, 'trace) result Lwt.t

val is_none : 'a option -> bool

val is_some : 'a option -> bool

val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool

val compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int

val to_result : none:'trace -> 'a option -> ('a, 'trace) result

val of_result : ('a, 'e) result -> 'a option

val to_list : 'a option -> 'a list

val to_seq : 'a option -> 'a Seq.t

(** [catch f] is [Some (f ())] if [f] does not raise an exception, it is
    [None] otherwise.

    You should only use [catch] when you truly do not care about
    what exception may be raised during the evaluation of [f ()]. If you need
    to inspect the raised exception, or if you need to pass it along, consider
    {!Result.catch} instead.

    If [catch_only] is set, then only exceptions [e] such that [catch_only e]
    is [true] are caught.

    Whether [catch_only] is set or not, you cannot catch non-deterministic
    runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system exceptions such as {!Unix.Unix_error}. *)
val catch : ?catch_only:(exn -> bool) -> (unit -> 'a) -> 'a option

(** [catch_s f] is a promise that resolves to [Some x] if and when [f ()]
    resolves to [x]. Alternatively, it resolves to [None] if and when [f ()]
    is rejected.

    You should only use [catch_s] when you truly do not care about
    what exception may be raised during the evaluation of [f ()]. If you need
    to inspect the raised exception, or if you need to pass it along, consider
    {!Result.catch_s} instead.

    If [catch_only] is set, then only exceptions [e] such that [catch_only e]
    is [true] are caught.

    Whether [catch_only] is set or not, you cannot catch non-deterministic
    runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system exceptions such as {!Unix.Unix_error}. *)
val catch_s :
  ?catch_only:(exn -> bool) -> (unit -> 'a Lwt.t) -> 'a option Lwt.t
end
# 58 "v4.in.ml"


  module Result : sig
# 1 "v4/result.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type ('a, 'e) t = ('a, 'e) result = Ok of 'a | Error of 'e (***)

val ok : 'a -> ('a, 'e) result

val ok_s : 'a -> ('a, 'e) result Lwt.t

val error : 'e -> ('a, 'e) result

val error_s : 'e -> ('a, 'e) result Lwt.t

val return : 'a -> ('a, 'e) result

val return_unit : (unit, 'e) result

val return_none : ('a option, 'e) result

val return_some : 'a -> ('a option, 'e) result

val return_nil : ('a list, 'e) result

val return_true : (bool, 'e) result

val return_false : (bool, 'e) result

val value : ('a, 'e) result -> default:'a -> 'a

val value_f : ('a, 'e) result -> default:(unit -> 'a) -> 'a

val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

val bind_s :
  ('a, 'e) result -> ('a -> ('b, 'e) result Lwt.t) -> ('b, 'e) result Lwt.t

val bind_error : ('a, 'e) result -> ('e -> ('a, 'f) result) -> ('a, 'f) result

val bind_error_s :
  ('a, 'e) result -> ('e -> ('a, 'f) result Lwt.t) -> ('a, 'f) result Lwt.t

val join : (('a, 'e) result, 'e) result -> ('a, 'e) result

val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result

(* NOTE: [map_e] is [bind] *)
val map_e : ('a -> ('b, 'e) result) -> ('a, 'e) result -> ('b, 'e) result

val map_s : ('a -> 'b Lwt.t) -> ('a, 'e) result -> ('b, 'e) result Lwt.t

(* NOTE: [map_es] is [bind_s] *)
val map_es :
  ('a -> ('b, 'e) result Lwt.t) -> ('a, 'e) result -> ('b, 'e) result Lwt.t

val map_error : ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result

(* NOTE: [map_error_e] is [bind_error] *)
val map_error_e : ('e -> ('a, 'f) result) -> ('a, 'e) result -> ('a, 'f) result

val map_error_s : ('e -> 'f Lwt.t) -> ('a, 'e) result -> ('a, 'f) result Lwt.t

(* NOTE: [map_error_es] is [bind_error_s] *)
val map_error_es :
  ('e -> ('a, 'f) result Lwt.t) -> ('a, 'e) result -> ('a, 'f) result Lwt.t

val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) result -> 'c

val iter : ('a -> unit) -> ('a, 'e) result -> unit

val iter_s : ('a -> unit Lwt.t) -> ('a, 'e) result -> unit Lwt.t

val iter_error : ('e -> unit) -> ('a, 'e) result -> unit

val iter_error_s : ('e -> unit Lwt.t) -> ('a, 'e) result -> unit Lwt.t

val is_ok : ('a, 'e) result -> bool

val is_error : ('a, 'e) result -> bool

val equal :
  ok:('a -> 'a -> bool) ->
  error:('e -> 'e -> bool) ->
  ('a, 'e) result ->
  ('a, 'e) result ->
  bool

val compare :
  ok:('a -> 'a -> int) ->
  error:('e -> 'e -> int) ->
  ('a, 'e) result ->
  ('a, 'e) result ->
  int

val to_option : ('a, 'e) result -> 'a option

val of_option : error:'e -> 'a option -> ('a, 'e) result

val to_list : ('a, 'e) result -> 'a list

val to_seq : ('a, 'e) result -> 'a Seq.t

(** [catch f] is [try Ok (f ()) with e -> Error e]: it is [Ok x] if [f ()]
    evaluates to [x], and it is [Error e] if [f ()] raises [e].

    See {!WithExceptions.S.Result.to_exn} for a converse function.

    If [catch_only] is set, then only exceptions [e] such that [catch_only e]
    is [true] are caught.

    Whether [catch_only] is set or not, you cannot catch non-deterministic
    runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system exceptions such as {!Unix.Unix_error}. *)
val catch : ?catch_only:(exn -> bool) -> (unit -> 'a) -> ('a, exn) result

(** [catch_f f handler] is equivalent to [map_error (catch f) handler].
    In other words, it catches exceptions in [f ()] and either returns the
    value in an [Ok] or passes the exception to [handler] for the [Error].

    [catch_only] has the same use as with [catch]. The same restriction on
    catching non-deterministic runtime exceptions applies. *)
val catch_f :
  ?catch_only:(exn -> bool) ->
  (unit -> 'a) ->
  (exn -> 'error) ->
  ('a, 'error) result

(** [catch_s] is [catch] but for Lwt promises. Specifically, [catch_s f]
    returns a promise that resolves to [Ok x] if and when [f ()] resolves to
    [x], or to [Error exc] if and when [f ()] is rejected with [exc].

    If [catch_only] is set, then only exceptions [e] such that [catch_only e]
    is [true] are caught.

    Whether [catch_only] is set or not, you cannot catch non-deterministic
    runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system exceptions such as {!Unix.Unix_error}. *)
val catch_s :
  ?catch_only:(exn -> bool) -> (unit -> 'a Lwt.t) -> ('a, exn) result Lwt.t
end
# 60 "v4.in.ml"


  module RPC_arg : sig
# 1 "v4/RPC_arg.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** See [src/lib_rpc/RPC_arg.mli] for documentation *)

type 'a t

type 'a arg = 'a t

val make :
  ?descr:string ->
  name:string ->
  destruct:(string -> ('a, string) result) ->
  construct:('a -> string) ->
  unit ->
  'a arg

type descr = {name : string; descr : string option}

val descr : 'a arg -> descr

val bool : bool arg

val int : int arg

val uint : int arg

val int32 : int32 arg

val uint31 : int32 arg

val int64 : int64 arg

val uint63 : int64 arg

val string : string arg

val like : 'a arg -> ?descr:string -> string -> 'a arg

type ('a, 'b) eq = Eq : ('a, 'a) eq

val eq : 'a arg -> 'b arg -> ('a, 'b) eq option
end
# 62 "v4.in.ml"


  module RPC_path : sig
# 1 "v4/RPC_path.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type ('prefix, 'params) t

type ('prefix, 'params) path = ('prefix, 'params) t

type 'prefix context = ('prefix, 'prefix) path

val root : unit context

val open_root : 'a context

val add_suffix : ('prefix, 'params) path -> string -> ('prefix, 'params) path

val ( / ) : ('prefix, 'params) path -> string -> ('prefix, 'params) path

val add_arg :
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a) path

val ( /: ) :
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a) path

val add_final_args :
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a list) path

val ( /:* ) :
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a list) path
end
# 64 "v4.in.ml"


  module RPC_query : sig
# 1 "v4/RPC_query.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type 'a t

type 'a query = 'a t

val empty : unit query

type ('a, 'b) field

val field :
  ?descr:string -> string -> 'a RPC_arg.t -> 'a -> ('b -> 'a) -> ('b, 'a) field

val opt_field :
  ?descr:string ->
  string ->
  'a RPC_arg.t ->
  ('b -> 'a option) ->
  ('b, 'a option) field

val flag : ?descr:string -> string -> ('b -> bool) -> ('b, bool) field

val multi_field :
  ?descr:string ->
  string ->
  'a RPC_arg.t ->
  ('b -> 'a list) ->
  ('b, 'a list) field

type ('a, 'b, 'c) open_query

val query : 'b -> ('a, 'b, 'b) open_query

val ( |+ ) :
  ('a, 'b, 'c -> 'd) open_query -> ('a, 'c) field -> ('a, 'b, 'd) open_query

val seal : ('a, 'b, 'a) open_query -> 'a t

type untyped = (string * string) list

exception Invalid of string

val parse : 'a query -> untyped -> 'a
end
# 66 "v4.in.ml"


  module RPC_service : sig
# 1 "v4/RPC_service.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** HTTP methods. *)
type meth = [`GET | `POST | `DELETE | `PUT | `PATCH]

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) t
  constraint 'meth = [< meth]

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output) t

val get_service :
  ?description:string ->
  query:'query RPC_query.t ->
  output:'output Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([`GET], 'prefix, 'params, 'query, unit, 'output) service

val post_service :
  ?description:string ->
  query:'query RPC_query.t ->
  input:'input Data_encoding.t ->
  output:'output Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([`POST], 'prefix, 'params, 'query, 'input, 'output) service

val delete_service :
  ?description:string ->
  query:'query RPC_query.t ->
  output:'output Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([`DELETE], 'prefix, 'params, 'query, unit, 'output) service

val patch_service :
  ?description:string ->
  query:'query RPC_query.t ->
  input:'input Data_encoding.t ->
  output:'output Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([`PATCH], 'prefix, 'params, 'query, 'input, 'output) service

val put_service :
  ?description:string ->
  query:'query RPC_query.t ->
  input:'input Data_encoding.t ->
  output:'output Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([`PUT], 'prefix, 'params, 'query, 'input, 'output) service
end
# 68 "v4.in.ml"


  module RPC_answer : sig
# 1 "v4/RPC_answer.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Return type for service handler *)
type 'o t =
  [ `Ok of 'o (* 200 *)
  | `OkChunk of 'o (* 200 but send answer as chunked transfer encoding *)
  | `OkStream of 'o stream (* 200 *)
  | `Created of string option (* 201 *)
  | `No_content (* 204 *)
  | `Unauthorized of error list option (* 401 *)
  | `Forbidden of error list option (* 403 *)
  | `Not_found of error list option (* 404 *)
  | `Conflict of error list option (* 409 *)
  | `Error of error list option (* 500 *) ]

and 'a stream = {next : unit -> 'a option Lwt.t; shutdown : unit -> unit}

val return : 'o -> 'o t Lwt.t

(** [return_chunked] is identical to [return] but it indicates to the server
    that the result might be long and that the serialisation should be done in
    mutliple chunks.

    You should use [return_chunked] when returning an (unbounded or potentially
    large) list, array, map, or other such set. *)
val return_chunked : 'o -> 'o t Lwt.t

val return_stream : 'o stream -> 'o t Lwt.t

val not_found : 'o t Lwt.t

val fail : error list -> 'a t Lwt.t
end
# 70 "v4.in.ml"


  module RPC_directory : sig
# 1 "v4/RPC_directory.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Dispatch tree *)
type 'prefix t

type 'prefix directory = 'prefix t

(** Empty list of dispatch trees *)
val empty : 'prefix directory

val map : ('a -> 'b Lwt.t) -> 'b directory -> 'a directory

val prefix : ('pr, 'p) RPC_path.path -> 'p directory -> 'pr directory

val merge : 'a directory -> 'a directory -> 'a directory

(** Possible error while registering services. *)
type step =
  | Static of string
  | Dynamic of RPC_arg.descr
  | DynamicTail of RPC_arg.descr

type conflict =
  | CService of RPC_service.meth
  | CDir
  | CBuilder
  | CTail
  | CTypes of RPC_arg.descr * RPC_arg.descr
  | CType of RPC_arg.descr * string list

exception Conflict of step list * conflict

(** Registering handler in service tree.

    The [chunked] parameter controls whether the answer to the RPC is chunk
    encoded (i.e., the serialisation is split and the caller receives the answer
    in multiple chunks) or not. Defaults to [false]. Set to [true] for RPCs that
    return potentially large collections (e.g., unbounded lists). *)
val register :
  chunked:bool ->
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output tzresult Lwt.t) ->
  'prefix directory

val opt_register :
  chunked:bool ->
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output option tzresult Lwt.t) ->
  'prefix directory

val gen_register :
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> [< 'output RPC_answer.t] Lwt.t) ->
  'prefix directory

val lwt_register :
  chunked:bool ->
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output Lwt.t) ->
  'prefix directory

(** Registering handler in service tree. Curryfied variant.  *)

val register0 :
  chunked:bool ->
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o tzresult Lwt.t) ->
  unit directory

val register1 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register2 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register3 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register4 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register5 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val opt_register0 :
  chunked:bool ->
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o option tzresult Lwt.t) ->
  unit directory

val opt_register1 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register2 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register3 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register4 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register5 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val gen_register0 :
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  unit directory

val gen_register1 :
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  'prefix directory

val gen_register2 :
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  'prefix directory

val gen_register3 :
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  'prefix directory

val gen_register4 :
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  'prefix directory

val gen_register5 :
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  'prefix directory

val lwt_register0 :
  chunked:bool ->
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o Lwt.t) ->
  unit directory

val lwt_register1 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register2 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register3 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register4 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register5 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

(** Registering dynamic subtree. *)
val register_dynamic_directory :
  ?descr:string ->
  'prefix directory ->
  ('prefix, 'a) RPC_path.t ->
  ('a -> 'a directory Lwt.t) ->
  'prefix directory
end
# 72 "v4.in.ml"


  module Base58 : sig
# 1 "v4/base58.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type 'a encoding

val simple_decode : 'a encoding -> string -> 'a option

val simple_encode : 'a encoding -> 'a -> string

type data = ..

val register_encoding :
  prefix:string ->
  length:int ->
  to_raw:('a -> string) ->
  of_raw:(string -> 'a option) ->
  wrap:('a -> data) ->
  'a encoding

val check_encoded_prefix : 'a encoding -> string -> int -> unit

val decode : string -> data option
end
# 74 "v4.in.ml"


  module S : sig
# 1 "v4/s.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(** Generic interface for a datatype with comparison, pretty-printer
    and serialization functions. *)
module type T = sig
  type t

  include Compare.S with type t := t

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val to_bytes : t -> bytes

  val of_bytes : bytes -> t option
end

(** Generic interface for a datatype with comparison, pretty-printer,
    serialization functions and a hashing function. *)
module type HASHABLE = sig
  include T

  type hash

  val hash : t -> hash

  val hash_raw : bytes -> hash
end

(** {2 Hash Types} *)

(** The signature of an abstract hash type, as produced by functor
    {!Make_SHA256}. The {!t} type is abstracted for separating the
    various kinds of hashes in the system at typing time. Each type is
    equipped with functions to use it as is of as keys in the database
    or in memory sets and maps. *)

module type MINIMAL_HASH = sig
  type t

  val name : string

  val title : string

  val pp : Format.formatter -> t -> unit

  val pp_short : Format.formatter -> t -> unit

  include Compare.S with type t := t

  val hash_bytes : ?key:bytes -> bytes list -> t

  val hash_string : ?key:string -> string list -> t

  val zero : t
end

module type RAW_DATA = sig
  type t

  val size : int (* in bytes *)

  val to_bytes : t -> bytes

  val of_bytes_opt : bytes -> t option

  val of_bytes_exn : bytes -> t
end

module type B58_DATA = sig
  type t

  val to_b58check : t -> string

  val to_short_b58check : t -> string

  val of_b58check_exn : string -> t

  val of_b58check_opt : string -> t option

  type Base58.data += Data of t

  val b58check_encoding : t Base58.encoding
end

module type ENCODER = sig
  type t

  val encoding : t Data_encoding.t

  val rpc_arg : t RPC_arg.t
end

module type INDEXES_SET = sig
  include Set.S

  val random_elt : t -> elt

  val encoding : t Data_encoding.t
end

module type INDEXES_MAP = sig
  include Map.S

  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end

module type INDEXES = sig
  type t

  module Set : INDEXES_SET with type elt = t

  module Map : INDEXES_MAP with type key = t
end

module type HASH = sig
  include MINIMAL_HASH

  include RAW_DATA with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  include INDEXES with type t := t
end

module type MERKLE_TREE = sig
  type elt

  include HASH

  val compute : elt list -> t

  val empty : t

  type path = Left of path * t | Right of t * path | Op

  val compute_path : elt list -> int -> path

  val check_path : path -> elt -> t * int

  val path_encoding : path Data_encoding.t
end

module type SIGNATURE_PUBLIC_KEY_HASH = sig
  type t

  val pp : Format.formatter -> t -> unit

  val pp_short : Format.formatter -> t -> unit

  include Compare.S with type t := t

  include RAW_DATA with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  include INDEXES with type t := t

  val zero : t
end

module type SIGNATURE_PUBLIC_KEY = sig
  type t

  val pp : Format.formatter -> t -> unit

  include Compare.S with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  type public_key_hash_t

  val hash : t -> public_key_hash_t

  val size : t -> int (* in bytes *)

  val of_bytes_without_validation : bytes -> t option
end

module type SIGNATURE = sig
  module Public_key_hash : SIGNATURE_PUBLIC_KEY_HASH

  module Public_key :
    SIGNATURE_PUBLIC_KEY with type public_key_hash_t := Public_key_hash.t

  type t

  val pp : Format.formatter -> t -> unit

  include RAW_DATA with type t := t

  include Compare.S with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  val zero : t

  type watermark

  (** Check a signature *)
  val check : ?watermark:watermark -> Public_key.t -> t -> bytes -> bool
end

module type FIELD = sig
  type t

  (** The order of the finite field *)
  val order : Z.t

  (** minimal number of bytes required to encode a value of the field. *)
  val size_in_bytes : int

  (** [check_bytes bs] returns [true] if [bs] is a correct byte
      representation of a field element *)
  val check_bytes : Bytes.t -> bool

  (** The neutral element for the addition *)
  val zero : t

  (** The neutral element for the multiplication *)
  val one : t

  (** [add a b] returns [a + b mod order] *)
  val add : t -> t -> t

  (** [mul a b] returns [a * b mod order] *)
  val mul : t -> t -> t

  (** [eq a b] returns [true] if [a = b mod order], else [false] *)
  val eq : t -> t -> bool

  (** [negate x] returns [-x mod order]. Equivalently, [negate x] returns the
      unique [y] such that [x + y mod order = 0]
  *)
  val negate : t -> t

  (** [inverse_opt x] returns [x^-1] if [x] is not [0] as an option, else [None] *)
  val inverse_opt : t -> t option

  (** [pow x n] returns [x^n] *)
  val pow : t -> Z.t -> t

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes [(Option.get (of_bytes_opt t)) = t]. By default,
      little endian encoding is used and the given element is modulo the prime
      order *)
  val of_bytes_opt : Bytes.t -> t option

  (** Convert the value t to a bytes representation which can be used for
      hashing for instance. It is not required that [to_bytes (Option.get
      (of_bytes_opt t)) = t]. By default, little endian encoding is used, and
      length of the resulting bytes may vary depending on the order.
  *)
  val to_bytes : t -> Bytes.t
end

(** Module type for the prime fields GF(p) *)
module type PRIME_FIELD = sig
  include FIELD

  (** [of_z x] builds an element t from the Zarith element [x]. [mod order] is
      applied if [x >= order] or [x < 0]. *)
  val of_z : Z.t -> t

  (** [to_z x] builds a Zarith element, using the decimal representation.
      Arithmetic on the result can be done using the modular functions on
      integers *)
  val to_z : t -> Z.t
end

module type CURVE = sig
  (** The type of the element in the elliptic curve *)
  type t

  (** The size of a point representation, in bytes *)
  val size_in_bytes : int

  module Scalar : FIELD

  (** Check if a point, represented as a byte array, is on the curve **)
  val check_bytes : Bytes.t -> bool

  (** Attempt to construct a point from a byte array *)
  val of_bytes_opt : Bytes.t -> t option

  (** Return a representation in bytes *)
  val to_bytes : t -> Bytes.t

  (** Zero of the elliptic curve *)
  val zero : t

  (** A fixed generator of the elliptic curve *)
  val one : t

  (** Return the addition of two element *)
  val add : t -> t -> t

  (** Double the element *)
  val double : t -> t

  (** Return the opposite of the element *)
  val negate : t -> t

  (** Return [true] if the two elements are algebraically the same *)
  val eq : t -> t -> bool

  (** Multiply an element by a scalar *)
  val mul : t -> Scalar.t -> t
end
end
# 76 "v4.in.ml"


  module Blake2B : sig
# 1 "v4/blake2B.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Builds a new Hash type using Blake2B. *)

(** The parameters for creating a new Hash type using
    {!Make_Blake2B}. Both {!name} and {!title} are only informative,
    used in error messages and serializers. *)

module type Name = sig
  val name : string

  val title : string

  val size : int option
end

module type PrefixedName = sig
  include Name

  val b58check_prefix : string
end

module Make_minimal (Name : Name) : S.MINIMAL_HASH

module type Register = sig
  val register_encoding :
    prefix:string ->
    length:int ->
    to_raw:('a -> string) ->
    of_raw:(string -> 'a option) ->
    wrap:('a -> Base58.data) ->
    'a Base58.encoding
end

module Make (Register : Register) (Name : PrefixedName) : S.HASH
end
# 78 "v4.in.ml"


  module Bls12_381 : sig
# 1 "v4/bls12_381.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.ch>                      *)
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

module Fr : S.PRIME_FIELD

module G1 : S.CURVE with type Scalar.t = Fr.t

module G2 : S.CURVE with type Scalar.t = Fr.t

val pairing_check : (G1.t * G2.t) list -> bool
end
# 80 "v4.in.ml"


  module Bls_signature : sig
# 1 "v4/bls_signature.mli"
(* MIT License
*
* Copyright (c) 2020 Danny Willems <be.danny.willems@gmail.com>
* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE. *)

(** Type of the public keys *)
type pk

(* Not abstracting the type to avoid to write (de)serialisation routines *)
type signature = Bytes.t

(** Build a value of type [pk] without performing any check on the input.
    It is safe to use this function when verifying a signature as the
    signature function verifies if the point is in the prime subgroup. Using
    [unsafe_pk_of_bytes] removes a verification performed twice when used
    [pk_of_bytes_exn] or [pk_of_bytes_opt].

    The expected bytes format are the compressed form of a point on G1. *)

val unsafe_pk_of_bytes : Bytes.t -> pk

(** Build a value of type [pk] safely, i.e. the function checks the bytes
    given in parameters represents a point on the curve and in the prime subgroup.
    Return [None] if the bytes are not in the correct format or does
    not represent a point in the prime subgroup.

    The expected bytes format are the compressed form of a point on G1.
*)
val pk_of_bytes_opt : Bytes.t -> pk option

(** Returns a bytes representation of a value of type [pk]. The output is the
    compressed form a the point G1.t the [pk] represents.
*)
val pk_to_bytes : pk -> Bytes.t

(** [aggregate_signature_opt signatures] aggregates the signatures [signatures], following
    https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-2.8.
    Return [None] if [INVALID] is expected in the specification
*)
val aggregate_signature_opt : Bytes.t list -> Bytes.t option

val verify : pk -> Bytes.t -> signature -> bool

val aggregate_verify : (pk * Bytes.t) list -> signature -> bool
end
# 82 "v4.in.ml"


  module Ed25519 : sig
# 1 "v4/ed25519.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos - Ed25519 cryptography *)

include S.SIGNATURE with type watermark := bytes
end
# 84 "v4.in.ml"


  module Secp256k1 : sig
# 1 "v4/secp256k1.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos - Secp256k1 cryptography *)

include S.SIGNATURE with type watermark := bytes
end
# 86 "v4.in.ml"


  module P256 : sig
# 1 "v4/p256.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos - P256 cryptography *)

include S.SIGNATURE with type watermark := bytes
end
# 88 "v4.in.ml"


  module Chain_id : sig
# 1 "v4/chain_id.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

include S.HASH
end
# 90 "v4.in.ml"


  module Signature : sig
# 1 "v4/signature.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1 of Secp256k1.Public_key_hash.t
  | P256 of P256.Public_key_hash.t

type public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1 of Secp256k1.Public_key.t
  | P256 of P256.Public_key.t

type watermark =
  | Block_header of Chain_id.t
  | Endorsement of Chain_id.t
  | Generic_operation
  | Custom of bytes

include
  S.SIGNATURE
    with type Public_key_hash.t = public_key_hash
     and type Public_key.t = public_key
     and type watermark := watermark
end
# 92 "v4.in.ml"


  module Block_hash : sig
# 1 "v4/block_hash.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Blocks hashes / IDs. *)
include S.HASH
end
# 94 "v4.in.ml"


  module Operation_hash : sig
# 1 "v4/operation_hash.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Operations hashes / IDs. *)
include S.HASH
end
# 96 "v4.in.ml"


  module Operation_list_hash : sig
# 1 "v4/operation_list_hash.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Blocks hashes / IDs. *)
include S.MERKLE_TREE with type elt = Operation_hash.t
end
# 98 "v4.in.ml"


  module Operation_list_list_hash : sig
# 1 "v4/operation_list_list_hash.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Blocks hashes / IDs. *)
include S.MERKLE_TREE with type elt = Operation_list_hash.t
end
# 100 "v4.in.ml"


  module Protocol_hash : sig
# 1 "v4/protocol_hash.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Protocol hashes / IDs. *)
include S.HASH
end
# 102 "v4.in.ml"


  module Context_hash : sig
# 1 "v4/context_hash.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Committed context hashes / IDs. *)
include S.HASH

(** The module for representing the hash version of a context *)
module Version : sig
  (** The type for hash versions. *)
  type t = private int

  include Compare.S with type t := t

  (** [pp] is the pretty-printer for hash versions. *)
  val pp : Format.formatter -> t -> unit

  (** [encoding] is the data encoding for hash versions. *)
  val encoding : t Data_encoding.t

  (** [of_int i] is the hash version equivalent to [i].
      This function raises [Invalid_argument] if [i] is not an unsigned 16-bit integer. *)
  val of_int : int -> t
end

type version = Version.t
end
# 104 "v4.in.ml"


  module Sapling : sig
# 1 "v4/sapling.mli"
(* The MIT License (MIT)
 *
 * Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. *)

module Ciphertext : sig
  type t

  val encoding : t Data_encoding.t

  val get_memo_size : t -> int
end

module Commitment : sig
  type t

  val encoding : t Data_encoding.t

  val valid_position : int64 -> bool
end

module CV : sig
  type t

  val encoding : t Data_encoding.t
end

module Hash : sig
  type t

  val compare : t -> t -> int

  val encoding : t Data_encoding.t

  val to_bytes : t -> Bytes.t

  val of_bytes_exn : Bytes.t -> t

  val uncommitted : height:int -> t

  val merkle_hash : height:int -> t -> t -> t

  val of_commitment : Commitment.t -> t

  val to_commitment : t -> Commitment.t
end

module Nullifier : sig
  type t

  val encoding : t Data_encoding.t

  val compare : t -> t -> int
end

module UTXO : sig
  type rk

  type spend_proof

  type spend_sig

  type output_proof

  type input = {
    cv : CV.t;
    nf : Nullifier.t;
    rk : rk;
    proof_i : spend_proof;
    signature : spend_sig;
  }

  val input_encoding : input Data_encoding.t

  type output = {
    cm : Commitment.t;
    proof_o : output_proof;
    ciphertext : Ciphertext.t;
  }

  val output_encoding : output Data_encoding.t

  type binding_sig

  type transaction = {
    inputs : input list;
    outputs : output list;
    binding_sig : binding_sig;
    balance : Int64.t;
    root : Hash.t;
  }

  val transaction_encoding : transaction Data_encoding.t

  val binding_sig_encoding : binding_sig Data_encoding.t
end

module Verification : sig
  type t

  val with_verification_ctx : (t -> 'a) -> 'a

  val check_spend : t -> UTXO.input -> Hash.t -> string -> bool

  val check_output : t -> UTXO.output -> bool

  val final_check : t -> UTXO.transaction -> string -> bool
end
end
# 106 "v4.in.ml"


  module Timelock : sig
# 1 "v4/timelock.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com                *)
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

(** Contains a value (the decryption of the ciphertext) that can be provably
recovered in [time] sequential operation or with the rsa secret. *)
type chest

val chest_encoding : chest Data_encoding.t

(** Provably opens a chest in a short time. *)
type chest_key

val chest_key_encoding : chest_key Data_encoding.t

(** Result of the opening of a chest.
    The opening can fail in two way which we distinguish to blame the right person.
    One can provide a false unlocked_value or unlocked_proof, in which case
    we return [Bogus_opening] and the provider of the chest key is at fault.
    Otherwise, one can lock the wrong key or put garbage in the ciphertext in which case
    we return [Bogus_cipher] and the provider of the chest is at fault.
    Otherwise we return [Correct payload] where payload was what had
    originally been put in the chest. *)
type opening_result = Correct of Bytes.t | Bogus_cipher | Bogus_opening

(** Takes a chest, chest key and time and tries to recover the underlying
    plaintext. See the documentation of opening_result. *)
val open_chest : chest -> chest_key -> time:int -> opening_result

(** Gives the size of the underlying plaintext in a chest in bytes.
    Used for gas accounting*)
val get_plaintext_size : chest -> int
end
# 108 "v4.in.ml"


  module Micheline : sig
# 1 "v4/micheline.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type annot = string list

type ('l, 'p) node =
  | Int of 'l * Z.t
  | String of 'l * string
  | Bytes of 'l * bytes
  | Prim of 'l * 'p * ('l, 'p) node list * annot
  | Seq of 'l * ('l, 'p) node list

type 'p canonical

type canonical_location

val dummy_location : canonical_location

val root : 'p canonical -> (canonical_location, 'p) node

val canonical_location_encoding : canonical_location Data_encoding.encoding

val canonical_encoding :
  variant:string ->
  'l Data_encoding.encoding ->
  'l canonical Data_encoding.encoding

val location : ('l, 'p) node -> 'l

val annotations : ('l, 'p) node -> string list

val strip_locations : (_, 'p) node -> 'p canonical
end
# 110 "v4.in.ml"


  module Block_header : sig
# 1 "v4/block_header.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type shell_header = {
  level : Int32.t;
      (** The number of preceding block in this chain, i.e. the genesis
      has level 0. *)
  proto_level : int;
      (** The number of preceding protocol change in the chain (modulo 256),
      i.e. the genesis has proto_level 0. *)
  predecessor : Block_hash.t;
  timestamp : Time.t;
  validation_passes : int;
  operations_hash : Operation_list_list_hash.t;
  fitness : Bytes.t list;
  context : Context_hash.t;
}

val shell_header_encoding : shell_header Data_encoding.t

type t = {shell : shell_header; protocol_data : bytes}

include S.HASHABLE with type t := t and type hash := Block_hash.t
end
# 112 "v4.in.ml"


  module Fitness : sig
# 1 "v4/fitness.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** The fitness of a block is defined as a list of bytes,
    compared in a lexicographical order (longer list are greater). *)
include S.T with type t = bytes list
end
# 114 "v4.in.ml"


  module Operation : sig
# 1 "v4/operation.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos operations. *)

type shell_header = {
  branch : Block_hash.t;
      (** The operation is only valid in a branch containing the
      block [branch]. *)
}

val shell_header_encoding : shell_header Data_encoding.t

type t = {shell : shell_header; proto : bytes}

include S.HASHABLE with type t := t and type hash := Operation_hash.t
end
# 116 "v4.in.ml"


  module Context : sig
# 1 "v4/context.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** View over the context store, restricted to types, access and
    functional manipulation of an existing context. *)

(* Copy/paste of Environment_context_sigs.Context.S *)

(** The tree depth of a fold. See the [fold] function for more information. *)
type depth = [`Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int]

module type VIEW = sig
  (** The type for context views. *)
  type t

  (** The type for context keys. *)
  type key

  (** The type for context values. *)
  type value

  (** The type for context trees. *)
  type tree

  (** {2 Getters} *)

  (** [mem t k] is an Lwt promise that resolves to [true] iff [k] is bound
      to a value in [t]. *)
  val mem : t -> key -> bool Lwt.t

  (** [mem_tree t k] is like {!mem} but for trees. *)
  val mem_tree : t -> key -> bool Lwt.t

  (** [find t k] is an Lwt promise that resolves to [Some v] if [k] is
      bound to the value [v] in [t] and [None] otherwise. *)
  val find : t -> key -> value option Lwt.t

  (** [find_tree t k] is like {!find} but for trees. *)
  val find_tree : t -> key -> tree option Lwt.t

  (** [list t key] is the list of files and sub-nodes stored under [k] in [t].
      The result order is not specified but is stable.

      [offset] and [length] are used for pagination. *)
  val list :
    t -> ?offset:int -> ?length:int -> key -> (string * tree) list Lwt.t

  (** {2 Setters} *)

  (** [add t k v] is an Lwt promise that resolves to [c] such that:

    - [k] is bound to [v] in [c];
    - and [c] is similar to [t] otherwise.

    If [k] was already bound in [t] to a value that is physically equal
    to [v], the result of the function is a promise that resolves to
    [t]. Otherwise, the previous binding of [k] in [t] disappears. *)
  val add : t -> key -> value -> t Lwt.t

  (** [add_tree] is like {!add} but for trees. *)
  val add_tree : t -> key -> tree -> t Lwt.t

  (** [remove t k v] is an Lwt promise that resolves to [c] such that:

    - [k] is unbound in [c];
    - and [c] is similar to [t] otherwise. *)
  val remove : t -> key -> t Lwt.t

  (** {2 Folding} *)

  (** [fold ?depth t root ~init ~f] recursively folds over the trees
      and values of [t]. The [f] callbacks are called with a key relative
      to [root]. [f] is never called with an empty key for values; i.e.,
      folding over a value is a no-op.

      Elements are traversed in lexical order of keys.

      The depth is 0-indexed. If [depth] is set (by default it is not), then [f]
      is only called when the conditions described by the parameter is true:

      - [Eq d] folds over nodes and contents of depth exactly [d].
      - [Lt d] folds over nodes and contents of depth strictly less than [d].
      - [Le d] folds over nodes and contents of depth less than or equal to [d].
      - [Gt d] folds over nodes and contents of depth strictly more than [d].
      - [Ge d] folds over nodes and contents of depth more than or equal to [d].

      If [order] is [`Sorted] (the default), the elements are traversed in
      lexicographic order of their keys. For large nodes, these two modes are memory-consuming,
      use [`Undefined] for a more memory efficient [fold]. *)
  val fold :
    ?depth:depth ->
    t ->
    key ->
    order:[`Sorted | `Undefined] ->
    init:'a ->
    f:(key -> tree -> 'a -> 'a Lwt.t) ->
    'a Lwt.t
end

module Kind : sig
  type t = [`Value | `Tree]
end

module type TREE = sig
  (** [Tree] provides immutable, in-memory partial mirror of the
      context, with lazy reads and delayed writes.

      Trees are immutable and non-persistent (they disappear if the
      host crash), held in memory for efficiency, where reads are done
      lazily and writes are done only when needed, e.g. on
      [Context.commit]. If a key is modified twice, only the last
      value will be written to disk on commit. *)

  (** The type for context views. *)
  type t

  (** The type for context trees. *)
  type tree

  include VIEW with type t := tree and type tree := tree

  (** [empty _] is the empty tree. *)
  val empty : t -> tree

  (** [is_empty t] is true iff [t] is [empty _]. *)
  val is_empty : tree -> bool

  (** [kind t] is [t]'s kind. It's either a tree node or a leaf
      value. *)
  val kind : tree -> Kind.t

  (** [to_value t] is an Lwt promise that resolves to [Some v] if [t]
      is a leaf tree and [None] otherwise. It is equivalent to [find t
      []]. *)
  val to_value : tree -> value option Lwt.t

  (** [of_value _ v] is an Lwt promise that resolves to the leaf tree
      [v]. Is is equivalent to [add (empty _) [] v]. *)
  val of_value : t -> value -> tree Lwt.t

  (** [hash t] is [t]'s Merkle hash. *)
  val hash : tree -> Context_hash.t

  (** [equal x y] is true iff [x] and [y] have the same Merkle hash. *)
  val equal : tree -> tree -> bool

  (** {2 Caches} *)

  (** [clear ?depth t] clears all caches in the tree [t] for subtrees with a
      depth higher than [depth]. If [depth] is not set, all of the subtrees are
      cleared. *)
  val clear : ?depth:int -> tree -> unit
end

include VIEW with type key = string list and type value = bytes

module Tree :
  TREE
    with type t := t
     and type key := key
     and type value := value
     and type tree := tree

val register_resolver :
  'a Base58.encoding -> (t -> string -> 'a list Lwt.t) -> unit

val complete : t -> string -> string list Lwt.t

(** Get the hash version used for the context *)
val get_hash_version : t -> Context_hash.Version.t

(** Set the hash version used for the context.  It may recalculate the hashes
    of the whole context, which can be a long process.
    Returns an Error if the hash version is unsupported. *)
val set_hash_version :
  t -> Context_hash.Version.t -> t Error_monad.shell_tzresult Lwt.t

type cache_key

type cache_value = ..

module type CACHE = sig
  (** Type for context view. A context contains a cache. A cache is
     made of subcaches. Each subcache has its own size limit. The
     limit of its subcache is called a layout and can be initialized
     via the [set_cache_layout] function. *)
  type t

  (** Size for subcaches and values of the cache. Units are not
     specified and left to the economic protocol. *)
  type size

  (** Index type to index caches. *)
  type index

  (** Identifier type for keys. *)
  type identifier

  (** A key uniquely identifies a cached [value] in some subcache. *)
  type key

  (** Cached values inhabit an extensible type. *)
  type value = ..

  (** [key_of_identifier ~cache_index identifier] builds a key from the
      [cache_index] and the [identifier].

      No check are made to ensure the validity of the index.  *)
  val key_of_identifier : cache_index:index -> identifier -> key

  (** [identifier_of_key key] returns the identifier associated to the
      [key]. *)
  val identifier_of_key : key -> identifier

  (** [pp fmt cache] is a pretty printter for a [cache]. *)
  val pp : Format.formatter -> t -> unit

  (** [find ctxt k = Some v] if [v] is the value associated to [k] in
     in the cache where [k] is. Returns [None] if there is no such
     value in the cache of [k].  This function is in the Lwt monad
     because if the value has not been constructed, it is constructed
     on the fly. *)
  val find : t -> key -> value option Lwt.t

  (** [set_cache_layout ctxt layout] sets the caches of [ctxt] to
     comply with given [layout]. If there was already a cache in
     [ctxt], it is erased by the new layout.

     Otherwise, a fresh collection of empty caches is reconstructed
     from the new [layout]. Notice that cache [key]s are invalidated
     in that case, i.e., [get t k] will return [None]. *)
  val set_cache_layout : t -> size list -> t Lwt.t

  (** [update ctxt k (Some (e, size))] returns a cache where the value
      [e] of [size] is associated to key [k]. If [k] is already in the
      cache, the cache entry is updated.

      [update ctxt k None] removes [k] from the cache. *)
  val update : t -> key -> (value * size) option -> t

  (** [sync ctxt ~cache_nonce] updates the context with the domain of
     the cache computed so far. Such function is expected to be called
     at the end of the validation of a block, when there is no more
     accesses to the cache.

     [cache_nonce] identifies the block that introduced new cache
     entries. The nonce should identify uniquely the block which
     modifies this value. It cannot be the block hash for circularity
     reasons: The value of the nonce is stored onto the context and
     consequently influences the context hash of the very same
     block. Such nonce cannot be determined by the shell and its
     computation is delegated to the economic protocol.
  *)
  val sync : t -> cache_nonce:Bytes.t -> t Lwt.t

  (** [clear ctxt] removes all cache entries. *)
  val clear : t -> t

  (** {3 Cache introspection} *)

  (** [list_keys ctxt ~cache_index] returns the list of cached keys in
     cache numbered [cache_index] along with their respective
     [size]. The returned list is sorted in terms of their age in the
     cache, the oldest coming first. If [cache_index] is invalid,
     then this function returns [None]. *)
  val list_keys : t -> cache_index:index -> (key * size) list option

  (** [key_rank index ctxt key] returns the number of cached value older
       than the given [key]; or, [None] if the [key] is not a cache key. *)
  val key_rank : t -> key -> int option

  (** {3 Cache helpers for RPCs} *)

  (** [future_cache_expectation ctxt ~time_in_blocks] returns [ctxt] except
      that the entries of the caches that are presumably too old to
      still be in the caches in [n_blocks] are removed.

      This function is based on a heuristic. The context maintains
      the median of the number of removed entries: this number is
      multipled by `n_blocks` to determine the entries that are
      likely to be removed in `n_blocks`. *)
  val future_cache_expectation : t -> time_in_blocks:int -> t

  (** [cache_size ctxt ~cache_index] returns an overapproximation of
      the size of the cache. Returns [None] if [cache_index] is not a
      valid cache index. *)
  val cache_size : t -> cache_index:index -> size option

  (** [cache_size_limit ctxt ~cache_index] returns the maximal size of
      the cache indexed by [cache_index]. Returns [None] if
      [cache_index] is not a valid cache index. *)
  val cache_size_limit : t -> cache_index:index -> size option
end

module Cache :
  CACHE
    with type t := t
     and type size := int
     and type index := int
     and type identifier := string
     and type key = cache_key
     and type value = cache_value
end
# 118 "v4.in.ml"


  module Updater : sig
# 1 "v4/updater.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Protocol Environment - Protocol updater. *)

(** Validation result: the record returned by the protocol
    on the successful validation of a block. *)
type validation_result = {
  context : Context.t;
      (** The resulting context, it will be used for the next block. *)
  fitness : Fitness.t;
      (** The effective fitness of the block (to be compared with the one
      'announced' in the block header). *)
  message : string option;
      (** An optional informative message, akin to a 'git commit' message,
      which can be attached to the [context] when it's being commited. *)
  max_operations_ttl : int;
      (** The "time-to-live" of operations for the next block: any
      operation whose 'branch' is older than 'ttl' blocks in the past
      cannot be included in the next block. *)
  last_allowed_fork_level : Int32.t;
      (** The level of the last block for which the node might consider an
      alternate branch. The shell should consider as invalid any branch
      whose fork point is older (has a lower level) than the
      given value. *)
}

type quota = {
  max_size : int;
      (** The maximum size (in bytes) of the serialized list of
      operations. *)
  max_op : int option;
      (** The maximum number of operations in a block.
      [None] means no limit. *)
}

type rpc_context = {
  block_hash : Block_hash.t;
  block_header : Block_header.shell_header;
  context : Context.t;
}

(** This is the signature of a Tezos protocol implementation. It has
    access to the standard library and the Environment module. *)
module type PROTOCOL = sig
  (** The maximum size of a block header in bytes. *)
  val max_block_length : int

  (** The maximum size of an {!operation} in bytes. This value is bigger than the size
      of the bytes required for {!operation_data}, because this value accounts
      for the shell header. *)
  val max_operation_data_length : int

  (** Operations quota for each validation pass. The length of the
     list denotes the number of validation passes. *)
  val validation_passes : quota list

  (** The economic protocol-specific type of blocks. *)
  type block_header_data

  (** Encoding for economic protocol-specific part of block headers. *)
  val block_header_data_encoding : block_header_data Data_encoding.t

  (** A fully parsed block header. *)
  type block_header = {
    shell : Block_header.shell_header;
    protocol_data : block_header_data;
  }

  (** Economic protocol-specific side information computed by the
     protocol during the validation of a block. Should not include
     information about the evaluation of operations which is handled
     separately by {!operation_metadata}. To be used as an execution
     trace by tools (client, indexer). Not necessary for
     validation. *)
  type block_header_metadata

  (** Encoding for economic protocol-specific block metadata. *)
  val block_header_metadata_encoding : block_header_metadata Data_encoding.t

  (** The economic protocol-specific type of operations. *)
  type operation_data

  (** Economic protocol-specific side information computed by the
     protocol during the validation of each operation, to be used
     conjointly with {!block_header_metadata}. *)
  type operation_receipt

  (** A fully parsed operation. *)
  type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
  }

  (** Encoding for economoic protocol-specific operation data. *)
  val operation_data_encoding : operation_data Data_encoding.t

  (** Encoding for eonomic protocol-specific operation receipts. *)
  val operation_receipt_encoding : operation_receipt Data_encoding.t

  (** Encoding that mixes an operation data and its receipt. *)
  val operation_data_and_receipt_encoding :
    (operation_data * operation_receipt) Data_encoding.t

  (** [acceptable_passes op] lists the validation passes in which the
     input operation [op] can appear. For instance, it results in
     [[0]] if [op] only belongs to the first pass. An answer of [[]]
     means that the [op] is ill-formed and cannot be included at
     all in a block. *)
  val acceptable_passes : operation -> int list

  (** [relative_position_within_block op1 op2] provides a partial and
     strict order of operations within a block. It is intended to be
     used as an argument to {!List.sort} (and other sorting/ordering
     functions) to arrange a set of operations into a sequence, the
     order of which is valid for the protocol.

     A negative (respectively, positive) results means that [op1]
     should appear before (and, respectively, after) [op2] in a
     block. This function does not provide a total ordering on the
     operations: a result of [0] entails that the protocol does not
     impose any preferences to the order in which [op1] and [op2]
     should be included in a block.

     {b Caveat Emptor!} [relative_position_within_block o1 o2 = 0]
     does NOT imply that [o1] is equal to [o2] in any way.
     Consequently, it {e MUST NOT} be used as a [compare] component of
     an {!Stdlib.Map.OrderedType}, or any such collection which relies
     on a total comparison function. *)
  val relative_position_within_block : operation -> operation -> int

  (** A functional state that is transmitted through the steps of a
     block validation sequence: it can be created by any of the
     [begin_x] functions below, and its final value is produced by
     {!finalize_block}. It must retain the current state of the store,
     and it can also contain additional information that must be
     remembered during the validation process. Said extra content must
     however be immutable: validator or baker implementations are
     allowed to pause, replay or backtrack throughout validation
     steps. *)
  type validation_state

  (** [begin_partial_application cid ctxt] checks that a block is
     well-formed in a given context. This function should run quickly,
     as its main use is to reject bad blocks from the chain as early
     as possible. The input [ancestor_context] is expected to result
     from the application of an ancestor block of the current head
     with the same economic protocol. Said ancestor block is also
     required to be more recent (i.e., it has a greater level), than
     the current head's "last_allowed_fork_level".

      The resulting `validation_state` will be used for multi-pass
     validation. *)
  val begin_partial_application :
    chain_id:Chain_id.t ->
    ancestor_context:Context.t ->
    predecessor_timestamp:Time.t ->
    predecessor_fitness:Fitness.t ->
    block_header ->
    validation_state tzresult Lwt.t

  (** [begin_application chain_id ... bh] defines the first step in a
     block validation sequence. It initializes a validation context
     for validating a block, whose header is [bh]. *)
  val begin_application :
    chain_id:Chain_id.t ->
    predecessor_context:Context.t ->
    predecessor_timestamp:Time.t ->
    predecessor_fitness:Fitness.t ->
    block_header ->
    validation_state tzresult Lwt.t

  (** [begin_construction] initializes a validation context for
     constructing a new block, as opposed to validating an existing
     block.

     This function can be used in two modes: with and without the
     optional [protocol_data] argument. With the latter, it is used by
     bakers to start the process for baking a new block. Without it,
     is used by the Shell's prevalidator to construct a virtual block,
     which carries the contents of the pre-applied operations of the
     mempool.

     When [protocol_data] is provided, it is not expected to be the
     final value of the field of the same name in the {!type-block_header}
     of the block eventually being baked. Instead, it is expected to
     construct a protocol-specific, good enough, "prototype" of its
     final value. For instance, if the economic protocol specifies
     that its block headers include a signature, [protocol_data] must
     include a (faked) signature.

     Moreover, these prototypes should not be distinguishable after
     the application of [begin_construction]: the function must
     produce the exact same context regardless of being passed a
     prototype, or an "equivalent-but-complete" header. *)
  val begin_construction :
    chain_id:Chain_id.t ->
    predecessor_context:Context.t ->
    predecessor_timestamp:Time.t ->
    predecessor_level:Int32.t ->
    predecessor_fitness:Fitness.t ->
    predecessor:Block_hash.t ->
    timestamp:Time.t ->
    ?protocol_data:block_header_data ->
    unit ->
    validation_state tzresult Lwt.t

  (** [apply_operation vs op] applies the input operation [op] on top
     of the given {!validation_state} [vs]. It must be called after
     {!begin_application} or {!begin_construction}, and before
     {!finalize_block}, for each operation in a block. On a successful
     application, it returns a pair consisting of the resulting
     [validation_state], and the corresponding [operation_receipt]. *)
  val apply_operation :
    validation_state ->
    operation ->
    (validation_state * operation_receipt) tzresult Lwt.t

  (** [finalize_block vs] finalizes the context resulting from the
     application of the contents of the block being validated.

      If there is no protocol migration, i.e., if the block being
     applied is not the last block of the current economic protocol, the
     resulting context can be used in the future as input for the
     validation of its successor blocks. *)
  val finalize_block :
    validation_state ->
    Block_header.shell_header option ->
    (validation_result * block_header_metadata) tzresult Lwt.t

  (** [rpc_services] provides the list of remote procedures exported
     by this protocol implementation. *)
  val rpc_services : rpc_context RPC_directory.t

  (** [init ctxt hd] initializes the context, or upgrades the context
     after a protocol amendment. This function receives as arguments
     the context [ctxt] resulting from the application of the block
     that triggered the amendment, as well as its header [hd]. This
     function should fail if the "protocol stitching", i.e., the
     transition from a valid previous protocol to the one being
     activated, has not been implemented. *)
  val init :
    Context.t -> Block_header.shell_header -> validation_result tzresult Lwt.t

  (** [value_of_key chain_id predecessor_context
     predecessor_timestamp predecessor_level predecessor_fitness
     predecessor timestamp] returns a function to build one value of
     the cache from its key.

     This function is used to restore all or part of the cache, for
     instance when booting a validator to preheat the cache, or when a
     reorganization happens. This function should never fail, returned
     errors are fatal.

     The generated function is passed to [Context.Cache.load_caches]
     which will use it either immediately a cache-loading time or
     on-demand, when a given cached value is accessed. *)
  val value_of_key :
    chain_id:Chain_id.t ->
    predecessor_context:Context.t ->
    predecessor_timestamp:Time.t ->
    predecessor_level:Int32.t ->
    predecessor_fitness:Fitness.t ->
    predecessor:Block_hash.t ->
    timestamp:Time.t ->
    (Context.Cache.key -> Context.Cache.value tzresult Lwt.t) tzresult Lwt.t
end

(** [activate ctxt ph] activates an economic protocol (given by its
   hash [ph]) from the context [ctxt]. The resulting context is still
   a context for the current economic protocol, and the migration is
   not complete until [init] in invoked. *)
val activate : Context.t -> Protocol_hash.t -> Context.t Lwt.t
end
# 120 "v4.in.ml"


  module RPC_context : sig
# 1 "v4/RPC_context.mli"
(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = Updater.rpc_context

class type ['pr] simple =
  object
    method call_proto_service0 :
      'm 'q 'i 'o.
      (([< RPC_service.meth] as 'm), t, t, 'q, 'i, 'o) RPC_service.t ->
      'pr ->
      'q ->
      'i ->
      'o Error_monad.shell_tzresult Lwt.t

    method call_proto_service1 :
      'm 'a 'q 'i 'o.
      (([< RPC_service.meth] as 'm), t, t * 'a, 'q, 'i, 'o) RPC_service.t ->
      'pr ->
      'a ->
      'q ->
      'i ->
      'o Error_monad.shell_tzresult Lwt.t

    method call_proto_service2 :
      'm 'a 'b 'q 'i 'o.
      (([< RPC_service.meth] as 'm), t, (t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
      'pr ->
      'a ->
      'b ->
      'q ->
      'i ->
      'o Error_monad.shell_tzresult Lwt.t

    method call_proto_service3 :
      'm 'a 'b 'c 'q 'i 'o.
      ( ([< RPC_service.meth] as 'm),
        t,
        ((t * 'a) * 'b) * 'c,
        'q,
        'i,
        'o )
      RPC_service.t ->
      'pr ->
      'a ->
      'b ->
      'c ->
      'q ->
      'i ->
      'o Error_monad.shell_tzresult Lwt.t
  end

val make_call0 :
  ([< RPC_service.meth], t, t, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple ->
  'pr ->
  'q ->
  'i ->
  'o shell_tzresult Lwt.t

val make_call1 :
  ([< RPC_service.meth], t, t * 'a, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple ->
  'pr ->
  'a ->
  'q ->
  'i ->
  'o shell_tzresult Lwt.t

val make_call2 :
  ([< RPC_service.meth], t, (t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple ->
  'pr ->
  'a ->
  'b ->
  'q ->
  'i ->
  'o shell_tzresult Lwt.t

val make_call3 :
  ([< RPC_service.meth], t, ((t * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple ->
  'pr ->
  'a ->
  'b ->
  'c ->
  'q ->
  'i ->
  'o shell_tzresult Lwt.t

val make_opt_call0 :
  ([< RPC_service.meth], t, t, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple ->
  'pr ->
  'q ->
  'i ->
  'o option shell_tzresult Lwt.t

val make_opt_call1 :
  ([< RPC_service.meth], t, t * 'a, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple ->
  'pr ->
  'a ->
  'q ->
  'i ->
  'o option shell_tzresult Lwt.t

val make_opt_call2 :
  ([< RPC_service.meth], t, (t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple ->
  'pr ->
  'a ->
  'b ->
  'q ->
  'i ->
  'o option shell_tzresult Lwt.t

val make_opt_call3 :
  ([< RPC_service.meth], t, ((t * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  'pr #simple ->
  'pr ->
  'a ->
  'b ->
  'c ->
  'q ->
  'i ->
  'o option shell_tzresult Lwt.t
end
# 122 "v4.in.ml"

end
