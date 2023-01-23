(* This file was automatically generated, do not edit.*)
(* Edit file v0.in.ml instead. *)
# 1 "v0.in.ml"
module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : sig
# 1 "v0/pervasives.mli"
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

(* TEZOS CHANGES

   * Import version 4.06.1
   * Remove [channel], [exit], ...
   * Remove polymorphic comparisons
   * Remove floating-point arithmetic
   * Remove string conversion functions for float
   * Remove deprecated functions
*)

(** The initially opened module.

    This module provides the basic operations over the built-in types
    (numbers, booleans, byte sequences, strings, exceptions, references,
    lists, arrays, input-output channels, ...).

    This module is automatically opened at the beginning of each compilation.
    All components of this module can therefore be referred by their short
    name, without prefixing them by [Pervasives].
*)

(** {1 Exceptions} *)

(** Raise the given exception value *)
external raise : exn -> 'a = "%raise"

(** A faster version [raise] which does not record the backtrace.
    @since 4.02.0
*)
external raise_notrace : exn -> 'a = "%raise_notrace"

(** Raise exception [Invalid_argument] with the given string. *)
val invalid_arg : string -> 'a

(** Raise exception [Failure] with the given string. *)
val failwith : string -> 'a

(** The [Exit] exception is not raised by any library function.  It is
    provided for use in your programs. *)
exception Exit

(** {1 Boolean operations} *)

(** The boolean negation. *)
external not : bool -> bool = "%boolnot"

(** The boolean 'and'. Evaluation is sequential, left-to-right:
    in [e1 && e2], [e1] is evaluated first, and if it returns [false],
    [e2] is not evaluated at all.
    Right-associative operator at precedence level 3/11. *)
external ( && ) : bool -> bool -> bool = "%sequand"

(** The boolean 'or'. Evaluation is sequential, left-to-right:
    in [e1 || e2], [e1] is evaluated first, and if it returns [true],
    [e2] is not evaluated at all.
    Right-associative operator at precedence level 2/11.
*)
external ( || ) : bool -> bool -> bool = "%sequor"

(** {1 Debugging} *)

(** [__LOC__] returns the location at which this expression appears in
    the file currently being parsed by the compiler, with the standard
    error format of OCaml: "File %S, line %d, characters %d-%d".
    @since 4.02.0
*)
external __LOC__ : string = "%loc_LOC"

(** [__FILE__] returns the name of the file currently being
    parsed by the compiler.
    @since 4.02.0
*)
external __FILE__ : string = "%loc_FILE"

(** [__LINE__] returns the line number at which this expression
    appears in the file currently being parsed by the compiler.
    @since 4.02.0
*)
external __LINE__ : int = "%loc_LINE"

(** [__MODULE__] returns the module name of the file being
    parsed by the compiler.
    @since 4.02.0
*)
external __MODULE__ : string = "%loc_MODULE"

(** [__POS__] returns a tuple [(file,lnum,cnum,enum)], corresponding
    to the location at which this expression appears in the file
    currently being parsed by the compiler. [file] is the current
    filename, [lnum] the line number, [cnum] the character position in
    the line and [enum] the last character position in the line.
    @since 4.02.0
*)
external __POS__ : string * int * int * int = "%loc_POS"

(** [__LOC_OF__ expr] returns a pair [(loc, expr)] where [loc] is the
    location of [expr] in the file currently being parsed by the
    compiler, with the standard error format of OCaml: "File %S, line
    %d, characters %d-%d".
    @since 4.02.0
*)
external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"

(** [__LINE__ expr] returns a pair [(line, expr)], where [line] is the
    line number at which the expression [expr] appears in the file
    currently being parsed by the compiler.
    @since 4.02.0
*)
external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"

(** [__POS_OF__ expr] returns a pair [(loc,expr)], where [loc] is a
    tuple [(file,lnum,cnum,enum)] corresponding to the location at
    which the expression [expr] appears in the file currently being
    parsed by the compiler. [file] is the current filename, [lnum] the
    line number, [cnum] the character position in the line and [enum]
    the last character position in the line.
    @since 4.02.0
*)
external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"

(** {1 Composition operators} *)

(** Reverse-application operator: [x |> f |> g] is exactly equivalent
    to [g (f (x))].
    Left-associative operator at precedence level 4/11.
    @since 4.01
*)
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

(** Application operator: [g @@ f @@ x] is exactly equivalent to
    [g (f (x))].
    Right-associative operator at precedence level 5/11.
    @since 4.01
*)
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

(** {1 Integer arithmetic} *)

(** Integers are 31 bits wide (or 63 bits on 64-bit processors).
    All operations are taken modulo 2{^31} (or 2{^63}).
    They do not fail on overflow. *)

(** Unary negation. You can also write [- e] instead of [~- e].
    Unary operator at precedence level 9/11 for [- e]
    and 11/11 for [~- e]. *)
external ( ~- ) : int -> int = "%negint"

(** Unary addition. You can also write [+ e] instead of [~+ e].
    Unary operator at precedence level 9/11 for [+ e]
    and 11/11 for [~+ e].
    @since 3.12.0
*)
external ( ~+ ) : int -> int = "%identity"

(** [succ x] is [x + 1]. *)
external succ : int -> int = "%succint"

(** [pred x] is [x - 1]. *)
external pred : int -> int = "%predint"

(** Integer addition.
    Left-associative operator at precedence level 6/11. *)
external ( + ) : int -> int -> int = "%addint"

(** Integer subtraction.
    Left-associative operator at precedence level 6/11. *)
external ( - ) : int -> int -> int = "%subint"

(** Integer multiplication.
    Left-associative operator at precedence level 7/11. *)
external ( * ) : int -> int -> int = "%mulint"

(** Integer division.
    Raise [Division_by_zero] if the second argument is 0.
    Integer division rounds the real quotient of its arguments towards zero.
    More precisely, if [x >= 0] and [y > 0], [x / y] is the greatest integer
    less than or equal to the real quotient of [x] by [y].  Moreover,
    [(- x) / y = x / (- y) = - (x / y)].
    Left-associative operator at precedence level 7/11. *)
external ( / ) : int -> int -> int = "%divint"

(** Integer remainder.  If [y] is not zero, the result
    of [x mod y] satisfies the following properties:
    [x = (x / y) * y + x mod y] and
    [abs(x mod y) <= abs(y) - 1].
    If [y = 0], [x mod y] raises [Division_by_zero].
    Note that [x mod y] is negative only if [x < 0].
    Raise [Division_by_zero] if [y] is zero.
    Left-associative operator at precedence level 7/11. *)
external ( mod ) : int -> int -> int = "%modint"

(** Return the absolute value of the argument.  Note that this may be
    negative if the argument is [min_int]. *)
val abs : int -> int

(** The greatest representable integer. *)
val max_int : int

(** The smallest representable integer. *)
val min_int : int

(** {2 Bitwise operations} *)

(** Bitwise logical and.
    Left-associative operator at precedence level 7/11. *)
external ( land ) : int -> int -> int = "%andint"

(** Bitwise logical or.
    Left-associative operator at precedence level 7/11. *)
external ( lor ) : int -> int -> int = "%orint"

(** Bitwise logical exclusive or.
    Left-associative operator at precedence level 7/11. *)
external ( lxor ) : int -> int -> int = "%xorint"

(** Bitwise logical negation. *)
val lnot : int -> int

(** [n lsl m] shifts [n] to the left by [m] bits.
    The result is unspecified if [m < 0] or [m >= bitsize],
    where [bitsize] is [32] on a 32-bit platform and
    [64] on a 64-bit platform.
    Right-associative operator at precedence level 8/11. *)
external ( lsl ) : int -> int -> int = "%lslint"

(** [n lsr m] shifts [n] to the right by [m] bits.
    This is a logical shift: zeroes are inserted regardless of
    the sign of [n].
    The result is unspecified if [m < 0] or [m >= bitsize].
    Right-associative operator at precedence level 8/11. *)
external ( lsr ) : int -> int -> int = "%lsrint"

(** [n asr m] shifts [n] to the right by [m] bits.
    This is an arithmetic shift: the sign bit of [n] is replicated.
    The result is unspecified if [m < 0] or [m >= bitsize].
    Right-associative operator at precedence level 8/11. *)
external ( asr ) : int -> int -> int = "%asrint"

(** {1 String operations}

    More string operations are provided in module {!String}.
*)

(** String concatenation.
    Right-associative operator at precedence level 5/11. *)
val ( ^ ) : string -> string -> string

(** {1 Character operations}

    More character operations are provided in module {!Char}.
*)

(** Return the ASCII code of the argument. *)
external int_of_char : char -> int = "%identity"

(** Return the character with the given ASCII code.
    Raise [Invalid_argument "char_of_int"] if the argument is
    outside the range 0--255. *)
val char_of_int : int -> char

(** {1 Unit operations} *)

(** Discard the value of its argument and return [()].
    For instance, [ignore(f x)] discards the result of
    the side-effecting function [f].  It is equivalent to
    [f x; ()], except that the latter may generate a
    compiler warning; writing [ignore(f x)] instead
    avoids the warning. *)
external ignore : 'a -> unit = "%ignore"

(** {1 String conversion functions} *)

(** Return the string representation of a boolean. As the returned values
    may be shared, the user should not modify them directly.
*)
val string_of_bool : bool -> string

(** Convert the given string to a boolean.
    Return [None] if the string is not
    ["true"] or ["false"].
    @since 4.05
*)
val bool_of_string_opt : string -> bool option

(** Return the string representation of an integer, in decimal. *)
val string_of_int : int -> string

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

    Return [None] if the given string is not a valid representation of
    an integer, or if the integer represented exceeds the range of
    integers representable in type [int].
    @since 4.05
*)
val int_of_string_opt : string -> int option

(** {1 Pair operations} *)

(** Return the first component of a pair. *)
external fst : 'a * 'b -> 'a = "%field0"

(** Return the second component of a pair. *)
external snd : 'a * 'b -> 'b = "%field1"

(** {1 List operations}

    More list operations are provided in module {!List}.
*)

(** List concatenation.  Not tail-recursive (length of the first argument).
    Right-associative operator at precedence level 5/11. *)
val ( @ ) : 'a list -> 'a list -> 'a list

(** {1 References} *)

(** The type of references (mutable indirection cells) containing
    a value of type ['a]. *)
type 'a ref = {mutable contents : 'a}

(** Return a fresh reference containing the given value. *)
external ref : 'a -> 'a ref = "%makemutable"

(** [!r] returns the current contents of reference [r].
    Equivalent to [fun r -> r.contents].
    Unary operator at precedence level 11/11.*)
external ( ! ) : 'a ref -> 'a = "%field0"

(** [r := a] stores the value of [a] in reference [r].
    Equivalent to [fun r v -> r.contents <- v].
    Right-associative operator at precedence level 1/11. *)
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

(** Increment the integer contained in the given reference.
    Equivalent to [fun r -> r := succ !r]. *)
external incr : int ref -> unit = "%incr"

(** Decrement the integer contained in the given reference.
    Equivalent to [fun r -> r := pred !r]. *)
external decr : int ref -> unit = "%decr"

(** {1 Result type} *)

(** @since 4.03.0 *)
type ('a, 'b) result = Ok of 'a | Error of 'b

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

(** Converts a format string into a string. *)
val string_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string

(** [format_of_string s] returns a format string read from the string
    literal [s].
    Note: [format_of_string] can not convert a string argument that is not a
    literal. If you need this functionality, use the more general
    {!Scanf.format_from_string} function.
*)
external format_of_string :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ('a, 'b, 'c, 'd, 'e, 'f) format6
  = "%identity"

(** [f1 ^^ f2] catenates format strings [f1] and [f2]. The result is a
    format string that behaves as the concatenation of format strings [f1] and
    [f2]: in case of formatted output, it accepts arguments from [f1], then
    arguments from [f2]; in case of formatted input, it returns results from
    [f1], then results from [f2].
    Right-associative operator at precedence level 5/11. *)
val ( ^^ ) :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('f, 'b, 'c, 'e, 'g, 'h) format6 ->
  ('a, 'b, 'c, 'd, 'g, 'h) format6
end
# 6 "v0.in.ml"


  open Pervasives

  module List : sig
# 1 "v0/list.mli"
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

(** List operations.

    Some functions are flagged as not tail-recursive.  A tail-recursive
    function uses constant stack space, while a non-tail-recursive function
    uses stack space proportional to the length of its list argument, which
    can be a problem with very long lists.  When the function takes several
    list arguments, an approximate formula giving stack usage (in some
    unspecified constant unit) is shown in parentheses.

    The above considerations can usually be ignored if your lists are not
    longer than about 10000 elements.
*)

(** Return the length (number of elements) of the given list. *)
val length : 'a list -> int

(** Compare the lengths of two lists. [compare_lengths l1 l2] is
    equivalent to [compare (length l1) (length l2)], except that
    the computation stops after itering on the shortest list.
    @since 4.05.0
*)
val compare_lengths : 'a list -> 'b list -> int

(** Compare the length of a list to an integer. [compare_length_with l n] is
    equivalent to [compare (length l) n], except that
    the computation stops after at most [n] iterations on the list.
    @since 4.05.0
*)
val compare_length_with : 'a list -> int -> int

(** [cons x xs] is [x :: xs]
    @since 4.03.0
*)
val cons : 'a -> 'a list -> 'a list

(** Return the first element of the given list. Raise
    [Failure "hd"] if the list is empty. *)
val hd : 'a list -> 'a

(** Return the given list without its first element. Raise
    [Failure "tl"] if the list is empty. *)
val tl : 'a list -> 'a list

(** Return the [n]-th element of the given list.
    The first element (head of the list) is at position 0.
    Return [None] if the list is too short.
    Raise [Invalid_argument "List.nth"] if [n] is negative.
    @since 4.05
*)
val nth_opt : 'a list -> int -> 'a option

(** List reversal. *)
val rev : 'a list -> 'a list

(** [List.init len f] is [f 0; f 1; ...; f (len-1)], evaluated left to right.

    @raise Invalid_argument if len < 0.
    @since 4.06.0
*)
val init : int -> (int -> 'a) -> 'a list

(** Concatenate two lists.  Same as the infix operator [@].
    Not tail-recursive (length of the first argument).  *)
val append : 'a list -> 'a list -> 'a list

(** [List.rev_append l1 l2] reverses [l1] and concatenates it to [l2].
    This is equivalent to {!List.rev}[ l1 @ l2], but [rev_append] is
    tail-recursive and more efficient. *)
val rev_append : 'a list -> 'a list -> 'a list

(** Concatenate a list of lists.  The elements of the argument are all
    concatenated together (in the same order) to give the result.
    Not tail-recursive
    (length of the argument + length of the longest sub-list). *)
val concat : 'a list list -> 'a list

(** An alias for [concat]. *)
val flatten : 'a list list -> 'a list

(** {1 Iterators} *)

(** [List.iter f [a1; ...; an]] applies function [f] in turn to
    [a1; ...; an]. It is equivalent to
    [begin f a1; f a2; ...; f an; () end]. *)
val iter : ('a -> unit) -> 'a list -> unit

(** Same as {!List.iter}, but the function is applied to the index of
    the element as first argument (counting from 0), and the element
    itself as second argument.
    @since 4.00.0
*)
val iteri : (int -> 'a -> unit) -> 'a list -> unit

(** [List.map f [a1; ...; an]] applies function [f] to [a1, ..., an],
    and builds the list [[f a1; ...; f an]]
    with the results returned by [f].  Not tail-recursive. *)
val map : ('a -> 'b) -> 'a list -> 'b list

(** Same as {!List.map}, but the function is applied to the index of
    the element as first argument (counting from 0), and the element
    itself as second argument.  Not tail-recursive.
    @since 4.00.0
*)
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

(** [List.rev_map f l] gives the same result as
    {!List.rev}[ (]{!List.map}[ f l)], but is tail-recursive and
    more efficient. *)
val rev_map : ('a -> 'b) -> 'a list -> 'b list

(** [List.fold_left f a [b1; ...; bn]] is
    [f (... (f (f a b1) b2) ...) bn]. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

(** [List.fold_right f [a1; ...; an] b] is
    [f a1 (f a2 (... (f an b) ...))].  Not tail-recursive. *)
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

(** {1 Iterators on two lists} *)

(** [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
    [f a1 b1; ...; f an bn].
    Raise [Invalid_argument] if the two lists are determined
    to have different lengths. *)
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(** [List.map2 f [a1; ...; an] [b1; ...; bn]] is
    [[f a1 b1; ...; f an bn]].
    Raise [Invalid_argument] if the two lists are determined
    to have different lengths.  Not tail-recursive. *)
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(** [List.rev_map2 f l1 l2] gives the same result as
    {!List.rev}[ (]{!List.map2}[ f l1 l2)], but is tail-recursive and
    more efficient. *)
val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(** [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
    [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
    Raise [Invalid_argument] if the two lists are determined
    to have different lengths. *)
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a

(** [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
    [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
    Raise [Invalid_argument] if the two lists are determined
    to have different lengths.  Not tail-recursive. *)
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c

(** {1 List scanning} *)

(** [for_all p [a1; ...; an]] checks if all elements of the list
    satisfy the predicate [p]. That is, it returns
    [(p a1) && (p a2) && ... && (p an)]. *)
val for_all : ('a -> bool) -> 'a list -> bool

(** [exists p [a1; ...; an]] checks if at least one element of
    the list satisfies the predicate [p]. That is, it returns
    [(p a1) || (p a2) || ... || (p an)]. *)
val exists : ('a -> bool) -> 'a list -> bool

(** Same as {!List.for_all}, but for a two-argument predicate.
    Raise [Invalid_argument] if the two lists are determined
    to have different lengths. *)
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

(** Same as {!List.exists}, but for a two-argument predicate.
    Raise [Invalid_argument] if the two lists are determined
    to have different lengths. *)
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

(** [mem a l] is true if and only if [a] is equal
    to an element of [l]. *)
val mem : 'a -> 'a list -> bool

(** Same as {!List.mem}, but uses physical equality instead of structural
    equality to compare list elements. *)
val memq : 'a -> 'a list -> bool

(** {1 List searching} *)

(** [find_opt p l] returns the first element of the list [l] that
    satisfies the predicate [p], or [None] if there is no value that
    satisfies [p] in the list [l].
    @since 4.05 *)
val find_opt : ('a -> bool) -> 'a list -> 'a option

(** [filter p l] returns all the elements of the list [l]
    that satisfy the predicate [p].  The order of the elements
    in the input list is preserved.  *)
val filter : ('a -> bool) -> 'a list -> 'a list

(** [find_all] is another name for {!List.filter}. *)
val find_all : ('a -> bool) -> 'a list -> 'a list

(** [partition p l] returns a pair of lists [(l1, l2)], where
    [l1] is the list of all the elements of [l] that
    satisfy the predicate [p], and [l2] is the list of all the
    elements of [l] that do not satisfy [p].
    The order of the elements in the input list is preserved. *)
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list

(** {1 Association lists} *)

(** [assoc_opt a l] returns the value associated with key [a] in the list of
    pairs [l]. That is,
    [assoc_opt a [ ...; (a,b); ...] = b]
    if [(a,b)] is the leftmost binding of [a] in list [l].
    Returns [None] if there is no value associated with [a] in the
    list [l].
    @since 4.05 *)
val assoc_opt : 'a -> ('a * 'b) list -> 'b option

(** Same as {!List.assoc_opt}, but uses physical equality instead of structural
    equality to compare keys.
    @since 4.05 *)
val assq_opt : 'a -> ('a * 'b) list -> 'b option

(** Same as {!List.assoc}, but simply return true if a binding exists,
    and false if no bindings exist for the given key. *)
val mem_assoc : 'a -> ('a * 'b) list -> bool

(** Same as {!List.mem_assoc}, but uses physical equality instead of
    structural equality to compare keys. *)
val mem_assq : 'a -> ('a * 'b) list -> bool

(** [remove_assoc a l] returns the list of
    pairs [l] without the first pair with key [a], if any.
    Not tail-recursive. *)
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list

(** Same as {!List.remove_assoc}, but uses physical equality instead
    of structural equality to compare keys.  Not tail-recursive. *)
val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list

(** {1 Lists of pairs} *)

(** Transform a list of pairs into a pair of lists:
    [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
    Not tail-recursive.
*)
val split : ('a * 'b) list -> 'a list * 'b list

(** Transform a pair of lists into a list of pairs:
    [combine [a1; ...; an] [b1; ...; bn]] is
    [[(a1,b1); ...; (an,bn)]].
    Raise [Invalid_argument] if the two lists
    have different lengths.  Not tail-recursive. *)
val combine : 'a list -> 'b list -> ('a * 'b) list

(** {1 Sorting} *)

(** Sort a list in increasing order according to a comparison
    function.  The comparison function must return 0 if its arguments
    compare as equal, a positive integer if the first is greater,
    and a negative integer if the first is smaller (see Array.sort for
    a complete specification).  For example,
    {!Stdlib.compare} is a suitable comparison function.
    The resulting list is sorted in increasing order.
    [List.sort] is guaranteed to run in constant heap space
    (in addition to the size of the result list) and logarithmic
    stack space.

    The current implementation uses Merge Sort. It runs in constant
    heap space and logarithmic stack space.
*)
val sort : ('a -> 'a -> int) -> 'a list -> 'a list

(** Same as {!List.sort}, but the sorting algorithm is guaranteed to
    be stable (i.e. elements that compare equal are kept in their
    original order) .

    The current implementation uses Merge Sort. It runs in constant
    heap space and logarithmic stack space.
*)
val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list

(** Same as {!List.sort} or {!List.stable_sort}, whichever is faster
    on typical input. *)
val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list

(** Same as {!List.sort}, but also remove duplicates.
    @since 4.02.0 *)
val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list

(** Merge two lists:
    Assuming that [l1] and [l2] are sorted according to the
    comparison function [cmp], [merge cmp l1 l2] will return a
    sorted list containing all the elements of [l1] and [l2].
    If several elements compare equal, the elements of [l1] will be
    before the elements of [l2].
    Not tail-recursive (sum of the lengths of the arguments).
*)
val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
end
# 10 "v0.in.ml"


  module String : sig
# 1 "v0/string.mli"
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

(* TEZOS CHANGES

   * Import version 4.06.1
   * Remove unsafe functions
   * Remove deprecated functions (enforcing string immutability)
   * Add binary data extraction functions
*)

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

    OCaml strings used to be modifiable in place, for instance via the
    {!String.set} and {!String.blit} functions described below. This
    usage is deprecated and only possible when the compiler is put in
    "unsafe-string" mode by giving the [-unsafe-string] command-line
    option (which is currently the default for reasons of backward
    compatibility). This is done by making the types [string] and
    [bytes] (see module {!Bytes}) interchangeable so that functions
    expecting byte sequences can also accept strings as arguments and
    modify them.

    All new code should avoid this feature and be compiled with the
    [-safe-string] command-line option to enforce the separation between
    the types [string] and [bytes].

*)

(** Return the length (number of characters) of the given string. *)
external length : string -> int = "%string_length"

(** [String.get s n] returns the character at index [n] in string [s].
    You can also write [s.[n]] instead of [String.get s n].

    Raise [Invalid_argument] if [n] not a valid index in [s]. *)
external get : string -> int -> char = "%string_safe_get"

(** [String.make n c] returns a fresh string of length [n],
    filled with the character [c].

    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)
val make : int -> char -> string

(** [String.init n f] returns a string of length [n], with character
    [i] initialized to the result of [f i] (called in increasing
    index order).

    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}.

    @since 4.02.0
*)
val init : int -> (int -> char) -> string

(** [String.sub s start len] returns a fresh string of length [len],
    containing the substring of [s] that starts at position [start] and
    has length [len].

    Raise [Invalid_argument] if [start] and [len] do not
    designate a valid substring of [s]. *)
val sub : string -> int -> int -> string

(** Same as {!Bytes.blit_string}. *)
val blit : string -> int -> bytes -> int -> int -> unit

(** [String.concat sep sl] concatenates the list of strings [sl],
    inserting the separator string [sep] between each.

    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_string_length} bytes. *)
val concat : string -> string list -> string

(** [String.iter f s] applies function [f] in turn to all
    the characters of [s].  It is equivalent to
    [f s.[0]; f s.[1]; ...; f s.[String.length s - 1]; ()]. *)
val iter : (char -> unit) -> string -> unit

(** Same as {!String.iter}, but the
    function is applied to the index of the element as first argument
    (counting from 0), and the character itself as second argument.
    @since 4.00.0 *)
val iteri : (int -> char -> unit) -> string -> unit

(** [String.map f s] applies function [f] in turn to all the
    characters of [s] (in increasing index order) and stores the
    results in a new string that is returned.
    @since 4.00.0 *)
val map : (char -> char) -> string -> string

(** [String.mapi f s] calls [f] with each character of [s] and its
    index (in increasing index order) and stores the results in a new
    string that is returned.
    @since 4.02.0 *)
val mapi : (int -> char -> char) -> string -> string

(** Return a copy of the argument, without leading and trailing
    whitespace.  The characters regarded as whitespace are: [' '],
    ['\012'], ['\n'], ['\r'], and ['\t'].  If there is neither leading nor
    trailing whitespace character in the argument, return the original
    string itself, not a copy.
    @since 4.00.0 *)
val trim : string -> string

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
val escaped : string -> string

(** [String.index_opt s c] returns the index of the first
    occurrence of character [c] in string [s], or
    [None] if [c] does not occur in [s].
    @since 4.05 *)
val index_opt : string -> char -> int option

(** [String.rindex_opt s c] returns the index of the last occurrence
    of character [c] in string [s], or [None] if [c] does not occur in
    [s].
    @since 4.05 *)
val rindex_opt : string -> char -> int option

(** [String.index_from_opt s i c] returns the index of the
    first occurrence of character [c] in string [s] after position [i]
    or [None] if [c] does not occur in [s] after position [i].

    [String.index_opt s c] is equivalent to [String.index_from_opt s 0 c].
    Raise [Invalid_argument] if [i] is not a valid position in [s].

    @since 4.05
*)
val index_from_opt : string -> int -> char -> int option

(** [String.rindex_from_opt s i c] returns the index of the
    last occurrence of character [c] in string [s] before position [i+1]
    or [None] if [c] does not occur in [s] before position [i+1].

    [String.rindex_opt s c] is equivalent to
    [String.rindex_from_opt s (String.length s - 1) c].

    Raise [Invalid_argument] if [i+1] is not a valid position in [s].

    @since 4.05
*)
val rindex_from_opt : string -> int -> char -> int option

(** [String.contains s c] tests if character [c]
    appears in the string [s]. *)
val contains : string -> char -> bool

(** [String.contains_from s start c] tests if character [c]
    appears in [s] after position [start].
    [String.contains s c] is equivalent to
    [String.contains_from s 0 c].

    Raise [Invalid_argument] if [start] is not a valid position in [s]. *)
val contains_from : string -> int -> char -> bool

(** [String.rcontains_from s stop c] tests if character [c]
    appears in [s] before position [stop+1].

    Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
    position in [s]. *)
val rcontains_from : string -> int -> char -> bool

(** Return a copy of the argument, with all lowercase letters
    translated to uppercase, using the US-ASCII character set.
    @since 4.03.0 *)
val uppercase_ascii : string -> string

(** Return a copy of the argument, with all uppercase letters
    translated to lowercase, using the US-ASCII character set.
    @since 4.03.0 *)
val lowercase_ascii : string -> string

(** Return a copy of the argument, with the first character set to uppercase,
    using the US-ASCII character set.
    @since 4.03.0 *)
val capitalize_ascii : string -> string

(** Return a copy of the argument, with the first character set to lowercase,
    using the US-ASCII character set.
    @since 4.03.0 *)
val uncapitalize_ascii : string -> string

(** An alias for the type of strings. *)
type t = string

(** The comparison function for strings, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [String] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)
val compare : t -> t -> int

(** The equal function for strings.
    @since 4.03.0 *)
val equal : t -> t -> bool

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
val split_on_char : char -> string -> string list

(** Functions reading bytes  *)

(** [get_char buff i] reads 1 byte at offset i as a char *)
val get_char : t -> int -> char

(** [get_uint8 buff i] reads 1 byte at offset i as an unsigned int of 8
    bits. i.e. It returns a value between 0 and 2^8-1 *)
val get_uint8 : t -> int -> int

(** [get_int8 buff i] reads 1 byte at offset i as a signed int of 8
    bits. i.e. It returns a value between -2^7 and 2^7-1 *)
val get_int8 : t -> int -> int

(** Functions reading according to Big Endian byte order *)

(** [get_uint16 buff i] reads 2 bytes at offset i as an unsigned int
      of 16 bits. i.e. It returns a value between 0 and 2^16-1 *)
val get_uint16 : t -> int -> int

(** [get_int16 buff i] reads 2 byte at offset i as a signed int of
      16 bits. i.e. It returns a value between -2^15 and 2^15-1 *)
val get_int16 : t -> int -> int

(** [get_int32 buff i] reads 4 bytes at offset i as an int32. *)
val get_int32 : t -> int -> int32

(** [get_int64 buff i] reads 8 bytes at offset i as an int64. *)
val get_int64 : t -> int -> int64

module LE : sig
  (** Functions reading according to Little Endian byte order *)

  (** [get_uint16 buff i] reads 2 bytes at offset i as an unsigned int
      of 16 bits. i.e. It returns a value between 0 and 2^16-1 *)
  val get_uint16 : t -> int -> int

  (** [get_int16 buff i] reads 2 byte at offset i as a signed int of
      16 bits. i.e. It returns a value between -2^15 and 2^15-1 *)
  val get_int16 : t -> int -> int

  (** [get_int32 buff i] reads 4 bytes at offset i as an int32. *)
  val get_int32 : t -> int -> int32

  (** [get_int64 buff i] reads 8 bytes at offset i as an int64. *)
  val get_int64 : t -> int -> int64
end
end
# 12 "v0.in.ml"


  module Int32 : sig
# 1 "v0/int32.mli"
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

(* TEZOS CHANGES

   * Import version 4.06.1
   * Remove deprecated functions
*)

(** 32-bit integers.

    This module provides operations on the type [int32]
    of signed 32-bit integers.  Unlike the built-in [int] type,
    the type [int32] is guaranteed to be exactly 32-bit wide on all
    platforms.  All arithmetic operations over [int32] are taken
    modulo 2{^32}.

    Performance notice: values of type [int32] occupy more memory
    space than values of type [int], and arithmetic operations on
    [int32] are generally slower than those on [int].  Use [int32]
    only when the application requires exact 32-bit arithmetic. *)

(** The 32-bit integer 0. *)
val zero : int32

(** The 32-bit integer 1. *)
val one : int32

(** The 32-bit integer -1. *)
val minus_one : int32

(** Unary negation. *)
external neg : int32 -> int32 = "%int32_neg"

(** Addition. *)
external add : int32 -> int32 -> int32 = "%int32_add"

(** Subtraction. *)
external sub : int32 -> int32 -> int32 = "%int32_sub"

(** Multiplication. *)
external mul : int32 -> int32 -> int32 = "%int32_mul"

(** Integer division.  Raise [Division_by_zero] if the second
    argument is zero.  This division rounds the real quotient of
    its arguments towards zero, as specified for {!Pervasives.(/)}. *)
external div : int32 -> int32 -> int32 = "%int32_div"

(** Integer remainder.  If [y] is not zero, the result
    of [Int32.rem x y] satisfies the following property:
    [x = Int32.add (Int32.mul (Int32.div x y) y) (Int32.rem x y)].
    If [y = 0], [Int32.rem x y] raises [Division_by_zero]. *)
external rem : int32 -> int32 -> int32 = "%int32_mod"

(** Successor.  [Int32.succ x] is [Int32.add x Int32.one]. *)
val succ : int32 -> int32

(** Predecessor.  [Int32.pred x] is [Int32.sub x Int32.one]. *)
val pred : int32 -> int32

(** Return the absolute value of its argument. *)
val abs : int32 -> int32

(** The greatest representable 32-bit integer, 2{^31} - 1. *)
val max_int : int32

(** The smallest representable 32-bit integer, -2{^31}. *)
val min_int : int32

(** Bitwise logical and. *)
external logand : int32 -> int32 -> int32 = "%int32_and"

(** Bitwise logical or. *)
external logor : int32 -> int32 -> int32 = "%int32_or"

(** Bitwise logical exclusive or. *)
external logxor : int32 -> int32 -> int32 = "%int32_xor"

(** Bitwise logical negation. *)
val lognot : int32 -> int32

(** [Int32.shift_left x y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= 32]. *)
external shift_left : int32 -> int -> int32 = "%int32_lsl"

(** [Int32.shift_right x y] shifts [x] to the right by [y] bits.
    This is an arithmetic shift: the sign bit of [x] is replicated
    and inserted in the vacated bits.
    The result is unspecified if [y < 0] or [y >= 32]. *)
external shift_right : int32 -> int -> int32 = "%int32_asr"

(** [Int32.shift_right_logical x y] shifts [x] to the right by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= 32]. *)
external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"

(** Convert the given integer (type [int]) to a 32-bit integer
    (type [int32]). *)
external of_int : int -> int32 = "%int32_of_int"

(** Convert the given 32-bit integer (type [int32]) to an
    integer (type [int]).  On 32-bit platforms, the 32-bit integer
    is taken modulo 2{^31}, i.e. the high-order bit is lost
    during the conversion.  On 64-bit platforms, the conversion
    is exact. *)
external to_int : int32 -> int = "%int32_to_int"

(** Convert the given floating-point number to a 32-bit integer,
    discarding the fractional part (truncate towards 0).
    The result of the conversion is undefined if, after truncation,
    the number is outside the range \[{!Int32.min_int}, {!Int32.max_int}\]. *)
external of_float : float -> int32
  = "caml_int32_of_float" "caml_int32_of_float_unboxed"
  [@@unboxed] [@@noalloc]

(** Convert the given 32-bit integer to a floating-point number. *)
external to_float : int32 -> float
  = "caml_int32_to_float" "caml_int32_to_float_unboxed"
  [@@unboxed] [@@noalloc]

(** Convert the given string to a 32-bit integer.
    The string is read in decimal (by default, or if the string
    begins with [0u]) or in hexadecimal, octal or binary if the
    string begins with [0x], [0o] or [0b] respectively.

    The [0u] prefix reads the input as an unsigned integer in the range
    [[0, 2*Int32.max_int+1]].  If the input exceeds {!Int32.max_int}
    it is converted to the signed integer
    [Int32.min_int + input - Int32.max_int - 1].

    The [_] (underscore) character can appear anywhere in the string
    and is ignored.
    Raise [Failure "Int32.of_string"] if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [int32]. *)
external of_string : string -> int32 = "caml_int32_of_string"

(** Same as [of_string], but return [None] instead of raising.
    @since 4.05 *)
val of_string_opt : string -> int32 option

(** Return the string representation of its argument, in signed decimal. *)
val to_string : int32 -> string

(** Return the internal representation of the given float according
    to the IEEE 754 floating-point 'single format' bit layout.
    Bit 31 of the result represents the sign of the float;
    bits 30 to 23 represent the (biased) exponent; bits 22 to 0
    represent the mantissa. *)
external bits_of_float : float -> int32
  = "caml_int32_bits_of_float" "caml_int32_bits_of_float_unboxed"
  [@@unboxed] [@@noalloc]

(** Return the floating-point number whose internal representation,
    according to the IEEE 754 floating-point 'single format' bit layout,
    is the given [int32]. *)
external float_of_bits : int32 -> float
  = "caml_int32_float_of_bits" "caml_int32_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

(** An alias for the type of 32-bit integers. *)
type t = int32

(** The comparison function for 32-bit integers, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Int32] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)
val compare : t -> t -> int

(** The equal function for int32s.
    @since 4.03.0 *)
val equal : t -> t -> bool
end
# 14 "v0.in.ml"


  module Int64 : sig
# 1 "v0/int64.mli"
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

(* TEZOS CHANGES

   * Import version 4.06.1
   * Remove deprecated functions
*)
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
*)

(** The 64-bit integer 0. *)
val zero : int64

(** The 64-bit integer 1. *)
val one : int64

(** The 64-bit integer -1. *)
val minus_one : int64

(** Unary negation. *)
external neg : int64 -> int64 = "%int64_neg"

(** Addition. *)
external add : int64 -> int64 -> int64 = "%int64_add"

(** Subtraction. *)
external sub : int64 -> int64 -> int64 = "%int64_sub"

(** Multiplication. *)
external mul : int64 -> int64 -> int64 = "%int64_mul"

(** Integer division.  Raise [Division_by_zero] if the second
    argument is zero.  This division rounds the real quotient of
    its arguments towards zero, as specified for {!Pervasives.(/)}. *)
external div : int64 -> int64 -> int64 = "%int64_div"

(** Integer remainder.  If [y] is not zero, the result
    of [Int64.rem x y] satisfies the following property:
    [x = Int64.add (Int64.mul (Int64.div x y) y) (Int64.rem x y)].
    If [y = 0], [Int64.rem x y] raises [Division_by_zero]. *)
external rem : int64 -> int64 -> int64 = "%int64_mod"

(** Successor.  [Int64.succ x] is [Int64.add x Int64.one]. *)
val succ : int64 -> int64

(** Predecessor.  [Int64.pred x] is [Int64.sub x Int64.one]. *)
val pred : int64 -> int64

(** Return the absolute value of its argument. *)
val abs : int64 -> int64

(** The greatest representable 64-bit integer, 2{^63} - 1. *)
val max_int : int64

(** The smallest representable 64-bit integer, -2{^63}. *)
val min_int : int64

(** Bitwise logical and. *)
external logand : int64 -> int64 -> int64 = "%int64_and"

(** Bitwise logical or. *)
external logor : int64 -> int64 -> int64 = "%int64_or"

(** Bitwise logical exclusive or. *)
external logxor : int64 -> int64 -> int64 = "%int64_xor"

(** Bitwise logical negation. *)
val lognot : int64 -> int64

(** [Int64.shift_left x y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= 64]. *)
external shift_left : int64 -> int -> int64 = "%int64_lsl"

(** [Int64.shift_right x y] shifts [x] to the right by [y] bits.
    This is an arithmetic shift: the sign bit of [x] is replicated
    and inserted in the vacated bits.
    The result is unspecified if [y < 0] or [y >= 64]. *)
external shift_right : int64 -> int -> int64 = "%int64_asr"

(** [Int64.shift_right_logical x y] shifts [x] to the right by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= 64]. *)
external shift_right_logical : int64 -> int -> int64 = "%int64_lsr"

(** Convert the given integer (type [int]) to a 64-bit integer
    (type [int64]). *)
external of_int : int -> int64 = "%int64_of_int"

(** Convert the given 64-bit integer (type [int64]) to an
    integer (type [int]).  On 64-bit platforms, the 64-bit integer
    is taken modulo 2{^63}, i.e. the high-order bit is lost
    during the conversion.  On 32-bit platforms, the 64-bit integer
    is taken modulo 2{^31}, i.e. the top 33 bits are lost
    during the conversion. *)
external to_int : int64 -> int = "%int64_to_int"

(** Convert the given floating-point number to a 64-bit integer,
    discarding the fractional part (truncate towards 0).
    The result of the conversion is undefined if, after truncation,
    the number is outside the range \[{!Int64.min_int}, {!Int64.max_int}\]. *)
external of_float : float -> int64
  = "caml_int64_of_float" "caml_int64_of_float_unboxed"
  [@@unboxed] [@@noalloc]

(** Convert the given 64-bit integer to a floating-point number. *)
external to_float : int64 -> float
  = "caml_int64_to_float" "caml_int64_to_float_unboxed"
  [@@unboxed] [@@noalloc]

(** Convert the given 32-bit integer (type [int32])
    to a 64-bit integer (type [int64]). *)
external of_int32 : int32 -> int64 = "%int64_of_int32"

(** Convert the given 64-bit integer (type [int64]) to a
    32-bit integer (type [int32]). The 64-bit integer
    is taken modulo 2{^32}, i.e. the top 32 bits are lost
    during the conversion.  *)
external to_int32 : int64 -> int32 = "%int64_to_int32"

(** Convert the given native integer (type [nativeint])
    to a 64-bit integer (type [int64]). *)
external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"

(** Convert the given 64-bit integer (type [int64]) to a
    native integer.  On 32-bit platforms, the 64-bit integer
    is taken modulo 2{^32}.  On 64-bit platforms,
    the conversion is exact. *)
external to_nativeint : int64 -> nativeint = "%int64_to_nativeint"

(** Convert the given string to a 64-bit integer.
    The string is read in decimal (by default, or if the string
    begins with [0u]) or in hexadecimal, octal or binary if the
    string begins with [0x], [0o] or [0b] respectively.

    The [0u] prefix reads the input as an unsigned integer in the range
    [[0, 2*Int64.max_int+1]].  If the input exceeds {!Int64.max_int}
    it is converted to the signed integer
    [Int64.min_int + input - Int64.max_int - 1].

    The [_] (underscore) character can appear anywhere in the string
    and is ignored.
    Raise [Failure "Int64.of_string"] if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [int64]. *)
external of_string : string -> int64 = "caml_int64_of_string"

(** Same as [of_string], but return [None] instead of raising.
    @since 4.05 *)
val of_string_opt : string -> int64 option

(** Return the string representation of its argument, in decimal. *)
val to_string : int64 -> string

(** Return the internal representation of the given float according
    to the IEEE 754 floating-point 'double format' bit layout.
    Bit 63 of the result represents the sign of the float;
    bits 62 to 52 represent the (biased) exponent; bits 51 to 0
    represent the mantissa. *)
external bits_of_float : float -> int64
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
  [@@unboxed] [@@noalloc]

(** Return the floating-point number whose internal representation,
    according to the IEEE 754 floating-point 'double format' bit layout,
    is the given [int64]. *)
external float_of_bits : int64 -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

(** An alias for the type of 64-bit integers. *)
type t = int64

(** The comparison function for 64-bit integers, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Int64] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)
val compare : t -> t -> int

(** The equal function for int64s.
    @since 4.03.0 *)
val equal : t -> t -> bool
end
# 16 "v0.in.ml"


  module Format : sig
# 1 "v0/format.mli"
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

(* TEZOS CHANGES

   * Import version 4.06.1
   * Remove channel functions
   * Remove toplevel effect based functions
   * Remove deprecated functions
   * Remove redirecting the standard formatter output
   * Remove redefining formatter output and output functions
   * Remove redefining semantic tag operations (too complex and
     imperative for the need of error message generation)
   * Remove defining formatters and symbolic pretty printing
*)

(** Pretty-printing.

    This module implements a pretty-printing facility to format values
    within {{!boxes}'pretty-printing boxes'}
    combined with a set of {{!fpp}printf-like functions}.
    The pretty-printer splits lines at specified {{!breaks}break hints},
    and indents lines according to the box structure.

    This pretty-printing facility is implemented as an overlay on top of
    abstract {{!section:formatter}formatters} which provide basic output
    functions.
    Some formatters are predefined, notably:
    - {!std_formatter} outputs to {{!Pervasives.stdout}stdout}
    - {!err_formatter} outputs to {{!Pervasives.stderr}stderr}

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
    calls to {!Pervasives} low level output functions is error prone.

    The pretty-printing functions output material that is delayed in the
    pretty-printer queue and stacks in order to compute proper line
    splitting. In contrast, basic I/O output functions write directly in
    their output device. As a consequence, the output of a basic I/O function
    may appear before the output of a pretty-printing function that has been
    called before. For instance,
    [
    Pervasives.print_string "<";
    Format.print_string "PRETTY";
    Pervasives.print_string ">";
    Format.print_string "TEXT";
    ]
    leads to output [<>PRETTYTEXT].

*)

(** Abstract data corresponding to a pretty-printer (also called a
    formatter) and all its machinery. See also {!section:formatter}. *)
type formatter

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
val pp_open_box : formatter -> int -> unit

(** Closes the most recently open pretty-printing box. *)
val pp_close_box : formatter -> unit -> unit

(** [pp_open_hbox ppf ()] opens a new 'horizontal' pretty-printing box.

    This box prints material on a single line.

    Break hints in a horizontal box never split the line.
    (Line splitting may still occur inside boxes nested deeper).
*)
val pp_open_hbox : formatter -> unit -> unit

(** [pp_open_vbox ppf d] opens a new 'vertical' pretty-printing box
    with offset [d].

    This box prints material on as many lines as break hints in the box.

    Every break hint in a vertical box splits the line.

    If the pretty-printer splits the line in the box, [d] is added to the
    current indentation.
*)
val pp_open_vbox : formatter -> int -> unit

(** [pp_open_hvbox ppf d] opens a new 'horizontal/vertical' pretty-printing box
    with offset [d].

    This box behaves as an horizontal box if it fits on a single line,
    otherwise it behaves as a vertical box.

    If the pretty-printer splits the line in the box, [d] is added to the
    current indentation.
*)
val pp_open_hvbox : formatter -> int -> unit

(** [pp_open_hovbox ppf d] opens a new 'horizontal-or-vertical'
    pretty-printing box with offset [d].

    This box prints material as much as possible on every line.

    A break hint splits the line if there is no more room on the line to
    print the remainder of the box.

    If the pretty-printer splits the line in the box, [d] is added to the
    current indentation.
*)
val pp_open_hovbox : formatter -> int -> unit

(** {1 Formatting functions} *)

(** [pp_print_string ppf s] prints [s] in the current pretty-printing box. *)
val pp_print_string : formatter -> string -> unit

(** [pp_print_as ppf len s] prints [s] in the current pretty-printing box.
    The pretty-printer formats [s] as if it were of length [len].
*)
val pp_print_as : formatter -> int -> string -> unit

(** Print an integer in the current pretty-printing box. *)
val pp_print_int : formatter -> int -> unit

(** Print a floating point number in the current pretty-printing box. *)
val pp_print_float : formatter -> float -> unit

(** Print a character in the current pretty-printing box. *)
val pp_print_char : formatter -> char -> unit

(** Print a boolean in the current pretty-printing box. *)
val pp_print_bool : formatter -> bool -> unit

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

(** [pp_print_space ppf ()] emits a 'space' break hint:
    the pretty-printer may split the line at this point,
    otherwise it prints one space.

    [pp_print_space ppf ()] is equivalent to [pp_print_break ppf 1 0].
*)
val pp_print_space : formatter -> unit -> unit

(** [pp_print_cut ppf ()] emits a 'cut' break hint:
    the pretty-printer may split the line at this point,
    otherwise it prints nothing.

    [pp_print_cut ppf ()] is equivalent to [pp_print_break ppf 0 0].
*)
val pp_print_cut : formatter -> unit -> unit

(** [pp_print_break ppf nspaces offset] emits a 'full' break hint:
    the pretty-printer may split the line at this point,
    otherwise it prints [nspaces] spaces.

    If the pretty-printer splits the line, [offset] is added to
    the current indentation.
*)
val pp_print_break : formatter -> int -> int -> unit

(** Force a new line in the current pretty-printing box.

    The pretty-printer must split the line at this point,

    Not the normal way of pretty-printing, since imperative line splitting may
    interfere with current line counters and box size calculation.
    Using break hints within an enclosing vertical box is a better
    alternative.
*)
val pp_force_newline : formatter -> unit -> unit

(** Execute the next formatting command if the preceding line
    has just been split. Otherwise, ignore the next formatting
    command.
*)
val pp_print_if_newline : formatter -> unit -> unit

(** {1 Pretty-printing termination} *)

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
    repeated calls to [print_flush] means repeated calls to {!Pervasives.flush}
    to flush the out channel; these explicit flush calls could foil the
    buffering strategy of output channels and could dramatically impact
    efficiency.
*)
val pp_print_flush : formatter -> unit -> unit

(** End of pretty-printing: resets the pretty-printer to initial state.

    All open pretty-printing boxes are closed, all pending text is printed.

    Equivalent to {!print_flush} followed by a new line.
    See corresponding words of caution for {!print_flush}.

    Note: this is not the normal way to output a new line;
    the preferred method is using break hints within a vertical pretty-printing
    box.
*)
val pp_print_newline : formatter -> unit -> unit

(** {1 Margin} *)

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
*)
val pp_set_margin : formatter -> int -> unit

(** Returns the position of the right margin. *)
val pp_get_margin : formatter -> unit -> int

(** {1 Maximum indentation limit} *)

(** [pp_set_max_indent ppf d] sets the maximum indentation limit of lines
    to [d] (in characters):
    once this limit is reached, new pretty-printing boxes are rejected to the
    left, if they do not fit on the current line.

    Nothing happens if [d] is smaller than 2.
    If [d] is too large, the limit is set to the maximum
    admissible value (which is greater than [10 ^ 9]).

    If [d] is greater or equal than the current margin, it is ignored,
    and the current maximum indentation limit is kept.
*)
val pp_set_max_indent : formatter -> int -> unit

(** Return the maximum indentation limit (in characters). *)
val pp_get_max_indent : formatter -> unit -> int

(** {1 Maximum formatting depth} *)

(** The maximum formatting depth is the maximum number of pretty-printing
    boxes simultaneously open.

    Material inside boxes nested deeper is printed as an ellipsis (more
    precisely as the text returned by {!get_ellipsis_text} [()]).
*)

(** [pp_set_max_boxes ppf max] sets the maximum number of pretty-printing
    boxes simultaneously open.

    Material inside boxes nested deeper is printed as an ellipsis (more
    precisely as the text returned by {!get_ellipsis_text} [()]).

    Nothing happens if [max] is smaller than 2.
*)
val pp_set_max_boxes : formatter -> int -> unit

(** Returns the maximum number of pretty-printing boxes allowed before
    ellipsis.
*)
val pp_get_max_boxes : formatter -> unit -> int

(** Tests if the maximum number of pretty-printing boxes allowed have already
    been opened.
*)
val pp_over_max_boxes : formatter -> unit -> bool

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

(** [open_tbox ()] opens a new tabulation box.

    This box prints lines separated into cells of fixed width.

    Inside a tabulation box, special {e tabulation markers} defines points of
    interest on the line (for instance to delimit cell boundaries).
    Function {!Format.set_tab} sets a tabulation marker at insertion point.

    A tabulation box features specific {e tabulation breaks} to move to next
    tabulation marker or split the line. Function {!Format.print_tbreak} prints
    a tabulation break.
*)
val pp_open_tbox : formatter -> unit -> unit

(** Closes the most recently opened tabulation box. *)
val pp_close_tbox : formatter -> unit -> unit

(** Sets a tabulation marker at current insertion point. *)
val pp_set_tab : formatter -> unit -> unit

(** [print_tab ()] emits a 'next' tabulation break hint: if not already set on
    a tabulation marker, the insertion point moves to the first tabulation
    marker on the right, or the pretty-printer splits the line and insertion
    point moves to the leftmost tabulation marker.

    It is equivalent to [print_tbreak 0 0]. *)
val pp_print_tab : formatter -> unit -> unit

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
val pp_print_tbreak : formatter -> int -> int -> unit

(** {1 Ellipsis} *)

(** Set the text of the ellipsis printed when too many pretty-printing boxes
    are open (a single dot, [.], by default).
*)
val pp_set_ellipsis_text : formatter -> string -> unit

(** Return the text of the ellipsis. *)
val pp_get_ellipsis_text : formatter -> unit -> string

(** {1 Convenience formatting functions.} *)

(** [pp_print_list ?pp_sep pp_v ppf l] prints items of list [l],
    using [pp_v] to print each item, and calling [pp_sep]
    between items ([pp_sep] defaults to {!pp_print_cut}.
    Does nothing on empty lists.

    @since 4.02.0
*)
val pp_print_list :
  ?pp_sep:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) ->
  formatter ->
  'a list ->
  unit

(** [pp_print_text ppf s] prints [s] with spaces and newlines respectively
    printed using {!pp_print_space} and {!pp_force_newline}.

    @since 4.02.0
*)
val pp_print_text : formatter -> string -> unit

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
val sprintf : ('a, unit, string) format -> 'a

(** Same as [printf] above, but instead of printing on a formatter,
    returns a string containing the result of formatting the arguments.
    The type of [asprintf] is general enough to interact nicely with [%a]
    conversions.

    @since 4.01.0
*)
val asprintf : ('a, formatter, unit, string) format4 -> 'a

(** Same as [fprintf] above, but does not print anything.
    Useful to ignore some material when conditionally printing.

    @since 3.10.0
*)
val ifprintf : formatter -> ('a, formatter, unit) format -> 'a

(** Formatted Pretty-Printing with continuations. *)

(** Same as [fprintf] above, but instead of returning immediately,
    passes the formatter to its first argument at the end of printing. *)
val kfprintf :
  (formatter -> 'a) -> formatter -> ('b, formatter, unit, 'a) format4 -> 'b

(** Same as [kfprintf] above, but does not print anything.
    Useful to ignore some material when conditionally printing.

    @since 3.12.0
*)
val ikfprintf :
  (formatter -> 'a) -> formatter -> ('b, formatter, unit, 'a) format4 -> 'b

(** Same as [sprintf] above, but instead of returning the string,
    passes it to the first argument. *)
val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b

(** Same as [asprintf] above, but instead of returning the string,
    passes it to the first argument.

    @since 4.03
*)
val kasprintf : (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b
end
# 18 "v0.in.ml"


  module MBytes : sig
# 1 "v0/mBytes.mli"
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

val create : int -> t

val length : t -> int

val copy : t -> t

(** [sub src ofs len] extract a sub-array of [src] starting at [ofs]
    and of length [len]. No copying of elements is involved: the
    sub-array and the original array share the same storage space. *)
val sub : t -> int -> int -> t

(** [blit src ofs_src dst ofs_dst len] copy [len] bytes from [src]
    starting at [ofs_src] into [dst] starting at [ofs_dst]. *)
val blit : t -> int -> t -> int -> int -> unit

(** See [blit] *)
val blit_of_string : string -> int -> t -> int -> int -> unit

(** See [blit] *)
val blit_to_bytes : t -> int -> bytes -> int -> int -> unit

(** [of_string s] create an byte array filled with the same content than [s]. *)
val of_string : string -> t

(** [to_string b] dump the array content in a [string]. *)
val to_string : t -> string

(** [sub_string b ofs len] is equivalent to [to_string (sub b ofs len)]. *)
val sub_string : t -> int -> int -> string

(** Functions reading and writing bytes  *)

(** [get_char buff i] reads 1 byte at offset i as a char *)
val get_char : t -> int -> char

(** [get_uint8 buff i] reads 1 byte at offset i as an unsigned int of 8
    bits. i.e. It returns a value between 0 and 2^8-1 *)
val get_uint8 : t -> int -> int

(** [get_int8 buff i] reads 1 byte at offset i as a signed int of 8
    bits. i.e. It returns a value between -2^7 and 2^7-1 *)
val get_int8 : t -> int -> int

(** [set_char buff i v] writes [v] to [buff] at offset [i] *)
val set_char : t -> int -> char -> unit

(** [set_int8 buff i v] writes the least significant 8 bits of [v]
    to [buff] at offset [i] *)
val set_int8 : t -> int -> int -> unit

(** Functions reading according to Big Endian byte order *)

(** [get_uint16 buff i] reads 2 bytes at offset i as an unsigned int
      of 16 bits. i.e. It returns a value between 0 and 2^16-1 *)
val get_uint16 : t -> int -> int

(** [get_int16 buff i] reads 2 byte at offset i as a signed int of
      16 bits. i.e. It returns a value between -2^15 and 2^15-1 *)
val get_int16 : t -> int -> int

(** [get_int32 buff i] reads 4 bytes at offset i as an int32. *)
val get_int32 : t -> int -> int32

(** [get_int64 buff i] reads 8 bytes at offset i as an int64. *)
val get_int64 : t -> int -> int64

(** [set_int16 buff i v] writes the least significant 16 bits of [v]
      to [buff] at offset [i] *)
val set_int16 : t -> int -> int -> unit

(** [set_int32 buff i v] writes [v] to [buff] at offset [i] *)
val set_int32 : t -> int -> int32 -> unit

(** [set_int64 buff i v] writes [v] to [buff] at offset [i] *)
val set_int64 : t -> int -> int64 -> unit

module LE : sig
  (** Functions reading according to Little Endian byte order *)

  (** [get_uint16 buff i] reads 2 bytes at offset i as an unsigned int
      of 16 bits. i.e. It returns a value between 0 and 2^16-1 *)
  val get_uint16 : t -> int -> int

  (** [get_int16 buff i] reads 2 byte at offset i as a signed int of
      16 bits. i.e. It returns a value between -2^15 and 2^15-1 *)
  val get_int16 : t -> int -> int

  (** [get_int32 buff i] reads 4 bytes at offset i as an int32. *)
  val get_int32 : t -> int -> int32

  (** [get_int64 buff i] reads 8 bytes at offset i as an int64. *)
  val get_int64 : t -> int -> int64

  (** [set_int16 buff i v] writes the least significant 16 bits of [v]
      to [buff] at offset [i] *)
  val set_int16 : t -> int -> int -> unit

  (** [set_int32 buff i v] writes [v] to [buff] at offset [i] *)
  val set_int32 : t -> int -> int32 -> unit

  (** [set_int64 buff i v] writes [v] to [buff] at offset [i] *)
  val set_int64 : t -> int -> int64 -> unit
end

val ( = ) : t -> t -> bool

val ( <> ) : t -> t -> bool

val ( < ) : t -> t -> bool

val ( <= ) : t -> t -> bool

val ( >= ) : t -> t -> bool

val ( > ) : t -> t -> bool

val compare : t -> t -> int

val concat : string -> t list -> t

val to_hex : t -> [`Hex of string]

val of_hex : [`Hex of string] -> t
end
# 20 "v0.in.ml"


  module Z : sig
# 1 "v0/z.mli"
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

(** Tezos Protocol Environment - Arbitrary precision arithmetic. *)

type t

val zero : t

val one : t

(** Returns its argument plus one. *)
val succ : t -> t

(** Absolute value. *)
val abs : t -> t

(** Unary negation. *)
val neg : t -> t

(** Addition. *)
val add : t -> t -> t

(** Subtraction. *)
val sub : t -> t -> t

(** Multiplication. *)
val mul : t -> t -> t

(** Euclidean division and remainder.  [ediv_rem a b] returns a pair [(q, r)]
    such that [a = b * q + r] and [0 <= r < |b|].
    Raises [Division_by_zero] if [b = 0].
*)
val ediv_rem : t -> t -> t * t

(** Bitwise logical and. *)
val logand : t -> t -> t

(** Bitwise logical or. *)
val logor : t -> t -> t

(** Bitwise logical exclusive or. *)
val logxor : t -> t -> t

(** Bitwise logical negation.
    The identity [lognot a]=[-a-1] always hold.
*)
val lognot : t -> t

(** Shifts to the left.
    Equivalent to a multiplication by a power of 2.
    The second argument must be non-negative.
*)
val shift_left : t -> int -> t

(** Shifts to the right.
    This is an arithmetic shift,
    equivalent to a division by a power of 2 with rounding towards -oo.
    The second argument must be non-negative.
*)
val shift_right : t -> int -> t

(** Gives a human-readable, decimal string representation of the argument. *)
val to_string : t -> string

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
val of_string : string -> t

(** Converts to a 64-bit integer. May raise [Overflow]. *)
val to_int64 : t -> int64

(** Converts from a 64-bit integer. *)
val of_int64 : int64 -> t

(** Converts to a base integer. May raise an [Overflow]. *)
val to_int : t -> int

(** Converts from a base integer. *)
val of_int : int -> t

val to_bits : ?pad_to:int -> t -> MBytes.t

val of_bits : MBytes.t -> t

val equal : t -> t -> bool

val compare : t -> t -> int

(** Returns the number of significant bits in the given number.
    If [x] is zero, [numbits x] returns 0.  Otherwise,
    [numbits x] returns a positive integer [n] such that
    [2^{n-1} <= |x| < 2^n].  Note that [numbits] is defined
    for negative arguments, and that [numbits (-x) = numbits x]. *)
val numbits : t -> int
end
# 22 "v0.in.ml"


  module Lwt : sig
# 1 "v0/lwt.mli"
(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Interface Lwt
 * Copyright (C) 2005-2008 J�r�me Vouillon
 * Laboratoire PPS - CNRS Universit� Paris Diderot
 *               2009-2012 J�r�mie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(* TEZOS CHANGES

   * import version 2.4.5
   * Comment a few function that shouldn't be used in the protocol:
     * choose: scheduling may be system dependent.
     * wait/wakeup
     * state
     * cancel
     * pause
     * async
     * thread storage
     * lwt exceptions
*)

(** Module [Lwt]: cooperative light-weight threads. *)

(** This module defines {e cooperative light-weight threads} with
    their primitives. A {e light-weight thread} represent a
    computation that may be not terminated, for example because it is
    waiting for some event to happen.

    Lwt threads are cooperative in the sense that switching to another
    thread is always explicit (with {!wakeup} or {!wakeup_exn}). When a
    thread is running, it executes as much as possible, and then
    returns (a value or an error) or sleeps.

    Note that inside a Lwt thread, exceptions must be raised with
    {!fail} instead of [raise]. Also the [try ... with ...]
    construction will not catch Lwt errors. You must use {!catch}
    instead. You can also use {!wrap} for functions that may raise
    normal exception.

    Lwt also provides the syntax extension {!Pa_lwt} to make code
    using Lwt more readable.
*)

(** {2 Definitions and basics} *)

(** The type of threads returning a result of type ['a]. *)
type +'a t

(** [return e] is a thread whose return value is the value of the
    expression [e]. *)
val return : 'a -> 'a t

(* val fail : exn -> 'a t *)
(*   (\** [fail e] is a thread that fails with the exception [e]. *\) *)

(** [bind t f] is a thread which first waits for the thread [t] to
    terminate and then, if the thread succeeds, behaves as the
    application of function [f] to the return value of [t].  If the
    thread [t] fails, [bind t f] also fails, with the same
    exception.

    The expression [bind t (fun x -> t')] can intuitively be read as
    [let x = t in t'], and if you use the {e lwt.syntax} syntax
    extension, you can write a bind operation like that: [lwt x = t in t'].

    Note that [bind] is also often used just for synchronization
    purpose: [t'] will not execute before [t] is terminated.

    The result of a thread can be bound several time. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** [t >>= f] is an alternative notation for [bind t f]. *)
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

(** [f =<< t] is [t >>= f] *)
val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t

(** [map f m] map the result of a thread. This is the same as [bind
    m (fun x -> return (f x))] *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [m >|= f] is [map f m] *)
val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

(** [f =|< m] is [map f m] *)
val ( =|< ) : ('a -> 'b) -> 'a t -> 'b t

(** {3 Pre-allocated threads} *)

(** [return_unit = return ()] *)
val return_unit : unit t

(** [return_none = return None] *)
val return_none : 'a option t

(** [return_nil = return \[\]] *)
val return_nil : 'a list t

(** [return_true = return true] *)
val return_true : bool t

(** [return_false = return false] *)
val return_false : bool t

(* (\** {2 Thread storage} *\) *)

(* type 'a key *)
(*   (\** Type of a key. Keys are used to store local values into *)
(*       threads *\) *)

(* val new_key : unit -> 'a key *)
(*   (\** [new_key ()] creates a new key. *\) *)

(* val get : 'a key -> 'a option *)
(*   (\** [get key] returns the value associated with [key] in the current *)
(*       thread. *\) *)

(* val with_value : 'a key -> 'a option -> (unit -> 'b) -> 'b *)
(*   (\** [with_value key value f] executes [f] with [value] associated to *)
(*       [key]. The previous value associated to [key] is restored after *)
(*       [f] terminates. *\) *)

(* (\** {2 Exceptions handling} *\) *)

(* val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t *)
(*   (\** [catch t f] is a thread that behaves as the thread [t ()] if *)
(*       this thread succeeds.  If the thread [t ()] fails with some *)
(*       exception, [catch t f] behaves as the application of [f] to this *)
(*       exception. *\) *)

(* val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t *)
(*   (\** [try_bind t f g] behaves as [bind (t ()) f] if [t] does not *)
(*       fail.  Otherwise, it behaves as the application of [g] to the *)
(*       exception associated to [t ()]. *\) *)

(* val finalize : (unit -> 'a t) -> (unit -> unit t) -> 'a t *)
(*   (\** [finalize f g] returns the same result as [f ()] whether it *)
(*       fails or not. In both cases, [g ()] is executed after [f]. *\) *)

(* val wrap : (unit -> 'a) -> 'a t *)
(*   (\** [wrap f] calls [f] and transform the result into a monad. If [f] *)
(*       raise an exception, it is caught by Lwt. *)

(*       This is actually the same as: *)

(*       {[ *)
(*         try *)
(*           return (f ()) *)
(*         with exn -> *)
(*           fail exn *)
(*       ]} *)
(*   *\) *)

(* val wrap1 : ('a -> 'b) -> 'a -> 'b t *)
(*   (\** [wrap1 f x] applies [f] on [x] and returns the result as a *)
(*       thread. If the application of [f] to [x] raise an exception it *)
(*       is caught and a thread is returned. *)

(*       Note that you must use {!wrap} instead of {!wrap1} if the *)
(*       evaluation of [x] may raise an exception. *)

(*       for example the following code is not ok: *)

(*       {[ *)
(*         wrap1 f (Hashtbl.find table key) *)
(*       ]} *)

(*       you should write instead: *)

(*       {[ *)
(*         wrap (fun () -> f (Hashtbl.find table key)) *)
(*       ]} *)
(*   *\) *)

(* val wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c t *)
(* val wrap3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd t *)
(* val wrap4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e t *)
(* val wrap5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f t *)
(* val wrap6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g t *)
(* val wrap7 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h t *)

(** {2 Multi-threads composition} *)

(* we shouldn't use choose: the scheduling may be system dependent *)

(* val choose : 'a t list -> 'a t *)
(*   (\** [choose l] behaves as the first thread in [l] to terminate.  If *)
(*       several threads are already terminated, one is chosen at *)
(*       random. *)

(*       Note: {!choose} leaves the local values of the current thread *)
(*       unchanged. *\) *)

(* val nchoose : 'a t list -> 'a list t *)
(*   (\** [nchoose l] returns the value of all that have successfully *)
(*       terminated. If all threads are sleeping, it waits for at least *)
(*       one to terminates. If one the threads of [l] fails, [nchoose] *)
(*       fails with the same exception. *)

(*       Note: {!nchoose} leaves the local values of the current thread *)
(*       unchanged. *\) *)

(* val nchoose_split : 'a t list -> ('a list * 'a t list) t *)
(*   (\** [nchoose_split l] does the same as {!nchoose} but also returns *)
(*       the list of threads that have not yet terminated. *\) *)

(** [join l] waits for all threads in [l] to terminate. If one of
    the threads fails, then [join l] will fails with the same
    exception as the first one to terminate.

    Note: {!join} leaves the local values of the current thread
    unchanged. *)
val join : unit t list -> unit t

(* val ( <?> ) : 'a t -> 'a t -> 'a t *)
(*   (\** [t <?> t'] is the same as [choose [t; t']] *\) *)

(** [t <&> t'] is the same as [join [t; t']] *)
val ( <&> ) : unit t -> unit t -> unit t

(* val async : (unit -> 'a t) -> unit *)
(*   (\** [async f] starts a thread without waiting for the result. If it *)
(*       fails (now or later), the exception is given to *)
(*       {!async_exception_hook}. *)

(*       You should use this function if you want to start a thread that *)
(*       might fail and don't care what its return value is, nor when it *)
(*       terminates (for instance, because it is looping). *\) *)

(* val ignore_result : 'a t -> unit *)
(*   (\** [ignore_result t] is like [Pervasives.ignore t] except that: *)

(*       - if [t] already failed, it raises the exception now, *)
(*       - if [t] is sleeping and fails later, the exception will be *)
(*         given to {!async_exception_hook}. *\) *)

(* val async_exception_hook : (exn -> unit) ref *)
(*   (\** Function called when a asynchronous exception is thrown. *)

(*       The default behavior is to print an error message with a *)
(*       backtrace if available and to exit the program. *)

(*       The behavior is undefined if this function raise an *)
(*       exception. *\) *)

(* (\** {2 Sleeping and resuming} *\) *)

(* type 'a u *)
(*   (\** The type of thread wakeners. *\) *)

(* val wait : unit -> 'a t * 'a u *)
(*   (\** [wait ()] is a pair of a thread which sleeps forever (unless it *)
(*       is resumed by one of the functions [wakeup], [wakeup_exn] below) *)
(*       and the corresponding wakener.  This thread does not block the *)
(*       execution of the remainder of the program (except of course, if *)
(*       another thread tries to wait for its termination). *\) *)

(* val wakeup : 'a u -> 'a -> unit *)
(*   (\** [wakeup t e] makes the sleeping thread [t] terminate and return *)
(*       the value of the expression [e]. *\) *)

(* val wakeup_exn : 'a u -> exn -> unit *)
(*   (\** [wakeup_exn t e] makes the sleeping thread [t] fail with the *)
(*       exception [e]. *\) *)

(* val wakeup_later : 'a u -> 'a -> unit *)
(*   (\** Same as {!wakeup} but it is not guaranteed that the thread will *)
(*       be woken up immediately. *\) *)

(* val wakeup_later_exn : 'a u -> exn -> unit *)
(*   (\** Same as {!wakeup_exn} but it is not guaranteed that the thread *)
(*       will be woken up immediately. *\) *)

(* val waiter_of_wakener : 'a u -> 'a t *)
(*   (\** Returns the thread associated to a wakener. *\) *)

(* type +'a result *)
(*   (\** Either a value of type ['a], either an exception. *\) *)

(* val make_value : 'a -> 'a result *)
(*   (\** [value x] creates a result containing the value [x]. *\) *)

(* val make_error : exn -> 'a result *)
(*   (\** [error e] creates a result containing the exception [e]. *\) *)

(* val of_result : 'a result -> 'a t *)
(*   (\** Returns a thread from a result. *\) *)

(* val wakeup_result : 'a u -> 'a result -> unit *)
(*   (\** [wakeup_result t r] makes the sleeping thread [t] terminate with *)
(*       the result [r]. *\) *)

(* val wakeup_later_result : 'a u -> 'a result -> unit *)
(*   (\** Same as {!wakeup_result} but it is not guaranteed that the *)
(*       thread will be woken up immediately. *\) *)

(* (\** {2 Threads state} *\) *)

(* (\** State of a thread *\) *)
(* type 'a state = *)
(*   | Return of 'a *)
(*       (\** The thread which has successfully terminated *\) *)
(*   | Fail of exn *)
(*       (\** The thread raised an exception *\) *)
(*   | Sleep *)
(*       (\** The thread is sleeping *\) *)

(* val state : 'a t -> 'a state *)
(*   (\** [state t] returns the state of a thread *\) *)

(* val is_sleeping : 'a t -> bool *)
(*   (\** [is_sleeping t] returns [true] iff [t] is sleeping. *\) *)

(* (\** {2 Cancelable threads} *\) *)

(* (\** Cancelable threads are the same as regular threads except that *)
(*     they can be canceled. *\) *)

(* exception Canceled *)
(*   (\** Canceled threads fails with this exception *\) *)

(* val task : unit -> 'a t * 'a u *)
(*   (\** [task ()] is the same as [wait ()] except that threads created *)
(*       with [task] can be canceled. *\) *)

(* val on_cancel : 'a t -> (unit -> unit) -> unit *)
(*   (\** [on_cancel t f] executes [f] when [t] is canceled. [f] will be *)
(*       executed before all other threads waiting on [t]. *)

(*       If [f] raises an exception it is given to *)
(*       {!async_exception_hook}. *\) *)

(* val add_task_r : 'a u Lwt_sequence.t -> 'a t *)
(*   (\** [add_task_r seq] creates a sleeping thread, adds its wakener to *)
(*       the right of [seq] and returns its waiter. When the thread is *)
(*       canceled, it is removed from [seq]. *\) *)

(* val add_task_l : 'a u Lwt_sequence.t -> 'a t *)
(*   (\** [add_task_l seq] creates a sleeping thread, adds its wakener to *)
(*       the left of [seq] and returns its waiter. When the thread is *)
(*       canceled, it is removed from [seq]. *\) *)

(* val cancel : 'a t -> unit *)
(*   (\** [cancel t] cancels the threads [t]. This means that the deepest *)
(*       sleeping thread created with [task] and connected to [t] is *)
(*       woken up with the exception {!Canceled}. *)

(*       For example, in the following code: *)

(*       {[ *)
(*         let waiter, wakener = task () in *)
(*         cancel (waiter >> printl "plop") *)
(*       ]} *)

(*       [waiter] will be woken up with {!Canceled}. *)
(*   *\) *)

(* val pick : 'a t list -> 'a t *)
(*   (\** [pick l] is the same as {!choose}, except that it cancels all *)
(*       sleeping threads when one terminates. *)

(*       Note: {!pick} leaves the local values of the current thread *)
(*       unchanged. *\) *)

(* val npick : 'a t list -> 'a list t *)
(*   (\** [npick l] is the same as {!nchoose}, except that it cancels all *)
(*       sleeping threads when one terminates. *)

(*       Note: {!npick} leaves the local values of the current thread *)
(*       unchanged. *\) *)

(* val protected : 'a t -> 'a t *)
(*   (\** [protected thread] creates a new cancelable thread which behave *)
(*       as [thread] except that cancelling it does not cancel *)
(*       [thread]. *\) *)

(* val no_cancel : 'a t -> 'a t *)
(*   (\** [no_cancel thread] creates a thread which behave as [thread] *)
(*       except that it cannot be canceled. *\) *)

(* (\** {2 Pause} *\) *)

(* val pause : unit -> unit t *)
(*   (\** [pause ()] is a sleeping thread which is wake up on the next *)
(*       call to {!wakeup_paused}. A thread created with [pause] can be *)
(*       canceled. *\) *)

(* val wakeup_paused : unit -> unit *)
(*   (\** [wakeup_paused ()] wakes up all threads which suspended *)
(*       themselves with {!pause}. *)

(*       This function is called by the scheduler, before entering the *)
(*       main loop. You usually do not have to call it directly, except *)
(*       if you are writing a custom scheduler. *)

(*       Note that if a paused thread resumes and pauses again, it will not *)
(*       be woken up at this point. *\) *)

(* val paused_count : unit -> int *)
(*   (\** [paused_count ()] returns the number of currently paused *)
(*       threads. *\) *)

(* val register_pause_notifier : (int -> unit) -> unit *)
(*   (\** [register_pause_notifier f] register a function [f] that will be *)
(*       called each time pause is called. The parameter passed to [f] is *)
(*       the new number of threads paused. It is useful to be able to *)
(*       call {!wakeup_paused} when there is no scheduler *\) *)

(* (\** {2 Misc} *\) *)

(* val on_success : 'a t -> ('a -> unit) -> unit *)
(*   (\** [on_success t f] executes [f] when [t] terminates without *)
(*       failing. If [f] raises an exception it is given to *)
(*       {!async_exception_hook}. *\) *)

(* val on_failure : 'a t -> (exn -> unit) -> unit *)
(*   (\** [on_failure t f] executes [f] when [t] terminates and fails. If *)
(*       [f] raises an exception it is given to *)
(*       {!async_exception_hook}. *\) *)

(* val on_termination : 'a t -> (unit -> unit) -> unit *)
(*   (\** [on_termination t f] executes [f] when [t] terminates. If [f] *)
(*       raises an exception it is given to {!async_exception_hook}. *\) *)

(* val on_any : 'a t -> ('a -> unit) -> (exn -> unit) -> unit *)
(*   (\** [on_any t f g] executes [f] or [g] when [t] terminates. If [f] *)
(*       or [g] raises an exception it is given to *)
(*       {!async_exception_hook}. *\) *)

(* (\**/**\) *)

(* (\* The functions below are probably not useful for the casual user. *)
(*    They provide the basic primitives on which can be built multi- *)
(*    threaded libraries such as Lwt_unix. *\) *)

(* val poll : 'a t -> 'a option *)
(*       (\* [poll e] returns [Some v] if the thread [e] is terminated and *)
(*          returned the value [v].  If the thread failed with some *)
(*          exception, this exception is raised.  If the thread is still *)
(*          running, [poll e] returns [None] without blocking. *\) *)

(* val apply : ('a -> 'b t) -> 'a -> 'b t *)
(*       (\* [apply f e] apply the function [f] to the expression [e].  If *)
(*          an exception is raised during this application, it is caught *)
(*          and the resulting thread fails with this exception. *\) *)
(* (\* Q: Could be called 'glue' or 'trap' or something? *\) *)

(* val backtrace_bind : (exn -> exn) -> 'a t -> ('a -> 'b t) -> 'b t *)
(* val backtrace_catch : (exn -> exn) -> (unit -> 'a t) -> (exn -> 'a t) -> 'a t *)
(* val backtrace_try_bind : (exn -> exn) -> (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t *)
(* val backtrace_finalize : (exn -> exn) -> (unit -> 'a t) -> (unit -> unit t) -> 'a t *)
end
# 24 "v0.in.ml"


  module Lwt_list : sig
# 1 "v0/lwt_list.mli"
(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_list
 * Copyright (C) 2010 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(** List helpers *)

(* TEZOS CHANGES

   * import version 2.4.5
   * Remove iter/iteri
*)

(** Note: this module use the same naming convention as
    {!Lwt_stream}. *)

(** {2 List iterators} *)

val map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val mapi_s : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val mapi_p : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val rev_map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val rev_map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

val fold_left_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b list -> 'a Lwt.t

val fold_right_s : ('a -> 'b -> 'b Lwt.t) -> 'a list -> 'b -> 'b Lwt.t

(** {2 List scanning} *)

val for_all_s : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

val for_all_p : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

val exists_s : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

val exists_p : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

(** {2 List searching} *)

val find_s : ('a -> bool Lwt.t) -> 'a list -> 'a Lwt.t

val filter_s : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

val filter_p : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

val filter_map_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

val filter_map_p : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

val partition_s : ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t

val partition_p : ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t
end
# 26 "v0.in.ml"


  module Raw_hashes : sig
# 1 "v0/raw_hashes.mli"
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

val blake2b : MBytes.t -> MBytes.t

val sha256 : MBytes.t -> MBytes.t

val sha512 : MBytes.t -> MBytes.t
end
# 28 "v0.in.ml"


  module Compare : sig
# 1 "v0/compare.mli"
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

module Float : S with type t = float

module String : S with type t = string

module Z : S with type t = Z.t

module List (P : COMPARABLE) : S with type t = P.t list

module Option (P : COMPARABLE) : S with type t = P.t option
end
# 30 "v0.in.ml"


  module Data_encoding : sig
# 1 "v0/data_encoding.mli"
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

val bytes : MBytes.t encoding

val float : float encoding

val option : 'a encoding -> 'a option encoding

val string_enum : (string * 'a) list -> 'a encoding

module Fixed : sig
  val string : int -> string encoding

  val bytes : int -> MBytes.t encoding

  val add_padding : 'a encoding -> int -> 'a encoding
end

module Variable : sig
  val string : string encoding

  val bytes : MBytes.t encoding

  val array : ?max_length:int -> 'a encoding -> 'a array encoding

  val list : ?max_length:int -> 'a encoding -> 'a list encoding
end

module Bounded : sig
  val string : int -> string encoding

  val bytes : int -> MBytes.t encoding
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

val union : ?tag_size:[`Uint8 | `Uint16] -> 't case list -> 't encoding

val def :
  string -> ?title:string -> ?description:string -> 't encoding -> 't encoding

val conv :
  ('a -> 'b) -> ('b -> 'a) -> ?schema:json_schema -> 'b encoding -> 'a encoding

val mu :
  string ->
  ?title:string ->
  ?description:string ->
  ('a encoding -> 'a encoding) ->
  'a encoding

type 'a lazy_t

val lazy_encoding : 'a encoding -> 'a lazy_t encoding

val force_decode : 'a lazy_t -> 'a option

val force_bytes : 'a lazy_t -> MBytes.t

val make_lazy : 'a encoding -> 'a -> 'a lazy_t

val apply_lazy :
  fun_value:('a -> 'b) ->
  fun_bytes:(MBytes.t -> 'b) ->
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
  val length : 'a encoding -> 'a -> int

  val fixed_length : 'a encoding -> int option

  val read : 'a encoding -> MBytes.t -> int -> int -> (int * 'a) option

  val write : 'a encoding -> 'a -> MBytes.t -> int -> int -> int option

  val to_bytes : 'a encoding -> 'a -> MBytes.t option

  val to_bytes_exn : 'a encoding -> 'a -> MBytes.t

  val of_bytes : 'a encoding -> MBytes.t -> 'a option

  type write_error

  exception Write_error of write_error
end

(** [check_size size encoding] ensures that the binary encoding
    of a value will not be allowed to exceed [size] bytes. The reader
    and the writer fails otherwise. This function do not modify
    the JSON encoding. *)
val check_size : int -> 'a encoding -> 'a encoding
end
# 32 "v0.in.ml"


  module Error_monad : sig
# 1 "v0/error_monad.mli"
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

(** Tezos Protocol Implementation - Error Monad *)

(** {2 Error classification} *)

(** Categories of error *)
type error_category =
  [ `Branch  (** Errors that may not happen in another context *)
  | `Temporary  (** Errors that may not happen in a later context *)
  | `Permanent  (** Errors that will happen no matter the context *) ]

(** Custom error handling for economic protocols. *)

type error = ..

val pp : Format.formatter -> error -> unit

(** A JSON error serializer *)
val error_encoding : error Data_encoding.t

val json_of_error : error -> Data_encoding.json

val error_of_json : Data_encoding.json -> error

(** Error information *)
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

(** For other modules to register specialized error serializers *)
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

(** Classify an error using the registered kinds *)
val classify_errors : error list -> error_category

(** {2 Monad definition} *)

(** The error monad wrapper type, the error case holds a stack of
    error, initialized by the first call to {!fail} and completed by
    each call to {!trace} as the stack is rewound. The most general
    error is thus at the top of the error stack, going down to the
    specific error that actually caused the failure. *)
type 'a tzresult = ('a, error list) result

(** A JSON serializer for result of a given type *)
val result_encoding : 'a Data_encoding.t -> 'a tzresult Data_encoding.encoding

(** Successful result *)
val ok : 'a -> 'a tzresult

(** Successful return *)
val return : 'a -> 'a tzresult Lwt.t

(** Successful return of [()] *)
val return_unit : unit tzresult Lwt.t

(** Successful return of [None] *)
val return_none : 'a option tzresult Lwt.t

(** [return_some x] is a successful return of [Some x] *)
val return_some : 'a -> 'a option tzresult Lwt.t

(** Successful return of [[]] *)
val return_nil : 'a list tzresult Lwt.t

(** Successful return of [true] *)
val return_true : bool tzresult Lwt.t

(** Successful return of [false] *)
val return_false : bool tzresult Lwt.t

(** Erroneous result *)
val error : error -> 'a tzresult

(** Erroneous return *)
val fail : error -> 'a tzresult Lwt.t

(** Non-Lwt bind operator *)
val ( >>? ) : 'a tzresult -> ('a -> 'b tzresult) -> 'b tzresult

(** Bind operator *)
val ( >>=? ) :
  'a tzresult Lwt.t -> ('a -> 'b tzresult Lwt.t) -> 'b tzresult Lwt.t

(** Lwt's bind reexported *)
val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

(** To operator *)
val ( >>|? ) : 'a tzresult Lwt.t -> ('a -> 'b) -> 'b tzresult Lwt.t

(** Non-Lwt to operator *)
val ( >|? ) : 'a tzresult -> ('a -> 'b) -> 'b tzresult

(** Enrich an error report (or do nothing on a successful result) manually *)
val record_trace : error -> 'a tzresult -> 'a tzresult

(** Automatically enrich error reporting on stack rewind *)
val trace : error -> 'b tzresult Lwt.t -> 'b tzresult Lwt.t

(** Same as record_trace, for unevaluated error *)
val record_trace_eval : (unit -> error tzresult) -> 'a tzresult -> 'a tzresult

(** Same as trace, for unevaluated Lwt error *)
val trace_eval :
  (unit -> error tzresult Lwt.t) -> 'b tzresult Lwt.t -> 'b tzresult Lwt.t

(** Erroneous return on failed assertion *)
val fail_unless : bool -> error -> unit tzresult Lwt.t

(** Erroneous return on successful assertion *)
val fail_when : bool -> error -> unit tzresult Lwt.t

(** {2 In-monad list iterators} *)

(** A {!List.iter} in the monad *)
val iter_s : ('a -> unit tzresult Lwt.t) -> 'a list -> unit tzresult Lwt.t

val iter_p : ('a -> unit tzresult Lwt.t) -> 'a list -> unit tzresult Lwt.t

(** A {!List.map} in the monad *)
val map_s : ('a -> 'b tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t

val map_p : ('a -> 'b tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t

(** A {!List.map2} in the monad *)
val map2 : ('a -> 'b -> 'c tzresult) -> 'a list -> 'b list -> 'c list tzresult

(** A {!List.map2} in the monad *)
val map2_s :
  ('a -> 'b -> 'c tzresult Lwt.t) ->
  'a list ->
  'b list ->
  'c list tzresult Lwt.t

(** A {!List.filter_map} in the monad *)
val filter_map_s :
  ('a -> 'b option tzresult Lwt.t) -> 'a list -> 'b list tzresult Lwt.t

(** A {!List.fold_left} in the monad *)
val fold_left_s :
  ('a -> 'b -> 'a tzresult Lwt.t) -> 'a -> 'b list -> 'a tzresult Lwt.t

(** A {!List.fold_right} in the monad *)
val fold_right_s :
  ('a -> 'b -> 'b tzresult Lwt.t) -> 'a list -> 'b -> 'b tzresult Lwt.t

(**/**)

type shell_error

type 'a shell_tzresult = ('a, shell_error list) result
end
# 34 "v0.in.ml"


  open Error_monad

  module Logging : sig
# 1 "v0/logging.mli"
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

val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

val log_info : ('a, Format.formatter, unit, unit) format4 -> 'a

val log_notice : ('a, Format.formatter, unit, unit) format4 -> 'a

val warn : ('a, Format.formatter, unit, unit) format4 -> 'a

val log_error : ('a, Format.formatter, unit, unit) format4 -> 'a

val fatal_error : ('a, Format.formatter, unit, unit) format4 -> 'a

val lwt_debug : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

val lwt_log_info : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

val lwt_log_notice : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

val lwt_warn : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

val lwt_log_error : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
end
# 38 "v0.in.ml"


  module Time : sig
# 1 "v0/time.mli"
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
# 40 "v0.in.ml"


  module Option : sig
# 1 "v0/option.mli"
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

val map : f:('a -> 'b) -> 'a option -> 'b option

val apply : f:('a -> 'b option) -> 'a option -> 'b option

val iter : f:('a -> unit) -> 'a option -> unit

val unopt : default:'a -> 'a option -> 'a

val unopt_map : f:('a -> 'b) -> default:'b -> 'a option -> 'b

val first_some : 'a option -> 'a option -> 'a option

val try_with : (unit -> 'a) -> 'a option

val some : 'a -> 'a option
end
# 42 "v0.in.ml"


  module RPC_arg : sig
# 1 "v0/RPC_arg.mli"
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

val int : int arg

val int32 : int32 arg

val int64 : int64 arg

val float : float arg

val string : string arg

val like : 'a arg -> ?descr:string -> string -> 'a arg

type ('a, 'b) eq = Eq : ('a, 'a) eq

val eq : 'a arg -> 'b arg -> ('a, 'b) eq option
end
# 44 "v0.in.ml"


  module RPC_path : sig
# 1 "v0/RPC_path.mli"
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
# 46 "v0.in.ml"


  module RPC_query : sig
# 1 "v0/RPC_query.mli"
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
# 48 "v0.in.ml"


  module RPC_service : sig
# 1 "v0/RPC_service.mli"
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
# 50 "v0.in.ml"


  module RPC_answer : sig
# 1 "v0/RPC_answer.mli"
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

val return_stream : 'o stream -> 'o t Lwt.t

val not_found : 'o t Lwt.t

val fail : error list -> 'a t Lwt.t
end
# 52 "v0.in.ml"


  module RPC_directory : sig
# 1 "v0/RPC_directory.mli"
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

(** Registering handler in service tree. *)
val register :
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output tzresult Lwt.t) ->
  'prefix directory

val opt_register :
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
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output Lwt.t) ->
  'prefix directory

(** Registering handler in service tree. Curryfied variant.  *)

val register0 :
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o tzresult Lwt.t) ->
  unit directory

val register1 :
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register2 :
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register3 :
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register4 :
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register5 :
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val opt_register0 :
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o option tzresult Lwt.t) ->
  unit directory

val opt_register1 :
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register2 :
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register3 :
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register4 :
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register5 :
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
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o Lwt.t) ->
  unit directory

val lwt_register1 :
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register2 :
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register3 :
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register4 :
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register5 :
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
# 54 "v0.in.ml"


  module Base58 : sig
# 1 "v0/base58.mli"
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
# 56 "v0.in.ml"


  module S : sig
# 1 "v0/s.mli"
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

(** Generic interface for a datatype with comparison, pretty-printer
    and serialization functions. *)
module type T = sig
  type t

  include Compare.S with type t := t

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val to_bytes : t -> MBytes.t

  val of_bytes : MBytes.t -> t option
end

(** Generic interface for a datatype with comparison, pretty-printer,
    serialization functions and a hashing function. *)
module type HASHABLE = sig
  include T

  type hash

  val hash : t -> hash

  val hash_raw : MBytes.t -> hash
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

  val hash_bytes : ?key:MBytes.t -> MBytes.t list -> t

  val hash_string : ?key:string -> string list -> t

  val zero : t
end

module type RAW_DATA = sig
  type t

  val size : int (* in bytes *)

  val to_bytes : t -> MBytes.t

  val of_bytes_opt : MBytes.t -> t option

  val of_bytes_exn : MBytes.t -> t
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

module type SET = sig
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

  val diff : t -> t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val map : (elt -> elt) -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val partition : (elt -> bool) -> t -> t * t

  val cardinal : t -> int

  val elements : t -> elt list

  val min_elt_opt : t -> elt option

  val max_elt_opt : t -> elt option

  val choose_opt : t -> elt option

  val split : elt -> t -> t * bool * t

  val find_opt : elt -> t -> elt option

  val find_first_opt : (elt -> bool) -> t -> elt option

  val find_last_opt : (elt -> bool) -> t -> elt option

  val of_list : elt list -> t
end

module type MAP = sig
  type key

  type +'a t

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

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t

  val cardinal : 'a t -> int

  val bindings : 'a t -> (key * 'a) list

  val min_binding_opt : 'a t -> (key * 'a) option

  val max_binding_opt : 'a t -> (key * 'a) option

  val choose_opt : 'a t -> (key * 'a) option

  val split : key -> 'a t -> 'a t * 'a option * 'a t

  val find_opt : key -> 'a t -> 'a option

  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option

  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end

module type INDEXES = sig
  type t

  val to_path : t -> string list -> string list

  val of_path : string list -> t option

  val of_path_exn : string list -> t

  val prefix_path : string -> string list

  val path_length : int

  module Set : sig
    include SET with type elt = t

    val encoding : t Data_encoding.t
  end

  module Map : sig
    include MAP with type key = t

    val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
  end
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

module type SIGNATURE = sig
  module Public_key_hash : sig
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

  module Public_key : sig
    type t

    val pp : Format.formatter -> t -> unit

    include Compare.S with type t := t

    include B58_DATA with type t := t

    include ENCODER with type t := t

    val hash : t -> Public_key_hash.t
  end

  type t

  val pp : Format.formatter -> t -> unit

  include RAW_DATA with type t := t

  include Compare.S with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  val zero : t

  type watermark

  (** Check a signature *)
  val check : ?watermark:watermark -> Public_key.t -> t -> MBytes.t -> bool
end
end
# 58 "v0.in.ml"


  module Set : sig
# 1 "v0/set.mli"
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

(** Sets over ordered types.

    This module implements the set data structure, given a total ordering
    function over the set elements. All operations over sets
    are purely applicative (no side-effects).
    The implementation uses balanced binary trees, and is therefore
    reasonably efficient: insertion and membership take time
    logarithmic in the size of the set, for instance.

    The {!Make} functor constructs implementations for any type, given a
    [compare] function.
    For instance:
    {[
      module IntPairs =
      struct
        type t = int * int
        let compare (x0,y0) (x1,y1) =
          match Stdlib.compare x0 x1 with
            0 -> Stdlib.compare y0 y1
          | c -> c
      end

      module PairsSet = Set.Make(IntPairs)

      let m = PairsSet.(empty |> add (2,3) |> add (5,7) |> add (11,13))
    ]}

    This creates a new module [PairsSet], with a new type [PairsSet.t]
    of sets of [int * int].
*)

(** Input signature of the functor {!Set.Make}. *)
module type OrderedType = sig
  (** The type of the set elements. *)
  type t

  (** A total ordering function over the set elements.
      This is a two-argument function [f] such that
      [f e1 e2] is zero if the elements [e1] and [e2] are equal,
      [f e1 e2] is strictly negative if [e1] is smaller than [e2],
      and [f e1 e2] is strictly positive if [e1] is greater than [e2].
      Example: a suitable ordering function is the generic structural
      comparison function {!Stdlib.compare}. *)
  val compare : t -> t -> int
end

(** Functor building an implementation of the set structure
    given a totally ordered type. *)
module Make (Ord : OrderedType) : S.SET with type elt = Ord.t
end
# 60 "v0.in.ml"


  module Map : sig
# 1 "v0/map.mli"
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

(** Association tables over ordered types.

    This module implements applicative association tables, also known as
    finite maps or dictionaries, given a total ordering function
    over the keys.
    All operations over maps are purely applicative (no side-effects).
    The implementation uses balanced binary trees, and therefore searching
    and insertion take time logarithmic in the size of the map.

    For instance:
    {[
      module IntPairs =
      struct
        type t = int * int
        let compare (x0,y0) (x1,y1) =
          match Stdlib.compare x0 x1 with
            0 -> Stdlib.compare y0 y1
          | c -> c
      end

      module PairsMap = Map.Make(IntPairs)

      let m = PairsMap.(empty |> add (0,1) "hello" |> add (1,0) "world")
    ]}

    This creates a new module [PairsMap], with a new type ['a PairsMap.t]
    of maps from [int * int] to ['a]. In this example, [m] contains [string]
    values so its type is [string PairsMap.t].
*)

(** Input signature of the functor {!Map.Make}. *)
module type OrderedType = sig
  (** The type of the map keys. *)
  type t

  (** A total ordering function over the keys.
      This is a two-argument function [f] such that
      [f e1 e2] is zero if the keys [e1] and [e2] are equal,
      [f e1 e2] is strictly negative if [e1] is smaller than [e2],
      and [f e1 e2] is strictly positive if [e1] is greater than [e2].
      Example: a suitable ordering function is the generic structural
      comparison function {!Stdlib.compare}. *)
  val compare : t -> t -> int
end

(** Functor building an implementation of the map structure
    given a totally ordered type. *)
module Make (Ord : OrderedType) : S.MAP with type key = Ord.t
end
# 62 "v0.in.ml"


  module Blake2B : sig
# 1 "v0/blake2B.mli"
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

module Make (Register : sig
  val register_encoding :
    prefix:string ->
    length:int ->
    to_raw:('a -> string) ->
    of_raw:(string -> 'a option) ->
    wrap:('a -> Base58.data) ->
    'a Base58.encoding
end)
(Name : PrefixedName) : S.HASH
end
# 64 "v0.in.ml"


  module Ed25519 : sig
# 1 "v0/ed25519.mli"
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

include S.SIGNATURE with type watermark := MBytes.t
end
# 66 "v0.in.ml"


  module Secp256k1 : sig
# 1 "v0/secp256k1.mli"
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

include S.SIGNATURE with type watermark := MBytes.t
end
# 68 "v0.in.ml"


  module P256 : sig
# 1 "v0/p256.mli"
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

include S.SIGNATURE with type watermark := MBytes.t
end
# 70 "v0.in.ml"


  module Chain_id : sig
# 1 "v0/chain_id.mli"
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
# 72 "v0.in.ml"


  module Signature : sig
# 1 "v0/signature.mli"
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
  | Custom of MBytes.t

include
  S.SIGNATURE
    with type Public_key_hash.t = public_key_hash
     and type Public_key.t = public_key
     and type watermark := watermark
end
# 74 "v0.in.ml"


  module Block_hash : sig
# 1 "v0/block_hash.mli"
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
# 76 "v0.in.ml"


  module Operation_hash : sig
# 1 "v0/operation_hash.mli"
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
# 78 "v0.in.ml"


  module Operation_list_hash : sig
# 1 "v0/operation_list_hash.mli"
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
# 80 "v0.in.ml"


  module Operation_list_list_hash : sig
# 1 "v0/operation_list_list_hash.mli"
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
# 82 "v0.in.ml"


  module Protocol_hash : sig
# 1 "v0/protocol_hash.mli"
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
# 84 "v0.in.ml"


  module Context_hash : sig
# 1 "v0/context_hash.mli"
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
end
# 86 "v0.in.ml"


  module Micheline : sig
# 1 "v0/micheline.mli"
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
  | Bytes of 'l * MBytes.t
  | Prim of 'l * 'p * ('l, 'p) node list * annot
  | Seq of 'l * ('l, 'p) node list

type 'p canonical

type canonical_location = int

val root : 'p canonical -> (canonical_location, 'p) node

val canonical_location_encoding : canonical_location Data_encoding.encoding

val canonical_encoding :
  variant:string ->
  'l Data_encoding.encoding ->
  'l canonical Data_encoding.encoding

val canonical_encoding_v1 :
  variant:string ->
  'l Data_encoding.encoding ->
  'l canonical Data_encoding.encoding

(*
val erased_encoding : variant:string -> 'l -> 'p Data_encoding.encoding -> ('l, 'p) node Data_encoding.encoding
val table_encoding : variant:string -> 'l Data_encoding.encoding -> 'p Data_encoding.encoding -> ('l, 'p) node Data_encoding.encoding
*)
val location : ('l, 'p) node -> 'l

val annotations : ('l, 'p) node -> string list

val strip_locations : (_, 'p) node -> 'p canonical

val extract_locations :
  ('l, 'p) node -> 'p canonical * (canonical_location * 'l) list

val inject_locations :
  (canonical_location -> 'l) -> 'p canonical -> ('l, 'p) node
end
# 88 "v0.in.ml"


  module Block_header : sig
# 1 "v0/block_header.mli"
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
  fitness : MBytes.t list;
  context : Context_hash.t;
}

val shell_header_encoding : shell_header Data_encoding.t

type t = {shell : shell_header; protocol_data : MBytes.t}

include S.HASHABLE with type t := t and type hash := Block_hash.t
end
# 90 "v0.in.ml"


  module Fitness : sig
# 1 "v0/fitness.mli"
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
include S.T with type t = MBytes.t list
end
# 92 "v0.in.ml"


  module Operation : sig
# 1 "v0/operation.mli"
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

type t = {shell : shell_header; proto : MBytes.t}

include S.HASHABLE with type t := t and type hash := Operation_hash.t
end
# 94 "v0.in.ml"


  module Context : sig
# 1 "v0/context.mli"
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

type t

(** Keys in (kex x value) database implementations *)
type key = string list

(** Values in (kex x value) database implementations *)
type value = MBytes.t

val mem : t -> key -> bool Lwt.t

val dir_mem : t -> key -> bool Lwt.t

val get : t -> key -> value option Lwt.t

val set : t -> key -> value -> t Lwt.t

(** [copy] returns None if the [from] key is not bound *)
val copy : t -> from:key -> to_:key -> t option Lwt.t

val del : t -> key -> t Lwt.t

val remove_rec : t -> key -> t Lwt.t

val fold :
  t ->
  key ->
  init:'a ->
  f:([`Key of key | `Dir of key] -> 'a -> 'a Lwt.t) ->
  'a Lwt.t

val keys : t -> key -> key list Lwt.t

val fold_keys : t -> key -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t

val register_resolver :
  'a Base58.encoding -> (t -> string -> 'a list Lwt.t) -> unit

val complete : t -> string -> string list Lwt.t
end
# 96 "v0.in.ml"


  module Updater : sig
# 1 "v0/updater.mli"
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
      (** The effective fitness of the block (to be compared with
      the 'announced' one in the block header. *)
  message : string option;
      (** An optional informative message to be used as in the 'git
      commit' of the block's context. *)
  max_operations_ttl : int;
      (** The "time-to-live" of operation for the next block: any
      operations whose 'branch' is older than 'ttl' blocks in the
      past cannot be included in the next block. *)
  last_allowed_fork_level : Int32.t;
      (** The level of the last block for which the node might consider an
      alternate branch. The shell should consider as invalid any
      branch whose fork point is older than the given level *)
}

type quota = {
  max_size : int;
      (** The maximum size (in bytes) of the serialized list of
      operations. *)
  max_op : int option;
      (** The maximum number of operation.
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

  (** The maximum size of an operation in bytes. *)
  val max_operation_data_length : int

  (** The number of validation passes (length of the list) and the
      operation's quota for each pass. *)
  val validation_passes : quota list

  (** The version specific type of blocks. *)
  type block_header_data

  (** Encoding for version specific part of block headers.  *)
  val block_header_data_encoding : block_header_data Data_encoding.t

  (** A fully parsed block header. *)
  type block_header = {
    shell : Block_header.shell_header;
    protocol_data : block_header_data;
  }

  (** Version-specific side information computed by the protocol
      during the validation of a block. Should not include information
      about the evaluation of operations which is handled separately by
      {!operation_metadata}. To be used as an execution trace by tools
      (client, indexer). Not necessary for validation. *)
  type block_header_metadata

  (** Encoding for version-specific block metadata. *)
  val block_header_metadata_encoding : block_header_metadata Data_encoding.t

  (** The version specific type of operations. *)
  type operation_data

  (** Version-specific side information computed by the protocol
      during the validation of each operation, to be used conjointly
      with {!block_header_metadata}. *)
  type operation_receipt

  (** A fully parsed operation. *)
  type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
  }

  (** Encoding for version-specific operation data. *)
  val operation_data_encoding : operation_data Data_encoding.t

  (** Encoding for version-specific operation receipts. *)
  val operation_receipt_encoding : operation_receipt Data_encoding.t

  (** Encoding that mixes an operation data and its receipt. *)
  val operation_data_and_receipt_encoding :
    (operation_data * operation_receipt) Data_encoding.t

  (** The Validation passes in which an operation can appear.
      For instance [[0]] if it only belongs to the first pass.
      An answer of [[]] means that the operation is ill-formed
      and cannot be included at all. *)
  val acceptable_passes : operation -> int list

  (** Basic ordering of operations. [compare_operations op1 op2] means
      that [op1] should appear before [op2] in a block. *)
  val compare_operations : operation -> operation -> int

  (** A functional state that is transmitted through the steps of a
      block validation sequence. It must retain the current state of
      the store (that can be extracted from the outside using
      {!current_context}, and whose final value is produced by
      {!finalize_block}). It can also contain the information that
      must be remembered during the validation, which must be
      immutable (as validator or baker implementations are allowed to
      pause, replay or backtrack during the validation process). *)
  type validation_state

  (** Access the context at a given validation step. *)
  val current_context : validation_state -> Context.t tzresult Lwt.t

  (** Checks that a block is well formed in a given context. This
      function should run quickly, as its main use is to reject bad
      blocks from the chain as early as possible. The input context
      is the one resulting of an ancestor block of same protocol
      version. This ancestor of the current head is guaranteed to be
      more recent than `last_allowed_fork_level`.

      The resulting `validation_state` will be used for multi-pass
      validation. *)
  val begin_partial_application :
    chain_id:Chain_id.t ->
    ancestor_context:Context.t ->
    predecessor_timestamp:Time.t ->
    predecessor_fitness:Fitness.t ->
    block_header ->
    validation_state tzresult Lwt.t

  (** The first step in a block validation sequence. Initializes a
      validation context for validating a block. Takes as argument the
      {!Block_header.t} to initialize the context for this block. The
      function {!precheck_block} may not have been called before
      [begin_application], so all the check performed by the former
      must be repeated in the latter. *)
  val begin_application :
    chain_id:Chain_id.t ->
    predecessor_context:Context.t ->
    predecessor_timestamp:Time.t ->
    predecessor_fitness:Fitness.t ->
    block_header ->
    validation_state tzresult Lwt.t

  (** Initializes a validation context for constructing a new block
      (as opposed to validating an existing block). When the
      [protocol_data] argument is specified, it should contains a
      'prototype' of a the protocol specific part of a block header,
      and the function should produce the exact same effect on the
      context than would produce the validation of a block containing
      an "equivalent" (but complete) header. For instance, if the
      block header usually includes a signature, the header provided
      to {!begin_construction} should includes a faked signature. *)
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

  (** Called after {!begin_application} (or {!begin_construction}) and
      before {!finalize_block}, with each operation in the block. *)
  val apply_operation :
    validation_state ->
    operation ->
    (validation_state * operation_receipt) tzresult Lwt.t

  (** The last step in a block validation sequence. It produces the
      context that will be used as input for the validation of its
      successor block candidates. *)
  val finalize_block :
    validation_state ->
    (validation_result * block_header_metadata) tzresult Lwt.t

  (** The list of remote procedures exported by this implementation *)
  val rpc_services : rpc_context RPC_directory.t

  (** Initialize the context (or upgrade the context after a protocol
      amendment). This function receives the context resulting of the
      application of a block that triggered the amendment. It also
      receives the header of the block that triggered the amendment. *)
  val init :
    Context.t -> Block_header.shell_header -> validation_result tzresult Lwt.t
end

(** Activates a given protocol version from a given context. This
    means that the context used for the next block will use this
    version (this is not an immediate change). The version must have
    been previously compiled successfully. *)
val activate : Context.t -> Protocol_hash.t -> Context.t Lwt.t

(** Fork a test chain. The forked chain will use the current block
    as genesis, and [protocol] as economic protocol. The chain will
    be destroyed when a (successor) block will have a timestamp greater
    than [expiration]. The protocol must have been previously compiled
    successfully. *)
val fork_test_chain :
  Context.t -> protocol:Protocol_hash.t -> expiration:Time.t -> Context.t Lwt.t
end
# 98 "v0.in.ml"


  module RPC_context : sig
# 1 "v0/RPC_context.mli"
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
# 100 "v0.in.ml"

end
