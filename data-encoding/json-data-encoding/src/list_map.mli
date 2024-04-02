(** Tail-recursive [List.map] alternative.

    This map function is the one described in
    https://discuss.ocaml.org/t/a-new-list-map-that-is-both-stack-safe-and-fast/865

    CAVEAT: The order of application of the function to the elements of the list
    is different from that of [Stdlib.List.map]. The function is applied in
    reverse order of the elements in the list. In other words:

       [map_pure f l] is equivalent to [List.rev_map f (List.rev l)]

    Considering this caveat, it is recommended to use this mapping function with
    a pure function as argument. This is why the name [map_pure] is chosen.
*)
val map_pure : ('a -> 'b) -> 'a list -> 'b list

val mapi_pure : (int -> 'a -> 'b) -> 'a list -> 'b list

(** [append] is a tail-rec variant of [List.append]. It special cases small
    lists, and then it switches to [List.rev_append]. *)
val append : 'a list -> 'a list -> 'a list
