type 'a key = 'a Hmap.key

module type STORAGE = sig
  val name : string

  val get_map : unit -> Hmap.t option

  val with_map : Hmap.t -> (unit -> 'b) -> 'b

  val create_key : unit -> 'a key

  val get : 'a key -> 'a option

  val with_binding : 'a key -> 'a -> (unit -> 'b) -> 'b

  val without_binding : 'a key -> (unit -> 'b) -> 'b
end

type storage = (module STORAGE)
