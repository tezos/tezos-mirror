(** {1 Topic definition } *)

type topic = {slot : int; shard : int}

module Topic = struct
  type t = topic

  let compare t1 t2 =
    let c = Int.compare t1.slot t2.slot in
    if c = 0 then Int.compare t1.shard t2.shard else c

  let pp fmtr {slot; shard} = Format.fprintf fmtr "slot=%d,shard=%d" slot shard
    [@@ocaml.warning "-32"]
end

include Topic
module Set = Set.Make (Topic)
