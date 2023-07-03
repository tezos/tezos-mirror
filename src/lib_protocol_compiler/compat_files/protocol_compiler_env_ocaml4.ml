(* the default is safe-string, and the default is to configure OCaml
   to reject [-unsafe-string], see
   https://gitlab.com/tezos/tezos/-/merge_requests/8036#note_1454833974

   So we just assert the value held by the ref rather than set it. *)
let () = assert (not !Clflags.unsafe_string)

let env = Env.initial_safe_string
