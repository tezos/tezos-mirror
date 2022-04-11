open Protocol

let ( >>=?? ) x y =
  x >>= function
  | Ok s -> y s
  | Error err -> Lwt.return @@ Error (Environment.wrap_tztrace err)

let ( >>??= ) x y =
  match x with
  | Ok s -> y s
  | Error err -> Lwt.return @@ Error (Environment.wrap_tztrace err)
