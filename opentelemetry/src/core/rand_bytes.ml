(* generate random IDs *)
let rand_ = Random.State.make_self_init ()

let ( let@ ) = ( @@ )

let default_rand_bytes_8 () : bytes =
  let@ () = Lock.with_lock in
  let b = Bytes.create 8 in
  for i = 0 to 1 do
    let r = Random.State.bits rand_ in
    (* 30 bits, of which we use 24 *)
    Bytes.set b (i * 3) (Char.chr (r land 0xff));
    Bytes.set b ((i * 3) + 1) (Char.chr ((r lsr 8) land 0xff));
    Bytes.set b ((i * 3) + 2) (Char.chr ((r lsr 16) land 0xff))
  done;
  let r = Random.State.bits rand_ in
  Bytes.set b 6 (Char.chr (r land 0xff));
  Bytes.set b 7 (Char.chr ((r lsr 8) land 0xff));
  b

let default_rand_bytes_16 () : bytes =
  let@ () = Lock.with_lock in
  let b = Bytes.create 16 in
  for i = 0 to 4 do
    let r = Random.State.bits rand_ in
    (* 30 bits, of which we use 24 *)
    Bytes.set b (i * 3) (Char.chr (r land 0xff));
    Bytes.set b ((i * 3) + 1) (Char.chr ((r lsr 8) land 0xff));
    Bytes.set b ((i * 3) + 2) (Char.chr ((r lsr 16) land 0xff))
  done;
  let r = Random.State.bits rand_ in
  Bytes.set b 15 (Char.chr (r land 0xff));
  (* last byte *)
  b

let rand_bytes_16 = ref default_rand_bytes_16

let rand_bytes_8 = ref default_rand_bytes_8
