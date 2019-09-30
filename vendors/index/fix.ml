module Blake2B = Digestif.Make_BLAKE2B (struct
    let digest_size = 32
  end)

module Hash = Irmin.Hash.Make(Blake2B)

module Key = struct
  type t = Hash.t

  let pp ppf t = Irmin.Type.pp Hash.t ppf t

  let hash t = Irmin.Type.short_hash Hash.t t

  let hash_size = 30

  let equal x y = Irmin.Type.equal Hash.t x y

  let encode x = Irmin.Type.to_bin_string Hash.t x

  let encoded_size = Hash.hash_size

  let decode s off =
    let _, v = Irmin.Type.decode_bin ~headers:false Hash.t s off in
    v
end

module Val = struct
  type t = int64 * int * char

  let pp = Irmin.Type.(pp (triple int64 int char))

  let encode (off, len, kind) =
    Irmin.Type.(to_bin_string (triple int64 int32 char))
      (off, Int32.of_int len, kind)

  let decode s off =
    let off, len, kind =
      snd (Irmin.Type.(decode_bin (triple int64 int32 char)) s off)
    in
    (off, Int32.to_int len, kind)

  let encoded_size = (64 / 8) + (32 / 8) + 1
end

module Index = Index_unix.Make (Key) (Val)

let run root =
  let context = Filename.concat root "context" in
  let fd = Index.v ~fresh:false ~log_size:100_000 context in
  Fmt.epr "Reseting fanout of index %s ...\n%!" root;
  Index.force_merge fd (Blake2B.digest_string "") (0L, 0, ' ')

let () =
  if Array.length Sys.argv <> 2 then (
    Fmt.epr "usage: fix <data-dir>\n%!";
    exit 1
  ) else
    run Sys.argv.(1)
