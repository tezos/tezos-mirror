(* This module parses keys from csv test vectors *)

module R = Rustzcash
module Sk = Core.Raw.Spending_key
module Vk = Core.Raw.Viewing_key

let ba_of_hex h = Hex.to_bytes_exn (`Hex h)

let path filename = project_root // Filename.dirname __FILE__ // filename

module Vector = struct
  type test_vector = {
    sk : Bytes.t;
    ask : R.ask;
    nsk : R.nsk;
    ovk : R.ovk;
    ak : R.ak;
    nk : R.nk;
    ivk : R.ivk;
    default_d : R.diversifier;
    default_pk_d : R.pkd;
    note_v : Int64.t;
    note_r : R.rcm;
    note_cm : R.commitment;
    note_pos : Int64.t;
    note_nf : R.nullifier;
  }

  let vectors =
    let vector_of_strings sk ask nsk ovk ak nk ivk default_d default_pk_d note_v
        note_r note_cm note_pos note_nf =
      let note_v =
        let amount = Int64.of_string note_v in
        assert (R.valid_amount amount) ;
        amount
      in
      let note_pos =
        let pos = Int64.of_string note_pos in
        assert (R.valid_position pos) ;
        pos
      in
      {
        sk = ba_of_hex sk;
        ask = R.to_ask @@ ba_of_hex ask;
        nsk = R.to_nsk @@ ba_of_hex nsk;
        ovk = R.to_ovk @@ ba_of_hex ovk;
        ak = R.to_ak @@ ba_of_hex ak;
        nk = R.to_nk @@ ba_of_hex nk;
        ivk = R.to_ivk @@ ba_of_hex ivk;
        default_d = Option.get @@ R.to_diversifier @@ ba_of_hex default_d;
        default_pk_d = R.to_pkd @@ ba_of_hex default_pk_d;
        note_v;
        note_r = R.to_rcm @@ ba_of_hex note_r;
        note_cm = R.to_commitment @@ ba_of_hex note_cm;
        note_pos;
        note_nf = R.to_nullifier @@ ba_of_hex note_nf;
      }
    in
    (* read file in memory skipping lines with # *)
    let file = path "vectors.csv" in
    let ic = open_in file in
    let rec read_lines ls =
      try
        let line = input_line ic in
        if Str.(string_match (regexp ".*#.*") line 0) then read_lines ls
        else read_lines (line :: ls)
      with End_of_file -> List.rev ls
    in
    let lines = read_lines [] in
    List.map
      (fun line ->
        Scanf.sscanf
          line
          "%s@, %s@, %s@, %s@, %s@, %s@, %s@, %s@, %s@, %s@, %s@, %s@, %s@, %s"
          vector_of_strings)
      lines
end

type vector_zip32 = {
  ask : Sk.ask option;
  nsk : Sk.nsk option;
  ovk : Sk.ovk;
  dk : Bytes.t;
  c : Bytes.t;
  ak : Vk.ak;
  nk : Vk.nk;
  ivk : Vk.ivk;
  xsk : Sk.t option;
  xfvk : Vk.t;
  fp : Bytes.t;
  (* fingerprint ? *)
  d0 : Vk.diversifier option;
  d1 : Vk.diversifier option;
  d2 : Vk.diversifier option;
  dmax : Vk.diversifier option;
}

let vectors_zip32 =
  let open R in
  let vector_of_strings ask nsk ovk dk c ak nk ivk xsk xfvk fp d0 d1 d2 dmax =
    let opt s = if s = "None" then None else Some (ba_of_hex s) in
    let ( >?| ) x f = Stdlib.Option.map f x in
    let ( >?? ) = Stdlib.Option.bind in
    {
      ask = opt ask >?| to_ask;
      nsk = opt nsk >?| to_nsk;
      ovk = to_ovk @@ ba_of_hex ovk;
      dk = ba_of_hex dk;
      c = ba_of_hex c;
      ak = to_ak @@ ba_of_hex ak;
      nk = to_nk @@ ba_of_hex nk;
      ivk = to_ivk @@ ba_of_hex ivk;
      xsk = opt xsk >?? Sk.of_bytes;
      xfvk = Stdlib.Option.get @@ Vk.of_bytes @@ ba_of_hex xfvk;
      fp = ba_of_hex fp;
      d0 = opt d0 >?? to_diversifier;
      d1 = opt d1 >?? to_diversifier;
      d2 = opt d2 >?? to_diversifier;
      dmax = opt dmax >?? to_diversifier;
    }
  in
  (* read file in memory skipping lines with # *)
  let file = path "vectors-zip32.csv" in
  let ic = open_in file in
  let rec read_lines ls =
    try
      let line = input_line ic in
      if Str.(string_match (regexp ".*#.*") line 0) then read_lines ls
      else read_lines (line :: ls)
    with End_of_file -> List.rev ls
  in
  let lines = read_lines [] in
  List.map
    (fun line ->
      Scanf.sscanf
        line
        "%s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s@,\
        \ %s"
        vector_of_strings)
    lines

let xsks_raw =
  List.fold_left
    (fun acc v ->
      match v.xsk with
      | None -> acc
      | Some xsk ->
          xsk |> R.of_zip32_expanded_spending_key |> Sk.of_bytes
          |> Stdlib.Option.get
          |> fun xsk -> xsk :: acc)
    []
    vectors_zip32

let xsks =
  List.map
    (fun x ->
      R.of_zip32_expanded_spending_key x
      |> Core.Wallet.Spending_key.of_bytes |> Stdlib.Option.get)
    xsks_raw
