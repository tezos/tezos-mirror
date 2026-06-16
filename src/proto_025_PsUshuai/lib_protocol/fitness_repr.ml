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

type t = {
  level : Raw_level_repr.t;
  locked_round : Round_repr.t option;
  predecessor_round : Round_repr.t;
  (* by convention, predecessor_round is 0 in case of protocol migration *)
  round : Round_repr.t;
}

let encoding =
  let open Data_encoding in
  let open Result_syntax in
  def
    "fitness"
    (conv_with_guard
       (fun {level; locked_round; predecessor_round; round} ->
         (level, locked_round, predecessor_round, round))
       (fun (level, locked_round, predecessor_round, round) ->
         match locked_round with
         | None -> return {level; locked_round; predecessor_round; round}
         | Some locked_round_val ->
             if Round_repr.(round <= locked_round_val) then
               Error "Locked round must be smaller than round."
             else return {level; locked_round; predecessor_round; round})
       (obj4
          (req "level" Raw_level_repr.encoding)
          (req "locked_round" (option Round_repr.encoding))
          (req "predecessor_round" Round_repr.encoding)
          (req "round" Round_repr.encoding)))

let pp ppf f =
  let minus_sign =
    if Round_repr.(f.predecessor_round = Round_repr.zero) then "" else "-"
  in
  let locked_round ppf locked_round =
    match locked_round with
    | None -> Format.pp_print_string ppf "unlocked"
    | Some round -> Format.fprintf ppf "locked: %a" Round_repr.pp round
  in
  Format.fprintf
    ppf
    "(%a, %a, %s%a, %a)"
    Raw_level_repr.pp
    f.level
    locked_round
    f.locked_round
    minus_sign
    Round_repr.pp
    f.predecessor_round
    Round_repr.pp
    f.round

type error +=
  | (* `Permanent *) Invalid_fitness
  | (* `Permanent *) Wrong_fitness
  | (* `Permanent *) Outdated_fitness
  | (* `Permanent *)
      Locked_round_not_less_than_round of {
      round : Round_repr.t;
      locked_round : Round_repr.t;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"invalid_fitness"
    ~title:"Invalid fitness"
    ~description:
      "Fitness representation should be exactly 4 times 4 bytes long."
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid fitness")
    Data_encoding.empty
    (function Invalid_fitness -> Some () | _ -> None)
    (fun () -> Invalid_fitness) ;
  register_error_kind
    `Permanent
    ~id:"wrong_fitness"
    ~title:"Wrong fitness"
    ~description:"Wrong fitness."
    ~pp:(fun ppf () -> Format.fprintf ppf "Wrong fitness.")
    Data_encoding.empty
    (function Wrong_fitness -> Some () | _ -> None)
    (fun () -> Wrong_fitness) ;
  register_error_kind
    `Permanent
    ~id:"outdated_fitness"
    ~title:"Outdated fitness"
    ~description:"Outdated fitness: referring to a previous version"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Outdated fitness: referring to a previous version.")
    Data_encoding.empty
    (function Outdated_fitness -> Some () | _ -> None)
    (fun () -> Outdated_fitness) ;
  register_error_kind
    `Permanent
    ~id:"locked_round_not_less_than_round"
    ~title:"Locked round not smaller than round"
    ~description:"The round is smaller than or equal to the locked round."
    ~pp:(fun ppf (round, locked_round) ->
      Format.fprintf
        ppf
        "Incorrect fitness: round %a is less than or equal to locked round %a."
        Round_repr.pp
        round
        Round_repr.pp
        locked_round)
    Data_encoding.(
      obj2
        (req "round" Round_repr.encoding)
        (req "locked_round" Round_repr.encoding))
    (function
      | Locked_round_not_less_than_round {round; locked_round} ->
          Some (round, locked_round)
      | _ -> None)
    (fun (round, locked_round) ->
      Locked_round_not_less_than_round {round; locked_round})

let create_without_locked_round ~level ~predecessor_round ~round =
  {level; locked_round = None; predecessor_round; round}

let create ~level ~locked_round ~predecessor_round ~round =
  let open Result_syntax in
  match locked_round with
  | None -> return {level; locked_round; predecessor_round; round}
  | Some locked_round_val ->
      let* () =
        error_when
          Round_repr.(round <= locked_round_val)
          (Locked_round_not_less_than_round
             {round; locked_round = locked_round_val})
      in
      return {level; locked_round; predecessor_round; round}

let int32_to_bytes i =
  let b = Bytes.make 4 '\000' in
  TzEndian.set_int32 b 0 i ;
  b

let int32_of_bytes b =
  let open Result_syntax in
  if Compare.Int.(Bytes.length b <> 4) then tzfail Invalid_fitness
  else return (TzEndian.get_int32 b 0)

(* Locked round is an option. And we want None to be smaller than any other
   value. The way the shell handles the order makes the empty Bytes smaller
   than any other *)
let locked_round_to_bytes = function
  | None -> Bytes.empty
  | Some locked_round -> int32_to_bytes (Round_repr.to_int32 locked_round)

let locked_round_of_bytes b =
  let open Result_syntax in
  match Bytes.length b with
  | 0 -> return_none
  | 4 ->
      let* r = Round_repr.of_int32 (TzEndian.get_int32 b 0) in
      return_some r
  | _ -> tzfail Invalid_fitness

(** [predecessor_round_of_bytes bytes] assumes that [bytes] encode the 32-bit
    big-endian representation of the predecessor of the opposite of the
    [predecessor_round] value (0 -> -1, 1 -> -2, etc.).

    This encoding ensures that, when compared lexicographically as raw bytes,
    the encoding of 0 is the largest possible 32-bit value (\255\255\255\255),
    1 the second largest (\255\255\255\254), and so on.

    We revert this transformation when decoding to recover the original
    [predecessor_round] value. *)
let predecessor_round_of_bytes neg_predecessor_round =
  let open Result_syntax in
  let* neg_predecessor_round = int32_of_bytes neg_predecessor_round in
  Round_repr.of_int32 @@ Int32.pred (Int32.neg neg_predecessor_round)

let round_of_bytes round =
  let open Result_syntax in
  let* value = int32_of_bytes round in
  Round_repr.of_int32 value

let to_raw {level; locked_round; predecessor_round; round} =
  [
    Bytes.of_string Constants_repr.fitness_version_number;
    int32_to_bytes (Raw_level_repr.to_int32 level);
    locked_round_to_bytes locked_round;
    (* [predecessor_round] is encoded as the 32-bit big-endian representation of
       the predecessor of the opposite of its value (0 -> -1, 1 -> -2, etc.).

       This encoding ensures that, when compared lexicographically as raw bytes,
       the encoding of 0 is the largest possible 32-bit value
       (\255\255\255\255), 1 the second largest (\255\255\255\254), etc.. *)
    int32_to_bytes
      (Int32.pred (Int32.neg (Round_repr.to_int32 predecessor_round)));
    int32_to_bytes (Round_repr.to_int32 round);
  ]

let from_raw =
  let open Result_syntax in
  function
  | [version; level; locked_round; neg_predecessor_round; round]
    when Compare.String.(
           Bytes.to_string version = Constants_repr.fitness_version_number) ->
      let* level =
        let* value = int32_of_bytes level in
        Raw_level_repr.of_int32 value
      in
      let* locked_round = locked_round_of_bytes locked_round in
      let* predecessor_round =
        predecessor_round_of_bytes neg_predecessor_round
      in
      let* round = round_of_bytes round in
      create ~level ~locked_round ~predecessor_round ~round
  | [version; _]
    when Compare.String.(
           Bytes.to_string version < Constants_repr.fitness_version_number) ->
      tzfail Outdated_fitness
  | [] (* genesis fitness *) -> tzfail Outdated_fitness
  | _ -> tzfail Invalid_fitness

let round_from_raw =
  let open Result_syntax in
  function
  | [version; _level; _locked_round; _neg_predecessor_round; round]
    when Compare.String.(
           Bytes.to_string version = Constants_repr.fitness_version_number) ->
      round_of_bytes round
  | [version; _]
    when Compare.String.(
           Bytes.to_string version < Constants_repr.fitness_version_number) ->
      return Round_repr.zero
  | [] (* genesis fitness *) -> return Round_repr.zero
  | _ -> tzfail Invalid_fitness

let predecessor_round_from_raw =
  let open Result_syntax in
  function
  | [version; _level; _locked_round; neg_predecessor_round; _round]
    when Compare.String.(
           Bytes.to_string version = Constants_repr.fitness_version_number) ->
      predecessor_round_of_bytes neg_predecessor_round
  | [version; _]
    when Compare.String.(
           Bytes.to_string version < Constants_repr.fitness_version_number) ->
      return Round_repr.zero
  | [] (* genesis fitness *) -> return Round_repr.zero
  | _ -> tzfail Invalid_fitness

let locked_round_from_raw =
  let open Result_syntax in
  function
  | [version; _level; locked_round; _neg_predecessor_round; _round]
    when Compare.String.(
           Bytes.to_string version = Constants_repr.fitness_version_number) ->
      locked_round_of_bytes locked_round
  | [version; _]
    when Compare.String.(
           Bytes.to_string version < Constants_repr.fitness_version_number) ->
      return_none
  | [] (* former genesis fitness *) -> return_none
  | _ -> tzfail Invalid_fitness

let check_except_locked_round fitness ~level ~predecessor_round =
  let {
    level = expected_level;
    locked_round = _;
    predecessor_round = expected_predecessor_round;
    round = _;
  } =
    fitness
  in
  let correct =
    Raw_level_repr.(level = expected_level)
    && Round_repr.(predecessor_round = expected_predecessor_round)
  in
  error_unless correct Wrong_fitness

let level fitness = fitness.level

let round fitness = fitness.round

let locked_round fitness = fitness.locked_round

let predecessor_round fitness = fitness.predecessor_round

module Internal_for_tests = struct
  module ListInt32Compare = Compare.List (Compare.Int32)

  let compare f ff =
    let unopt l =
      match l with Some l -> Round_repr.to_int32 l | None -> -1l
    in
    let to_list {level; locked_round; predecessor_round; round} =
      Int32.
        [
          Raw_level_repr.to_int32 level;
          unopt locked_round;
          neg (Round_repr.to_int32 predecessor_round);
          Round_repr.to_int32 round;
        ]
    in
    ListInt32Compare.compare (to_list f) (to_list ff)
end
