(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type key_group = {signatories : string list; group_treshold : int}

type key_info = {key_groups : key_group list; overall_treshold : int}

type pour_info = {pour_dest : string; pour_authorizer : string}

type t = {
  replay_counter : int;
  key_info : key_info;
  vested_balance : Tez.t;
  vesting_increment : Tez.t;
  next_payout : Ptime.t;
  payout_interval : Ptime.Span.t;
  pour_info : pour_info option;
}

let initial ?overall_treshold ?(vesting_increment = Tez.of_int 100)
    ?(next_payout = Ptime.epoch) ?(payout_interval = Ptime.Span.of_int_s 60)
    ?pour_info keys =
  let overall_treshold =
    Option.value ~default:(List.length keys - 1) overall_treshold
  in
  {
    replay_counter = 0;
    key_info =
      {
        key_groups =
          List.map (fun (ks, t) -> {signatories = ks; group_treshold = t}) keys;
        overall_treshold;
      };
    vested_balance = Tez.zero;
    vesting_increment;
    next_payout;
    payout_interval;
    pour_info;
  }

let update_keys key_groups overall_treshold s =
  {
    s with
    key_info = {key_groups; overall_treshold};
    replay_counter = s.replay_counter + 1;
  }

let increment_vested_balance s =
  {s with vested_balance = Tez.(s.vested_balance + s.vesting_increment)}

let next_payout s =
  {
    s with
    next_payout =
      Option.value
        ~default:Ptime.epoch
        (Ptime.add_span s.next_payout s.payout_interval);
  }

let pay_out amount s =
  {
    s with
    vested_balance = Tez.(s.vested_balance - amount);
    replay_counter = s.replay_counter + 1;
  }

let bump_replay_counter s = {s with replay_counter = s.replay_counter + 1}

let set_pour_info info s =
  {s with pour_info = info; replay_counter = s.replay_counter + 1}

let interval (span : Ptime.span) = Int.of_float @@ Ptime.Span.to_float_s span

let pour_info_micheline pi =
  let open Test_michelson in
  optional (fun i -> pair (str i.pour_dest) (str i.pour_authorizer)) pi

let key_groups_micheline key_groups =
  let open Test_michelson in
  list
  @@ List.map
       (fun {signatories; group_treshold} ->
         pair (list @@ List.map str signatories) (num group_treshold))
       key_groups

let key_info_micheline {key_groups; overall_treshold} =
  let open Test_michelson in
  pair (key_groups_micheline key_groups) (num overall_treshold)

let to_micheline storage =
  let open Test_michelson in
  tuple
    [
      tuple
        [
          num storage.replay_counter;
          key_groups_micheline storage.key_info.key_groups;
          num storage.key_info.overall_treshold;
        ];
      tuple
        [
          tuple [tez storage.vested_balance; tez storage.vesting_increment];
          timestamp storage.next_payout;
          num @@ interval storage.payout_interval;
        ];
      pour_info_micheline storage.pour_info;
    ]
