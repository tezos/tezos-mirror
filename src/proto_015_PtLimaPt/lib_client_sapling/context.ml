(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Tezos_sapling.Core.Client

let _ = Random.self_init ()

module Tez = Protocol.Alpha_context.Tez

module Shielded_tez : sig
  type t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val zero : t

  val of_mutez : int64 -> t option

  val to_mutez : t -> int64

  val of_tez : Tez.t -> t

  val ( +? ) : t -> t -> t tzresult

  val ( -? ) : t -> t -> t tzresult
end = struct
  include Tez

  let ( +? ) a b = a +? b |> Environment.wrap_tzresult

  let ( -? ) a b = a -? b |> Environment.wrap_tzresult

  let of_tez t =
    let i = Tez.to_mutez t in
    assert (UTXO.valid_amount i) ;
    WithExceptions.Option.get ~loc:__LOC__ @@ of_mutez i
end

let sapling_transaction_as_arg t =
  let pp_michelson ppf t =
    let open Tezos_micheline in
    let list_of_transactions_expr =
      let transaction_expr =
        Micheline.Bytes
          (0, Data_encoding.Binary.to_bytes_exn UTXO.transaction_encoding t)
      in
      Micheline.strip_locations @@ Micheline.Seq (0, [transaction_expr])
    in
    Michelson_v1_printer.print_expr ppf list_of_transactions_expr
  in
  Format.asprintf "%a" pp_michelson t

(** The inputs and outputs are shuffled to prevent meta-data analysis. **)
module Shuffle = struct
  let list l =
    let a = Array.of_list l in
    let len = Array.length a in
    for i = len downto 2 do
      let idx = Random.int i in
      let swp_idx = i - 1 in
      let tmp = a.(swp_idx) in
      a.(swp_idx) <- a.(idx) ;
      a.(idx) <- tmp
    done ;
    Array.to_list a

  let pair x y = if Random.bool () then [y; x] else [x; y]
end

type error += Balance_too_low of Shielded_tez.t * Shielded_tez.t

let register_error_kind category ~id ~title ~description ?pp encoding from_error
    to_error =
  let id = "client_sapling." ^ Protocol.name ^ "." ^ id in
  register_error_kind
    category
    ~id
    ~title
    ~description
    ?pp
    encoding
    from_error
    to_error

let () =
  register_error_kind
    `Temporary
    ~id:"balance_too_low"
    ~title:"Balance too low"
    ~description:"The sender contract does not have enough tokens."
    ~pp:(fun ppf (balance, amount) ->
      Format.fprintf
        ppf
        "@[<h>Balance too low (%a) to spend %a@]"
        Shielded_tez.pp
        balance
        Shielded_tez.pp
        amount)
    Data_encoding.(
      obj2
        (req "actual_balance" Shielded_tez.encoding)
        (req "amount" Shielded_tez.encoding))
    (function
      | Balance_too_low (balance, amount) -> Some (balance, amount) | _ -> None)
    (fun (balance, amount) -> Balance_too_low (balance, amount))

module Storage = Tezos_sapling.Storage
module F = Tezos_sapling.Forge

module Input_set = struct
  include Set.Make (F.Input)

  let to_list = elements

  let pp_f pp i =
    Format.fprintf
      pp
      "@[<h>%s %Ld@]"
      (Tezos_crypto.Base58.simple_encode
         Viewing_key.address_b58check_encoding
         (F.Input.address i))
      (F.Input.amount i)
end

module Account = struct
  type t = {
    vk : Viewing_key.t;
    unspents : Input_set.t;
    balance : Shielded_tez.t;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun cs -> (cs.vk, Input_set.to_list cs.unspents, cs.balance))
      (fun (vk, unspents, balance) ->
        {vk; unspents = Input_set.of_list unspents; balance})
      (obj3
         (req "vk" Viewing_key.encoding)
         (req "unspents" (list F.Input.encoding))
         (req "balance" Shielded_tez.encoding))

  let create vk = {vk; unspents = Input_set.empty; balance = Shielded_tez.zero}

  let balance c = c.balance

  let add_unspent c input =
    let amount =
      WithExceptions.Option.get ~loc:__LOC__
      @@ Shielded_tez.of_mutez (F.Input.amount input)
    in
    match Shielded_tez.(c.balance +? amount) with
    | Error _ -> assert false (* overflow *)
    | Ok balance ->
        let unspents = Input_set.add input c.unspents in
        {c with balance; unspents}

  let remove_unspent c input =
    let amount =
      WithExceptions.Option.get ~loc:__LOC__
      @@ Shielded_tez.of_mutez (F.Input.amount input)
    in
    match Shielded_tez.(c.balance -? amount) with
    | Error _ -> assert false (* negative balance *)
    | Ok balance ->
        let unspents = Input_set.remove input c.unspents in
        {c with balance; unspents}

  let filter_spent account storage =
    Input_set.fold
      (fun input acc ->
        if F.Input.is_spent input storage account.vk then
          remove_unspent acc input
        else acc)
      account.unspents
      account

  let pick_input c =
    let ( >?| ) x f = Option.map f x in
    Input_set.choose c.unspents >?| fun unspent ->
    let c = remove_unspent c unspent in
    (unspent, c)

  let pp_unspent : Format.formatter -> t -> unit =
   fun ppf a ->
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut Input_set.pp_f ppf)
      (Input_set.elements a.unspents)
end

module Contract_state = struct
  module Accounts = struct
    include Set.Make (struct
      type t = Account.t

      let compare a b =
        let open Account in
        Bytes.compare (Viewing_key.to_bytes a.vk) (Viewing_key.to_bytes b.vk)
    end)

    let replace a set = add a (remove a set)

    let find vk accounts = find (Account.create vk) accounts
  end

  let accounts_encoding =
    let open Data_encoding in
    conv
      Accounts.elements
      (List.fold_left (fun m e -> Accounts.add e m) Accounts.empty)
      (list Account.encoding)

  type t = {accounts : Accounts.t; storage : Storage.state}

  let encoding =
    let open Data_encoding in
    conv
      (fun t -> (t.accounts, t.storage))
      (fun (accounts, storage) -> {accounts; storage})
      (obj2
         (req "accounts" accounts_encoding)
         (req "storage" Storage.state_encoding))

  let empty ~memo_size =
    {accounts = Accounts.empty; storage = Storage.empty ~memo_size}

  let find_account vk contract_state = Accounts.find vk contract_state.accounts

  let init ~force vk state =
    Accounts.find vk state.accounts |> function
    | None ->
        let accounts = Accounts.add (Account.create vk) state.accounts in
        return {state with accounts}
    | Some _ ->
        if force then
          let accounts = Accounts.add (Account.create vk) state.accounts in
          return {state with accounts}
        else failwith "vk already present"

  let add_unspent vk input accounts =
    let account =
      Accounts.find vk accounts |> WithExceptions.Option.get ~loc:__LOC__
    in
    let account = Account.add_unspent account input in
    Accounts.replace account accounts

  (** Scan the Sapling storage of a smart contract and update the accounts of
      all known viewing keys for that contract  *)
  let scan state storage =
    (* remove newly spent inputs *)
    let accounts =
      Accounts.map
        (fun account -> Account.filter_spent account storage)
        state.accounts
    in
    (* get all the vks that need to be scanned for *)
    let vks =
      Accounts.fold (fun account acc -> Account.(account.vk) :: acc) accounts []
    in
    let size, _ = Storage.size storage in
    let rec aux pos accounts =
      if pos < size then
        (* try to decrypt each inputs with all vks *)
        List.fold_left
          (fun acc vk ->
            match F.Input.get storage pos vk with
            | None -> acc
            | Some input -> (vk, input) :: acc)
          []
          vks
        |> function
        | [] -> aux (Int64.succ pos) accounts
        | [(vk, (_message, forge_input))] ->
            let is_spent = F.Input.is_spent forge_input storage vk in
            if is_spent then aux (Int64.succ pos) accounts
            else aux (Int64.succ pos) (add_unspent vk forge_input accounts)
        | _ -> assert false (* got more than one decrypting key *)
      else accounts
    in
    let current_size, _ = Storage.size state.storage in
    let accounts = aux current_size accounts in
    {accounts; storage}

  (** Update the Sapling storage of a smart contract using a diff, checking that
      the resulting Merkle tree has a root equal to the one in the diff. *)
  let update_storage contract_state (root, diff) =
    let open Protocol.Alpha_context.Sapling in
    let storage =
      Tezos_sapling.Storage.add
        contract_state.storage
        diff.commitments_and_ciphertexts
    in
    let computed_root = Storage.get_root storage in
    if computed_root <> root then
      Stdlib.failwith "Commitment tree inconsistent wrt to node."
    else
      let storage =
        List.fold_left
          (fun s nf -> Storage.add_nullifier s nf)
          storage
          diff.nullifiers
      in
      scan contract_state storage
end

module Client_state = struct
  module Map = Map.Make (Protocol.Contract_hash)

  type t = Contract_state.t Map.t

  let encoding =
    let open Data_encoding in
    conv
      Map.bindings
      (List.fold_left (fun m (k, v) -> Map.add k v m) Map.empty)
      (list
         (obj2
            (req "contract" Protocol.Alpha_context.Contract.originated_encoding)
            (req "state" Contract_state.encoding)))

  let filename = "sapling_state"

  let load (cctxt : #Client_context.wallet) =
    cctxt#load filename ~default:Map.empty encoding

  let write (cctxt : #Client_context.wallet) t = cctxt#write filename t encoding

  let get_or_init ~default_memo_size contract client_state =
    Map.find contract client_state |> function
    | None -> (
        match default_memo_size with
        | None ->
            failwith
              "Unknown memo size for contract %s and none was provided in \
               options"
            @@ Protocol.Contract_hash.to_b58check contract
        | Some memo_size ->
            let contract_state = Contract_state.empty ~memo_size in
            let client_state = Map.add contract contract_state client_state in
            return (contract_state, client_state))
    | Some contract_state -> return (contract_state, client_state)

  let register cctxt ~force ~default_memo_size contract vk =
    load cctxt >>=? fun client_state ->
    get_or_init ~default_memo_size contract client_state
    >>=? fun (contract_state, client_state) ->
    Contract_state.init ~force vk contract_state >>=? fun contract_state ->
    let client_state = Map.add contract contract_state client_state in
    write cctxt client_state

  let find (cctxt : #Client_context.full) contract state =
    Map.find contract state |> function
    | None ->
        cctxt#error
          "Contract %s not found"
          (Protocol.Contract_hash.to_b58check contract)
    | Some v -> return v

  (** Call the node RPC to obtain the storage diff of a contract *)
  let get_diff cctxt contract offset_commitment offset_nullifier =
    Protocol.Alpha_services.Contract.single_sapling_get_diff
      cctxt
      (cctxt#chain, cctxt#block)
      contract
      ~offset_commitment
      ~offset_nullifier
      ()

  let sync_and_scan cctxt contract =
    load cctxt >>=? fun state ->
    find cctxt contract state >>=? fun contract_state ->
    let cm_pos, nf_pos = Storage.size contract_state.storage in
    get_diff cctxt contract cm_pos nf_pos >>=? fun diff ->
    let contract_state = Contract_state.update_storage contract_state diff in
    let state = Map.add contract contract_state state in
    write cctxt state >>=? fun () -> return contract_state
end

(** Truncate or pad the message to fit the memo_size *)
let adjust_message_length (cctxt : #Client_context.full) ?message memo_size =
  match message with
  | None ->
      cctxt#warning
        "no message provided, adding a zeroes filled message of the required \
         length: %d "
        memo_size
      >|= fun () -> Bytes.make memo_size '\000'
  | Some message ->
      let message_length = Bytes.length message in
      if message_length = memo_size then Lwt.return message
      else if message_length > memo_size then
        cctxt#warning
          "Your message is too long (%d bytes) and will therefore be truncated \
           to %d bytes"
          message_length
          memo_size
        >|= fun () -> Bytes.sub message 0 memo_size
      else
        cctxt#warning
          "Your message is too short (%d bytes) and will therefore be \
           right-padded with zero bytes to reach a %d-byte length"
          message_length
          memo_size
        >|= fun () ->
        Bytes.cat message (Bytes.make (memo_size - message_length) '\000')

let create_payment ~message dst amount =
  let amount = Shielded_tez.to_mutez amount in
  F.make_output dst amount message

(** Return a list of inputs belonging to an account sufficient to cover an
    amount, together with the change remaining. *)
let get_shielded_amount amount account =
  let balance = Account.balance account in
  error_unless (balance >= amount) (Balance_too_low (balance, amount))
  >|? fun () ->
  let to_pay = Shielded_tez.to_mutez amount in
  let inputs_to_spend = [] in
  let rec loop to_pay chosen_inputs account =
    if Int64.(compare to_pay zero) > 0 then
      Account.pick_input account |> function
      | None ->
          Stdlib.failwith "Not enough inputs" (* TODO raise a proper error *)
      | Some (next_in, account) ->
          let next_val = F.Input.amount next_in in
          let rest_to_pay = Int64.sub to_pay next_val in
          loop rest_to_pay (next_in :: chosen_inputs) account
    else
      let change =
        WithExceptions.Option.get ~loc:__LOC__
        @@ Shielded_tez.of_mutez @@ Int64.abs to_pay
      in
      (chosen_inputs, change)
  in
  loop to_pay inputs_to_spend account

let create_payback ~memo_size address amount =
  let plaintext_message = Bytes.make memo_size '\000' in
  let amount = Shielded_tez.to_mutez amount in
  F.make_output address amount plaintext_message

(* The caller should check that the account exists already *)
let unshield ~src ~bound_data ~backdst amount (state : Contract_state.t)
    anti_replay =
  let vk = Viewing_key.of_sk src in
  let account =
    Contract_state.find_account vk state
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  get_shielded_amount amount account >|? fun (inputs, change) ->
  let memo_size = Storage.get_memo_size state.storage in
  let payback = create_payback ~memo_size backdst change in
  F.forge_transaction
    (Shuffle.list inputs)
    [payback]
    src
    anti_replay
    ~bound_data
    state.storage

let shield cctxt ~dst ?message amount (state : Contract_state.t) anti_replay =
  let shielded_amount = Shielded_tez.of_tez amount in
  let memo_size = Storage.get_memo_size Contract_state.(state.storage) in
  adjust_message_length cctxt ?message memo_size >>= fun message ->
  let payment = create_payment ~message dst shielded_amount in
  let negative_amount = Int64.neg (Tez.to_mutez amount) in
  return
  @@ F.forge_shield_transaction
       [payment]
       negative_amount
       anti_replay
       ~bound_data:""
       Contract_state.(state.storage)

(* The caller should check that the account exists already *)
let transfer cctxt ~src ~dst ~backdst ?message amount (state : Contract_state.t)
    anti_replay =
  let vk = Viewing_key.of_sk src in
  let account =
    Contract_state.find_account vk state
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  let memo_size = Storage.get_memo_size state.storage in
  adjust_message_length cctxt ?message memo_size >|= fun message ->
  get_shielded_amount amount account >|? fun (inputs, change) ->
  let payment = create_payment ~message dst amount in
  let payback = create_payback ~memo_size backdst change in
  let sapling_transaction =
    F.forge_transaction
      (Shuffle.list inputs)
      (Shuffle.pair payback payment)
      src
      anti_replay
      ~bound_data:""
      state.storage
  in
  sapling_transaction
