#import "../types/ticket.mligo" "Ticket"
#import "../types/fast-withdrawal.mligo" "FastWithdrawal"
#import "../errors.mligo" "Errors"


(*
    `settle-withdrawal` is the fast withdrawal interface used for processing
    XTZ-native tickets from the Etherlink rollup during fast withdrawals:

    - withdrawal_id: a unique identifier for the withdrawal,
    - ticket: the provided XTZ-native ticket to be burned,
    - timestamp: the time when withdrawal was applied on the Etherlink side,
    - base_withdrawer: the target receiver of the fast withdrawal,
    - payload: additional data containing the fast withdrawal conditions,
    - l2_caller: the original sender address on the Etherlink side.
 *)
type t = {
    withdrawal_id : nat;
    ticket : Ticket.t;
    timestamp : timestamp;
    base_withdrawer : address;
    payload : bytes;
    l2_caller : bytes;
}

let get (fast_withdrawal_contract : address) : t contract =
    match Tezos.get_contract_opt fast_withdrawal_contract with
    | None -> failwith(Errors.settle_withdrawal_entrypoint_not_found)
    | Some entry -> entry

let send
        (fast_withdrawal_contract : address)
        (params : t) : operation =
    let entry = get fast_withdrawal_contract in
    Tezos.Next.Operation.transaction params 0mutez entry

let from_key
        (ticket : Ticket.t)
        (key : FastWithdrawal.t) : t =
    let {
        withdrawal_id;
        full_amount = _;
        ticketer = _;
        content = _;
        timestamp;
        base_withdrawer;
        payload;
        l2_caller
    } = key in {
        withdrawal_id;
        ticket;
        timestamp;
        base_withdrawer;
        payload;
        l2_caller
    }

let to_key_and_ticket (params : t) : Ticket.t * FastWithdrawal.t =
    let {
        withdrawal_id;
        ticket;
        timestamp;
        base_withdrawer;
        payload;
        l2_caller
    } = params in
    let (ticketer, (content, full_amount)), ticket = Tezos.Next.Ticket.read ticket in
    let key = {
        withdrawal_id;
        full_amount;
        ticketer;
        content;
        timestamp;
        base_withdrawer;
        payload;
        l2_caller
    } in (ticket, key)
