#import "./ticket.mligo" "Ticket"

(*
    Key used in the FastWithdrawal contract ledger
*)
type t = {
    withdrawal_id : nat;
    full_amount : nat;
    ticketer : address;
    content : Ticket.content_t;
    timestamp : timestamp;
    base_withdrawer : address;
    payload : bytes;
    l2_caller : bytes;
}
