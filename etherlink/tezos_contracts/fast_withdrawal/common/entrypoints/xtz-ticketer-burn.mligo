#import "../errors.mligo" "Errors"
#import "../types/ticket.mligo" "Ticket"


(*
    `xtz-ticketer-burn` is an exchanger interface that allows burning a wrapped XTZ
    ticket and unlocking XTZ for the receiver:

    - receiver: an address that will receive the unlocked Tezos.
    - ticket: provided wrapped XTZ ticket to be burned.
*)
type t = [@layout:comb] {
    receiver: address;
    ticket: Ticket.t;
}
type t_without_annotations = address * Ticket.t

let get (xtz_ticketer : address) : t_without_annotations contract =
    match Tezos.get_entrypoint_opt "%burn" xtz_ticketer with
    | None -> failwith(Errors.invalid_xtz_ticketer)
    | Some entry -> entry

let send
        (xtz_ticketer : address)
        (params : t) : operation =
    let entry = get xtz_ticketer in
    let { receiver; ticket } = params in
    Tezos.Next.Operation.transaction (receiver, ticket) 0mutez entry
