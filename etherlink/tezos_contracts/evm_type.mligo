(* SPDX-CopyrightText Nomadic Labs <contact@nomadic-labs.com> *)

type evm =
[@layout tree]
| [@annot:""] DepositTicket of (bytes * tez_ticket)
| [@annot:""] Upgrade of bytes
| [@annot:""] Other of bytes
