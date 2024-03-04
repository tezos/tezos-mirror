#import "../common/rollup.mligo" "Rollup"

module RollupMock = struct

    type storage_t = bytes
    type return_t = operation list * storage_t

    [@entry]
    let rollup
            (rollup_entry : Rollup.t)
            (_ : storage_t)
            : return_t =
        [], Rollup.decode_upgrade_payload rollup_entry
end