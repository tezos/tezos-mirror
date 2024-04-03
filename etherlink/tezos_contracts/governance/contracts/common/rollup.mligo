#import "errors.mligo" "Errors"
#import "utils/converters.mligo" "Converters"
#import "utils/rlp.mligo" "RLP"
#import "utils/byte_utils.mligo" "ByteUtils"
#import "utils/converters.mligo" "Converters"

type content_t = nat * bytes option
type ticket_t = content_t ticket
type deposit_t = bytes * ticket_t

type deposit_or_bytes_t = (
    deposit_t,
    "d",
    bytes,
    "a"
) michelson_or

type t = (
    deposit_or_bytes_t,
    "",
    bytes,
    "u"
) michelson_or


let get_entry   // NOTE: the entrypoint is used to upgrade kernel and sequencer committee as well
        (rollup : address) 
        : t contract =
    match Tezos.get_contract_opt rollup with
        | None -> failwith Errors.rollup_entrypoint_not_found
        | Some entry -> entry


let get_upgrade_params 
        (payload : bytes)
        : t =
    M_right payload


let timestamp_to_padded_little_endian_bytes
        (value : timestamp)
        : bytes =
    let timestamp_number : nat = Converters.timestamp_to_nat value in
    let timestamp_bytes = Converters.nat_to_little_endian_bytes timestamp_number in
    ByteUtils.pad_end timestamp_bytes 8n


let assert_kernel_root_hash_has_correct_length
        (kernel_root_hash : bytes)
        : unit =
    assert_with_error ((Bytes.length kernel_root_hash) = 33n) Errors.incorrect_kernel_root_hash_length


let get_kernel_upgrade_payload
        (kernel_root_hash : bytes)
        (activation_timestamp : timestamp)
        : bytes =
    let timestamp_bytes = timestamp_to_padded_little_endian_bytes activation_timestamp in
    RLP.encode_list [kernel_root_hash ; timestamp_bytes]


let assert_sequencer_upgrade_payload_is_correct
        (sequencer_pk : string)
        (pool_address : bytes)
        : unit =
    let sequencer_pk_length = String.length sequencer_pk in
    let _ = assert_with_error ((sequencer_pk_length = 54n) || (sequencer_pk_length = 55n)) Errors.incorrect_sequencer_pk_length in
    assert_with_error ((Bytes.length pool_address) = 20n) Errors.incorrect_pool_address_length


let public_key_to_bytes
        (public_key : string)
        : bytes =
    let michelson_bytes = Bytes.pack public_key in
    let length = Converters.bytes_to_nat (Bytes.sub 2n 4n michelson_bytes) in
    Bytes.sub 6n length michelson_bytes


let get_sequencer_upgrade_payload
        (sequencer_pk : string)
        (pool_address : bytes)
        (activation_timestamp : timestamp)
        : bytes =
    let sequencer_pk_bytes = public_key_to_bytes sequencer_pk in
    let timestamp_bytes = timestamp_to_padded_little_endian_bytes activation_timestamp in
    RLP.encode_list [sequencer_pk_bytes ; pool_address ; timestamp_bytes]


let decode_upgrade_payload
        (rollup_entry : t)
        : bytes =
     match rollup_entry with
        | M_right bytes -> bytes
        | M_left _ -> failwith Errors.wrong_rollup_entrypoint


let get_activation_timestamp
        (adoption_period_sec : nat)
        : timestamp =
    Tezos.get_now () + (int adoption_period_sec)
