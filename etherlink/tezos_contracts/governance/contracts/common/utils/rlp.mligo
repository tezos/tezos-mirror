#import "converters.mligo" "Converters"
#import "../errors.mligo" "Errors"

let encode_length
        (length : nat)
        (offset : nat)
        : bytes =
    if length < 56n
        then Converters.nat_to_bytes (length + offset)
        else
            let length_bytes = Converters.nat_to_bytes length in
            let length_of_length = Bytes.length length_bytes in
            let _ = assert_with_error (length_of_length < 9n) Errors.input_too_long_for_rlp in
            Bytes.concat (Converters.nat_to_bytes (length_of_length + offset + 55n)) length_bytes

let encode_item
        (value : bytes)
        : bytes =
    let length = Bytes.length value in
    let prefix = if length = 1n && value < 0x80 
        then 0x
        else encode_length length 128n in
    Bytes.concat prefix value

let encode_list
        (items : bytes list)
        : bytes =
    let list_body = Bytes.concats (List.map encode_item items) in
    let list_length = Bytes.length list_body in
    let prefix = encode_length list_length 192n in
    Bytes.concat prefix list_body
