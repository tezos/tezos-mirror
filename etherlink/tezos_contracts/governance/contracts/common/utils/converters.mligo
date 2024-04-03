#import "../errors.mligo" "Errors"

[@inline]
let nat_to_bytes 
        (value : nat) 
        : bytes =
    [%Michelson ({| { BYTES } |} : nat -> bytes)] value

let bytes_to_nat 
        (value : bytes) 
        : nat =
    [%Michelson ({| { NAT } |} : bytes -> nat)] value

let nat_to_little_endian_bytes
        (value : nat)
        : bytes = 
    let bytes = nat_to_bytes value in
    let mut res : bytes = 0x in
    let bytes_length = Bytes.length bytes in
    let _ = for i = 0 upto bytes_length - 1 do
        let index : nat = Option.value_with_error Errors.index_not_nat (is_nat i) in
        res := Bytes.concat (Bytes.sub index 1n bytes) res
    done in
    res

let timestamp_to_nat
        (value : timestamp)
        : nat =
    Option.value_with_error Errors.negative_timestamp (is_nat (value - (0 : timestamp)))

let address_to_key_hash
        (address : address)
        : key_hash =
    (*
        NOTE:
        This is a workaround solution. 
        Use the IS_IMPLICIT_ACCOUNT instruction of type address -> option key_hash when it will be available
        (See: https://gitlab.com/tezos/tezos/-/merge_requests/12436)
        Or pass address instead of key_hash to Tezos.voting_power instruction when it will be available
        (See: https://gitlab.com/tezos/tezos/-/merge_requests/12425)
        
        Explanation of workaround
        Bytes.pack for an address can return the following variants depending on address type 
            tz1NyAf1KeeFCCPPAZ9ard9YVshVGFibzVKa -> 0x050a0000001600002486eda3c7bbbe6be511b46d6deeb1594258a7fd
            tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc -> 0x050a000000160001e5c6d1f726796e98b2bad2a819a36f742b2fe25b
            tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA -> 0x050a0000001600026c9b3ad59e0f8bdc2bd2011675825f9f547131da
            tz4Jxn8MpRndqWUzkuZbQKmE3aNWJzYsSEso -> 0x050a0000001600036d39817a1e9a4a66bd28c2f2bb430a04b2d93ecf
            KT1GzjQs7HLLVGG95GxURZXAPqEquAVyYD4c -> 0x050a00000016015c49311596d2b140b04401f2824b8ff4fe1256a200
        
            the template for address in michelson bytes is 0xMMTT(LLx4)AA(DDx21)
                MM      - Michelson bytes mark (0x05)
                TT      - Michelson type (0x0a - address)
                LLx4    - Data length (0x00000016 - 22 bytes)
                AA      - Address Type (0x00 - implicit, 0x01 - originated)
                DDx21   - Address Data

        Bytes.pack for a key hash can return the following variants depending on address type 
            tz1NyAf1KeeFCCPPAZ9ard9YVshVGFibzVKa -> 0x050a00000015002486eda3c7bbbe6be511b46d6deeb1594258a7fd
            tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc -> 0x050a0000001501e5c6d1f726796e98b2bad2a819a36f742b2fe25b
            tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA -> 0x050a00000015026c9b3ad59e0f8bdc2bd2011675825f9f547131da
            tz4Jxn8MpRndqWUzkuZbQKmE3aNWJzYsSEso -> 0x050a00000015036d39817a1e9a4a66bd28c2f2bb430a04b2d93ecf

            the template for key hash in michelson bytes is 0xMMTT(LLx4)(DDx21)
                MM      - Michelson bytes mark (0x05)
                TT      - Michelson type (0x0a - address)
                LLx4    - Data length (0x00000015 - 21 bytes)
                DDx21   - Address Data

        so it's enough to check, that AA byte == 0x00, remove it, and update LL bytes to reflect that data length is 21 byte now 
    *)
    let address_packed = Bytes.pack address in
    let address_type = Bytes.sub 6n 1n address_packed in
    let _ = assert_with_error (address_type = 0x00) Errors.not_implicit_address in
    let length = 0x00000015 in
    let key_hash_packed = Bytes.concats [(Bytes.sub 0n 2n address_packed); length; (Bytes.sub 7n 21n address_packed)] in
    let key_hash = Option.value_with_error Errors.failed_to_cast_address_to_key_hash (Bytes.unpack key_hash_packed) in
    let _ = assert_with_error (Tezos.address (Tezos.implicit_account key_hash) = address) Errors.key_hash_not_equal_to_source_address in
    key_hash