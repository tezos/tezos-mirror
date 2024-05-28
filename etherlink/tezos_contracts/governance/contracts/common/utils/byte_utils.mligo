let pad_end
        (source : bytes)
        (target_length : nat)
        : bytes =
    let mut res = source in
    let source_length = int (Bytes.length source) in
    let _ = for _i = source_length upto target_length - 1 do
        res := Bytes.concat res 0x00
    done in
    res