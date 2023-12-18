# Instruction coverage

This file documents Michelson instruction coverage by tzt tests as of protocol
Nairobi.

## Control structures

### `APPLY`

- [apply_00.tzt](apply_00.tzt)

Does not check the behavior that the values that are not both pushable and
storable cannot be captured.

### `EXEC`

- [exec_00.tzt](exec_00.tzt)
- [exec_01.tzt](exec_01.tzt)
- [exec_02.tzt](exec_02.tzt)
- [exec_03.tzt](exec_03.tzt)

### `FAILWITH`

- [failwith_00.tzt](failwith_00.tzt)

### `IF`

- [if_00.tzt](if_00.tzt)
- [if_01.tzt](if_01.tzt)

These tests do not check that the non-participating end of the stack, if it
exists, is preserved.

### `IF_CONS`

- [ifcons_listint_00.tzt](ifcons_listint_00.tzt)
- [ifcons_listint_01.tzt](ifcons_listint_01.tzt)
- [ifcons_listnat_00.tzt](ifcons_listnat_00.tzt)
- [ifcons_listnat_01.tzt](ifcons_listnat_01.tzt)

These tests do not check that the non-participating end of the stack, if it
exists, is preserved.

### `IF_LEFT`

- [ifleft_orintstring_00.tzt](ifleft_orintstring_00.tzt)
- [ifleft_orstringint_00.tzt](ifleft_orstringint_00.tzt)

These tests do not check that the non-participating end of the stack, if it
exists, is preserved.
### `IF_NONE`

- [ifnone_optionint_00.tzt](ifnone_optionint_00.tzt)
- [ifnone_optionnat_00.tzt](ifnone_optionnat_00.tzt)

These tests do not check that the non-participating end of the stack, if it
exists, is preserved.

### `LAMBDA`

***None***

### `LAMBDA_REC`

***None***

### `LOOP`

- [loop_00.tzt](loop_00.tzt)
- [loop_01.tzt](loop_01.tzt)
- [loop_02.tzt](loop_02.tzt)

### `LOOP_LEFT`

- [loopleft_00.tzt](loopleft_00.tzt)
- [loopleft_01.tzt](loopleft_01.tzt)
- [loopleft_02.tzt](loopleft_02.tzt)
- [loopleft_03.tzt](loopleft_03.tzt)
- [loopleft_04.tzt](loopleft_04.tzt)

### `;`

***None***

Instruction sequencing is indirectly covered by tzts including multiple instructions, but there are no dedicated unit tests.

### `{}`

***None***

## Stack manipulation

### `DIG`

- [dig_00.tzt](dig_00.tzt)
- [dig_01.tzt](dig_01.tzt)
- [dig_02.tzt](dig_02.tzt)
- [dig_03.tzt](dig_03.tzt)
- [dig_04.tzt](dig_04.tzt)

Even numbers are conspicuous by their absence.

### `DIP`

- [dip_00.tzt](dip_00.tzt)
- [dip_01.tzt](dip_01.tzt)
- [dip_02.tzt](dip_02.tzt)

DIP is used in quite a few other tests as an utility, so it's indirectly
covered.

### `DIP n`

- [dipn_00.tzt](dipn_00.tzt)
- [dipn_01.tzt](dipn_01.tzt)
- [dipn_02.tzt](dipn_02.tzt)
- [dipn_03.tzt](dipn_03.tzt)

### `DROP`

- [drop_00.tzt](drop_00.tzt)

Used a few times as an utility in other tzts.

### `DROP n`

- [dropn_00.tzt](dropn_00.tzt)
- [dropn_01.tzt](dropn_01.tzt)
- [dropn_02.tzt](dropn_02.tzt)
- [dropn_03.tzt](dropn_03.tzt)

### `DUG`

- [dugn_00.tzt](dugn_00.tzt)

`DUG 0`, `DUG 1` edge cases are missing.

### `DUP`

- [dup_00.tzt](dup_00.tzt)

### `DUP n`

- [dupn_00.tzt](dupn_00.tzt)
- [dupn_01.tzt](dupn_01.tzt)
- [dupn_02.tzt](dupn_02.tzt)
- [dupn_03.tzt](dupn_03.tzt)
- [dupn_04.tzt](dupn_04.tzt)

### `PUSH`

- [push_int_00.tzt](push_int_00.tzt)
- [push_string_00.tzt](push_string_00.tzt)

`PUSH` is used quite a bit as a utility in other tzts, hence indirectly covered
to some extent. Not all pushable types are tested with `PUSH`, however.

### `SWAP`

- [swap_00.tzt](swap_00.tzt)

## Arithmetic

### `ABS`

- [abs_00.tzt](abs_00.tzt)
- [abs_01.tzt](abs_01.tzt)
- [abs_02.tzt](abs_02.tzt)

### `ADD: nat : nat`

- [add_nat-nat_00.tzt](add_nat-nat_00.tzt)

### `ADD: nat : int`

- [add_nat-int_00.tzt](add_nat-int_00.tzt)

### `ADD: int : nat`

- [add_int-nat_00.tzt](add_int-nat_00.tzt)
- [add_int-nat_01.tzt](add_int-nat_01.tzt)

### `ADD: int : int`

- [add_int-int_00.tzt](add_int-int_00.tzt)

### `ADD: timestamp : int`

- [add_timestamp-int_00.tzt](add_timestamp-int_00.tzt)
- [add_timestamp-int_01.tzt](add_timestamp-int_01.tzt)
- [add_timestamp-int_02.tzt](add_timestamp-int_02.tzt)
- [add_timestamp-int_03.tzt](add_timestamp-int_03.tzt) -- doesn't use `ADD` instruction, only testing `timestamp`

### `ADD: int : timestamp`

- [add_int-timestamp_00.tzt](add_int-timestamp_00.tzt)

### `ADD: mutez : mutez`

- [add_mutez-mutez_00.tzt](add_mutez-mutez_00.tzt)
- [add_mutez-mutez_01.tzt](add_mutez-mutez_01.tzt)

### `ADD: bls12_381_g1 : bls12_381_g1`

***None***

### `ADD: bls12_381_g2 : bls12_381_g2`

***None***

### `ADD: bls12_381_fr : bls12_381_fr`

***None***

### `BYTES: int`

***None***

### `BYTES: nat`

***None***

### `COMPARE`

- [compare_bool_00.tzt](compare_bool_00.tzt)
- [compare_bool_01.tzt](compare_bool_01.tzt)
- [compare_bool_02.tzt](compare_bool_02.tzt)
- [compare_bool_03.tzt](compare_bool_03.tzt)
- [compare_bytes_00.tzt](compare_bytes_00.tzt)
- [compare_bytes_01.tzt](compare_bytes_01.tzt)
- [compare_bytes_02.tzt](compare_bytes_02.tzt)
- [compare_bytes_03.tzt](compare_bytes_03.tzt)
- [compare_bytes_04.tzt](compare_bytes_04.tzt)
- [compare_int_00.tzt](compare_int_00.tzt)
- [compare_int_01.tzt](compare_int_01.tzt)
- [compare_int_02.tzt](compare_int_02.tzt)
- [compare_int_03.tzt](compare_int_03.tzt)
- [compare_int_04.tzt](compare_int_04.tzt)
- [compare_keyhash_00.tzt](compare_keyhash_00.tzt)
- [compare_keyhash_01.tzt](compare_keyhash_01.tzt)
- [compare_keyhash_02.tzt](compare_keyhash_02.tzt)
- [compare_mutez_00.tzt](compare_mutez_00.tzt)
- [compare_mutez_01.tzt](compare_mutez_01.tzt)
- [compare_mutez_02.tzt](compare_mutez_02.tzt)
- [compare_mutez_03.tzt](compare_mutez_03.tzt)
- [compare_mutez_04.tzt](compare_mutez_04.tzt)
- [compare_mutez_05.tzt](compare_mutez_05.tzt)
- [compare_nat_00.tzt](compare_nat_00.tzt)
- [compare_nat_01.tzt](compare_nat_01.tzt)
- [compare_nat_02.tzt](compare_nat_02.tzt)
- [compare_nat_03.tzt](compare_nat_03.tzt)
- [compare_nat_04.tzt](compare_nat_04.tzt)
- [compare_nat_05.tzt](compare_nat_05.tzt)
- [compare_pairintint_00.tzt](compare_pairintint_00.tzt)
- [compare_pairintint_01.tzt](compare_pairintint_01.tzt)
- [compare_pairintint_02.tzt](compare_pairintint_02.tzt)
- [compare_pairintint_03.tzt](compare_pairintint_03.tzt)
- [compare_string_00.tzt](compare_string_00.tzt)
- [compare_string_01.tzt](compare_string_01.tzt)
- [compare_string_02.tzt](compare_string_02.tzt)
- [compare_string_03.tzt](compare_string_03.tzt)
- [compare_string_04.tzt](compare_string_04.tzt)
- [compare_timestamp_00.tzt](compare_timestamp_00.tzt)
- [compare_timestamp_01.tzt](compare_timestamp_01.tzt)
- [compare_timestamp_02.tzt](compare_timestamp_02.tzt)
- [compare_timestamp_03.tzt](compare_timestamp_03.tzt)
- [compare_timestamp_04.tzt](compare_timestamp_04.tzt)
- [compare_timestamp_05.tzt](compare_timestamp_05.tzt)

Missing edge cases:

- No comparison of negative integers
- No comparison for `0 int`
- No comparison for `0 mutez`
- No comparison for `0 nat`
- Only zero- or single-character strings are compared

Duplicate files:

- `compare_mutez_03.tzt` is a duplicate of `compare_mutez_00.tzt`
- `compare_nat_03.tzt` is a duplicate of `compare_nat_00.tzt`

Types `COMPARE` isn't tested for:

- `address`
- `chain_id`
- `key`
- `never`
- `signature`
- `timestamp`
- `unit`
- `or`
- `option`

### `EDIV: nat : nat`

***None***

### `EDIV: nat : int`

***None***

### `EDIV: int : nat`

***None***

### `EDIV: int : int`

- [ediv_int-int_00.tzt](ediv_int-int_00.tzt)
- [ediv_int-int_01.tzt](ediv_int-int_01.tzt)
- [ediv_int-int_02.tzt](ediv_int-int_02.tzt)
- [ediv_int-int_03.tzt](ediv_int-int_03.tzt)

Missing edge cases:

- No division of positive over positive
- No division of negative over negative
- No division of zero over non-zero
- No division of zero over zero
- No division with the result of 1

### `EDIV: mutez : nat`

- [ediv_mutez-nat_00.tzt](ediv_mutez-nat_00.tzt)
- [ediv_mutez-nat_01.tzt](ediv_mutez-nat_01.tzt)
- [ediv_mutez-nat_02.tzt](ediv_mutez-nat_02.tzt)
- [ediv_mutez-nat_03.tzt](ediv_mutez-nat_03.tzt)
- [ediv_mutez-nat_04.tzt](ediv_mutez-nat_04.tzt)
- [ediv_mutez-nat_05.tzt](ediv_mutez-nat_05.tzt)
- [ediv_mutez-nat_06.tzt](ediv_mutez-nat_06.tzt)

### `EDIV: mutez : mutez`

- [ediv_mutez-mutez_00.tzt](ediv_mutez-mutez_00.tzt)
- [ediv_mutez-mutez_01.tzt](ediv_mutez-mutez_01.tzt)
- [ediv_mutez-mutez_02.tzt](ediv_mutez-mutez_02.tzt)
- [ediv_mutez-mutez_03.tzt](ediv_mutez-mutez_03.tzt)

Missing edge cases:

- No division of zero over non-zero
- No division of zero over zero

### `EQ`

- [eq_00.tzt](eq_00.tzt)
- [eq_01.tzt](eq_01.tzt)
- [eq_02.tzt](eq_02.tzt)
- [eq_03.tzt](eq_03.tzt)
- [eq_04.tzt](eq_04.tzt)

### `GE`

- [ge_00.tzt](ge_00.tzt)
- [ge_01.tzt](ge_01.tzt)
- [ge_02.tzt](ge_02.tzt)
- [ge_03.tzt](ge_03.tzt)
- [ge_04.tzt](ge_04.tzt)

### `GT`

- [gt_00.tzt](gt_00.tzt)
- [gt_01.tzt](gt_01.tzt)
- [gt_02.tzt](gt_02.tzt)
- [gt_03.tzt](gt_03.tzt)
- [gt_04.tzt](gt_04.tzt)

### `INT: nat`

- [int_nat_00.tzt](int_nat_00.tzt)
- [int_nat_01.tzt](int_nat_01.tzt)

### `INT: bls12_381_fr`

***None***

### `INT: bytes`

***None***

### `ISNAT`

- [isnat_00.tzt](isnat_00.tzt)
- [isnat_01.tzt](isnat_01.tzt)

Missing edge cases:

- Only tests `0` and `-1`, missing tests for positive integers.

### `LE`

- [le_00.tzt](le_00.tzt)
- [le_01.tzt](le_01.tzt)
- [le_02.tzt](le_02.tzt)
- [le_03.tzt](le_03.tzt)
- [le_04.tzt](le_04.tzt)

### `LSL: nat : nat`

- [lsl_00.tzt](lsl_00.tzt)
- [lsl_01.tzt](lsl_01.tzt)
- [lsl_02.tzt](lsl_02.tzt)
- [lsl_03.tzt](lsl_03.tzt)
- [lsl_04.tzt](lsl_04.tzt)
- [lsl_05.tzt](lsl_05.tzt)
- [lsl_06.tzt](lsl_06.tzt)

Missing edge cases:

- No zero shift test for non-zero argument

### `LSL: bytes : nat`

***None***

### `LSR: nat : nat`

- [lsr_00.tzt](lsr_00.tzt)
- [lsr_01.tzt](lsr_01.tzt)
- [lsr_02.tzt](lsr_02.tzt)
- [lsr_03.tzt](lsr_03.tzt)
- [lsr_04.tzt](lsr_04.tzt)
- [lsr_05.tzt](lsr_05.tzt)

Missing edge cases:

- No zero shift test for non-zero argument

### `LSR: bytes : nat`

***None***

### `LT`

- [lt_00.tzt](lt_00.tzt)
- [lt_01.tzt](lt_01.tzt)
- [lt_02.tzt](lt_02.tzt)
- [lt_03.tzt](lt_03.tzt)
- [lt_04.tzt](lt_04.tzt)

### `MUL: nat : nat`

- [mul_nat-nat_00.tzt](mul_nat-nat_00.tzt)

Missing edge cases:

- No multiplication by zero (both from left and right)

### `MUL: nat : int`

- [mul_nat-int_00.tzt](mul_nat-int_00.tzt)

Missing edge cases:

- No multiplication by zero (both from left and right)
- No multiplication by positive int

### `MUL: int : nat`

- [mul_int-nat_00.tzt](mul_int-nat_00.tzt)

Missing edge cases:

- No multiplication by zero (both from left and right)
- No multiplication by negative int

### `MUL: int : int`

- [mul_int-int_00.tzt](mul_int-int_00.tzt)

Missing edge cases:

- No multiplication by zero (both from left and right)
- No multiplication of two negatives
- No multiplication of two positives
- No multiplication of negative by positive

### `MUL: mutez : nat`

- [mul_mutez-nat_00.tzt](mul_mutez-nat_00.tzt)
- [mul_mutez-nat_01.tzt](mul_mutez-nat_01.tzt)

Missing edge cases:

- No multiplication by zero (both from left and right)

### `MUL: nat : mutez`

- [mul_nat-mutez_00.tzt](mul_nat-mutez_00.tzt)
- [mul_nat-mutez_01.tzt](mul_nat-mutez_01.tzt)

Missing edge cases:

- No multiplication by zero (both from left and right)

### `MUL: bls12_381_g1 : bls12_381_fr`

***None***

### `MUL: bls12_381_g2 : bls12_381_fr`

***None***

### `MUL: bls12_381_fr : bls12_381_fr`

***None***

### `MUL: nat : bls12_381_fr`

***None***

### `MUL: int : bls12_381_fr`

***None***

### `MUL: bls12_381_fr : nat`

***None***

### `MUL: bls12_381_fr : int`

***None***

### `NAT`

***None***

### `NEG: nat`

- [neg_nat_00.tzt](neg_nat_00.tzt)
- [neg_nat_01.tzt](neg_nat_01.tzt)

### `NEG: int`

- [neg_int_00.tzt](neg_int_00.tzt)
- [neg_int_01.tzt](neg_int_01.tzt)
- [neg_int_02.tzt](neg_int_02.tzt)

### `NEG: bls12_381_g1`

***None***

### `NEG: bls12_381_g2`

***None***

### `NEG: bls12_381_fr`

***None***

### `NEQ`

- [neq_00.tzt](neq_00.tzt)
- [neq_01.tzt](neq_01.tzt)
- [neq_02.tzt](neq_02.tzt)
- [neq_03.tzt](neq_03.tzt)
- [neq_04.tzt](neq_04.tzt)

### `SUB: nat : nat`

***None***

### `SUB: nat : int`

***None***

### `SUB: int : nat`

***None***

### `SUB: int : int`

- [sub_int-int_00.tzt](sub_int-int_00.tzt)
- [sub_int-int_01.tzt](sub_int-int_01.tzt)

Missing edge cases:

- No subtraction of 0
- No subtraction from 0
- No subtraction of negative integers

### `SUB: timestamp : int`

- [sub_timestamp-int_00.tzt](sub_timestamp-int_00.tzt)
- [sub_timestamp-int_01.tzt](sub_timestamp-int_01.tzt)
- [sub_timestamp-int_02.tzt](sub_timestamp-int_02.tzt)
- [sub_timestamp-int_03.tzt](sub_timestamp-int_03.tzt)
- [sub_timestamp-int_04.tzt](sub_timestamp-int_04.tzt)

Missing edge cases:

- No subtraction of zero
- No subtraction from zero

### `SUB: timestamp : timestamp`

- [sub_timestamp-timestamp_00.tzt](sub_timestamp-timestamp_00.tzt)
- [sub_timestamp-timestamp_01.tzt](sub_timestamp-timestamp_01.tzt)
- [sub_timestamp-timestamp_02.tzt](sub_timestamp-timestamp_02.tzt)
- [sub_timestamp-timestamp_03.tzt](sub_timestamp-timestamp_03.tzt)

Missing edge cases:

- No test for realistic timestamps producing negative difference

### `SUB_MUTEZ`

***None***, but there are tests for the deprecated `SUB: mutez : mutez`:

- [sub_mutez-mutez_00.tzt](sub_mutez-mutez_00.tzt)
- [sub_mutez-mutez_01.tzt](sub_mutez-mutez_01.tzt)

## Boolean operations

### `AND bool:bool`

- [and_bool-bool_00.tzt](and_bool-bool_00.tzt)
- [and_bool-bool_01.tzt](and_bool-bool_01.tzt)
- [and_bool-bool_02.tzt](and_bool-bool_02.tzt)
- [and_bool-bool_03.tzt](and_bool-bool_03.tzt)

### `AND nat:nat`

- [and_nat-nat_00.tzt](and_nat-nat_00.tzt)
- [and_nat-nat_01.tzt](and_nat-nat_01.tzt)
- [and_nat-nat_02.tzt](and_nat-nat_02.tzt)

Missing edge cases:

- One argument is `0` (both from left and right)
- Result is `0` with non-zero arguments

### `AND int:nat`

- [and_int-nat_00.tzt](and_int-nat_00.tzt)
- [and_int-nat_01.tzt](and_int-nat_01.tzt)
- [and_int-nat_02.tzt](and_int-nat_02.tzt)
- [and_int-nat_03.tzt](and_int-nat_03.tzt)
- [and_int-nat_04.tzt](and_int-nat_04.tzt)
- [and_int-nat_05.tzt](and_int-nat_05.tzt)
- [and_int-nat_06.tzt](and_int-nat_06.tzt)

### `AND: bytes:bytes`

- [and_bytes-bytes_00.tzt](and_bytes-bytes_00.tzt)
- [and_bytes-bytes_01.tzt](and_bytes-bytes_01.tzt)
- [and_bytes-bytes_02.tzt](and_bytes-bytes_02.tzt)
- [and_bytes-bytes_03.tzt](and_bytes-bytes_03.tzt)
- [and_bytes-bytes_04.tzt](and_bytes-bytes_04.tzt)
- [and_bytes-bytes_05.tzt](and_bytes-bytes_05.tzt)
- [and_bytes-bytes_06.tzt](and_bytes-bytes_06.tzt)

### `NOT: bool`

- [not_bool_00.tzt](not_bool_00.tzt)
- [not_bool_01.tzt](not_bool_01.tzt)

### `NOT: nat`

- [not_nat_00.tzt](not_nat_00.tzt)
- [not_nat_01.tzt](not_nat_01.tzt)
- [not_nat_02.tzt](not_nat_02.tzt)

### `NOT: int`

- [not_int_00.tzt](not_int_00.tzt)
- [not_nat_03.tzt](not_nat_03.tzt)
- [not_nat_04.tzt](not_nat_04.tzt)
- [not_nat_05.tzt](not_nat_05.tzt)
- [not_nat_06.tzt](not_nat_06.tzt)
- [not_nat_07.tzt](not_nat_07.tzt)

Files do not follow naming convention.

### `NOT: bytes`

- [not_bytes_00.tzt](not_bytes_00.tzt)
- [not_bytes_01.tzt](not_bytes_01.tzt)
- [not_bytes_02.tzt](not_bytes_02.tzt)

### `OR bool:bool`

- [or_bool-bool_00.tzt](or_bool-bool_00.tzt)
- [or_bool-bool_01.tzt](or_bool-bool_01.tzt)
- [or_bool-bool_02.tzt](or_bool-bool_02.tzt)
- [or_bool-bool_03.tzt](or_bool-bool_03.tzt)

### `OR nat:nat`

- [or_nat-nat_00.tzt](or_nat-nat_00.tzt)
- [or_nat-nat_01.tzt](or_nat-nat_01.tzt)
- [or_nat-nat_02.tzt](or_nat-nat_02.tzt)
- [or_nat-nat_03.tzt](or_nat-nat_03.tzt)
- [or_nat-nat_04.tzt](or_nat-nat_04.tzt)
- [or_nat-nat_05.tzt](or_nat-nat_05.tzt)
- [or_nat-nat_06.tzt](or_nat-nat_06.tzt)

### `OR bytes`

- [or_bytes-bytes_00.tzt](or_bytes-bytes_00.tzt)
- [or_bytes-bytes_01.tzt](or_bytes-bytes_01.tzt)
- [or_bytes-bytes_02.tzt](or_bytes-bytes_02.tzt)
- [or_bytes-bytes_03.tzt](or_bytes-bytes_03.tzt)
- [or_bytes-bytes_04.tzt](or_bytes-bytes_04.tzt)
- [or_bytes-bytes_05.tzt](or_bytes-bytes_05.tzt)
- [or_bytes-bytes_06.tzt](or_bytes-bytes_06.tzt)

### `XOR: bool:bool`

- [xor_bool-bool_00.tzt](xor_bool-bool_00.tzt)
- [xor_bool-bool_01.tzt](xor_bool-bool_01.tzt)
- [xor_bool-bool_02.tzt](xor_bool-bool_02.tzt)
- [xor_bool-bool_03.tzt](xor_bool-bool_03.tzt)

### `XOR: nat:nat`

- [xor_nat-nat_00.tzt](xor_nat-nat_00.tzt)
- [xor_nat-nat_01.tzt](xor_nat-nat_01.tzt)
- [xor_nat-nat_02.tzt](xor_nat-nat_02.tzt)
- [xor_nat-nat_03.tzt](xor_nat-nat_03.tzt)
- [xor_nat-nat_04.tzt](xor_nat-nat_04.tzt)
- [xor_nat-nat_05.tzt](xor_nat-nat_05.tzt)
- [xor_nat-nat_06.tzt](xor_nat-nat_06.tzt)

### `XOR: bytes:bytes`

- [xor_bytes-bytes_00.tzt](xor_bytes-bytes_00.tzt)
- [xor_bytes-bytes_01.tzt](xor_bytes-bytes_01.tzt)
- [xor_bytes-bytes_02.tzt](xor_bytes-bytes_02.tzt)
- [xor_bytes-bytes_03.tzt](xor_bytes-bytes_03.tzt)
- [xor_bytes-bytes_04.tzt](xor_bytes-bytes_04.tzt)
- [xor_bytes-bytes_05.tzt](xor_bytes-bytes_05.tzt)
- [xor_bytes-bytes_06.tzt](xor_bytes-bytes_06.tzt)

## Data structure manipulation

### `CAR`

- [car_00.tzt](car_00.tzt)
- [car_01.tzt](car_01.tzt)

### `CDR`

- [cdr_00.tzt](cdr_00.tzt)
- [cdr_01.tzt](cdr_01.tzt)

### `CONCAT: string : string`

- [concat_string_00.tzt](concat_string_00.tzt)
- [concat_string_01.tzt](concat_string_01.tzt)
- [concat_string_02.tzt](concat_string_02.tzt)

### `CONCAT: list string`

- [concat_liststring_00.tzt](concat_liststring_00.tzt)
- [concat_liststring_01.tzt](concat_liststring_01.tzt)
- [concat_liststring_02.tzt](concat_liststring_02.tzt)
- [concat_liststring_03.tzt](concat_liststring_03.tzt)
- [concat_liststring_04.tzt](concat_liststring_04.tzt)

### `CONCAT: bytes : bytes`

- [concat_bytes_00.tzt](concat_bytes_00.tzt)
- [concat_bytes_01.tzt](concat_bytes_01.tzt)

### `CONCAT: list bytes`

- [concat_listbytes_00.tzt](concat_listbytes_00.tzt)
- [concat_listbytes_01.tzt](concat_listbytes_01.tzt)
- [concat_listbytes_02.tzt](concat_listbytes_02.tzt)

### `CONS`

- [cons_int_00.tzt](cons_int_00.tzt)
- [cons_int_01.tzt](cons_int_01.tzt)
- [cons_int_02.tzt](cons_int_02.tzt)
- [cons_string_00.tzt](cons_string_00.tzt)

### `EMPTY_BIG_MAP`

- [emptybigmap_nat-nat_00.tzt](emptybigmap_nat-nat_00.tzt)

### `EMPTY_MAP`

- [emptymap_nat-nat_00.tzt](emptymap_nat-nat_00.tzt)
- [emptymap_string-string_00.tzt](emptymap_string-string_00.tzt)

### `EMPTY_SET`

- [emptyset_nat_00.tzt](emptyset_nat_00.tzt)

### `GET: kty : map kty vty`

- [get_mapintint_00.tzt](get_mapintint_00.tzt)
- [get_mapintint_01.tzt](get_mapintint_01.tzt)
- [get_mapstringstring_00.tzt](get_mapstringstring_00.tzt)
- [get_mapstringstring_01.tzt](get_mapstringstring_01.tzt)
- [get_mapstringstring_02.tzt](get_mapstringstring_02.tzt)

### `GET: kty : big_map kty vty`

- [get_bigmapstringstring_00.tzt](get_bigmapstringstring_00.tzt)
- [get_bigmapstringstring_01.tzt](get_bigmapstringstring_01.tzt)
- [get_bigmapstringstring_02.tzt](get_bigmapstringstring_02.tzt)

### `GET n`

***None***

### `GET_AND_UPDATE: kty : option vty : map kty vty`

***None***

### `GET_AND_UPDATE: kty : option vty : big_map kty vty`

***None***

### `ITER: list ty`

- [iter_listint_00.tzt](iter_listint_00.tzt)
- [iter_listint_01.tzt](iter_listint_01.tzt)
- [iter_listint_02.tzt](iter_listint_02.tzt)
- [iter_listint_03.tzt](iter_listint_03.tzt)
- [iter_liststring_00.tzt](iter_liststring_00.tzt)
- [iter_liststring_01.tzt](iter_liststring_01.tzt)

### `ITER: set cty`

- [iter_setint_00.tzt](iter_setint_00.tzt)
- [iter_setint_01.tzt](iter_setint_01.tzt)
- [iter_setint_02.tzt](iter_setint_02.tzt)
- [iter_setstring_00.tzt](iter_setstring_00.tzt)
- [iter_setstring_01.tzt](iter_setstring_01.tzt)
- [iter_setstring_02.tzt](iter_setstring_02.tzt)

### `ITER: map kty vty`

- [iter_mapintint_00.tzt](iter_mapintint_00.tzt)
- [iter_mapintint_01.tzt](iter_mapintint_01.tzt)
- [iter_mapintint_02.tzt](iter_mapintint_02.tzt)
- [iter_mapintint_03.tzt](iter_mapintint_03.tzt)
- [iter_mapintint_04.tzt](iter_mapintint_04.tzt)
- [iter_mapstringstring_00.tzt](iter_mapstringstring_00.tzt)

### `LEFT`

- [left_int-nat_00.tzt](left_int-nat_00.tzt)

### `MAP: list ty`

- [map_listint_00.tzt](map_listint_00.tzt)
- [map_listint_01.tzt](map_listint_01.tzt)
- [map_listint_02.tzt](map_listint_02.tzt)
- [map_listint_03.tzt](map_listint_03.tzt)
- [map_listint_04.tzt](map_listint_04.tzt)
- [map_listint_05.tzt](map_listint_05.tzt)
- [map_listint_06.tzt](map_listint_06.tzt)
- [map_liststring_00.tzt](map_liststring_00.tzt)
- [map_liststring_01.tzt](map_liststring_01.tzt)
- [map_liststring_02.tzt](map_liststring_02.tzt)
- [map_liststring_04.tzt](map_liststring_04.tzt)
- [map_liststring_05.tzt](map_liststring_05.tzt)
- [map_liststring_06.tzt](map_liststring_06.tzt)
- [map_liststring_07.tzt](map_liststring_07.tzt)
- [map_liststring_08.tzt](map_liststring_08.tzt)

### `MAP: option ty`

***None***

### `MAP: map kty ty1`

- [map_mapintint_00.tzt](map_mapintint_00.tzt)
- [map_mapintint_01.tzt](map_mapintint_01.tzt)
- [map_mapintstring_00.tzt](map_mapintstring_00.tzt)
- [map_mapintstring_01.tzt](map_mapintstring_01.tzt)
- [map_mapstringnat_00.tzt](map_mapstringnat_00.tzt)
- [map_mapstringnat_01.tzt](map_mapstringnat_01.tzt)
- [map_mapstringnat_02.tzt](map_mapstringnat_02.tzt)

### `MEM: cty : set cty`

- [mem_setint_00.tzt](mem_setint_00.tzt)
- [mem_setint_01.tzt](mem_setint_01.tzt)
- [mem_setstring_00.tzt](mem_setstring_00.tzt)
- [mem_setstring_01.tzt](mem_setstring_01.tzt)
- [mem_setstring_02.tzt](mem_setstring_02.tzt)

### `MEM: kty : map kty vty`

- [mem_mapintint_00.tzt](mem_mapintint_00.tzt)
- [mem_mapnatnat_00.tzt](mem_mapnatnat_00.tzt)
- [mem_mapnatnat_01.tzt](mem_mapnatnat_01.tzt)
- [mem_mapnatnat_02.tzt](mem_mapnatnat_02.tzt)
- [mem_mapnatnat_03.tzt](mem_mapnatnat_03.tzt)
- [mem_mapnatnat_04.tzt](mem_mapnatnat_04.tzt)
- [mem_mapnatnat_05.tzt](mem_mapnatnat_05.tzt)
- [mem_mapstringnat_00.tzt](mem_mapstringnat_00.tzt)
- [mem_mapstringnat_01.tzt](mem_mapstringnat_01.tzt)
- [mem_mapstringnat_02.tzt](mem_mapstringnat_02.tzt)
- [mem_mapstringnat_03.tzt](mem_mapstringnat_03.tzt)
- [mem_mapstringnat_04.tzt](mem_mapstringnat_04.tzt)
- [mem_mapstringnat_05.tzt](mem_mapstringnat_05.tzt)

### `MEM: kty : big_map kty vty`

- [mem_bigmapnatnat_00.tzt](mem_bigmapnatnat_00.tzt)
- [mem_bigmapnatnat_01.tzt](mem_bigmapnatnat_01.tzt)
- [mem_bigmapnatnat_02.tzt](mem_bigmapnatnat_02.tzt)
- [mem_bigmapnatnat_03.tzt](mem_bigmapnatnat_03.tzt)
- [mem_bigmapnatnat_04.tzt](mem_bigmapnatnat_04.tzt)
- [mem_bigmapnatnat_05.tzt](mem_bigmapnatnat_05.tzt)
- [mem_bigmapstringnat_00.tzt](mem_bigmapstringnat_00.tzt)
- [mem_bigmapstringnat_01.tzt](mem_bigmapstringnat_01.tzt)
- [mem_bigmapstringnat_02.tzt](mem_bigmapstringnat_02.tzt)
- [mem_bigmapstringnat_03.tzt](mem_bigmapstringnat_03.tzt)
- [mem_bigmapstringnat_04.tzt](mem_bigmapstringnat_04.tzt)
- [mem_bigmapstringnat_05.tzt](mem_bigmapstringnat_05.tzt)

### `NEVER`

***None***

### `NIL`

- [nil_nat_00.tzt](nil_nat_00.tzt)

### `NONE`

- [none_int_00.tzt](none_int_00.tzt)
- [none_pair-nat-string.tzt](none_pair-nat-string.tzt)

### `PACK`

- `pack_*.tzt`
- [packunpack_address_00.tzt](packunpack_address_00.tzt)
- [packunpack_bool_00.tzt](packunpack_bool_00.tzt)
- [packunpack_bytes_00.tzt](packunpack_bytes_00.tzt)
- [packunpack_int_00.tzt](packunpack_int_00.tzt)
- [packunpack_keyhash_00.tzt](packunpack_keyhash_00.tzt)
- [packunpack_mutez_00.tzt](packunpack_mutez_00.tzt)
- [packunpack_nat_00.tzt](packunpack_nat_00.tzt)
- [packunpack_string_00.tzt](packunpack_string_00.tzt)
- [packunpack_timestamp_00.tzt](packunpack_timestamp_00.tzt)

Only few value types are covered.

Among tests on values serialization:
- Addresses with entrypoints are not covered.

### `PAIR`

- [pair_int-int_00.tzt](pair_int-int_00.tzt)
- [pair_nat-string_00.tzt](pair_nat-string_00.tzt)
- [pair_pair-nat-string-pair-string-nat_00.tzt](pair_pair-nat-string-pair-string-nat_00.tzt)

### `PAIR n`

***None***

### `RIGHT`

- [right_nat-int_00.tzt](right_nat-int_00.tzt)

### `SIZE: set cty`

- [size_setint_00.tzt](size_setint_00.tzt)
- [size_setint_01.tzt](size_setint_01.tzt)
- [size_setint_02.tzt](size_setint_02.tzt)
- [size_setint_03.tzt](size_setint_03.tzt)
- [size_setstring_00.tzt](size_setstring_00.tzt)

### `SIZE: map kty vty`

- [size_mapintint_00.tzt](size_mapintint_00.tzt)
- [size_mapstringnat_00.tzt](size_mapstringnat_00.tzt)
- [size_mapstringnat_01.tzt](size_mapstringnat_01.tzt)
- [size_mapstringnat_02.tzt](size_mapstringnat_02.tzt)
- [size_mapstringnat_03.tzt](size_mapstringnat_03.tzt)

### `SIZE: list ty`

- [size_listint_00.tzt](size_listint_00.tzt)
- [size_listint_01.tzt](size_listint_01.tzt)
- [size_listint_02.tzt](size_listint_02.tzt)
- [size_listint_03.tzt](size_listint_03.tzt)

### `SIZE: string`

- [size_string_00.tzt](size_string_00.tzt)

### `SIZE: bytes`

- [size_bytes_00.tzt](size_bytes_00.tzt)

### `SLICE: nat : nat : string`

- [slice_string_00.tzt](slice_string_00.tzt)
- [slice_string_01.tzt](slice_string_01.tzt)
- [slice_string_02.tzt](slice_string_02.tzt)
- [slice_string_03.tzt](slice_string_03.tzt)
- [slice_string_04.tzt](slice_string_04.tzt)
- [slice_string_05.tzt](slice_string_05.tzt)

### `SLICE: nat : nat : bytes`

- [slice_bytes_00.tzt](slice_bytes_00.tzt)
- [slice_bytes_01.tzt](slice_bytes_01.tzt)
- [slice_bytes_02.tzt](slice_bytes_02.tzt)
- [slice_bytes_03.tzt](slice_bytes_03.tzt)
- [slice_bytes_04.tzt](slice_bytes_04.tzt)

### `SOME`

- [some_int_00.tzt](some_int_00.tzt)
- [some_pairintint_00.tzt](some_pairintint_00.tzt)
- [some_string_00.tzt](some_string_00.tzt)

### `UNIT`

- [unit_00.tzt](unit_00.tzt)

### `UNPACK`

- [packunpack_address_00.tzt](packunpack_address_00.tzt)
- [packunpack_bool_00.tzt](packunpack_bool_00.tzt)
- [packunpack_bytes_00.tzt](packunpack_bytes_00.tzt)
- [packunpack_int_00.tzt](packunpack_int_00.tzt)
- [packunpack_keyhash_00.tzt](packunpack_keyhash_00.tzt)
- [packunpack_mutez_00.tzt](packunpack_mutez_00.tzt)
- [packunpack_nat_00.tzt](packunpack_nat_00.tzt)
- [packunpack_string_00.tzt](packunpack_string_00.tzt)
- [packunpack_timestamp_00.tzt](packunpack_timestamp_00.tzt)

Tested only with conjunction with `PACK`

### `UNPAIR`

- [unpair_pairstringstring_00.tzt](unpair_pairstringstring_00.tzt)

### `UPDATE: cty : bool : set cty`

- [update_setint_00.tzt](update_setint_00.tzt)
- [update_setint_01.tzt](update_setint_01.tzt)
- [update_setint_02.tzt](update_setint_02.tzt)

### `UPDATE: kty : option vty : map kty vty`

- [update_mapintint_00.tzt](update_mapintint_00.tzt)
- [update_mapintint_01.tzt](update_mapintint_01.tzt)

### `UPDATE: kty : option vty : big_map kty vty`

- [update_bigmapstringstring_00.tzt](update_bigmapstringstring_00.tzt)
- [update_bigmapstringstring_01.tzt](update_bigmapstringstring_01.tzt)
- [update_bigmapstringstring_02.tzt](update_bigmapstringstring_02.tzt)
- [update_bigmapstringstring_03.tzt](update_bigmapstringstring_03.tzt)
- [update_bigmapstringstring_04.tzt](update_bigmapstringstring_04.tzt)
- [update_bigmapstringstring_05.tzt](update_bigmapstringstring_05.tzt)
- [update_bigmapstringstring_06.tzt](update_bigmapstringstring_06.tzt)
- [update_bigmapstringstring_07.tzt](update_bigmapstringstring_07.tzt)

### `UPDATE n`

***None***

## Ticket manipulation

Not covered

### `JOIN_TICKETS`

***None***

### `READ_TICKET`

***None***

### `SPLIT_TICKET`

***None***

### `TICKET`

***None***

## Cryptographic operations

Not covered

### `BLAKE2B`

***None***

### `CHECK_SIGNATURE`

- [checksignature_00.tzt](checksignature_00.tzt)
- [checksignature_01.tzt](checksignature_01.tzt)

Does not check different types of key.

### `HASH_KEY`

***None***

### `KECCAK`

***None***

### `PAIRING_CHECK`

***None***

### `SAPLING_EMPTY_STATE ms`

***None***

### `SAPLING_VERIFY_UPDATE`

***None***

### `SHA256`

***None***

### `SHA3`

***None***

### `SHA512`

***None***

## Blockchain operations

### `ADDRESS`

- [address_00.tzt](address_00.tzt)
- [address_01.tzt](address_01.tzt) -- on implicit contract
- [address_02.tzt](address_02.tzt)

### `AMOUNT`

- [amount_00.tzt](amount_00.tzt)

### `BALANCE`

- [balance_00.tzt](balance_00.tzt)

### `CHAIN_ID`

- [chain_id_00.tzt](chain_id_00.tzt)
- [chain_id_01.tzt](chain_id_01.tzt)

### `CONTRACT`

Note: invariants are taken from the table in <https://tezos.gitlab.io/michelson-reference/#instr-CONTRACT> section, copied below for posterity.

- [contract_00.tzt](contract_00.tzt) -- valid_contract_type "addr" t holds
- [contract_01.tzt](contract_01.tzt) -- no_contract "addr" holds
- [contract_02.tzt](contract_02.tzt) -- invalid_contract_type "addr" t holds
- [contract_03.tzt](contract_03.tzt) -- valid_contract_type "addr" t holds
- [contract_04.tzt](contract_04.tzt) -- valid_contract_type "addr" t holds on implicit contract
- [contract_05.tzt](contract_05.tzt) -- no_contract "addr" holds

No tests with entrypoints (the cases represented by rows 3, 4, 5, 6, 7, 9, 10 from the table are not covered)

```
+---------------+---------------------+------------------------------------------+-------------------------------------------------+
| input address | instruction         | output contract                          | predicate                                       |
+---------------+---------------------+------------------------------------------+-------------------------------------------------+
| "addr"        | CONTRACT t          | None if addr does not exist              | no_contract "addr" holds                        |
+---------------+---------------------+------------------------------------------+-------------------------------------------------+
| "addr"        | CONTRACT t          | None if addr exists, but has a default   | invalid_contract_type "addr" t holds            |
|               |                     | entrypoint not of type t, or has no      |                                                 |
|               |                     | default entrypoint and parameter is not  |                                                 |
|               |                     | of type t                                |                                                 |
+---------------+---------------------+------------------------------------------+-------------------------------------------------+
| "addr%name"   | CONTRACT t          | None if addr does not exist, or exists   | no_contract "addr%name" holds                   |
|               |                     | but does not have a "name" entrypoint    |                                                 |
+---------------+---------------------+                                          |                                                 |
| "addr"        | CONTRACT %name t    |                                          |                                                 |
|               |                     |                                          |                                                 |
+---------------+---------------------+------------------------------------------+-------------------------------------------------+
| "addr%name"   | CONTRACT t          | None if addr exists, but has an          | invalid_contract_type "addr%name" t holds       |
|               |                     | entrypoint %name not of type t           |                                                 |
+---------------+---------------------+                                          |                                                 |
| "addr"        | CONTRACT %name t    |                                          |                                                 |
+---------------+---------------------+------------------------------------------+-------------------------------------------------+
| "addr%name1"  | CONTRACT %name2 t   | None                                     | entrypoint_ambiguity "addr%name1" "name2" holds |
+---------------+---------------------+------------------------------------------+-------------------------------------------------+
| "addr"        | CONTRACT t          | (Some "addr") if contract exists, has a  | valid_contract_type "addr" t holds              |
|               |                     | default entrypoint of type t, or has no  |                                                 |
|               |                     | default entrypoint and parameter type t  |                                                 |
+---------------+---------------------+------------------------------------------+-------------------------------------------------+
| "addr%name"   | CONTRACT t          | (Some "addr%name") if addr exists and    | valid_contract_type "addr%name" t holds         |
+---------------+---------------------+ has an entrypoint %name of type t        |                                                 |
| "addr"        | CONTRACT %name t    |                                          |                                                 |
+---------------+---------------------+------------------------------------------+-------------------------------------------------+
```

### `CREATE_CONTRACT`

- [createcontract_00.tzt](createcontract_00.tzt)
- [createcontract_01.tzt](createcontract_01.tzt)

### `EMIT`

***None***

### `IMPLICIT_ACCOUNT`

- [implicitaccount_00.tzt](implicitaccount_00.tzt)

### `LEVEL`

***None***

TZT format doesn't have the necessary field(s) to set the return value of `LEVEL` instruction

### `MIN_BLOCK_TIME`

***None***

TZT format doesn't have the necessary field(s) to set the return value of
`MIN_BLOCK_TIME` instruction

### `NOW`

- [now_00.tzt](now_00.tzt)

### `SELF`

- [self_00.tzt](self_00.tzt)

### `SELF_ADDRESS`

***None***

### `SENDER`

- [sender_00.tzt](sender_00.tzt)

It would be nice to add test where the result of `SOURCE` isn't equal to the result of `SENDER`

### `SET_DELEGATE`

- [setdelegate_00.tzt](setdelegate_00.tzt)

No test with `None` parameter

### `SOURCE`

- [source_00.tzt](source_00.tzt)

### `TOTAL_VOTING_POWER`

***None***

TZT format doesn't have the necessary field(s) to set the return value of
`TOTAL_VOTING_POWER` instruction

### `TRANSFER_TOKENS`

- [transfertokens_00.tzt](transfertokens_00.tzt)
- [transfertokens_01.tzt](transfertokens_01.tzt)

### `VIEW`

***None***

### `VOTING_POWER`

***None***

TZT format doesn't have the necessary field(s) to set the return value of
`VOTING_POWER` instruction

## Missing tests summary

There are no tests for ill-typed code.

Instructions with no tests:

- `{}`
- `ADD: bls12_381_fr : bls12_381_fr`
- `ADD: bls12_381_g1 : bls12_381_g1`
- `ADD: bls12_381_g2 : bls12_381_g2`
- `AND: bytes:bytes`
- `BLAKE2B`
- `BYTES: int`
- `BYTES: nat`
- `CHECK_SIGNATURE`
- `COMPARE: address : address`
- `COMPARE: chain_id : chain_id`
- `COMPARE: key : key`
- `COMPARE: never : never`
- `COMPARE: option _ : option _`
- `COMPARE: or _ _ : or _ _`
- `COMPARE: signature : signature`
- `COMPARE: timestamp : timestamp`
- `COMPARE: unit : unit`
- `DUP n`
- `DUP`
- `EDIV: int : nat`
- `EDIV: nat : int`
- `EDIV: nat : nat`
- `EMIT`
- `GET n`
- `GET_AND_UPDATE: kty : option vty : big_map kty vty`
- `GET_AND_UPDATE: kty : option vty : map kty vty`
- `HASH_KEY`
- `INT: bls12_381_fr`
- `INT: bytes`
- `JOIN_TICKETS`
- `KECCAK`
- `LAMBDA_REC`
- `LAMBDA`
- `LEVEL`
- `LSL: bytes : nat`
- `LSR: bytes : nat`
- `MAP: option ty`
- `MIN_BLOCK_TIME`
- `MUL: bls12_381_fr : bls12_381_fr`
- `MUL: bls12_381_fr : int`
- `MUL: bls12_381_fr : nat`
- `MUL: bls12_381_g1 : bls12_381_fr`
- `MUL: bls12_381_g2 : bls12_381_fr`
- `MUL: int : bls12_381_fr`
- `MUL: nat : bls12_381_fr`
- `NAT`
- `NEG: bls12_381_fr`
- `NEG: bls12_381_g1`
- `NEG: bls12_381_g2`
- `NEVER`
- `NOT: bytes`
- `OR bytes`
- `PAIR n`
- `PAIRING_CHECK`
- `READ_TICKET`
- `SAPLING_EMPTY_STATE ms`
- `SAPLING_VERIFY_UPDATE`
- `SELF_ADDRESS`
- `SHA256`
- `SHA3`
- `SHA512`
- `SPLIT_TICKET`
- `SUB_MUTEZ`
- `SUB: int : nat`
- `SUB: nat : int`
- `SUB: nat : nat`
- `SWAP`
- `TICKET`
- `TOTAL_VOTING_POWER`
- `UPDATE n`
- `VIEW`
- `VOTING_POWER`
- `XOR: bytes:bytes`

Instructions with missing edge cases:

- `AND int:nat` 0 & x
- `AND int:nat` positive & x
- `AND int:nat` result `0` with non-zero arguments
- `AND int:nat` x & 0
- `AND nat:nat` 0 & x
- `AND nat:nat` result `0` with non-zero arguments
- `AND nat:nat` x & 0
- `COMPARE: int : int` with 0
- `COMPARE: int : int` with negative argument(s)
- `COMPARE: mutez : mutez` with 0
- `COMPARE: nat : nat` with 0
- `COMPARE: string : string` with strings longer than 1 character
- `CONTRACT` with entrypoints
- `DIG n` for even n (e.g. n = 2)
- `DIP n` for n = 0
- `DIP n` for n = 1
- `DROP n` for n = 1
- `DUG n` for n = 0
- `DUG n` for n = 1
- `EDIV: int : int` negative / negative
- `EDIV: int : int` positive / positive
- `EDIV: int : int` with result 1
- `EDIV: int : int` zero / non-zero
- `EDIV: int : int` zero / zero
- `EDIV: mutez : mutez` zero / non-zero
- `EDIV: mutez : mutez` zero / zero
- `IF_CONS` -- check that stack tail is preserved
- `IF_LEFT` -- check that stack tail is preserved
- `IF_NONE` -- check that stack tail is preserved
- `IF` -- check that stack tail is preserved
- `ISNAT` for positive argument
- `LSL: nat : nat` zero shift for non-zero argument
- `LSR: nat : nat` zero shift for non-zero argument
- `MUL: int : int` 0 * x
- `MUL: int : int` negative * negative
- `MUL: int : int` negative * positive
- `MUL: int : int` positive * positive
- `MUL: int : int` x * 0
- `MUL: int : nat` 0 * x
- `MUL: int : nat` negative * x
- `MUL: int : nat` x * 0
- `MUL: mutez : nat` 0 * x
- `MUL: mutez : nat` x * 0
- `MUL: nat : int` 0 * x
- `MUL: nat : int` x * 0
- `MUL: nat : int` x * positive
- `MUL: nat : mutez` 0 * x
- `MUL: nat : mutez` x * 0
- `MUL: nat : nat` 0 * x
- `MUL: nat : nat` x * 0
- `PACK` standalone tests (without `UNPACK`)
- `PUSH` for more pushable types
- `SENDER` when sender != source
- `SET_DELEGATE` with `None`
- `SUB: int : int` 0 - x
- `SUB: int : int` x - 0
- `SUB: int : int` x - negative
- `SUB: timestamp : int` 0 - x
- `SUB: timestamp : int` x - 0
- `SUB: timestamp : timestamp` realistic timestamps with negative difference
- `UNPACK` standalone tests (without `PACK`)
