import os
import pytest
from tools import paths
from tools.utils import check_run_failure

from client.client import Client
from client.client_output import BakeForResult, RunScriptResult

CONTRACT_PATH = f'{paths.TEZOS_HOME}/src/bin_client/test/contracts/opcodes/'
KEY1 = 'foo'
KEY2 = 'bar'


@pytest.mark.incremental
@pytest.mark.slow
@pytest.mark.contract
class TestContractOpcodes:
    def test_gen_keys(self, client):
        """Add keys used by later tests."""
        client.gen_key(KEY1)
        client.gen_key(KEY2)

    @pytest.mark.parametrize(
        "contract,param,storage,expected",
        [   # FORMAT: assert_output contract_file storage input expected_result

            # TODO add tests for map_car.tz, subset.tz
            # NB: noop.tz is tested in test_basic.sh

            ('ret_int.tz', 'None', 'Unit', '(Some 300)'),

            # Map block on lists
            ('list_map_block.tz', '{0}', '{}', '{}'),
            ('list_map_block.tz', '{0}', '{ 1 ; 1 ; 1 ; 1 }',
             '{ 1 ; 2 ; 3 ; 4 }'),
            ('list_map_block.tz', '{0}', '{ 1 ; 2 ; 3 ; 0 }',
             '{ 1 ; 3 ; 5 ; 3 }'),

            # Reverse a list
            ('reverse.tz', '{""}', '{}', '{}'),
            ('reverse.tz', '{""}', '{ "c" ; "b" ; "a" }',
             '{ "a" ; "b" ; "c" }'),

            # Reverse using LOOP_LEFT
            ('loop_left.tz', '{""}', '{}', '{}'),
            ('loop_left.tz', '{""}', '{ "c" ; "b" ; "a" }',
             '{ "a" ; "b" ; "c" }'),

            # Identity on strings
            ('str_id.tz', 'None', '"Hello"', '(Some "Hello")'),
            ('str_id.tz', 'None', '"abcd"', '(Some "abcd")'),

            # Identity on pairs
            ('pair_id.tz', 'None', '(Pair True False)',
             '(Some (Pair True False))'),
            ('pair_id.tz', 'None', '(Pair False True)',
             '(Some (Pair False True))'),
            ('pair_id.tz', 'None', '(Pair True True)',
             '(Some (Pair True True))'),
            ('pair_id.tz', 'None', '(Pair False False)',
             '(Some (Pair False False))'),

            # Logical not
            ('not.tz', 'None', 'True', '(Some False)'),
            ('not.tz', 'None', 'False', '(Some True)'),

            # Logical and
            ('and.tz', 'None', '(Pair False False)', '(Some False)'),
            ('and.tz', 'None', '(Pair False True)', '(Some False)'),
            ('and.tz', 'None', '(Pair True False)', '(Some False)'),
            ('and.tz', 'None', '(Pair True True)', '(Some True)'),

            # Logical or
            ('or.tz', 'None', '(Pair False False)', '(Some False)'),
            ('or.tz', 'None', '(Pair False True)', '(Some True)'),
            ('or.tz', 'None', '(Pair True False)', '(Some True)'),
            ('or.tz', 'None', '(Pair True True)', '(Some True)'),

            # XOR
            ('xor.tz', 'None', '(Pair False False)', '(Some False)'),
            ('xor.tz', 'None', '(Pair False True)', '(Some True)'),
            ('xor.tz', 'None', '(Pair True False)', '(Some True)'),
            ('xor.tz', 'None', '(Pair True True)', '(Some False)'),


            # Concatenate all strings of a list into one string
            ('concat_list.tz', '""', '{ "a" ; "b" ; "c" }', '"abc"'),
            ('concat_list.tz', '""', '{}', '""'),
            ('concat_list.tz', '""', '{ "Hello" ; " " ; "World" ; "!" }',
             '"Hello World!"'),

            # Identity on lists
            ('list_id.tz', '{""}', '{ "1" ; "2" ; "3" }',
             '{ "1" ; "2" ; "3" }'),
            ('list_id.tz', '{""}', '{}', '{}'),
            ('list_id.tz', '{""}', '{ "a" ; "b" ; "c" }',
             '{ "a" ; "b" ; "c" }'),

            ('list_id_map.tz', '{""}', '{ "1" ; "2" ; "3" }',
             '{ "1" ; "2" ; "3" }'),
            ('list_id_map.tz', '{""}', '{}', '{}'),
            ('list_id_map.tz', '{""}', '{ "a" ; "b" ; "c" }',
             '{ "a" ; "b" ; "c" }'),


            # Identity on maps
            ('map_id.tz', '{}', '{ Elt 0 1 }', '{ Elt 0 1 }'),
            ('map_id.tz', '{}', '{ Elt 0 0 }', '{ Elt 0 0 }'),
            ('map_id.tz', '{}', '{ Elt 0 0 ; Elt 3 4 }',
             '{ Elt 0 0 ; Elt 3 4 }'),

            # Identity on sets
            ('set_id.tz', '{}', '{ "a" ; "b" ; "c" }', '{ "a" ; "b" ; "c" }'),
            ('set_id.tz', '{}', '{}', '{}'),
            ('set_id.tz', '{}', '{ "asdf" ; "bcde" }', '{ "asdf" ; "bcde" }'),

            # List concat
            ('list_concat.tz', '"abc"', '{ "d" ; "e" ; "f" }', '"abcdef"'),
            ('list_concat.tz', '"abc"', '{}', '"abc"'),

            ('list_concat_bytes.tz', '0x00ab', '{ 0xcd ; 0xef ; 0x00 }',
             '0x00abcdef00'),
            ('list_concat_bytes.tz', '0x', '{ 0x00 ; 0x11 ; 0x00 }',
             '0x001100'),
            ('list_concat_bytes.tz', '0xabcd', '{}', '0xabcd'),
            ('list_concat_bytes.tz', '0x', '{}', '0x'),

            # List iter
            ('list_iter.tz', '0', '{ 10 ; 2 ; 1 }', '20'),
            ('list_iter.tz', '0', '{ 3 ; 6 ; 9 }', '162'),

            # Set member -- set is in storage
            ('set_member.tz', '(Pair {} None)', '"Hi"',
             '(Pair {} (Some False))'),
            ('set_member.tz', '(Pair { "Hi" } None)', '"Hi"',
             '(Pair { "Hi" } (Some True))'),
            ('set_member.tz', '(Pair { "Hello" ; "World" } None)', '""',
             '(Pair { "Hello" ; "World" } (Some False))'),

            # Set size
            ('set_size.tz', '111', '{}', '0'),
            ('set_size.tz', '111', '{ 1 }', '1'),
            ('set_size.tz', '111', '{ 1 ; 2 ; 3 }', '3'),
            ('set_size.tz', '111', '{ 1 ; 2 ; 3 ; 4 ; 5 ; 6 }', '6'),

            # Set iter
            ('set_iter.tz', '111', '{}', '0'),
            ('set_iter.tz', '111', '{ 1 }', '1'),
            ('set_iter.tz', '111', '{ -100 ; 1 ; 2 ; 3 }', '-94'),

            # Map size
            ('map_size.tz', '111', '{}', '0'),
            ('map_size.tz', '111', '{ Elt "a" 1 }', '1'),
            ('map_size.tz', '111', '{ Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 }',
             '3'),
            ('map_size.tz', '111', '{ Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 ; \
            Elt "d" 4 ; Elt "e" 5 ; Elt "f" 6 }',
             '6'),

            # Contains all elements -- does the second list contain
            # all of the same elements as the first one? I'm ignoring
            # element multiplicity
            ('contains_all.tz', 'None', '(Pair {} {})',
             '(Some True)'),
            ('contains_all.tz', 'None', '(Pair { "a" } { "B" })',
             '(Some False)'),
            ('contains_all.tz', 'None', '(Pair { "A" } { "B" })',
             '(Some False)'),
            ('contains_all.tz', 'None', '(Pair { "B" } { "B" })',
             '(Some True)'),
            ('contains_all.tz', 'None',
             '(Pair { "B" ; "C" ; "asdf" } { "B" ; "B" ; "asdf" ; "C" })',
             '(Some True)'),
            ('contains_all.tz', 'None',
             '(Pair { "B" ; "B" ; "asdf" ; "C" } { "B" ; "C" ; "asdf" })',
             '(Some True)'),

            # Concatenate the string in storage with all strings in
            # the given list
            ('concat_hello.tz', '{}', '{ "World!" }', '{ "Hello World!" }'),
            ('concat_hello.tz', '{}', '{}', '{}'),
            ('concat_hello.tz', '{}', '{ "test1" ; "test2" }',
             '{ "Hello test1" ; "Hello test2" }'),

            # Create an empty map and add a string to it
            ('empty_map.tz', '{}', 'Unit', '{ Elt "hello" "world" }'),

            # Get the value stored at the given key in the map
            ('get_map_value.tz', '(Pair None { Elt "hello" "hi" })',
             '"hello"', '(Pair (Some "hi") { Elt "hello" "hi" })'),
            ('get_map_value.tz', '(Pair None { Elt "hello" "hi" })',
             '""', '(Pair None { Elt "hello" "hi" })'),
            ('get_map_value.tz', '(Pair None { Elt "1" "one" ; \
            Elt "2" "two" })',
             '"1"', '(Pair (Some "one") { Elt "1" "one" ; Elt "2" "two" })'),

            # Map iter
            ('map_iter.tz', '(Pair 0 0)', '{ Elt 0 100 ; Elt 2 100 }',
             '(Pair 2 200)'),
            ('map_iter.tz', '(Pair 0 0)', '{ Elt 1 1 ; Elt 2 100 }',
             '(Pair 3 101)'),

            # Return True if True branch of if was taken and False otherwise
            ('if.tz', 'None', 'True', '(Some True)'),
            ('if.tz', 'None', 'False', '(Some False)'),

            # Generate a pair of or types
            ('left_right.tz', '(Left "X")', '(Left True)', '(Right True)'),
            ('left_right.tz', '(Left "X")', '(Right "a")', '(Left "a")'),

            # Reverse a list
            ('reverse_loop.tz', '{""}', '{}', '{}'),
            ('reverse_loop.tz', '{""}', '{ "c" ; "b" ; "a" }',
             '{ "a" ; "b" ; "c" }'),

            # Exec concat contract
            ('exec_concat.tz', '"?"', '""', '"_abc"'),
            ('exec_concat.tz', '"?"', '"test"', '"test_abc"'),

            # Get current steps to quota
            ('steps_to_quota.tz', '111', 'Unit', '799813'),

            # Get the current balance of the contract
            ('balance.tz', '111', 'Unit', '4000000000000'),

            # Test addition and subtraction on tez
            ('tez_add_sub.tz', 'None', '(Pair 2000000 1000000)',
             '(Some (Pair 3000000 1000000))'),
            ('tez_add_sub.tz', 'None', '(Pair 2310000 1010000)',
             '(Some (Pair 3320000 1300000))'),

            # Test get first element of list
            ('first.tz', '111', '{ 1 ; 2 ; 3 ; 4 }', '1'),
            ('first.tz', '111', '{ 4 }', '4'),

            # Hash input string
            # Test assumed to be correct -- hash is based on encoding of AST
            ('hash_string.tz', '0x00', '"abcdefg"', '0x46fdbcb4ea4eadad5615c' +
             'daa17d67f783e01e21149ce2b27de497600b4cd8f4e'),
            ('hash_string.tz', '0x00', '"12345"', '0xb4c26c20de52a4eaf0d8a34' +
             '0db47ad8cb1e74049570859c9a9a3952b204c772f'),

            # IF_SOME
            ('if_some.tz', '"?"', '(Some "hello")', '"hello"'),
            ('if_some.tz', '"?"', 'None', '""'),

            # Tests the SET_CAR and SET_CDR instructions
            ('set_car.tz', '(Pair "hello" 0)', '"world"', '(Pair "world" 0)'),
            ('set_car.tz', '(Pair "hello" 0)', '"abc"', '(Pair "abc" 0)'),
            ('set_car.tz', '(Pair "hello" 0)', '""', '(Pair "" 0)'),

            ('set_cdr.tz', '(Pair "hello" 0)', '1', '(Pair "hello" 1)'),
            ('set_cdr.tz', '(Pair "hello" 500)', '3', '(Pair "hello" 3)'),
            ('set_cdr.tz', '(Pair "hello" 7)', '100', '(Pair "hello" 100)'),

            # Convert a public key to a public key hash
            ('hash_key.tz', 'None',
             '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"',
             '(Some "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")'),
            ('hash_key.tz', 'None',
             '"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES"',
             '(Some "tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k")'),

            # Test timestamp operations
            ('add_timestamp_delta.tz', 'None',
             '(Pair 100 100)', '(Some "1970-01-01T00:03:20Z")'),
            ('add_timestamp_delta.tz', 'None',
             '(Pair 100 -100)', '(Some "1970-01-01T00:00:00Z")'),
            ('add_timestamp_delta.tz', 'None',
             '(Pair "1970-01-01T00:00:00Z" 0)',
             '(Some "1970-01-01T00:00:00Z")'),

            ('add_delta_timestamp.tz', 'None',
             '(Pair 100 100)', '(Some "1970-01-01T00:03:20Z")'),
            ('add_delta_timestamp.tz', 'None',
             '(Pair -100 100)', '(Some "1970-01-01T00:00:00Z")'),
            ('add_delta_timestamp.tz', 'None',
             '(Pair 0 "1970-01-01T00:00:00Z")',
             '(Some "1970-01-01T00:00:00Z")'),

            ('sub_timestamp_delta.tz', '111', '(Pair 100 100)',
             '"1970-01-01T00:00:00Z"'),
            ('sub_timestamp_delta.tz', '111', '(Pair 100 -100)',
             '"1970-01-01T00:03:20Z"'),
            ('sub_timestamp_delta.tz', '111', '(Pair 100 2000000000000000000)',
             '-1999999999999999900'),

            ('diff_timestamps.tz', '111', '(Pair 0 0)', '0'),
            ('diff_timestamps.tz', '111', '(Pair 0 1)', '-1'),
            ('diff_timestamps.tz', '111', '(Pair 1 0)', '1'),
            ('diff_timestamps.tz', '111',
             '(Pair "1970-01-01T00:03:20Z" "1970-01-01T00:00:00Z")', '200'),

            # Test pack/unpack
            ('packunpack_rev.tz', 'Unit',
             '(Pair -1  (Pair 1 (Pair "foobar" (Pair 0x00AABBCC (Pair 1000 ' +
             '(Pair False (Pair "tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5" ' +
             '(Pair "2019-09-09T08:35:33Z" ' +
             '"tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5"))))))))', 'Unit'),

            ('packunpack_rev.tz', 'Unit',
             '(Pair -1  (Pair 1 (Pair "foobar" (Pair 0x00AABBCC (Pair 1000 ' +
             '(Pair False (Pair "tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5" ' +
             '(Pair "2019-09-09T08:35:33Z" ' +
             '"tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5"))))))))', 'Unit'),

            ('packunpack_rev_cty.tz', 'Unit',
             '(Pair "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9' +
             'sDVC9yav" (Pair Unit (Pair "edsigthTzJ8X7MPmNeEwybRAv' +
             'dxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8' +
             'V2w8ayB5dMJzrYCHhD8C7" (Pair (Some "edsigthTzJ8X7MPmN' +
             'eEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5' +
             'CwoNgqs8V2w8ayB5dMJzrYCHhD8C7") (Pair { Unit }  (Pair' +
             ' { True }  (Pair (Pair 19 10) (Pair (Left "tz1cxcwwnz' +
             'ENRdhe2Kb8ZdTrdNy4bFNyScx5") (Pair { Elt 0 "foo" ; El' +
             't 1 "bar" }  { PACK } )))))))))',
             'Unit'),

            ('packunpack_rev_cty.tz', 'Unit',
             '(Pair "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9' +
             'sDVC9yav" (Pair Unit (Pair "edsigthTzJ8X7MPmNeEwybRAv' +
             'dxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8' +
             'V2w8ayB5dMJzrYCHhD8C7" (Pair None (Pair {  }  (Pair {' +
             '  }  (Pair (Pair 40 -10) (Pair (Right "2019-09-09T08:' +
             '35:33Z") (Pair {  }  { DUP ; DROP ; PACK } )))))))))',
             'Unit'),

            # Test EDIV on nat and int
            ('ediv.tz',
             '(Pair None (Pair None (Pair None None)))',
             '(Pair 10 -3)',
             '(Pair (Some (Pair -3 1)) (Pair (Some (Pair 3 1)) ' +
             '(Pair (Some (Pair -3 1)) (Some (Pair 3 1)))))'),
            ('ediv.tz',
             '(Pair None (Pair None (Pair None None)))',
             '(Pair 10 0)',
             '(Pair None (Pair None (Pair None None)))'),
            ('ediv.tz',
             '(Pair None (Pair None (Pair None None)))',
             '(Pair -8 2)',
             '(Pair (Some (Pair -4 0)) (Pair (Some (Pair -4 0)) ' +
             '(Pair (Some (Pair 4 0)) (Some (Pair 4 0)))))'),

            # Test EDIV on mutez
            ('ediv_mutez.tz', '(Left None)', '(Pair 10 (Left 10))', '(Left (Some (Pair 1 0)))'),
            ('ediv_mutez.tz', '(Left None)', '(Pair 10 (Left 3))', '(Left (Some (Pair 3 1)))'),
            ('ediv_mutez.tz', '(Left None)', '(Pair 10 (Left 0))', '(Left None)'),

            ('ediv_mutez.tz', '(Left None)', '(Pair 10 (Right 10))', '(Right (Some (Pair 1 0)))'),
            ('ediv_mutez.tz', '(Left None)', '(Pair 10 (Right 3))', '(Right (Some (Pair 3 1)))'),
            ('ediv_mutez.tz', '(Left None)', '(Pair 10 (Right 0))', '(Right None)'),
            ('ediv_mutez.tz', '(Left None)', '(Pair 5 (Right 10))', '(Right (Some (Pair 0 5)))'),

            # Test compare
            ('compare.tz', 'Unit', 'Unit', 'Unit'),

            # Test comparison combinators:
            #   GT, GE, LT, LE, NEQ, EQ

            ('comparisons.tz', '{}',
             '{ -9999999; -1 ; 0 ; 1 ; 9999999 }',
             '{ ' +
             '{ False ; False ; False ; True ; True } ;' "\n"
             '    { False ; False ; True ; True ; True } ;' "\n"
             '    { True ; True ; False ; False ; False } ;' "\n"
             '    { True ; True ; True ; False ; False } ;' "\n"
             '    { True ; True ; False ; True ; True } ;' "\n"
             '    { False ; False ; True ; False ; False }'
             ' }'),

            # Test ADDRESS
            ('address.tz', 'None', '"tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5"',
             '(Some "tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5")'),

            # Test (CONTRACT unit)
            ('contract.tz', 'Unit', '"tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5"',
             'Unit'),

            # Test create_contract
            ('create_contract.tz', 'None', 'Unit', '(Some "KT1Mjjcb6tmSsLm7Cb3DSQszePjfchPM4Uxm")'),

            # Test multiplication - success case (no overflow)
            # Failure case is tested in m̀ul_overflow.tz
            ('mul.tz', 'Unit', 'Unit', 'Unit'),

            # Test NEG
            ('neg.tz', '0', '(Left 2)', '-2'),
            ('neg.tz', '0', '(Right 2)', '-2'),
            ('neg.tz', '0', '(Left 0)', '0'),
            ('neg.tz', '0', '(Right 0)', '0'),
            ('neg.tz', '0', '(Left -2)', '2'),

            # Test DIGN, DUGN, DROPN, DIPN
            ('dign.tz','0', '(Pair (Pair (Pair (Pair 1 2) 3) 4) 5)', '5'),
            ('dugn.tz', '0', '(Pair (Pair (Pair (Pair 1 2) 3) 4) 5)', '1'),
            ('dropn.tz','0', '(Pair (Pair (Pair (Pair 1 2) 3) 4) 5)', '5'),
            ('dipn.tz', '0', '(Pair (Pair (Pair (Pair 1 2) 3) 4) 5)', '6'),

            # Test Partial Exec
            ('pexec.tz', '14', '38', '52'),
            ('pexec_2.tz', "{ 0 ; 1 ; 2 ; 3}", '4', "{ 0 ; 7 ; 14 ; 21 }")
        ])
    def test_contract_input_output(self,
                                   client,
                                   contract,
                                   param,
                                   storage,
                                   expected):
        if contract.endswith('.tz'):
            contract = f'{CONTRACT_PATH}/{contract}'
            run_script_res = client.run_script(contract, param, storage)
            assert run_script_res.storage == expected

    def test_packunpack(self, client):
        """Test PACK/UNPACK and binary format."""
        assert_run_script_success(
            client,
            f'{CONTRACT_PATH}/packunpack.tz',
            'Unit',
            '(Pair (Pair (Pair "toto" {3;7;9;1}) {1;2;3}) ' +
            '0x05070707070100000004746f746f020000000800030' +
            '007000900010200000006000100020003)'
        )
        assert_run_script_failwith(
            client,
            f'{CONTRACT_PATH}/packunpack.tz',
            'Unit',
            '(Pair (Pair (Pair "toto" {3;7;9;1}) {1;2;3}) ' +
            '0x05070707070100000004746f746f020000000800030' +
            '0070009000102000000060001000200030004)'
        )

    def test_fails_annotated_set_car_cdr(self, client):
        """Tests the SET_CAR and SET_CDR instructions."""
        def cmd():
            client.run_script(f'{CONTRACT_PATH}/set_car.tz',
                              '(Pair %wrong %field "hello" 0)',
                              '""')
        assert check_run_failure(cmd, r'The two annotations do not match')

    def test_check_signature(self, client):
        sig = 'edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZA' \
              + 'e7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7'
        assert_run_script_success(
            client,
            f'{CONTRACT_PATH}/check_signature.tz',
            f'(Pair "{sig}" "hello")',
            '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"'
        )
        assert_run_script_failwith(
            client,
            f'{CONTRACT_PATH}/check_signature.tz',
            f'(Pair "{sig}" "abcd")',
            '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"'
        )

    def test_store_input(self, client):
        client.transfer(1000, "bootstrap1", KEY1, ['--burn-cap', '0.257'])
        bake(client)

        client.transfer(2000, "bootstrap1", KEY2, ['--burn-cap', '0.257'])
        bake(client)

        assert_balance(client, KEY1, 1000)
        assert_balance(client, KEY2, 2000)

        # Create a contract and transfer 100 ꜩ to it
        init_with_transfer(client, f'{CONTRACT_PATH}/store_input.tz',
                           KEY1, '""', 100, 'bootstrap1')

        client.transfer(100, "bootstrap1", "store_input",
                        ["-arg", '"abcdefg"', '--burn-cap', '10'])
        bake(client)

        assert_balance(client, "store_input", 200)

        assert_storage_contains(client, "store_input", '"abcdefg"')

        client.transfer(100, "bootstrap1", "store_input",
                        ["-arg", '"xyz"', '--burn-cap', '10'])
        bake(client)

        assert_storage_contains(client, "store_input", '"xyz"')

    def test_transfer_amount(self, client):
        init_with_transfer(client,
                           f'{CONTRACT_PATH}/transfer_amount.tz',
                           KEY1, '0', 100, 'bootstrap1')

        client.transfer(500, "bootstrap1", 'transfer_amount',
                        ['-arg', 'Unit', '--burn-cap', '10'])
        bake(client)

        assert_storage_contains(client, "transfer_amount", '500000000')

    def test_now(self, client):
        init_with_transfer(client,
                           f'{CONTRACT_PATH}/store_now.tz',
                           KEY1, '"2017-07-13T09:19:01Z"', 100, 'bootstrap1')

        client.transfer(500, "bootstrap1", 'store_now',
                        ['-arg', 'Unit', '--burn-cap', '10'])
        bake(client)

        assert_storage_contains(client, 'store_now',
                                f'"{client.get_timestamp()}"')

    def test_transfer_tokens(self, client):
        """Tests TRANSFER_TOKENS."""
        client.originate_account('test_transfer_account1', KEY1, 100,
                                 'bootstrap1', ['--burn-cap', '10'])
        bake(client)

        client.originate_account('test_transfer_account2', KEY2, 20,
                                 'bootstrap1', ['--burn-cap', '10'])
        bake(client)

        init_with_transfer(client, f'{CONTRACT_PATH}/transfer_tokens.tz',
                           KEY2, 'Unit', 1000, 'bootstrap1')

        assert_balance(client, 'test_transfer_account1', 100)

        account1_addr = client.get_contract_address('test_transfer_account1')
        client.transfer(100, 'bootstrap1', 'transfer_tokens',
                        ['-arg', f'"{account1_addr}"', '--burn-cap', '10'])
        bake(client)

        # Why isn't this 200 ꜩ? Baking fee?
        assert_balance(client, 'test_transfer_account1', 200)

        account2_addr = client.get_contract_address('test_transfer_account2')
        client.transfer(100, 'bootstrap1', 'transfer_tokens',
                        ['-arg', f'"{account2_addr}"', '--burn-cap', '10'])
        bake(client)

        assert_balance(client, 'test_transfer_account2', 120)

    def test_self(self, client):
        init_with_transfer(client, f'{CONTRACT_PATH}/self.tz',
                           KEY1, '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"',
                           1000, 'bootstrap1')

        client.transfer(0, 'bootstrap1', 'self', ['--burn-cap', '10'])
        bake(client)

        self_addr = client.get_contract_address('self')
        assert_storage_contains(client, 'self', f'"{self_addr}"')

    def test_slice(self, client):
        init_with_transfer(
            client, f'{CONTRACT_PATH}/slices.tz', 'bootstrap1',
            '"sppk7dBPqMPjDjXgKbb5f7V3PuKUrA4Zuwc3c3H7XqQerqPUWbK7Hna"',
            1000, 'bootstrap1')

    @pytest.mark.parametrize('contract_arg',
                             [line.rstrip('\n')
                              for line
                              in open(f'{paths.TEZOS_HOME}/tests_python/tests/'
                                      + 'test_slice_fails_params.txt')])
    def test_slice_fails(self, client, contract_arg):
        def cmd():
            client.transfer(
                0, 'bootstrap1', 'slices',
                ['-arg', contract_arg, '--burn-cap', '10'])

        assert check_run_failure(cmd, r'script reached FAILWITH instruction')
        # bake(client)

    @pytest.mark.parametrize('contract_arg',
                             [line.rstrip('\n')
                              for line
                              in open(f'{paths.TEZOS_HOME}/tests_python/tests/'
                                      + 'test_slice_success_params.txt')])
    def test_slice_success(self, client, contract_arg):
        client.transfer(0, 'bootstrap1', 'slices',
                        ['-arg', contract_arg, '--burn-cap', '10'])
        bake(client)

    def test_split_string(self, client):
        init_with_transfer(client, f'{CONTRACT_PATH}/split_string.tz',
                           'bootstrap1', '{}',
                           1000, 'bootstrap1')

        client.transfer(0, 'bootstrap1', 'split_string',
                        ['-arg', '"abc"', '--burn-cap', '10'])
        bake(client)
        assert_storage_contains(client, 'split_string',
                                '{ "a" ; "b" ; "c" }')

        client.transfer(0, 'bootstrap1', 'split_string',
                        ['-arg', '"def"', '--burn-cap', '10'])
        bake(client)
        assert_storage_contains(client, 'split_string',
                                '{ "a" ; "b" ; "c" ; "d" ; "e" ; "f" }')

    def test_split_bytes(self, client):
        init_with_transfer(client, f'{CONTRACT_PATH}/split_bytes.tz',
                           'bootstrap1', '{}',
                           1000, 'bootstrap1')

        client.transfer(0, 'bootstrap1', 'split_bytes',
                        ['-arg', '0xaabbcc', '--burn-cap', '10'])
        bake(client)
        assert_storage_contains(client, 'split_bytes',
                                '{ 0xaa ; 0xbb ; 0xcc }')

        client.transfer(0, 'bootstrap1', 'split_bytes',
                        ['-arg', '0xddeeff', '--burn-cap', '10'])
        bake(client)
        assert_storage_contains(client, 'split_bytes',
                                '{ 0xaa ; 0xbb ; 0xcc ; 0xdd ; 0xee ; 0xff }')

    def test_hash_consistency_michelson_cli(self, client):
        hash_result = client.hash(
            '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))',
            '(pair mutez (pair timestamp int))').blake2b
        hash_contract = f'{CONTRACT_PATH}/hash_consistency_checker.tz'
        run_script_res = client.run_script(
            hash_contract, '0x00',
            '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))')
        assert run_script_res.storage == hash_result
        run_script_res = client.run_script(
            hash_contract, '0x00',
            '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))')
        assert run_script_res.storage == hash_result


def assert_storage_contains(client: Client,
                            contract: str,
                            expected_storage: str) -> None:
    actual_storage = client.get_script_storage(contract)
    assert actual_storage == expected_storage


def contract_name_of_file(contract_path: str) -> str:
    return os.path.splitext(os.path.basename(contract_path))[0]


def bake(client: Client) -> BakeForResult:
    return client.bake('bootstrap1',
                       ['--max-priority', '512',
                        '--minimal-timestamp',
                        '--minimal-fees', '0',
                        '--minimal-nanotez-per-byte', '0',
                        '--minimal-nanotez-per-gas-unit', '0'])


def init_with_transfer(client: Client,
                       contract: str,
                       manager: str,
                       initial_storage: str,
                       amount: float,
                       sender: str):
    client.originate(contract_name_of_file(contract), manager, amount,
                     sender, contract,
                     ['-init', initial_storage, '--burn-cap', '10'])
    bake(client)


def assert_balance(client: Client,
                   account: str,
                   expected_balance: float) -> None:
    actual_balance = client.get_balance(account)
    assert actual_balance == expected_balance


def assert_run_script_success(client: Client,
                              contract: str,
                              param: str,
                              storage: str) -> RunScriptResult:
    return client.run_script(contract, param, storage)


def assert_run_script_failwith(client: Client,
                               contract: str,
                               param: str,
                               storage: str) -> None:
    # TODO: ideally check that an the actual FAILWITH instruction is
    # reached. to do this, client needs modification
    def cmd():
        client.run_script(contract, param, storage)

    assert check_run_failure(cmd, r'script reached FAILWITH instruction')
