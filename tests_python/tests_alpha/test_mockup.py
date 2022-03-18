""" This file tests the mockup mode (tezos-client --mode mockup).
    In this mode the client does not need a node running.

    Make sure to either use the fixture mockup_client or
    to mimick it if you want a mockup with custom parameters.

    Care is taken not to leave any base_dir dangling after
    tests are finished. Please continue doing this.
"""
import json
import os
import re
import shutil
import tempfile
from pathlib import Path
from typing import Any, List, Optional, Tuple
import pytest
from launchers.sandbox import Sandbox
from client.client import Client
from client.client_output import CreateMockupResult

from . import protocol

_BA_FLAG = "bootstrap-accounts"
_PC_FLAG = "protocol-constants"


@pytest.mark.client
def test_list_mockup_protocols(sandbox: Sandbox):
    """Executes `tezos-client list mockup protocols`
    The call must succeed and return a non empty list.
    """
    try:
        client = sandbox.create_client()
        protocols = client.list_mockup_protocols().mockup_protocols
        assert protocols
    finally:
        shutil.rmtree(client.base_dir)


@pytest.mark.client
def test_create_mockup_dir_exists_nonempty(sandbox: Sandbox):
    """Executes `tezos-client --base-dir /tmp/mdir create mockup`
    when /tmp/mdir is a non empty directory which is NOT a mockup
    directory. The call must fail.
    """
    with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir:
        # Make the directory not empty
        with open(os.path.join(base_dir, "whatever"), "w") as handle:
            handle.write("")
        unmanaged_client = sandbox.create_client(base_dir=base_dir)
        res = unmanaged_client.create_mockup(
            protocol=protocol.HASH, check=False
        ).create_mockup_result
        assert res == CreateMockupResult.DIR_NOT_EMPTY


@pytest.mark.client
def test_retrieve_addresses(mockup_client: Client):
    """Retrieves known addresses of a fresh mockup.
    The call must succeed.
    """
    addresses = mockup_client.get_known_addresses().wallet
    assert addresses == {
        'bootstrap1': 'tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx',
        'bootstrap2': 'tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN',
        'bootstrap3': 'tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU',
        'bootstrap4': 'tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv',
        'bootstrap5': 'tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv',
    }


@pytest.mark.client
def test_create_mockup_already_initialized(mockup_client: Client):
    """Executes `tezos-client --base-dir /tmp/mdir create mockup`
    when /tmp/mdir is not fresh.
    The call must fail.
    """
    # mockup was created already by fixture, try to create it second time:
    res = mockup_client.create_mockup(
        protocol=protocol.HASH, check=False
    ).create_mockup_result
    # it should fail:
    assert res == CreateMockupResult.ALREADY_INITIALIZED


@pytest.mark.client
def test_transfer(mockup_client: Client):
    """Executes `tezos-client --base-dir /tmp/mdir -M mockup
              transfer 1 from bootstrap1 to bootstrap2`
    in a valid mockup environment.
    The call must succeed and the balances must be updated correctly.
    """
    giver = "bootstrap1"
    receiver = "bootstrap2"
    transferred = 1.0

    giver_balance_before = mockup_client.get_balance(giver)
    receiver_balance_before = mockup_client.get_balance(receiver)
    mockup_client.transfer(transferred, giver, receiver)
    giver_balance_after = mockup_client.get_balance(giver)
    receiver_balance_after = mockup_client.get_balance(receiver)

    assert giver_balance_after < giver_balance_before - transferred
    assert receiver_balance_after == receiver_balance_before + transferred


# It's impossible to guess values of chain_id, these ones have been
# obtained by looking at the output of `compute chain id from seed`
@pytest.mark.parametrize(
    'chain_id',
    [
        "NetXcqTGZX74DxG",
        "NetXaFDF7xZQCpR",
        "NetXkKbtqncJcAz",
        "NetXjjE5cZUeWPy",
        "NetXi7C1pyLhQNe",
    ],
)
@pytest.mark.parametrize(
    'initial_timestamp', ["2020-07-21T17:11:10+02:00", "1970-01-01T00:00:00Z"]
)
@pytest.mark.client
def test_create_mockup_custom_constants(
    sandbox: Sandbox, chain_id: str, initial_timestamp: str
):
    """Tests `tezos-client create mockup` --protocols-constants  argument
    The call must succeed.

    Args:
        mockup_client: the client to use
        chain_id (str): the string to pass for field `chain_id`
        initial_timestamp(str): an ISO-8601 formatted date string
    """
    # Use another directory so that the constants change takes effect
    with tempfile.TemporaryDirectory(
        prefix='tezos-client.'
    ) as base_dir, tempfile.NamedTemporaryFile(
        prefix='tezos-custom-constants', mode='w+t'
    ) as json_file:
        json_data = {
            "hard_gas_limit_per_operation": "400000",
            "chain_id": chain_id,
            "initial_timestamp": initial_timestamp,
        }
        json.dump(json_data, json_file)
        json_file.flush()
        unmanaged_client = sandbox.create_client(base_dir=base_dir)
        res = unmanaged_client.create_mockup(
            protocol=protocol.HASH, protocol_constants_file=json_file.name
        ).create_mockup_result
        assert res == CreateMockupResult.OK


def _create_accounts_list():
    """
    Returns a list of dictionary with 3 entries, that are
    valid for being translated to json and passed
    to `--bootstrap-accounts`
    """
    accounts_list = []

    def add_account(name: str, sk_uri: str, amount: str):
        entry = {
            "name": name,
            "sk_uri": "unencrypted:" + sk_uri,
            "amount": amount,
        }
        accounts_list.append(entry)

    # Took json structure from
    # https://gitlab.com/tezos/tezos/-/merge_requests/1720
    add_account(
        "bootstrap0",
        "edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3",
        "2000000000000",
    )
    add_account(
        "bootstrap1",
        "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh",
        "1000000000000",
    )

    return accounts_list


@pytest.mark.client
def test_create_mockup_custom_bootstrap_accounts(sandbox: Sandbox):
    """Tests `tezos-client create mockup` --bootstrap-accounts argument
    The call must succeed.
    """
    accounts_list = _create_accounts_list()

    # Use another directory so that the constants change takes effect
    with tempfile.TemporaryDirectory(
        prefix='tezos-client.'
    ) as base_dir, tempfile.NamedTemporaryFile(
        prefix='tezos-bootstrap-accounts', mode='w+t'
    ) as json_file:
        json.dump(accounts_list, json_file)
        json_file.flush()
        # Follow pattern of mockup_client fixture:
        unmanaged_client = sandbox.create_client(base_dir=base_dir)
        res = unmanaged_client.create_mockup(
            protocol=protocol.HASH, bootstrap_accounts_file=json_file.name
        ).create_mockup_result
        assert res == CreateMockupResult.OK
        mock_client = sandbox.create_client(base_dir=base_dir, mode="mockup")
        addresses_result = mock_client.get_known_addresses()
        names_sent = sorted([account["name"] for account in accounts_list])
        names_witnessed = sorted(list(addresses_result.wallet.keys()))
        assert names_sent == names_witnessed


@pytest.mark.client
def test_transfer_bad_base_dir(sandbox: Sandbox):
    """Executes `tezos-client --base-dir /tmp/mdir create mockup`
    when /tmp/mdir looks like a dubious base directory.
    Checks that a warning is printed.
    """
    try:
        unmanaged_client = sandbox.create_client()
        res = unmanaged_client.create_mockup(
            protocol=protocol.HASH
        ).create_mockup_result
        assert res == CreateMockupResult.OK
        base_dir = unmanaged_client.base_dir
        mockup_dir = os.path.join(base_dir, "mockup")

        # A valid mockup has a directory named "mockup", in its directory:
        assert os.path.isdir(mockup_dir)
        mock_client = sandbox.create_client(base_dir=base_dir, mode="mockup")
        # Delete this directory:
        shutil.rmtree(mockup_dir)
        # And put a file instead:
        Path(os.path.join(mockup_dir)).touch()

        # Now execute a command
        cmd = ["transfer", "1", "from", "bootstrap1", "to", "bootstrap2"]
        (_, err_output, _) = mock_client.run_generic(cmd, check=False)
        # See
        # https://gitlab.com/tezos/tezos/-/merge_requests/1760#note_329071488
        # for the content being matched
        searched = "Some commands .* might not work correctly."
        # Witness that warning is printed:
        assert re.search(
            searched, err_output
        ), f"'{searched}' not matched in error output"
    finally:
        shutil.rmtree(base_dir)


@pytest.mark.client
def test_config_show_mockup(mockup_client: Client):
    """Executes `tezos-client --mode mockup config show` in
    a state where it should succeed.
    """
    mockup_client.run_generic(["--protocol", protocol.HASH, "config", "show"])


@pytest.mark.client
def test_config_show_mockup_fail(mockup_client: Client):
    """Executes `tezos-client --mode mockup config show` when
    base dir is NOT a mockup. It should fail as this is dangerous
    (the default base directory could contain sensitive data,
     such as private keys)
    """
    shutil.rmtree(mockup_client.base_dir)  # See test_config_init_mockup_fail
    # for a variant of how to make the base dir invalid for the mockup mode
    _, _, return_code = mockup_client.run_generic(
        ["config", "show"], check=False
    )

    # recreate directory: the cleanup later on expects its existence
    os.mkdir(mockup_client.base_dir)
    assert return_code != 0


@pytest.mark.client
def test_config_init_mockup(mockup_client: Client):
    """Executes `tezos-client config init mockup` in
    a state where it should succeed.
    """
    # We cannot use NamedTemporaryFile because `config init mockup`
    # does not overwrite files. Because NamedTemporaryFile creates the file
    # it would make the test fail.
    ba_json_file = tempfile.mktemp(prefix='tezos-bootstrap-accounts')
    pc_json_file = tempfile.mktemp(prefix='tezos-proto-consts')
    # 1/ call `config init mockup`
    mockup_client.run(
        [
            "--protocol",
            protocol.HASH,
            "config",
            "init",
            f"--{_BA_FLAG}",
            ba_json_file,
            f"--{_PC_FLAG}",
            pc_json_file,
        ]
    )

    # 2/ Try loading the files, to check they are valid json
    with open(ba_json_file) as handle:
        json.load(handle)
    with open(pc_json_file) as handle:
        json.load(handle)

    # Cleanup
    os.remove(ba_json_file)
    os.remove(pc_json_file)


@pytest.mark.client
def test_config_init_mockup_fail(mockup_client: Client):
    """Executes `tezos-client config init mockup` when
    base dir is NOT a mockup. It should fail as this is dangerous
    (the default base directory could contain sensitive data,
     such as private keys)
    """
    ba_json_file = tempfile.mktemp(prefix='tezos-bootstrap-accounts')
    pc_json_file = tempfile.mktemp(prefix='tezos-proto-consts')
    cmd = [
        "--protocol",
        protocol.HASH,
        "config",
        "init",
        f"--{_BA_FLAG}",
        ba_json_file,
        f"--{_PC_FLAG}",
        pc_json_file,
    ]

    # A valid mockup has a directory named "mockup" in its base_dir:
    mockup_dir = os.path.join(mockup_client.base_dir, "mockup")
    assert os.path.isdir(mockup_dir)
    # Delete this directory, so that the base_dir is not a valid mockup
    # base dir anymore:
    shutil.rmtree(mockup_dir)  # See test_config_show_mockup_fail above
    # for a variant of how to make the base_dir invalid for the mockup mode

    _, _, return_code = mockup_client.run_generic(cmd, check=False)
    assert return_code != 0
    # Check the test doesn't leak directories:
    assert not os.path.exists(ba_json_file)
    assert not os.path.exists(pc_json_file)


def _try_json_loads(flag: str, string: str) -> Any:
    """Converts the given string to a json object"""
    try:
        return json.loads(string)
    except json.JSONDecodeError:
        pytest.fail(
            f"""Write back of {flag} value is not valid json:
{string}"""
        )
        # Added to get rid of pylint warning inconsistent-return-statements.
        # pytest.fail has no return value (NoReturn).
        return None


def _get_state_using_config_init_mockup(
    mock_client: Client,
) -> Tuple[str, str]:
    """
    Calls `config init mockup` on a mockup client and returns
    the strings of the bootstrap accounts and the protocol
    constants

    Note that because this a mockup specific operation, the `mock_client`
    parameter must be in mockup mode; do not give a vanilla client.
    """
    ba_json_file = tempfile.mktemp(prefix='tezos-bootstrap-accounts')
    pc_json_file = tempfile.mktemp(prefix='tezos-proto-consts')

    mock_client.run(
        [
            "--protocol",
            protocol.HASH,
            "config",
            "init",
            f"--{_BA_FLAG}",
            ba_json_file,
            f"--{_PC_FLAG}",
            pc_json_file,
        ]
    )

    with open(ba_json_file) as handle:
        ba_str = handle.read()
    with open(pc_json_file) as handle:
        pc_str = handle.read()

    # Cleanup of tempfile.mktemp
    os.remove(ba_json_file)
    os.remove(pc_json_file)

    return (ba_str, pc_str)


def _get_state_using_config_show_mockup(
    mock_client: Client,
) -> Tuple[str, str]:
    """
    Calls `--mode mockup config show` on a mockup client and returns
    the strings of the bootstrap accounts and the protocol
    constants, by parsing standard output.

    Note that because this a mockup specific operation, the `mock_client`
    parameter must be in mockup mode; do not give a vanilla client.
    """

    def _find_line_starting_with(strings, searched) -> int:
        i = 0
        for string in strings:
            if string.startswith(searched):
                return i
            i += 1
        return -1

    def _parse_config_init_output(string: str) -> Tuple[str, str]:
        """Parses the output of `--mode mockup config init`
        and return the json of the bootstrap accounts
        and the protocol constants
        """
        tagline1 = f"Default value of --{_BA_FLAG}:"
        bootstrap_accounts_index = string.find(tagline1)
        assert bootstrap_accounts_index >= 0, f"{_BA_FLAG} line not found"

        tagline2 = f"Default value of --{_PC_FLAG}:"
        proto_constants_index = string.find(tagline2)
        assert proto_constants_index > 0, f"{_PC_FLAG} line not found"

        bc_json = string[
            bootstrap_accounts_index + len(tagline1) : proto_constants_index - 1
        ]

        pc_json = string[proto_constants_index + len(tagline2) + 1 :]
        return (bc_json, pc_json)

    stdout = mock_client.run(["--protocol", protocol.HASH, "config", "show"])
    return _parse_config_init_output(stdout)


def write_file(filename, contents):
    filename.write(contents)
    filename.flush()


def _gen_assert_msg(flag, sent, received):
    return (
        f"Json sent with --{flag} differs from json received"
        f"\nJson sent is:\n{sent}"
        f"\nwhile json received is:\n{received}"
    )


def rm_amounts(bootstrap_accounts):
    for account in bootstrap_accounts:
        account.pop('amount', None)


def compute_expected_amounts(
    bootstrap_accounts, frozen_deposits_percentage: int
) -> None:
    pct = 100 - frozen_deposits_percentage
    for account in bootstrap_accounts:
        account['amount'] = str(int(pct * int(account['amount']) / 100))


def _test_create_mockup_init_show_roundtrip(
    sandbox: Sandbox,
    read_initial_state,
    read_final_state,
    bootstrap_json: Optional[str] = None,
    protocol_constants_json: Optional[str] = None,
):
    """1/ Creates a mockup, using possibly custom bootstrap_accounts
       (as specified by `bootstrap_json`)
    2/ Then execute either `--mode mockup config show` or
       `--mode mockup config init` to obtain the mockup's parameters
       (parse stdout if `show` is called,
       read the files generated by `init` otherwise)

       This is done by executing `read_initial_state`
    3/ Recreate a mockup using the output gathered in 2/ and call
       `--mode mockup config show`/`--mode mockup config init`
       (this is done by executing `read_final_state`) to check that output
       received is similar to output seen in 2.

    This is a roundtrip test.
    """

    ba_file = None
    pc_file = None

    try:
        if protocol_constants_json is not None:
            pc_file = tempfile.mktemp(prefix='tezos-proto-consts')
            with open(pc_file, 'w') as handle:
                handle.write(protocol_constants_json)

        if bootstrap_json is not None:
            ba_file = tempfile.mktemp(prefix='tezos-bootstrap-accounts')
            with open(ba_file, 'w') as handle:
                handle.write(bootstrap_json)

        with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir:
            # Follow pattern of mockup_client fixture:
            unmanaged_client = sandbox.create_client(base_dir=base_dir)
            res = unmanaged_client.create_mockup(
                protocol=protocol.HASH,
                bootstrap_accounts_file=ba_file,
                protocol_constants_file=pc_file,
            ).create_mockup_result
            assert res == CreateMockupResult.OK
            mock_client = sandbox.create_client(
                base_dir=base_dir, mode="mockup"
            )
            (ba_str, pc_str) = read_initial_state(mock_client)
    finally:
        if pc_file is not None:
            os.remove(pc_file)
        if ba_file is not None:
            os.remove(ba_file)

    # 2a/ Check the json obtained is valid by building json objects
    ba_sent = _try_json_loads(_BA_FLAG, ba_str)
    pc_sent = _try_json_loads(_PC_FLAG, pc_str)

    # Test that the initial mockup call honored the values it received. If
    # it didn't, all calls would return the default values all along, and
    # everything would seem fine; but it wouldn't be. This was witnessed in
    # https://gitlab.com/tezos/tezos/-/issues/938
    if bootstrap_json:
        ba_input = json.loads(bootstrap_json)
        # adjust amount field on Tenderbake w.r.t. to frozen_deposits_percentage
        compute_expected_amounts(
            ba_input, int(pc_sent['frozen_deposits_percentage'])
        )
        assert ba_sent == ba_input

    if protocol_constants_json:
        pc_input = json.loads(protocol_constants_json)
        assert pc_sent == pc_input

    # 3/ Pass obtained json to a new mockup instance, to check json
    # is valid w.r.t. ocaml encoding

    # Use another directory so that the constants change takes effect
    with tempfile.TemporaryDirectory(
        prefix='tezos-client.'
    ) as base_dir, tempfile.NamedTemporaryFile(
        prefix='tezos-bootstrap-accounts', mode='w+t'
    ) as ba_json_file, tempfile.NamedTemporaryFile(
        prefix='tezos-proto-consts', mode='w+t'
    ) as pc_json_file, tempfile.TemporaryDirectory(
        prefix='tezos-client.'
    ) as base_dir:

        write_file(ba_json_file, ba_str)
        write_file(pc_json_file, pc_str)

        with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir:
            # Follow pattern of mockup_client fixture:
            unmanaged_client = sandbox.create_client(base_dir=base_dir)
            res = unmanaged_client.create_mockup(
                protocol=protocol.HASH,
                protocol_constants_file=pc_json_file.name,
                bootstrap_accounts_file=ba_json_file.name,
            ).create_mockup_result
            assert res == CreateMockupResult.OK
            mock_client = sandbox.create_client(
                base_dir=base_dir, mode="mockup"
            )
            # 4/ Retrieve state again
            (ba_received_str, pc_received_str) = read_final_state(mock_client)

    # Convert it to json objects (check that json is valid)
    ba_received = _try_json_loads(_BA_FLAG, ba_received_str)
    pc_received = _try_json_loads(_PC_FLAG, pc_received_str)

    # and finally check that json objects received are the same
    # as the ones that were given as input

    # adjust amount field on Tenderbake w.r.t. to frozen_deposits_percentage
    compute_expected_amounts(
        ba_sent, int(pc_sent['frozen_deposits_percentage'])
    )

    assert ba_sent == ba_received, _gen_assert_msg(
        _BA_FLAG, ba_sent, ba_received
    )
    assert pc_sent == pc_received, _gen_assert_msg(
        _PC_FLAG, pc_sent, pc_received
    )


@pytest.mark.client
@pytest.mark.parametrize(
    'initial_bootstrap_accounts', [None, json.dumps(_create_accounts_list())]
)
# The following values should be different from the default ones in
# order to check loading of the parameters.
@pytest.mark.parametrize(
    'protocol_constants',
    [
        None,
        json.dumps(
            {
                "initial_timestamp": "2021-02-03T12:34:56Z",
                "chain_id": "NetXaFDF7xZQCpR",
                "min_proposal_quorum": 501,
                "quorum_max": 7001,
                "quorum_min": 2001,
                "hard_storage_limit_per_operation": "60001",
                "cost_per_byte": "251",
                "baking_reward_fixed_portion": "20000000",
                "baking_reward_bonus_per_slot": "2500",
                "endorsing_reward_per_slot": "2857",
                "origination_size": 258,
                "seed_nonce_revelation_tip": "125001",
                "tokens_per_roll": "8000000001",
                "proof_of_work_threshold": "-2",
                "hard_gas_limit_per_block": "10400001",
                "hard_gas_limit_per_operation": "1040001",
                'consensus_committee_size': 12,
                # DO NOT EDIT the value consensus_threshold this is actually a
                # constant, not a parameter
                'consensus_threshold': 0,
                'initial_seed': None,
                'minimal_participation_ratio': {
                    'denominator': 5,
                    'numerator': 1,
                },
                'minimal_block_delay': '1',
                'delay_increment_per_round': '1',
                'max_slashing_period': 12,
                "cycles_per_voting_period": 7,
                "blocks_per_stake_snapshot": 5,
                "blocks_per_commitment": 5,
                "blocks_per_cycle": 9,
                "preserved_cycles": 3,
                "liquidity_baking_toggle_ema_threshold": 1000000000,
                "liquidity_baking_subsidy": "2500000",
                "liquidity_baking_sunset_level": 1024,
                "max_operations_time_to_live": 120,
                "frozen_deposits_percentage": 10,
                'ratio_of_frozen_deposits_slashed_per_double_endorsement': {
                    'numerator': 1,
                    'denominator': 2,
                },
                "double_baking_punishment": "640000001",
                "cache_script_size": 100000001,
                "cache_stake_distribution_cycles": 10,
                "cache_sampler_state_cycles": 10,
                "tx_rollup_enable": False,
                "tx_rollup_origination_size": 30_000,
                "tx_rollup_hard_size_limit_per_inbox": 100_000,
                "tx_rollup_hard_size_limit_per_message": 5_000,
                "tx_rollup_commitment_bond": "10000000000",
                "tx_rollup_finality_period": 2000,
                "tx_rollup_withdraw_period": 123456,
                "tx_rollup_max_unfinalized_levels": 2100,
                "tx_rollup_max_messages_per_inbox": 1010,
                'tx_rollup_max_withdrawals_per_batch': 255,
                "tx_rollup_max_finalized_levels": 60_100,
                "tx_rollup_cost_per_byte_ema_factor": 321,
                "tx_rollup_max_ticket_payload_size": 10_240,
                "sc_rollup_enable": False,
                "sc_rollup_origination_size": 6_314,
                "sc_rollup_challenge_window_in_blocks": 20_160,
            }
        ),
    ],
)
@pytest.mark.parametrize(
    'read_initial_state',
    [_get_state_using_config_show_mockup, _get_state_using_config_init_mockup],
)
@pytest.mark.parametrize(
    'read_final_state',
    [_get_state_using_config_show_mockup, _get_state_using_config_init_mockup],
)
def test_create_mockup_config_show_init_roundtrip(
    sandbox: Sandbox,
    initial_bootstrap_accounts,
    protocol_constants,
    read_initial_state,
    read_final_state,
):
    """1/ Create a mockup, using possibly custom bootstrap_accounts
       (as specified by `initial_bootstrap_json`).
    2/ Then execute either `--mode mockup config show`
       or `--mode mockup config init` to obtain the mockup's parameters,
       as specified by `read_initial_state`.
    3/ Recreate a mockup using the output gathered in 2/ and call
       `read_final_state` to check that output
       received is similar to output seen in 2.
    This is a roundtrip test using a matrix.
    """
    _test_create_mockup_init_show_roundtrip(
        sandbox,
        read_initial_state,
        read_final_state,
        initial_bootstrap_accounts,
        protocol_constants,
    )


def test_transfer_rpc(mockup_client: Client):
    """Variant of test_transfer that uses RPCs to get the balances."""
    giver = "bootstrap1"
    receiver = "bootstrap2"
    transferred = 1.0
    transferred_mutz = transferred * 1000000

    def get_balance(tz1):
        res = mockup_client.rpc(
            'get',
            f'chains/main/blocks/head/context/contracts/{tz1}/balance',
        )
        return float(res)

    addresses = mockup_client.get_known_addresses()
    giver_tz1 = addresses.wallet[giver]
    recvr_tz1 = addresses.wallet[receiver]
    giver_balance_before = get_balance(giver_tz1)
    receiver_balance_before = get_balance(recvr_tz1)
    mockup_client.transfer(transferred, giver, receiver)
    giver_balance_after = get_balance(giver_tz1)
    receiver_balance_after = get_balance(recvr_tz1)

    assert giver_balance_after < giver_balance_before - transferred_mutz
    assert receiver_balance_after == receiver_balance_before + transferred_mutz


@pytest.mark.parametrize(
    'protos',
    [
        (proto1, proto2)
        for proto1 in [protocol.HASH, protocol.PREV_HASH]
        for proto2 in [protocol.HASH, protocol.PREV_HASH, ""]
    ],
)
@pytest.mark.parametrize(
    'command',
    [
        ["config", "show"],
        ["config", "init"],
        ["list", "known", "addresses"],
        ["get", "balance", "for", "bootstrap1"],
    ],
)
def test_proto_mix(
    sandbox: Sandbox, protos: Tuple[str, str], command: List[str]
):
    """
    This test covers 3 cases:

    1/ When proto's second element equals the first member:
       it tests that the command works.
    2/ When proto's second element is empty:
       it tests that the correct mockup implementation is picked
       (i.e. the one of the first element) and that the command works.
    3/ When protos' second element is not empty and differs from
       the first member: it tests
       that creating a mockup with a protocol and using it with another
       protocol fails.
    """
    proto1 = protos[0]
    proto2 = protos[1]
    with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir:
        # Follow pattern of mockup_client fixture:
        unmanaged_client = sandbox.create_client(base_dir=base_dir)
        res = unmanaged_client.create_mockup(protocol=proto1)
        assert res.create_mockup_result == CreateMockupResult.OK
        mock_client = sandbox.create_client(base_dir=base_dir, mode="mockup")
        cmd = (["--protocol", proto2] if proto2 else []) + command
        success = (proto2 == proto1) or (not proto2)
        (_, _, return_code) = mock_client.run_generic(cmd, check=False)
        assert (return_code == 0) == success
