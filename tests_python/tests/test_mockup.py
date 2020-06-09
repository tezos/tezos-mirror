""" This file tests the mockup mode (tezos-client --mode mockup).
    In this mode the client does not need a node running.

    In the tests fiddling with --base-dir, make sure to
    call client.py's set_base_dir method prior to doing
    queries.
"""
import json
import os
import re
import tempfile
from typing import Any, Iterator, Optional, Tuple
import pytest
from launchers.sandbox import Sandbox
from client.client import Client

from tools.constants import ALPHA

_PROTO = ALPHA
_BA_FLAG = "bootstrap-accounts"
_PC_FLAG = "protocol-constants"


@pytest.fixture
def mockup_client(sandbox: Sandbox) -> Client:
    """
        If you don't know what you're doing, you likely want
        the next fixture, not this one.
    """
    sandbox.add_mockup_client()
    client = sandbox.mockup_client
    assert client is not None
    return client


@pytest.fixture
def base_dir_n_mockup(mockup_client: Client) -> Iterator[Tuple[str, Client]]:
    """
        This is THE fixture to use when 1/ you're unsure or 2/ you're doing
        a positive test (i.e. a test which must succeed, in an
        environment where things are expected to work).

        In this scenario, you likely want to call this fixture
        and retrieve solely it's second value like this:

        `_, mockup_client = base_dir_n_mockup`
    """
    with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir:
        mockup_client.set_base_dir(base_dir)
        res = mockup_client.create_mockup(protocol=_PROTO).create_mockup_result
        assert res == 'ok'
        yield (base_dir, mockup_client)  # yield instead of return: so that
        # 'with' block is exited during teardown, see
        # https://docs.pytest.org/en/latest/fixture.html#fixture-finalization-executing-teardown-code


@pytest.mark.client
def test_list_mockup_protocols(mockup_client: Client):
    """ Executes `tezos-client list mockup protocols`
        The call must succeed and return a non empty list.
    """
    protocols = mockup_client.list_mockup_protocols().mockup_protocols
    assert protocols[0] == _PROTO


@pytest.mark.client
def test_create_mockup_file_exists(mockup_client: Client):
    """ Executes `tezos-client --base-dir /tmp/mdir create mockup`
        when /tmp/mdir is a file, whereas a directory is expected.
        The call must fail.
    """
    with tempfile.NamedTemporaryFile(prefix='tezos-client.') as tmp_file:
        mockup_client.set_base_dir(tmp_file.name)
        res = mockup_client.create_mockup(protocol=_PROTO,
                                          check=False)
        assert res.exit_code == 1 and res.create_mockup_result == 'is_not_dir'


@pytest.mark.client
def test_create_mockup_dir_exists_nonempty(mockup_client: Client):
    """ Executes `tezos-client --base-dir /tmp/mdir create mockup`
        when /tmp/mdir is a non empty directory which is NOT a mockup
        directory. The call must fail.
    """
    with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir:
        # Make the directory not empty
        with open(os.path.join(base_dir, "whatever"), "w") as handle:
            handle.write("")
        mockup_client.set_base_dir(base_dir)
        res = mockup_client.create_mockup(protocol=_PROTO,
                                          check=False).create_mockup_result
        assert res == 'dir_not_empty'


@pytest.mark.client
def test_create_mockup_fresh_dir(mockup_client: Client):
    """ Executes `tezos-client --base-dir /tmp/mdir create mockup`
        when /tmp/mdir is fresh and retrieves known addresses.
        Calls must succeed.
    """
    res = mockup_client.create_mockup(protocol=_PROTO).create_mockup_result
    assert res == 'ok'
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
    """ Executes `tezos-client --base-dir /tmp/mdir create mockup`
        when /tmp/mdir is not fresh.
        The call must fail.
    """
    res = mockup_client.create_mockup(protocol=_PROTO).create_mockup_result
    assert res == 'ok'
    res = mockup_client.create_mockup(protocol=_PROTO,
                                      check=False).create_mockup_result
    assert res == 'already_initialized'


@pytest.mark.client
def test_transfer(base_dir_n_mockup: Tuple[str, Client]):
    """ Executes `tezos-client --base-dir /tmp/mdir -M mockup
                  transfer 1 from bootstrap1 to bootstrap2`
        in a valid mockup environment.
        The call must succeed and the balances must be updated correctly.
    """
    _, mockup_client = base_dir_n_mockup
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


@pytest.mark.client
def test_create_mockup_custom_constants(mockup_client: Client):
    """ Tests `tezos-client create mockup` --protocols-constants  argument
        The call must succeed.
    """
    # Use another directory so that the constants change takes effect
    with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir,\
            tempfile.NamedTemporaryFile(prefix='tezos-custom-constants',
                                        mode='w+t') as json_file:
        json_data = {"hard_gas_limit_per_operation": "400000"}
        json.dump(json_data, json_file)
        json_file.flush()
        mockup_client.set_base_dir(base_dir)
        res = mockup_client.create_mockup(
            protocol=_PROTO,
            protocol_constants_file=json_file.name).create_mockup_result
        assert res == "ok"


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
            "amount": amount
        }
        accounts_list.append(entry)

    # Took json structure from
    # https://gitlab.com/tezos/tezos/-/merge_requests/1720
    add_account("bootstrap0",
                "edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3",
                "2000000000000")
    add_account("bootstrap1",
                "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh",
                "1000000000000")

    return accounts_list


@pytest.mark.client
def test_create_mockup_custom_bootstrap_accounts(mockup_client):
    """ Tests `tezos-client create mockup` --bootstrap-accounts argument
        The call must succeed.
    """
    accounts_list = _create_accounts_list()

    # Use another directory so that the constants change takes effect
    with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir,\
        tempfile.NamedTemporaryFile(prefix='tezos-bootstrap-accounts',
                                    mode='w+t') as json_file:
        json.dump(accounts_list, json_file)
        json_file.flush()
        mockup_client.set_base_dir(base_dir)
        res = mockup_client.create_mockup(
            protocol=_PROTO,
            bootstrap_accounts_file=json_file.name).create_mockup_result
        assert res == "ok"
        addresses_result = mockup_client.get_known_addresses()
        names_sent = sorted([account["name"] for account in accounts_list])
        names_witnessed = sorted(list(addresses_result.wallet.keys()))
        assert names_sent == names_witnessed


@pytest.mark.client
def test_transfer_bad_base_dir(mockup_client: Client):
    """ Executes `tezos-client --base-dir /tmp/mdir create mockup`
        when /tmp/mdir looks like a dubious base directory.
        Checks that a warning is printed.
    """
    with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir:
        # Create a FILE named "mockup", whereas a directory is expected;
        # so that the base_dir is invalid
        with open(os.path.join(base_dir, "mockup"), "w") as handle:
            handle.write("")
        mockup_client.set_base_dir(base_dir)
        cmd = ["transfer", "1", "from", "bootstrap1", "to", "boostrap2"]
        (_, err_output, _) = mockup_client.run_generic(cmd, check=False)
        # See
        # https://gitlab.com/tezos/tezos/-/merge_requests/1760#note_329071488
        # for the content being matched
        searched = "Some commands .* might not work correctly."
        assert re.search(
            searched, err_output), f"'{searched}' not matched in error output"


@pytest.mark.client
def test_config_show_mockup(base_dir_n_mockup):
    """ Executes `tezos-client config show mockup` in
        a state where it should succeed.
    """
    _, mockup_client = base_dir_n_mockup
    mockup_client.run_generic(["config", "show", "mockup"])


@pytest.mark.client
def test_config_show_mockup_fail(mockup_client):
    """ Executes `tezos-client config show mockup` when
        base dir is NOT a mockup. It should fail as this is dangerous
        (the default base directory could contain sensitive data,
         such as private keys)
    """
    _, _, return_code = mockup_client.run_generic(["config", "show", "mockup"],
                                                  check=False)
    assert return_code != 0


@pytest.mark.client
def test_config_init_mockup(base_dir_n_mockup):
    """ Executes `tezos-client config init mockup` in
        a state where it should succeed.
    """
    _, mockup_client = base_dir_n_mockup
    # We cannot use NamedTemporaryFile because `config init mockup`
    # does not overwrite files. Because NamedTemporaryFile creates the file
    # it would make the test fail.
    ba_json_file = tempfile.mktemp(prefix='tezos-bootstrap-accounts')
    pc_json_file = tempfile.mktemp(prefix='tezos-proto-consts')
    # 1/ call `config init mockup`
    mockup_client.run([
        "config", "init", "mockup", f"--{_BA_FLAG}", ba_json_file,
        f"--{_PC_FLAG}", pc_json_file
    ])

    # 2/ Try loading the files, to check they are valid json
    with open(ba_json_file) as handle:
        json.load(handle)
    with open(pc_json_file) as handle:
        json.load(handle)

    # Cleanup of tempfile.mktemp
    os.remove(ba_json_file)
    os.remove(pc_json_file)


@pytest.mark.client
def test_config_init_mockup_fail(mockup_client):
    """ Executes `tezos-client config init mockup` when
        base dir is NOT a mockup. It should fail as this is dangerous
        (the default base directory could contain sensitive data,
         such as private keys)
    """
    with tempfile.NamedTemporaryFile(
            prefix='tezos-bootstrap-accounts',
            mode='w+t') as ba_json_file, tempfile.NamedTemporaryFile(
                prefix='tezos-proto-consts', mode='w+t') as pc_json_file:
        cmd = [
            "config", "init", "mockup", f"--{_BA_FLAG}", ba_json_file.name,
            f"--{_PC_FLAG}", pc_json_file.name
        ]
        _, _, return_code = mockup_client.run_generic(cmd, check=False)
        assert return_code != 0


def _try_json_loads(flag: str, string: str) -> Any:
    """ Converts the given string to a json object """
    try:
        return json.loads(string)
    except json.JSONDecodeError:
        pytest.fail(f"""Write back of {flag} value is not valid json:
{string}""")


def _get_state_using_config_init_mockup(mockup_client) -> Tuple[str, str]:
    """
        Calls `config init mockup` on `m_client` and returns
        the strings of the bootstrap accounts and the protocol
        constants
    """
    ba_json_file = tempfile.mktemp(prefix='tezos-bootstrap-accounts')
    pc_json_file = tempfile.mktemp(prefix='tezos-proto-consts')

    mockup_client.run([
        "config", "init", "mockup", f"--{_BA_FLAG}", ba_json_file,
        f"--{_PC_FLAG}", pc_json_file
    ])

    with open(ba_json_file) as handle:
        ba_str = handle.read()
    with open(pc_json_file) as handle:
        pc_str = handle.read()

    # Cleanup of tempfile.mktemp
    os.remove(ba_json_file)
    os.remove(pc_json_file)

    return (ba_str, pc_str)


def _get_state_using_config_show_mockup(mockup_client) -> Tuple[str, str]:
    """
        Calls `config show mockup` on `mockup_client` and returns
        the strings of the bootstrap accounts and the protocol
        constants, by parsing standard output.
    """
    def _find_line_starting_with(strings, searched) -> int:
        i = 0
        for string in strings:
            if string.startswith(searched):
                return i
            i += 1
        return -1

    def _parse_config_init_output(string: str) -> Tuple[str, str]:
        """ Parses the output of `config init mockup`
            and return the json of the bootstrap accounts
            and the protocol constants
        """
        tagline1 = f"Default value of --{_BA_FLAG}:"
        bootstrap_accounts_index = string.find(tagline1)
        assert bootstrap_accounts_index >= 0, f"{_BA_FLAG} line not found"

        tagline2 = f"Default value of --{_PC_FLAG}:"
        proto_constants_index = string.find(tagline2)
        assert proto_constants_index > 0, f"{_PC_FLAG} line not found"

        bc_json = string[bootstrap_accounts_index +
                         len(tagline1):proto_constants_index - 1]

        pc_json = string[proto_constants_index + len(tagline2) + 1:]
        return (bc_json, pc_json)

    stdout = mockup_client.run(["config", "show", "mockup"])
    return _parse_config_init_output(stdout)


def _test_create_mockup_init_show_roundtrip(
        mockup_client,
        read_initial_state,
        read_final_state,
        bootstrap_json: Optional[str] = None):
    """ 1/ Creates a mockup, using possibly custom bootstrap_accounts
           (as specified by `bootstrap_json`)
        2/ Then execute either `config show mockup` or `config init mockup`
           to obtain the mockup's parameters (parse stdout if `show` is called,
           read the files generated by `init` otherwise)

           This is done by executing `read_initial_state`
        3/ Recreate a mockup using the output gathered in 2/ and call
           `config show mockup`/`config init mockup` (this is done by
           executing `read_final_state`) to check that output
           received is similar to output seen in 2.

        This is a roundtrip test.
    """
    if bootstrap_json is None:
        res = mockup_client.create_mockup(protocol=_PROTO).create_mockup_result
    else:
        with tempfile.NamedTemporaryFile(prefix='tezos-bootstrap-accounts',
                                         mode='w+t') as json_file:
            json_file.write(bootstrap_json)
            json_file.flush()
            res = mockup_client.create_mockup(
                protocol=_PROTO,
                bootstrap_accounts_file=json_file.name).create_mockup_result

    assert res == 'ok'
    (ba_str, pc_str) = read_initial_state(mockup_client)
    # 2/ Check the json obtained is valid by building json objects
    ba_sent = _try_json_loads(_BA_FLAG, ba_str)
    pc_sent = _try_json_loads(_PC_FLAG, pc_str)

    # 3/ Pass obtained json to a new mockup instance, to check json
    # is valid w.r.t. ocaml encoding

    # Use another directory so that the constants change takes effect
    with tempfile.TemporaryDirectory(
            prefix='tezos-client.') as base_dir, tempfile.NamedTemporaryFile(
                prefix='tezos-bootstrap-accounts',
                mode='w+t') as ba_json_file, tempfile.NamedTemporaryFile(
                    prefix='tezos-proto-consts', mode='w+t') as pc_json_file:

        ba_json_file.write(ba_str)
        ba_json_file.flush()
        pc_json_file.write(pc_str)
        pc_json_file.flush()

        mockup_client.set_base_dir(base_dir)
        res = mockup_client.create_mockup(
            protocol=_PROTO,
            protocol_constants_file=pc_json_file.name,
            bootstrap_accounts_file=ba_json_file.name).create_mockup_result
        assert res == "ok"

        # 4/ Retrieve state again
        (ba_received_str, pc_received_str) = read_final_state(mockup_client)

    # Convert it to json objects (check that json is valid)
    ba_received = _try_json_loads(_BA_FLAG, ba_received_str)
    pc_received = _try_json_loads(_PC_FLAG, pc_received_str)

    def _gen_assert_msg(flag, sent, received):
        result = f"Json sent with --{flag} differs from"
        result += " json received"
        result += f"\nJson sent is:\n{sent}"
        result += f"\nwhile json received is:\n{received}"

    # and finally check that json objects received are the same
    # as the ones that were given as input
    assert ba_sent == ba_received,\
        _gen_assert_msg(_BA_FLAG, ba_sent, ba_received)
    assert pc_sent == pc_received,\
        _gen_assert_msg(_PC_FLAG, pc_sent, pc_received)


@pytest.mark.client
@pytest.mark.parametrize('initial_bootstrap_accounts',
                         [None, json.dumps(_create_accounts_list())])
@pytest.mark.parametrize(
    'read_initial_state',
    [_get_state_using_config_show_mockup, _get_state_using_config_init_mockup])
@pytest.mark.parametrize(
    'read_final_state',
    [_get_state_using_config_show_mockup, _get_state_using_config_init_mockup])
def test_create_mockup_config_show_init_roundtrip(mockup_client,
                                                  initial_bootstrap_accounts,
                                                  read_initial_state,
                                                  read_final_state):
    """ 1/ Create a mockup, using possibly custom bootstrap_accounts
           (as specified by `initial_bootstrap_json`).
        2/ Then execute either `config show mockup` or `config init mockup`
           to obtain the mockup's parameters, as specified by
           `read_initial_state`.
        3/ Recreate a mockup using the output gathered in 2/ and call
           `read_final_state` to check that output
           received is similar to output seen in 2.

        This is a roundtrip test using a matrix.
    """
    _test_create_mockup_init_show_roundtrip(mockup_client, read_initial_state,
                                            read_final_state,
                                            initial_bootstrap_accounts)


def test_transfer_rpc(base_dir_n_mockup):
    """ Variant of test_transfer that uses RPCs to get the balances.
    """
    _, mockup_client = base_dir_n_mockup
    giver = "bootstrap1"
    receiver = "bootstrap2"
    transferred = 1.0
    transferred_mutz = transferred * 1000000

    def get_balance(tz1):
        res = mockup_client.rpc('get',
                                f'chains/main/blocks/head/'
                                f'context/contracts/{tz1}/balance')
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
