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
import pytest

from tools.constants import ALPHA

_PROTO = ALPHA


@pytest.fixture
def mockup_client(sandbox):
    sandbox.add_mockup_client()
    return sandbox.mockup_client


@pytest.fixture
def base_dir_n_mockup(mockup_client):
    with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir:
        mockup_client.set_base_dir(base_dir)
        res = mockup_client.create_mockup(protocol=_PROTO).create_mockup_result
        assert res == 'ok'
        yield (base_dir, mockup_client)  # yield instead of return: so that
        # 'with' block is exited during teardown, see
        # https://docs.pytest.org/en/latest/fixture.html#fixture-finalization-executing-teardown-code


@pytest.mark.client
def test_list_mockup_protocols(mockup_client):
    """ Executes `tezos-client list mockup protocols`
        The call must succeed and return a non empty list.
    """
    protocols = mockup_client.list_mockup_protocols().mockup_protocols
    assert protocols[0] == _PROTO


@pytest.mark.client
def test_create_mockup_file_exists(mockup_client):
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
def test_create_mockup_dir_exists_nonempty(mockup_client):
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
def test_create_mockup_fresh_dir(mockup_client):
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
def test_create_mockup_already_initialized(mockup_client):
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
def test_transfer(base_dir_n_mockup):
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
def test_create_mockup_custom_constants(mockup_client):
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


@pytest.mark.client
def test_create_mockup_custom_bootstrap_accounts(mockup_client):
    """ Tests `tezos-client create mockup` --bootstrap-accounts argument
        The call must succeed.
    """
    accounts_list = []

    def add_account(name: str, sk_uri: str, amount: int):
        entry = {
            "name": name,
            "sk_uri": "unencrypted:" + sk_uri,
            "amount": str(amount)
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


# Note: this test should break once it's merged with
# https://gitlab.com/tezos/tezos/-/merge_requests/1760
# When !1760 is merged, the error output will be more explicit
# hence the error output should change and this test should break.
@pytest.mark.client
@pytest.mark.skip("depends on !1760")
# Test is skipped for now because it depends
# on https://gitlab.com/tezos/tezos/-/merge_requests/1760 in particular
# the following commit:
# https://gitlab.com/tezos/tezos/-/merge_requests/1760/diffs?commit_id=781b9748da4f4305a1baa3d82e15362761293be0
def test_transfer_bad_base_dir(mockup_client):
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
        (_, err_output, _) = mockup_client.run_generic(cmd,
                                                       check=False)
        # See
        # https://gitlab.com/tezos/tezos/-/merge_requests/1760#note_329071488
        # for the content being matched
        searched = "Some commands .* might not work correctly."
        assert re.search(
            searched, err_output), f"'{searched}' not matched in error output"
