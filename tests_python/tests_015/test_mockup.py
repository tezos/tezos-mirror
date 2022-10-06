""" This file tests the mockup mode (octez-client --mode mockup).
    In this mode the client does not need a node running.

    Make sure to either use the fixture mockup_client or
    to mimick it if you want a mockup with custom parameters.

    Care is taken not to leave any base_dir dangling after
    tests are finished. Please continue doing this.
"""
import json
import os
import re
import tempfile
from typing import Any, List, Optional, Tuple, Iterator, Dict
from pprint import pformat
import pytest
from launchers.sandbox import Sandbox
from client.client import Client
from client.client_output import CreateMockupResult
import tools

from . import protocol

_BA_FLAG = "bootstrap-accounts"
_PC_FLAG = "protocol-constants"


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

        with tempfile.TemporaryDirectory(prefix='octez-client.') as base_dir:
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
        # A hack? If the user-provided overrides contains a `null` for
        # an optional field (corresponding to `None` in Python), then
        # that field will simply be absent in the output
        # `pc_sent`. Therefore, we filter such values from the
        # comparison.
        pc_input = {
            k: v
            for k, v in json.loads(protocol_constants_json).items()
            if v is not None
        }
        assert pc_sent == pc_input

    # 3/ Pass obtained json to a new mockup instance, to check json
    # is valid w.r.t. ocaml encoding

    # Use another directory so that the constants change takes effect
    with tempfile.TemporaryDirectory(
        prefix='octez-client.'
    ) as base_dir, tempfile.NamedTemporaryFile(
        prefix='tezos-bootstrap-accounts', mode='w+t'
    ) as ba_json_file, tempfile.NamedTemporaryFile(
        prefix='tezos-proto-consts', mode='w+t'
    ) as pc_json_file, tempfile.TemporaryDirectory(
        prefix='octez-client.'
    ) as base_dir:

        write_file(ba_json_file, ba_str)
        write_file(pc_json_file, pc_str)

        with tempfile.TemporaryDirectory(prefix='octez-client.') as base_dir:
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


@pytest.fixture(scope="class")
def protocol_constants_fixture_none() -> Iterator[Optional[str]]:
    """A fixture that simply returns None"""
    yield None


def succ(obj: Optional[object], schema: dict) -> object:
    """Given the [obj] of JSON schema [schema], attempt to return a value
    of the same schema that is not equal to [obj].
    """

    # pylint: disable=R0911

    def succ_numeric(
        val: Optional[int],
        minimum: Optional[int] = None,
        maximum: Optional[int] = None,
    ) -> int:
        """Returns an integer different from val. If minimum and/or maximum is
        given, then a value greater or equal than minimum and/or
        greater or equal than maximum is returned.
        """
        val = (
            val
            if val is not None
            else minimum
            if minimum is not None
            else (maximum - 1)
            if maximum is not None
            else 0
        )

        if maximum is not None:
            assert val <= maximum
        if minimum is not None:
            assert minimum <= val
        if maximum is not None and minimum is not None:
            assert abs(maximum - minimum) >= 0
            assert minimum < maximum

        if maximum is None:
            return val + 1
        if minimum is None:
            return val - 1
        return minimum + ((val - minimum + 1) % (maximum - minimum + 1))

    def succ_list(val: Optional[Any], candidates: List[Any]) -> Any:
        if val is not None:
            candidates = [c for c in candidates if c != val]
        return candidates[0]

    typ = schema.get("type", schema.get("$ref"))
    if typ == "object":
        obj = obj if obj is not None else {}
        assert isinstance(obj, dict)

        obj_succ: Dict[str, Any] = {}
        for key, key_schema in schema["properties"].items():
            obj_succ[key] = succ(obj.get(key), key_schema)
        return obj_succ
    if typ == "integer":
        minimum = schema.get('minimum')
        maximum = schema.get('maximum')
        assert obj is None or isinstance(obj, int)
        int_succ = succ_numeric(obj, minimum, maximum)
        return int_succ
    if typ == "boolean":
        obj = obj if obj is not None else False
        assert isinstance(obj, bool)
        return not obj
    if typ == "#/definitions/bignum":
        if obj is not None:
            assert isinstance(obj, str)
            obj = int(obj)
        return str(succ_numeric(obj))
    if typ == "#/definitions/int64":
        if obj is not None:
            assert isinstance(obj, str)
            obj = int(obj)
        return str(succ_numeric(obj, minimum=-(2**63) + 1, maximum=2**63))
    if re.match(r"\#/definitions/(.*)\.mutez", typ):
        if obj is not None:
            assert isinstance(obj, str)
            obj = int(obj)
        return str(succ_numeric(obj, minimum=0))
    if typ == "#/definitions/Signature.Public_key_hash":
        bootstrap1 = tools.constants.IDENTITIES['bootstrap1']['identity']
        bootstrap2 = tools.constants.IDENTITIES['bootstrap2']['identity']
        return succ_list(obj, [bootstrap1, bootstrap2])
    if typ == "#/definitions/random":
        return succ_list(
            obj,
            [
                "rngFtAUcm1EneHCCrxxSWAaxSukwEhSPvpTnFjVdKLEjgkapUy1pP",
                "rngGPSm87ZqWxJmZu7rewiLiyKY72ffCQQvxDuWmFBw59dWAL5VTB",
            ],
        )

    raise NotImplementedError(
        f"'succ' is not implemented for types [{typ}] (value: {pformat(obj)})"
    )


@pytest.fixture(scope="function")
def protocol_constants_fixture_from_rpc(
    mockup_client: Client,
) -> Iterator[Optional[str]]:
    """A fixture that creates a protocol constant value adapted for
    the mockup client initialization, while attempting to change each
    parameter from the default in order to check loading of the
    parameters."""

    # Fetch default values
    endpoint = '/chains/main/blocks/head/context/constants/parametric'
    parametric_constants = mockup_client.rpc('get', endpoint)
    # Fetch schema, used to move from default values
    parametric_constants_schema = mockup_client.rpc_schema('get', endpoint)[
        "output"
    ]
    # Move from default values
    parametric_constants_succ = succ(
        parametric_constants, parametric_constants_schema
    )
    assert isinstance(parametric_constants_succ, dict)
    # Some constants are very constant.
    constant_parametric_constants = {
        # DO NOT EDIT the value consensus_threshold this is actually a
        # constant, not a parameter
        'consensus_threshold': 0,
    }
    # These are the mockup specific protocol parameters as per
    # src/proto_015_PtLimaPt/lib_client/mockup.ml
    mockup_constants = {
        'initial_timestamp': '2021-02-03T12:34:56Z',
        'chain_id': 'NetXaFDF7xZQCpR',
    }
    constants: Dict[str, Any] = {
        **parametric_constants_succ,
        **constant_parametric_constants,
        **mockup_constants,
    }
    yield json.dumps(constants)


@pytest.mark.client
@pytest.mark.parametrize(
    'initial_bootstrap_accounts', [None, json.dumps(_create_accounts_list())]
)
@pytest.mark.parametrize(
    'protocol_constants',
    [
        # These are fixtures defined above loaded dynamically
        'protocol_constants_fixture_none',
        'protocol_constants_fixture_from_rpc',
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
    request,
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
        request.getfixturevalue(protocol_constants),
    )
