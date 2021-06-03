"""Structured representation of client output."""
import json
import re
from enum import auto, Enum, unique
from typing import List, Dict

# TODO This is incomplete. Add additional attributes and result classes as
#      they are needed


class InvalidClientOutput(Exception):
    """Raised when client output couldn't be parsed."""

    def __init__(self, client_output: str):
        super().__init__(self)
        self.client_output = client_output


class InvalidExitCode(Exception):
    """Raised when client existed with unexpected exit code."""

    def __init__(self, exit_code: int):
        super().__init__(self)
        self.exit_code = exit_code


class EndorseResult:
    """Result of a 'endorse for' operation."""

    def __init__(self, client_output: str):
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.operation_hash = match.groups()[0]


class RevealResult:
    """Result of a 'reveal key for' operation."""

    def __init__(self, client_output: str):
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.operation_hash = match.groups()[0]
        pattern = r"Fee to the baker: ꜩ(.*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.fees = float(match.groups()[0])


class TransferResult:
    """Result of a 'transfer' operation."""

    def __init__(self, client_output: str):
        self.client_output = client_output
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.operation_hash = match.groups()[0]
        pattern = r"--branch ?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            self.branch_hash = None
        else:
            self.branch_hash = match.groups()[0]
        pattern = r"Fee to the baker: ꜩ(.*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.fees = float(match.groups()[0])


class GetReceiptResult:
    """Result of 'get receipt' operation.

    If operation wasn't found, 'black_hash' is set to None.
    """

    def __init__(self, client_output: str):
        if client_output == "Couldn't find operation\n":
            self.block_hash = None
            return
        pattern = r"Operation found in block: ?(\w*) "
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.block_hash = match.groups()[0]


class GetAddressesResult:
    """Result of 'list known addresses' operation."""

    def __init__(self, client_output: str):

        pattern = re.compile(r"^(\w+):\s*(\w+).*$", re.MULTILINE)
        self.wallet = dict(re.findall(pattern, client_output))


class RunScriptResult:
    """Result of a 'get script' operation."""

    def __init__(self, client_output: str):
        # read storage output
        pattern_str = r"(?s)storage\n\s*(.*)\nemitted operations\n"
        match = re.search(pattern_str, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.storage = match.groups()[0]

        # read operation output
        self.internal_operations = None
        pattern = r"(?s)emitted operations\n\s*(.*)\n  big_map diff"
        match = re.search(pattern, client_output)
        if match is not None:
            self.internal_operations = match.group(1)

        # read map diff output
        self.big_map_diff = []  # type: List
        pattern_str = r"big_map diff\n"
        match = re.search(pattern_str, client_output)
        if match is not None:
            compiled_pattern = re.compile(r"  ((New|Set|Del|Unset).*?)\n")
            for match_diff in compiled_pattern.finditer(
                client_output, match.end(0)
            ):
                self.big_map_diff.append([match_diff.group(1)])

        self.client_output = client_output


class OriginationResult:
    """Result of an 'originate contract' operation."""

    def __init__(self, client_output: str):
        pattern = r"New contract ?(\w*) originated"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.contract = match.groups()[0]
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput
        self.operation_hash = match.groups()[0]


class SubmitProposalsResult:
    """Result of an 'submit proposals' operation."""

    def __init__(self, client_output: str):
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.operation_hash = match.groups()[0]


class BakeForResult:
    """Result of a 'bake for' operation."""

    def __init__(self, client_output: str):
        pattern = r"Injected block ?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.block_hash = match.groups()[0]


class ProposeForResult:
    """Result of a 'propose for' operation."""

    def __init__(self, client_output: str):
        pattern = r"Injected block ?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            return
            # raise InvalidClientOutput(client_output)
        # self.block_hash = match.groups()[0]


class ShowAddressResult:
    """Result of a 'show address' command."""

    def __init__(self, client_output: str):
        pattern = r"Hash: ?(\w+)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.hash = match.groups()[0]
        pattern = r"Public Key: ?(\w+)"
        match = re.search(pattern, client_output)
        if match is None:
            self.public_key = None
        else:
            self.public_key = match.groups()[0]
        pattern = r"Secret Key: ?(\w+:\w+)"
        match = re.search(pattern, client_output)
        if match is None:
            self.secret_key = None
        else:
            self.secret_key = match.groups()[0]


class ActivationResult:
    """Result of 'activate protocol' command"""

    def __init__(self, client_output: str):
        pattern = r"Injected ?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.block_hash = match.groups()[0]


class WaitForResult:
    """Result of a 'wait for' command."""

    def __init__(self, client_output: str):
        pattern = r"Operation found in block: ?(\w*) "
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.block_hash = match.groups()[0]


class HashResult:
    """Result of a 'hash data' command."""

    def __init__(self, client_output: str):

        pattern = r'''Raw packed data: ?(0x[0-9a-f]*)
Script-expression-ID-Hash: ?(\w*)
Raw Script-expression-ID-Hash: ?(\w*)
.*
Raw Sha256 hash: ?(\w*)
Raw Sha512 hash: ?(\w*)'''
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.packed = match.groups()[0]
        self.hash = match.groups()[1]
        self.blake2b = match.groups()[2]
        self.sha256 = match.groups()[3]
        self.sha512 = match.groups()[4]


class SignBytesResult:
    """Result of a 'sign bytes ...' command."""

    def __init__(self, client_output: str):

        pattern = r'Signature: ?(\w*)\n'
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.signature = match.groups()[0]


class SignMessageResult:
    """Result of a 'sign message ...' command."""

    def __init__(self, client_output: str):

        pattern = r'Signature: ?(\w*)\n'
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.signature = match.groups()[0]


class SetDelegateResult:
    """Result of a 'set delegate' operation."""

    def __init__(self, client_output: str):
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.operation_hash = match.groups()[0]
        pattern = r"--branch ?(\w*)"
        match = re.search(pattern, client_output)
        self.branch_hash = None if match is None else match.groups()[0]


class GetDelegateResult:
    """Result of a 'get delegate' command."""

    def __init__(self, client_output: str):
        if client_output == 'none\n':
            self.delegate = None
        else:
            pattern = r"(\w+)( \(known as (\w+)\))*"
            match = re.search(pattern, client_output)
            if match is None:
                raise InvalidClientOutput(client_output)
            self.address = match.groups()[0]
            self.alias = match.groups()[2]
            self.delegate = self.address


class SaplingGenKeyResult:
    """Result of a 'sapling gen key' operation."""

    def __init__(self, client_output: str):
        pattern = (
            r'It is important to save this mnemonic in a secure '
            r'place:\n\n([\w\s]+)\n\nThe mnemonic'
        )
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.mnemonic = match.groups()[0].split()


class SaplingGenAddressResult:
    """Result of a 'sapling gen address' operation."""

    def __init__(self, client_output: str):
        address_match = re.search(r"Generated address:\n(\w+)\n", client_output)
        if address_match is None:
            raise InvalidClientOutput(client_output)
        self.address = address_match.groups()[0]
        index_match = re.search(r"at index (\d+)", client_output)
        if index_match is None:
            raise InvalidClientOutput(client_output)
        self.index = int(index_match.groups()[0])


class SaplingDeriveKeyResult:
    """Result of a 'sapling derive key' operation."""

    def __init__(self, client_output: str):
        path_match = re.search(r"with path (\S+)", client_output)
        if path_match is None:
            raise InvalidClientOutput(client_output)
        self.path = path_match.groups()[0]


class SaplingGetBalanceResult:
    """Result of a 'sapling get balance' query."""

    def __init__(self, client_output: str):
        balance_match = re.search(
            r"Total Sapling funds ([\d\.]+)", client_output
        )
        if balance_match is None:
            raise InvalidClientOutput(client_output)
        self.balance = float(balance_match.groups()[0])


def extract_rpc_answer(client_output: str) -> dict:
    """Convert json client output to a dict representation.

    For some RPC, the answer isn't json, in that case, the client_output
    can be retrieved from the InvalidClientOutput exception"""
    try:
        return json.loads(client_output)
    except json.JSONDecodeError as json_decode_error:
        raise InvalidClientOutput(client_output) from json_decode_error


def extract_balance(client_output: str) -> float:
    """Extract float balance from the output of 'get_balance' operation."""
    try:
        pattern = r"([\w.]*) ꜩ"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        return float(match.groups()[0])
    except Exception as exception:
        raise InvalidClientOutput(client_output) from exception


def extract_protocols(client_output: str) -> List[str]:
    """Extract protocol from the output of 'get_protocols' operation."""
    return client_output.split()


def extract_environment_protocol(client_output: str) -> str:
    """Extract environment protocol version from the output of
    'protocol_environment' operation."""
    try:
        pattern = r"Protocol \S* uses environment (V\d)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        return match.groups()[0]
    except Exception as exception:
        raise InvalidClientOutput(client_output) from exception


class PointInfo:
    def __init__(self, peer_id=None, is_connected=None, is_trusted=None):
        self.peer_id = peer_id
        self.is_connected = is_connected
        self.is_trusted = is_trusted

    def __str__(self):
        return f'{self.peer_id}, {self.is_connected}, {self.is_trusted}'


def parse_peer(line):
    # Expected format
    #  ⚌  1 idr9R9xzYpSt98b9GspNQj9QZxj8zi ↗ 668 B (133 B/s) ↘ 668 B (133 B/s)
    return line.split()[2]


def parse_point(line):
    # Expected format
    #  ⚌  127.0.0.1:19731 idr9R9xzYpSt98b9GspNQj9QZxj8zi ★
    #  ⚏  127.0.0.1:19764 ★
    #  ⚏  127.0.0.1:19730
    #  (last seen: idtbwXjfV38usn36SoL5sMcdYRk5sL 2019-08-07T12:13:13-00:00) ★
    last_seen_or_id = r"(?:id\w*)|\(last seen: id\w* \S*"
    pattern = r"(⚏|⚌)  (\S*)\s?(" + last_seen_or_id + ")? (★)?"
    match = re.search(pattern, line)
    assert match is not None
    groups = match.groups()
    assert len(groups) == 4
    is_trusted = groups[3] == '★'
    is_connected = groups[0] == '⚌'
    point_id = groups[1]
    peer_id = groups[2]
    if peer_id and peer_id.startswith('(last seen: '):
        peer_id = peer_id[12:42]
    res = (point_id, peer_id, is_connected, is_trusted)
    return res


class P2pStatResult:
    """Result of a 'p2p stat' command."""

    def __init__(self, client_output: str):
        self.peers = []  # type: List[str]
        self.points = {}  # type: Dict[str, PointInfo]
        lines = client_output.splitlines()
        j = lines.index('KNOWN PEERS')
        k = lines.index('KNOWN POINTS')
        self.peers = [parse_peer(line) for line in lines[j + 1 : k]]
        points_list = (parse_point(line) for line in lines[k + 1 :])
        for addr, peer_id, is_connected, is_trusted in points_list:
            self.points[addr] = PointInfo(peer_id, is_connected, is_trusted)


class GetContractEntrypointTypeResult:
    """Result of a 'get contract entrypoint type of' command."""

    def __init__(self, client_output: str):
        pattern = r"Entrypoint .*?: (.*)\n"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.entrypoint_type = match.groups()[0]


class ListMockupProtocols:
    """Result of 'list mockup protocols' query."""

    def __init__(self, client_output: str):
        pattern = re.compile(r"^(\w+)$", re.MULTILINE)
        self.mockup_protocols = re.findall(pattern, client_output)


@unique
class CreateMockupResult(Enum):
    """
    Possible behaviors of `tezos-client create mockup`
    """

    ALREADY_INITIALIZED = auto()
    DIR_NOT_EMPTY = auto()
    OK = auto()

    def to_return_code(self) -> int:
        """ The expected return code of the client when 'self' is returned """
        if self == CreateMockupResult.OK:
            return 0
        return 1


class CreateMockup:
    """Result of 'create mockup' command."""

    def __init__(self, client_stdout: str, client_stderr: str, exit_code):
        self.client_stdout = client_stdout
        self.exit_code = exit_code
        self.create_mockup_result = None
        self.chain_id = None
        match = re.search(r"Chain id is (.*)", self.client_stdout)
        if match is not None:
            self.chain_id = match.group(1)

        # an element of outputs contains:
        # - a pattern that we expect the output to contain
        # - the output channel itself (stdout/stderr)
        #   aka, where to look for the pattern
        # - the result to set in self.create_mockup_result
        outputs = [
            (
                r"^  \S+ is not empty, please specify a fresh base directory$",
                client_stderr,
                CreateMockupResult.DIR_NOT_EMPTY,
            ),
            (
                r"^  \S+ is already initialized as a mockup directory$",
                client_stderr,
                CreateMockupResult.ALREADY_INITIALIZED,
            ),
            (
                r"^Created mockup client base dir in \S+$",
                client_stdout,
                CreateMockupResult.OK,
            ),
        ]

        for outp in outputs:
            pattern = re.compile(outp[0], re.MULTILINE)
            out_channel = outp[1]
            result = outp[2]
            expected_exit_code = result.to_return_code()
            if re.search(pattern, out_channel) is not None:
                self.create_mockup_result = result
                if exit_code != expected_exit_code:
                    raise InvalidExitCode(exit_code)
                return


class CheckSignMessageResult:
    """Result of a 'check that message...' command."""

    def __init__(self, client_output: str):

        pattern = r'Signature check successful *\n'
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.check = True


class ViewResult:
    """Result of a 'run tzip4 view...' command."""

    def __init__(self, client_output: str):
        self.result = client_output


class FA12CheckResult:
    """Result of a 'check contract .. has fa1.2 interface` command."""

    def __init__(self, client_output: str):
        pattern = r"has an FA1.2 interface."
        match = re.search(pattern, client_output)
        if match is None:
            pattern = r"Not a supported FA1.2 contract."
            match = re.search(pattern, client_output)
            if match is None:
                raise InvalidClientOutput(client_output)
            self.check = False
        else:
            self.check = True


class FA12ViewResult:
    """Result of a 'from fa1.2 contract .. get ..` command."""

    def __init__(self, client_output: str):
        try:
            pattern = r"([\w.]*)"
            match = re.search(pattern, client_output)
            if match is None:
                raise InvalidClientOutput(client_output)
            self.amount = int(match.groups()[0])
        except Exception as exception:
            raise InvalidClientOutput(client_output) from exception
