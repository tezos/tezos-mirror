"""Structured representation of client output."""
import json
import re
from typing import List, Dict

# TODO This is incomplete. Add additional attributes and result classes as
#      they are needed


class InvalidClientOutput(Exception):
    """Raised when client output couldn't be parsed."""

    def __init__(self, client_output: str):
        super().__init__(self)
        self.client_output = client_output


class EndorseResult:
    """Result of a 'endorse for' operation."""

    def __init__(self, client_output: str):
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.operation_hash = match.groups()[0]


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
            raise InvalidClientOutput(client_output)
        self.branch_hash = match.groups()[0]


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
    """Result of 'list known addresses' operation.

    """

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

        # read map diff output
        self.big_map_diff = []  # type: List
        pattern_str = r"big_map diff\n"
        match = re.search(pattern_str, client_output)
        if match is not None:
            compiled_pattern = re.compile(r"  ((New|Set|Del|Unset).*?)\n")
            for match_diff in compiled_pattern.finditer(client_output,
                                                        match.end(0)):
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
    """Result of a 'baker for' operation."""

    def __init__(self, client_output: str):
        pattern = r"Injected block ?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.block_hash = match.groups()[0]


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


class SignatureResult:
    """Result of a 'sign bytes' command."""

    def __init__(self, client_output: str):

        pattern = r'Signature: ?(\w*)\n'
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.sig = match.groups()[0]


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
        if match is None:
            raise InvalidClientOutput(client_output)
        self.branch_hash = match.groups()[0]


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


class SignBytesResult:
    """Result of a 'sign bytes' command."""

    def __init__(self, client_output: str):
        pattern = r"Signature: ?(\w+)"
        match = re.search(pattern, client_output[:-1])
        if match is None:
            raise InvalidClientOutput(client_output)
        self.signature = match.groups()[0]


def extract_rpc_answer(client_output: str) -> dict:
    """Convert json client output to a dict representation.

    For some RPC, the answer isn't json, in that case, the client_output
    can be retrieved from the InvalidClientOutput exception"""
    try:
        return json.loads(client_output)
    except json.JSONDecodeError:
        raise InvalidClientOutput(client_output)


def extract_balance(client_output: str) -> float:
    """Extract float balance from the output of 'get_balance' operation."""
    try:
        pattern = r"([\w.]*) ꜩ"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        return float(match.groups()[0])
    except Exception:
        raise InvalidClientOutput(client_output)


def extract_protocols(client_output: str) -> List[str]:
    """Extract protocol from the output of 'get_protocols' operation."""
    return client_output.split()


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
        peer_id = peer_id[12: 42]
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
        self.peers = [parse_peer(line) for line in lines[j+1:k]]
        points_list = (parse_point(line) for line in lines[k+1:])
        for addr, peer_id, is_connected, is_trusted in points_list:
            self.points[addr] = PointInfo(peer_id, is_connected, is_trusted)
