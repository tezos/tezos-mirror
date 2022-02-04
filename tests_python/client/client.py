import csv
import datetime
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.request
from typing import Any, List, Optional, Tuple

from process.process_utils import format_command
from . import client_output


class Client:
    """Client to a Tezos node.

    Manage the persistent client state and provides methods to call
    tezos-client/tezos-admin-client commands, and return structured
    representation of the client output.

    The most generic method to call the client is `run`. It calls the client
    with an arbitrary sequence of parameters and returns the stdout of the
    command. `CalledProcessError` is raised if command fails.

    Other commands such as `run_script`, `transfer`... are wrapper over `run`,
    they set up the commands parameters and return a structured representation
    of the client output.

    TODO: - the set of methods isn't complete. To be added when needed... some
            methods return client stdout instead of structured representation
          - deal correctly with additional parameters in command wrappers
          - this works for the current tests but should be more generic
    """

    def __init__(
        self,
        client_path: str,
        admin_client_path: str,
        host: Optional[str] = None,
        base_dir: Optional[str] = None,
        rpc_port: Optional[int] = None,
        use_tls: Optional[bool] = None,
        endpoint: Optional[str] = 'http://127.0.0.1:8732',
        disable_disclaimer: bool = True,
        mode: str = None,
    ):
        """
        Args:
            client (str): path to the client executable file
            admin_client (str): path to the admin-client executable file
            host (str): IP of the host; deprecated: use endpoint instead
            base_dir (str): path to the client dir. If None, a temp file is
                            created.
            rpc_port (int): port of the server; deprecated: use endpoint
                            instead
            use_tls (bool): use TLS
            endpoint (str): the RPC endpoint
            disable_disclaimer (bool): disable disclaimer
            mode (str): the mode to use, one of "client", "mockup", or
                        "proxy", default=None (equivalent to "client").
        Returns:
            A Client instance.
        """
        assert os.path.isfile(client_path), f"{client_path} is not a file"
        assert os.path.isfile(admin_client_path), (
            f"{admin_client_path} is " f"not a file"
        )
        assert base_dir is None or os.path.isdir(base_dir), (
            f'{base_dir} not ' f'a dir'
        )

        self.host = host
        self._disable_disclaimer = disable_disclaimer
        self._is_tmp_dir = base_dir is None

        if base_dir is None:
            base_dir = tempfile.mkdtemp(prefix='tezos-client.')
            assert base_dir
        self.base_dir = base_dir

        connectivity_options = []
        if host is not None:
            connectivity_options += ['-addr', host]
        if rpc_port is not None:
            connectivity_options += ['-port', str(rpc_port)]
        if use_tls is True:
            connectivity_options += ['-S']
        if endpoint is not None:
            connectivity_options += ['-endpoint', endpoint]

        client = [client_path, '-base-dir', base_dir]
        if mode is None or mode == "client":
            client.extend(connectivity_options)
        elif mode == "mockup":
            client.extend(['-mode', mode])
        elif mode == "proxy":
            client.extend(['-mode', mode])
            client.extend(connectivity_options)
        else:
            msg = (
                f"Unexpected mode: {mode}."
                + "Expected one of 'client', 'mockup', or 'proxy'."
            )
            assert False, msg
        admin_client = [admin_client_path, '-base-dir', base_dir]

        admin_client.extend(connectivity_options)

        self._client = client
        self._admin_client = admin_client
        self.rpc_port = rpc_port

        if endpoint is not None:
            self.endpoint = endpoint
        else:
            protocol = 'https' if use_tls else 'http'
            host = host if host else '127.0.0.1'
            rpc_port = rpc_port if rpc_port else 8732
            self.endpoint = f'{protocol}://{host}:{rpc_port}'

    def run_generic(
        self,
        params: List[str],
        admin: bool = False,
        check: bool = True,
        trace: bool = False,
        stdin: str = "",
        env_change: dict = None,
    ) -> Tuple[str, str, int]:
        """Run an arbitrary command

        Args:
            params (list): list of parameters given to the tezos-client,
            admin (bool): False to call tezos-client, True to call
                          tezos-admin-client
            check (bool): raises an exception if client call fails
            trace (bool): use '-l' option to trace RPCs
            stdin (string): string that will be passed as standard
                            input to the process
            env_change (dict): overrides to environment variables
        Returns:
            (stdout of command, stderr of command, return code)

        The actual command will be displayed according to 'format_command'.
        Client output (stdout, stderr) will be displayed unprocessed.
        Fails with `CalledProcessError` if command fails
        """
        client = self._admin_client if admin else self._client
        trace_opt = ['-l'] if trace else []
        cmd = client + trace_opt + params

        print(format_command(cmd))

        new_env = os.environ.copy()
        if env_change is not None:
            new_env.update(env_change)
        if self._disable_disclaimer:
            new_env["TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER"] = "Y"
        process = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=new_env,
        )
        outstream, errstream = process.communicate(input=stdin.encode())
        stdout = outstream.decode('utf-8')
        stderr = errstream.decode('utf-8')
        if stdout:
            print(stdout)
        if stderr:
            print(stderr, file=sys.stderr)
        if check:
            if process.returncode != 0:
                raise subprocess.CalledProcessError(
                    process.returncode, cmd, stdout, stderr
                )
        # `+ ""` makes pylint happy. It can't infer stdout/stderr can't
        # be `None` thanks to the `capture_output=True` option.
        return (stdout + "", stderr + "", process.returncode)

    def run(
        self,
        params: List[str],
        admin: bool = False,
        check: bool = True,
        trace: bool = False,
    ) -> str:
        """Like 'run_generic' but returns just stdout."""
        (stdout, _, _) = self.run_generic(params, admin, check, trace)
        return stdout

    def rpc(
        self, verb: str, path: str, data: Any = None, params: List[str] = None
    ) -> Any:
        """Run an arbitrary RPC command

        Args:
            verb (str): either `get`, `post`, `put`, `patch` or `delete`
            path (str): rpc path
            data (dict): json data for post
            params (list): any additional parameters to pass to the client
        Returns:
            dict representing the json output, raise exception
            if output isn't json.

        See `run` for more details.
        """
        assert verb in {'put', 'get', 'post', 'delete', 'patch'}
        params = [] if params is None else params
        params = params + ['rpc', verb, path]
        if data is not None:
            params = params + ['with', json.dumps(data)]
        compl_pr = self.run(params)
        return client_output.extract_rpc_answer(compl_pr)

    def rpc_raw(self, verb: str, path: str, data: Any = None):
        """Run an arbitrary RPC request directly to the client's endpoint
        without going through the client.

        Args:
            verb (str): either `get`, `post`, `put`, `patch` or `delete`
            path (str): rpc path
            data (dict): json data for post
        Returns:
            dict representing the json output, raise exception
            if output isn't json.
        """
        url = self.endpoint + path
        print(format_command(['raw rpc', verb, url]))
        request = urllib.request.Request(url, method=verb.upper(), data=data)
        with urllib.request.urlopen(request) as results:
            body = results.read()
            print(body)
            return json.loads(body.decode())

    def remember_contract(
        self, alias: str, contract_address: str, force: bool = False
    ):
        params = ["remember", "contract", alias, contract_address]
        if force:
            params.append("--force")
        return self.run(params)

    def remember(self, alias: str, contract: str) -> str:
        assert os.path.isfile(contract), f'{contract} is not a file'
        return self.run(['remember', 'script', alias, f'file:{contract}'])

    def typecheck(
        self, contract: str, file: bool = True, legacy=False, details=False
    ) -> str:
        if file:
            assert os.path.isfile(contract), f'{contract} is not a file'
        params = ['typecheck', 'script', contract]
        if legacy:
            params += ['--legacy']
        if details:
            params += ['--details']
        return self.run(params)

    def typecheck_data(self, data: str, typ: str, legacy=False) -> str:
        params = ['typecheck', 'data', data, 'against', 'type', typ]
        if legacy:
            params += ['--legacy']
        return self.run(params)

    def get_script(self, contract: str) -> str:
        return self.run(['get', 'contract', 'code', 'for', contract])

    def run_script(
        self,
        contract: str,
        storage: str,
        inp: str,
        amount: float = None,
        balance: float = None,
        now: str = None,
        level: int = None,
        trace_stack: bool = False,
        gas: int = None,
        file: bool = True,
    ) -> client_output.RunScriptResult:
        if file:
            assert os.path.isfile(contract), f'{contract} is not a file'
        cmd = [
            'run',
            'script',
            contract,
            'on',
            'storage',
            storage,
            'and',
            'input',
            inp,
        ]
        if amount is not None:
            cmd += ['-z', '%.6f' % amount]
        if balance is not None:
            cmd += ['--balance', '%.6f' % balance]
        if now is not None:
            cmd += ['--now', now]
        if level is not None:
            cmd += ['--level', f'{level}']
        if trace_stack:
            cmd += ['--trace-stack']
        if gas is not None:
            cmd += ['--gas', '%d' % gas]
        return client_output.RunScriptResult(self.run(cmd))

    def hash_script(
        self,
        contracts: List[str],
        display_names: bool = False,
        for_script: Optional[str] = None,
    ) -> List[Tuple[str, Optional[str]]]:
        params = ['hash', 'script']
        if display_names:
            params += ['--display-names']
        if for_script is not None:
            params += ['--for-script', for_script]
        params += contracts
        output = self.run(params)

        lines = output.split("\n")[:-1]
        delimiter = "," if for_script == 'csv' else "\t"
        return [
            (row[0], row[1] if display_names else None)
            for row in csv.reader(lines, delimiter=delimiter)
        ]

    def get_script_hash(self, contract: str) -> str:
        params = ['get', 'contract', 'script', 'hash', 'for', contract]
        return self.run(params).strip()

    def convert_script(self, inp: str, input_fmt: str, output_fmt: str) -> str:
        params = [
            'convert',
            'script',
            inp,
            'from',
            input_fmt,
            'to',
            output_fmt,
        ]
        return self.run(params)

    def gen_key(
        self, alias: str, args: List[str] = None, stdin: str = ""
    ) -> str:
        cmd = ['gen', 'keys', alias]
        if args is None:
            args = []
        cmd += args
        stdout, _, _ = self.run_generic(cmd, stdin=stdin)
        return stdout

    def import_secret_key(
        self, name: str, secret: str, password: str = None
    ) -> str:
        prms = ['import', 'secret', 'key', name, secret]
        stdout, _, _ = self.run_generic(
            prms, stdin=f"{password}" if password else ""
        )
        return stdout

    def add_address(self, name: str, address: str, force: bool = False):
        cmd = ['add', 'address', name, address]
        if force:
            cmd += ['--force']
        output = self.run(cmd)
        assert not output

    def show_address(
        self, name: str, show_secret: bool = False
    ) -> client_output.ShowAddressResult:
        cmd = ['show', 'address', name]
        if show_secret:
            cmd += ['--show-secret']
        return client_output.ShowAddressResult(self.run(cmd))

    def activate_protocol(
        self,
        protocol: str,
        parameter_file: str,
        fitness: str = '1',
        key: str = 'activator',
        timestamp: str = None,
        delay: datetime.timedelta = None,
    ) -> client_output.ActivationResult:
        assert os.path.isfile(parameter_file), f'{parameter_file} not a file'
        if timestamp is None:
            if delay is None:
                delay = datetime.timedelta(seconds=0)
            utc_now = datetime.datetime.utcnow() - delay
            timestamp = utc_now.strftime("%Y-%m-%dT%H:%M:%SZ")

        cmd = [
            '-block',
            'genesis',
            'activate',
            'protocol',
            protocol,
            'with',
            'fitness',
            str(fitness),
            'and',
            'key',
            key,
            'and',
            'parameters',
            parameter_file,
            '--timestamp',
            timestamp,
        ]
        return client_output.ActivationResult(self.run(cmd))

    def activate_protocol_json(
        self,
        protocol: str,
        parameters: dict,
        fitness: str = '1',
        key: str = 'activator',
        timestamp: str = None,
        delay: datetime.timedelta = None,
    ) -> client_output.ActivationResult:
        if delay is None:
            delay = datetime.timedelta(seconds=0)
        with tempfile.NamedTemporaryFile(mode='w+', delete=False) as params:
            param_json = json.dumps(parameters)
            params.write(param_json)
            params.close()
            return self.activate_protocol(
                protocol, params.name, fitness, key, timestamp, delay
            )

    def show_voting_period(self) -> str:
        return self.run(['show', 'voting', 'period'])

    def ban_peer(self, port: int) -> dict:
        return self.rpc('get', f'/network/points/127.0.0.1:{port}/ban')

    def unban_peer(self, port: int) -> dict:
        return self.rpc('get', f'/network/points/127.0.0.1:{port}/unban')

    def trust_peer(self, port: int) -> dict:
        return self.rpc('get', f'/network/points/127.0.0.1:{port}/trust')

    def untrust_peer(self, port: int) -> dict:
        return self.rpc('get', f'/network/points/127.0.0.1:{port}/untrust')

    def get_expected_peer_id(self, port: int) -> dict:
        path = f'/network/points/127.0.0.1:{port}'
        return self.rpc('get', path)["expected_peer_id"]

    def set_expected_peer_id(self, port: int, peer_id) -> dict:
        path = f'/network/points/127.0.0.1:{port}'
        return self.rpc('patch', path, data={"peer_id": peer_id})

    def endorse(self, account: str) -> client_output.EndorseResult:
        res = self.run(['endorse', 'for', account])
        return client_output.EndorseResult(res)

    def bake(
        self, account: str, args: List[str] = None
    ) -> client_output.BakeForResult:
        cmd = ['bake', 'for', account]
        if args is None:
            args = []
        cmd += args
        return client_output.BakeForResult(self.run(cmd))

    def multibake(
        self, delegates: List[str] = None, args: List[str] = None
    ) -> client_output.BakeForResult:
        """The empty list for delegates means 'all known delegates'"""
        cmd = ['bake', 'for']
        if delegates is None:
            delegates = []
        cmd += delegates
        if args is None:
            args = []
        cmd += args
        return client_output.BakeForResult(self.run(cmd))

    def propose(
        self, delegates: List[str] = None, args: List[str] = None
    ) -> client_output.ProposeForResult:
        cmd = ['propose', 'for']
        if delegates is None:
            delegates = []
        cmd += delegates
        if args is None:
            args = []
        cmd += args
        return client_output.ProposeForResult(self.run(cmd))

    def originate(
        self,
        contract_name: str,
        amount: float,
        sender: str,
        contract: str,
        args: List[str] = None,
    ) -> client_output.OriginationResult:
        cmd = [
            'originate',
            'contract',
            contract_name,
            'transferring',
            str(amount),
            'from',
            sender,
            'running',
            contract,
        ]
        if args is None:
            args = []
        cmd += args
        return client_output.OriginationResult(self.run(cmd))

    def hash(self, data: str, typ: str) -> client_output.HashResult:
        cmd = ['hash', 'data', data, 'of', 'type', typ]
        return client_output.HashResult(self.run(cmd))

    def pack(self, data: str, typ: str) -> str:
        return self.hash(data, typ).packed

    def normalize(
        self, data: str, typ: str, mode: str = None, legacy: bool = False
    ) -> str:
        cmd = ['normalize', 'data', data, 'of', 'type', typ]
        if mode is not None:
            cmd += ['--unparsing-mode', mode]
        if legacy:
            cmd += ['--legacy']
        return self.run(cmd)

    def normalize_script(
        self, script: str, mode: str = None, file: bool = True
    ) -> str:
        if file:
            assert os.path.isfile(script), f'{script} is not a file'
        cmd = ['normalize', 'script', script]
        if mode is not None:
            cmd += ['--unparsing-mode', mode]
        return self.run(cmd)

    def normalize_type(self, typ: str) -> str:
        cmd = ['normalize', 'type', typ]
        return self.run(cmd)

    def activate_account(self, manager: str, contract: str):
        cmd = ['activate', 'account', manager, 'with', contract]
        return self.run(cmd)

    def cmd_batch(self, source: str, json_ops: str) -> List[str]:
        return ['multiple', 'transfers', 'from', source, 'using', json_ops]

    def sign_message(self, data: str, identity: str, block=None) -> str:
        cmd = ['sign', 'message', data, 'for', identity]
        if block is not None:
            cmd += ["--branch", block]
        return client_output.SignMessageResult(self.run(cmd)).signature

    def check_message(self, data: str, identity: str, signature: str) -> bool:
        cmd = [
            'check',
            'that',
            'message',
            data,
            'was',
            'signed',
            'by',
            identity,
            'to',
            'produce',
            signature,
        ]
        return client_output.CheckSignMessageResult(self.run(cmd)).check

    def reveal(
        self,
        account: str,
        args: List[str] = None,
    ) -> client_output.RevealResult:
        cmd = ['reveal', 'key', 'for', account]
        if args is None:
            args = []
        cmd += args
        res = self.run(cmd)
        return client_output.RevealResult(res)

    def transfer(
        self,
        amount: float,
        giver: str,
        receiver: str,
        args: List[str] = None,
        stdin: str = "",
        chain: str = None,
    ) -> client_output.TransferResult:
        cmd = ['transfer', str(amount), 'from', giver, 'to', receiver]
        if chain is not None:
            cmd = ['--chain', chain] + cmd
        if args is None:
            args = []
        cmd += args
        stdout, _, _ = self.run_generic(cmd, stdin=stdin)
        return client_output.TransferResult(stdout)

    def transfer_json(
        self, amount: int, giver: str, receiver: str, args: List[str] = None
    ) -> client_output.TransferResult:
        json_obj = [{"destination": receiver, "amount": str(amount)}]
        json_ops = json.dumps(json_obj, separators=(',', ':'))
        if args is None:
            args = []
        cmd = self.cmd_batch(giver, json_ops) + args
        res = self.run(cmd)
        return client_output.TransferResult(res)

    def call(
        self, source: str, destination: str, args: List[str] = None
    ) -> client_output.TransferResult:
        cmd = ['call', destination, 'from', source]
        if args is None:
            args = []
        cmd += args
        res = self.run(cmd)
        return client_output.TransferResult(res)

    def set_delegate(
        self, account1: str, account2: str, args: List[str] = None
    ) -> client_output.SetDelegateResult:
        cmd = ['set', 'delegate', 'for', account1, 'to', account2]
        if args is None:
            args = []
        cmd += args
        res = self.run(cmd)
        return client_output.SetDelegateResult(res)

    def get_delegate(
        self, account1: str, args: List[str] = None
    ) -> client_output.GetDelegateResult:
        cmd = ['get', 'delegate', 'for', account1]
        if args is None:
            args = []
        cmd += args
        res = self.run(cmd)
        return client_output.GetDelegateResult(res)

    def get_contract_entrypoint_type(
        self, entrypoint: str, contract_name: str
    ) -> client_output.GetContractEntrypointTypeResult:
        cmd = [
            'get',
            'contract',
            'entrypoint',
            'type',
            'of',
            entrypoint,
            'for',
            contract_name,
        ]
        return client_output.GetContractEntrypointTypeResult(self.run(cmd))

    def withdraw_delegate(self, account1: str, args: List[str] = None) -> str:
        cmd = ['withdraw', 'delegate', 'from', account1]
        if args is None:
            args = []
        cmd += args
        res = self.run(cmd)
        return res

    def register_delegate(self, delegate: str) -> str:
        return self.run(['register', 'key', delegate, 'as', 'delegate'])

    def p2p_stat(self) -> client_output.P2pStatResult:
        res = self.run(['p2p', 'stat'], admin=True)
        return client_output.P2pStatResult(res)

    def get_balance(self, account) -> float:
        res = self.run(['get', 'balance', 'for', account])
        return client_output.extract_balance(res)

    def get_mutez_balance(self, account) -> float:
        res = self.run(['get', 'balance', 'for', account])
        return int(client_output.extract_balance(res) * 1000000)

    def get_timestamp(self) -> str:
        res = self.run(['get', 'timestamp'])
        return res[:-1]

    def get_block_timestamp(
        self, params: List[str] = None, chain: str = 'main', block: str = 'head'
    ) -> datetime.datetime:
        assert chain in {'main', 'test'}
        rpc_res = self.rpc(
            'get', f'/chains/{chain}/blocks/{block}/header/shell', params=params
        )
        timestamp = rpc_res['timestamp']

        rfc3399_format = "%Y-%m-%dT%H:%M:%SZ"
        timestamp_date = datetime.datetime.strptime(timestamp, rfc3399_format)
        timestamp_date = timestamp_date.replace(tzinfo=datetime.timezone.utc)

        return timestamp_date

    def get_receipt(
        self, operation: str, args: List[str] = None
    ) -> client_output.GetReceiptResult:
        cmd = ['get', 'receipt', 'for', operation]
        if args is None:
            args = []
        cmd += args
        return client_output.GetReceiptResult(self.run(cmd))

    def get_storage(self, contract: str) -> str:
        cmd = ['get', 'contract', 'storage', 'for', contract]
        res = self.run(cmd)
        return res.rstrip()

    def get_prevalidator(self) -> dict:
        return self.rpc('get', '/workers/prevalidators')

    def get_mempool(self) -> dict:
        return self.rpc('get', '/chains/main/mempool/pending_operations')

    def mempool_is_empty(self) -> bool:
        rpc_res = self.rpc('get', '/chains/main/mempool/pending_operations')
        return (
            rpc_res['applied'] == []
            and rpc_res['refused'] == []
            and rpc_res['branch_refused'] == []
            and rpc_res['branch_delayed'] == []
            and rpc_res['unprocessed'] == []
        )

    def get_head(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head')

    def get_header(self, block='head') -> dict:
        return self.rpc('get', f'/chains/main/blocks/{block}/header')

    def get_block(self, block_hash) -> dict:
        return self.rpc('get', f'/chains/main/blocks/{block_hash}')

    def get_operations(self, block_hash='head') -> dict:
        return self.rpc('get', f'/chains/main/blocks/{block_hash}/operations')

    def get_ballot_list(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head/votes/ballot_list')

    def get_ballots(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head/votes/ballots')

    def get_contract_address(self, contract) -> str:
        return self.run(['show', 'known', 'contract', contract]).strip()

    def get_known_addresses(self) -> client_output.GetAddressesResult:
        return client_output.GetAddressesResult(
            self.run(['list', 'known', 'addresses'])
        )

    def get_current_period(self) -> dict:
        return self.rpc('get', 'chains/main/blocks/head/votes/current_period')

    def get_current_period_kind(self) -> dict:
        return self.rpc(
            'get', 'chains/main/blocks/head/votes/current_period_kind'
        )

    def get_succ_period(self) -> dict:
        return self.rpc('get', 'chains/main/blocks/head/votes/successor_period')

    def get_current_proposal(self) -> dict:
        return self.rpc(
            'get', '/chains/main/blocks/head/votes/current_proposal'
        )

    def get_current_quorum(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head/votes/current_quorum')

    def get_listings(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head/votes/listings')

    def get_proposals(self) -> dict:
        return self.rpc('get', '/chains/main/blocks/head/votes/proposals')

    def get_metadata(
        self, params: List[str] = None, chain: str = 'main', block: str = 'head'
    ) -> dict:
        return self.rpc(
            'get', f'/chains/{chain}/blocks/{block}/metadata', params=params
        )

    def get_protocol(self, params: List[str] = None) -> str:
        metadata = self.get_metadata(params=params)
        return metadata['protocol']

    def get_next_protocol(self, params: List[str] = None) -> str:
        metadata = self.get_metadata(params=params)
        return metadata['next_protocol']

    def get_current_level(self, block='head', offset=0) -> dict:
        return self.rpc(
            'get',
            (
                f'/chains/main/blocks/{block}/helpers/'
                f'current_level?offset={str(offset)}'
            ),
        )

    def get_period_position(self) -> str:
        rpc_res = self.get_current_level(offset=1)
        return rpc_res['voting_period_position']

    def get_level(
        self, params: List[str] = None, chain: str = 'main', block: str = 'head'
    ) -> int:
        assert chain in {'main', 'test'}
        rpc_res = self.rpc(
            'get', f'/chains/{chain}/blocks/{block}/header/shell', params=params
        )
        return int(rpc_res['level'])

    def get_tenderbake_round(
        self, params: List[str] = None, chain: str = 'main', level: str = 'head'
    ) -> int:
        assert chain in {'main', 'test'}
        rpc_res = self.rpc(
            'get',
            f'/chains/{chain}/blocks/{level}/helpers/' 'round',
            params=params,
        )
        return int(rpc_res)

    def get_checkpoint(self) -> dict:
        rpc_res = self.rpc('get', '/chains/main/checkpoint')
        return rpc_res

    def get_savepoint(self) -> str:
        rpc_res = self.get_checkpoint()
        return rpc_res['savepoint']

    def get_caboose(self) -> str:
        rpc_res = self.get_checkpoint()
        return rpc_res['caboose']

    def wait_for_inclusion(
        self,
        operation_hash: str,
        branch: str = None,
        check_previous: int = None,
        args=None,
    ) -> client_output.WaitForResult:
        cmd = ['wait', 'for', operation_hash, 'to', 'be', 'included']
        if check_previous is not None:
            cmd += ['--check-previous', str(check_previous)]
        if branch is not None:
            cmd += ['--branch', branch]
        if args is None:
            args = []
        cmd += args
        return client_output.WaitForResult(self.run(cmd))

    def inject_protocol(self, proto) -> str:
        return self.run(['inject', 'protocol', proto], admin=True)

    def list_protocols(self) -> List[str]:
        cmd = ['list', 'protocols']
        return client_output.extract_protocols(self.run(cmd, admin=True))

    def environment_protocol(self, proto) -> str:
        cmd = ['protocol', 'environment', proto]
        return client_output.extract_environment_protocol(
            self.run(cmd, admin=True)
        )

    def list_understood_protocols(self) -> List[str]:
        cmd = ['list', 'understood', 'protocols']
        return client_output.extract_protocols(self.run(cmd, admin=True))

    def submit_proposals(
        self, account: str, protos: List[str]
    ) -> client_output.SubmitProposalsResult:
        cmd = ['submit', 'proposals', 'for', account] + protos
        return client_output.SubmitProposalsResult(self.run(cmd))

    def submit_ballot(self, account: str, proto: str, vote: str) -> str:
        return self.run(['submit', 'ballot', 'for', account, proto, vote])

    def bootstrapped(self) -> str:
        return self.run(['bootstrapped'])

    def sync_state(self) -> str:
        res = self.rpc('get', 'chains/main/is_bootstrapped')
        return res['sync_state']

    def is_bootstrapped(self, chain: str = 'main') -> bool:
        assert chain in {'main', 'test'}
        res = self.rpc('get', f'chains/{chain}/is_bootstrapped')
        return res['bootstrapped']

    def cleanup(self) -> None:
        """Remove base dir, only if not provided by user."""
        if self._is_tmp_dir:
            shutil.rmtree(self.base_dir)

    def deploy_msig(
        self,
        msig_name: str,
        amount: float,
        src: str,
        threshold: int,
        keys: List[str],
        args: List[str] = None,
    ) -> client_output.OriginationResult:
        cmd = [
            'deploy',
            'multisig',
            msig_name,
            'transferring',
            str(amount),
            'from',
            src,
            'with',
            'threshold',
            str(threshold),
            'on',
            'public',
            'keys',
        ]
        cmd += keys
        if args is None:
            args = []
        cmd += args
        return client_output.OriginationResult(self.run(cmd))

    def msig_sign_transfer(
        self,
        msig_name: str,
        amount: float,
        dest: str,
        secret_key: str,
        args: List[str] = None,
        expected_warning: str = "",
    ) -> str:
        cmd = [
            'sign',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'transferring',
            str(amount),
            'to',
            dest,
            'using',
            'secret',
            'key',
            secret_key,
        ]
        if args is None:
            args = []
        cmd += args
        (res, err, _) = self.run_generic(cmd)
        assert err == expected_warning
        return res[:-1]

    def msig_sign_lambda(
        self, msig_name: str, lam: str, secret_key: str, args: List[str] = None
    ) -> str:
        cmd = [
            'sign',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'running',
            'lambda',
            lam,
            'using',
            'secret',
            'key',
            secret_key,
        ]
        if args is None:
            args = []
        cmd += args
        res = self.run(cmd)
        return res[:-1]

    def msig_sign_withdraw(
        self, msig_name: str, amount: float, dest: str, secret_key: str
    ) -> str:
        cmd = [
            'sign',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'transferring',
            str(amount),
            'to',
            dest,
            'using',
            'secret',
            'key',
            secret_key,
        ]
        return self.run(cmd)

    def msig_sign_set_delegate(
        self, msig_name: str, delegate: str, secret_key: str
    ) -> str:
        cmd = [
            'sign',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'setting',
            'delegate',
            'to',
            delegate,
            'using',
            'secret',
            'key',
            secret_key,
        ]
        res = self.run(cmd)
        return res[:-1]

    def msig_sign_withdrawing_delegate(
        self, msig_name: str, secret_key: str
    ) -> str:
        cmd = [
            'sign',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'withdrawing',
            'delegate',
            'using',
            'secret',
            'key',
            secret_key,
        ]
        res = self.run(cmd)
        return res[:-1]

    def msig_sign_setting_threshold(
        self,
        msig_name: str,
        secret_key: str,
        threshold: int,
        public_keys: List[str],
    ) -> str:
        cmd = [
            'sign',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'using',
            'secret',
            'key',
            secret_key,
            'setting',
            'threshold',
            'to',
            str(threshold),
            'and',
            'public',
            'keys',
            'to',
        ]
        cmd += public_keys
        res = self.run(cmd)
        return res[:-1]

    def sign_bytes_of_string(self, data: str, identity: str) -> str:
        cmd = ['sign', 'bytes', data, 'for', identity]
        return client_output.SignBytesResult(self.run(cmd)).signature

    def msig_prepare_transfer(
        self,
        msig_name: str,
        amount: float,
        dest: str,
        args: List[str] = None,
        expected_warning: str = "",
    ):
        cmd = [
            'prepare',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'transferring',
            str(amount),
            'to',
            dest,
        ]
        if args is None:
            args = []
        cmd += args
        (res, err, _) = self.run_generic(cmd)
        assert err == expected_warning
        return res[:-1]

    def msig_prepare_lambda(
        self, msig_name: str, lam: str, args: List[str] = None
    ):
        cmd = [
            'prepare',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'running',
            'lambda',
            lam,
        ]
        if args is None:
            args = []
        cmd += args
        return self.run(cmd)[:-1]

    def msig_prepare_set_delegate(
        self, msig_name: str, delegate: str, args: List[str] = None
    ):
        cmd = [
            'prepare',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'setting',
            'delegate',
            'to',
            delegate,
        ]
        if args is None:
            args = []
        cmd += args
        return self.run(cmd)[:-1]

    def msig_prepare_withdrawing_delegate(
        self, msig_name: str, args: List[str] = None
    ):
        cmd = [
            'prepare',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'withdrawing',
            'delegate',
        ]
        if args is None:
            args = []
        cmd += args
        return self.run(cmd)[:-1]

    def msig_prepare_setting_threshold(
        self,
        msig_name: str,
        threshold: int,
        public_keys: List[str],
        args: List[str] = None,
    ):
        cmd = [
            'prepare',
            'multisig',
            'transaction',
            'on',
            msig_name,
            'setting',
            'threshold',
            'to',
            str(threshold),
            'and',
            'public',
            'keys',
            'to',
        ]
        cmd += public_keys
        if args is None:
            args = []
        cmd += args
        return self.run(cmd)[:-1]

    def msig_transfer(
        self,
        msig_name: str,
        amount: float,
        dest: str,
        src: str,
        signatures: List[str],
        args: List[str] = None,
        expected_warning: str = "",
    ) -> str:
        cmd = [
            'from',
            'multisig',
            'contract',
            msig_name,
            'transfer',
            str(amount),
            'to',
            dest,
            'on',
            'behalf',
            'of',
            src,
            'with',
            'signatures',
        ] + signatures
        if args is None:
            args = []
        cmd += args
        (res, err, _) = self.run_generic(cmd)
        assert err == expected_warning
        return res[:-1]

    def msig_run_lambda(
        self,
        msig_name: str,
        lam: str,
        src: str,
        signatures: List[str],
        args: List[str] = None,
    ) -> str:
        cmd = [
            'from',
            'multisig',
            'contract',
            msig_name,
            'run',
            'lambda',
            lam,
            'on',
            'behalf',
            'of',
            src,
            'with',
            'signatures',
        ] + signatures
        if args is None:
            args = []
        cmd += args
        return self.run(cmd)

    def msig_set_delegate(
        self,
        msig_name: str,
        delegate: str,
        src: str,
        signatures: List[str],
        args: List[str] = None,
    ) -> str:
        cmd = [
            'set',
            'delegate',
            'of',
            'multisig',
            'contract',
            msig_name,
            'to',
            delegate,
            'on',
            'behalf',
            'of',
            src,
            'with',
            'signatures',
        ] + signatures
        if args is None:
            args = []
        cmd += args
        return self.run(cmd)

    def msig_withdrawing_delegate(
        self,
        msig_name: str,
        src: str,
        signatures: List[str],
        args: List[str] = None,
    ) -> str:
        cmd = [
            'withdraw',
            'delegate',
            'of',
            'multisig',
            'contract',
            msig_name,
            'on',
            'behalf',
            'of',
            src,
            'with',
            'signatures',
        ] + signatures
        if args is None:
            args = []
        cmd += args
        return self.run(cmd)

    def msig_run_transaction(
        self,
        msig_name: str,
        transaction: bytes,
        src: str,
        signatures: List[str],
        args: List[str] = None,
        expected_warning: str = "",
    ) -> str:
        cmd = [
            'run',
            'transaction',
            str(transaction),
            'on',
            'multisig',
            'contract',
            msig_name,
            'on',
            'behalf',
            'of',
            src,
            'with',
            'signatures',
        ] + signatures
        if args is None:
            args = []
        cmd += args
        (res, err, _) = self.run_generic(cmd)
        assert err == expected_warning
        return res[:-1]

    def msig_set_threshold(
        self,
        msig_name: str,
        threshold: int,
        public_keys: List[str],
        src: str,
        signatures: List[str],
        args: List[str] = None,
    ) -> str:
        cmd = [
            'set',
            'threshold',
            'of',
            'multisig',
            'contract',
            msig_name,
            'to',
            str(threshold),
            'and',
            'public',
            'keys',
            'to',
        ]
        cmd += public_keys
        cmd += ['on', 'behalf', 'of', src, 'with', 'signatures']
        cmd += signatures
        if args is None:
            args = []
        cmd += args
        return self.run(cmd)

    def fa12_check(self, fa_name: str) -> client_output.FA12CheckResult:
        cmd = ['check', 'contract', fa_name, 'implements', 'fa1.2']
        try:
            output = self.run(cmd)
            return client_output.FA12CheckResult(output)
        except subprocess.CalledProcessError as exc:
            return client_output.FA12CheckResult(exc.stderr)

    def fa12_transfer(
        self,
        contract_name: str,
        amount: int,
        src: str,
        dst: str,
        args: List[str],
    ) -> str:
        cmd = [
            'from',
            'fa1.2',
            'contract',
            contract_name,
            'transfer',
            str(amount),
            'from',
            src,
            'to',
            dst,
        ] + args
        return self.run(cmd)

    def fa12_transfer_as(
        self,
        contract_name: str,
        amount: int,
        src: str,
        dst: str,
        as_: str,
        args: List[str],
    ) -> str:
        args = ['--as', as_] + args
        return self.fa12_transfer(contract_name, amount, src, dst, args)

    def fa12_approve(
        self,
        contract_name: str,
        amount: int,
        src: str,
        dst: str,
        args: List[str],
    ) -> str:
        cmd = [
            'from',
            'fa1.2',
            'contract',
            contract_name,
            'as',
            src,
            'approve',
            str(amount),
            'from',
            dst,
        ] + args
        return self.run(cmd)

    def fa12_get_balance_offchain(
        self, contract_name: str, src: str, args: List[str]
    ) -> client_output.FA12ViewResult:
        cmd = [
            'from',
            'fa1.2',
            'contract',
            contract_name,
            'get',
            'balance',
            'for',
            src,
        ] + args
        return client_output.FA12ViewResult(self.run(cmd))

    def fa12_get_allowance_offchain(
        self,
        contract_name: str,
        src: str,
        dst: str,
        args: List[str],
    ) -> client_output.FA12ViewResult:
        cmd = [
            'from',
            'fa1.2',
            'contract',
            contract_name,
            'get',
            'allowance',
            'on',
            src,
            'as',
            dst,
        ] + args
        return client_output.FA12ViewResult(self.run(cmd))

    def fa12_get_total_supply_offchain(
        self, contract_name: str, args: List[str]
    ) -> client_output.FA12ViewResult:
        cmd = [
            'from',
            'fa1.2',
            'contract',
            contract_name,
            'get',
            'total',
            'supply',
        ] + args
        return client_output.FA12ViewResult(self.run(cmd))

    def fa12_get_balance_callback(
        self, contract_name: str, src: str, callback: str, args: List[str]
    ) -> str:
        cmd = [
            'from',
            'fa1.2',
            'contract',
            contract_name,
            'get',
            'balance',
            'for',
            src,
            'callback',
            'on',
            callback,
        ] + args
        return self.run(cmd)

    def fa12_get_allowance_callback(
        self,
        contract_name: str,
        src: str,
        dst: str,
        callback: str,
        args: List[str],
    ) -> str:
        cmd = [
            'from',
            'fa1.2',
            'contract',
            contract_name,
            'get',
            'allowance',
            'on',
            src,
            'as',
            dst,
            'callback',
            'on',
            callback,
        ] + args
        return self.run(cmd)

    def fa12_get_total_supply_callback(
        self, contract_name: str, src: str, callback: str, args: List[str]
    ) -> str:
        cmd = [
            'from',
            'fa1.2',
            'contract',
            contract_name,
            'get',
            'total',
            'supply',
            'as',
            src,
            'callback',
            'on',
            callback,
        ] + args
        return self.run(cmd)

    def fa12_deploy_viewer(
        self, viewer_name: str, typ: str, src: str, args: List[str]
    ) -> str:
        cmd = [
            'deploy',
            'viewer',
            'contract',
            viewer_name,
            'of',
            'type',
            typ,
            'from',
            src,
        ] + args
        return self.run(cmd)

    def fa12_mk_batch_transfer(self, dst: str, token: str, amount: int):
        json_obj = [
            {"destination": dst, "amount": str(amount), "token_contract": token}
        ]
        return json_obj

    def fa12_multiple_tokens_transfers(
        self, src: str, ops: str, args: List[str]
    ) -> str:
        cmd = [
            'multiple',
            'fa1.2',
            'transfers',
            'from',
            src,
            'using',
            ops,
        ] + args
        res = self.run(cmd)
        return res

    def fa12_multiple_tokens_transfers_as(
        self, src: str, as_: str, ops: str, args: List[str]
    ) -> str:
        args = ['--as', as_] + args
        res = self.fa12_multiple_tokens_transfers(src, ops, args)
        return res

    def check_node_listening(
        self, timeout: float = 1, attempts: int = 20
    ) -> bool:
        """Checks whether the node is responsive, by polling it
        using the `version` rpc.

        Args:
            timeout (float): time (sec) to wait between retries
            attempts (int): maximal number of attempts
        Returns:
            True iff the node is running, and successfully answered the
            `version` rpc.
        """
        for _ in range(attempts):
            try:
                time.sleep(timeout)
                # any shell RPC will do, this one is light-weight
                self.rpc('get', '/network/version')
                return True
            except Exception:  # pylint: disable=broad-except
                pass
        return False

    def expand_macros(self, src: str) -> str:
        cmd = ['expand', 'macros', 'in', src]
        return self.run(cmd)

    def list_mockup_protocols(self) -> client_output.ListMockupProtocols:
        cmd = ['list', 'mockup', 'protocols']
        return client_output.ListMockupProtocols(self.run(cmd))

    def create_mockup(
        self,
        protocol: str,
        check: bool = True,
        protocol_constants_file: str = None,
        bootstrap_accounts_file: str = None,
    ) -> client_output.CreateMockup:
        cmd = ['--protocol', protocol, 'create', 'mockup']
        if protocol_constants_file is not None:
            cmd += ["--protocol-constants", protocol_constants_file]
        if bootstrap_accounts_file is not None:
            cmd += ["--bootstrap-accounts", bootstrap_accounts_file]
        (stdout, stderr, exit_code) = self.run_generic(cmd, check=check)
        return client_output.CreateMockup(stdout, stderr, exit_code)

    def get_block_metadata_hash(self) -> str:
        return self.rpc('get', '/chains/main/blocks/head/metadata_hash')

    def get_operations_metadata_hash(self) -> str:
        return self.rpc(
            'get', '/chains/main/blocks/head/operations_metadata_hash'
        )

    def sapling_gen_key(
        self, key_name: str, force: bool = False, args: List[str] = None
    ) -> client_output.SaplingGenKeyResult:

        cmd = ['sapling', 'gen', 'key', key_name, '--unencrypted']
        args = args or []
        if force:
            args += ['--force']
        cmd += args
        return client_output.SaplingGenKeyResult(self.run(cmd))

    def sapling_use_key_for_contract(
        self,
        key_name: str,
        contract_name: str,
        memo_size: int = None,
    ) -> None:
        cmd = [
            'sapling',
            'use',
            'key',
            key_name,
            'for',
            'contract',
            contract_name,
        ]
        if memo_size is not None:
            cmd += ['--memo-size', str(memo_size)]
        self.run(cmd)

    def sapling_gen_address(
        self, key_name: str, index: int = None, args: List[str] = None
    ) -> client_output.SaplingGenAddressResult:
        args = args or []
        cmd = ['sapling', 'gen', 'address', key_name]
        if index is not None:
            cmd += ['--address-index', str(index)]
        cmd += args
        return client_output.SaplingGenAddressResult(self.run(cmd))

    def sapling_import_key(
        self,
        key_name: str,
        mnemonic: List[str],
        force: bool = False,
        args: List[str] = None,
    ) -> None:

        mnemonic_str = " ".join(mnemonic)
        cmd = [
            'sapling',
            'import',
            'key',
            key_name,
            '--unencrypted',
            '--mnemonic',
            mnemonic_str,
        ]
        args = args or []
        if force:
            cmd += ['--force']
        cmd += args
        self.run(cmd)

    def sapling_derive_key(
        self,
        source_key_name: str,
        target_key_name: str,
        contract_name: str,
        index: int,
        force: bool = False,
    ) -> client_output.SaplingDeriveKeyResult:

        cmd = [
            'sapling',
            'derive',
            'key',
            target_key_name,
            'from',
            source_key_name,
            'at',
            'index',
            str(index),
            '--for-contract',
            contract_name,
            '--unencrypted',
        ]
        if force:
            cmd += ['--force']
        return client_output.SaplingDeriveKeyResult(self.run(cmd))

    def sapling_get_balance(
        self, key_name: str, contract_name: str, args: List[str] = None
    ) -> client_output.SaplingGetBalanceResult:

        cmd = [
            'sapling',
            'get',
            'balance',
            'for',
            key_name,
            'in',
            'contract',
            contract_name,
        ]
        args = args or []
        cmd += args
        return client_output.SaplingGetBalanceResult(self.run(cmd))

    def sapling_shield(
        self,
        amount: float,
        src: str,
        dest: str,
        contract: str,
        args: List[str] = None,
    ) -> None:
        cmd = [
            'sapling',
            'shield',
            str(amount),
            'from',
            src,
            'to',
            dest,
            'using',
            contract,
        ]
        args = args or []
        cmd += args
        self.run(cmd)

    def sapling_unshield(
        self,
        amount: float,
        src: str,
        dest: str,
        contract: str,
        args: List[str] = None,
    ) -> None:
        cmd = [
            'sapling',
            'unshield',
            str(amount),
            'from',
            src,
            'to',
            dest,
            'using',
            contract,
        ]
        args = args or []
        cmd += args
        self.run(cmd)

    def sapling_forge_transaction(
        self,
        amount: float,
        src: str,
        dest: str,
        contract: str,
        file: str,
        args: List[str] = None,
    ) -> None:
        cmd = [
            'sapling',
            'forge',
            'transaction',
            str(amount),
            'from',
            src,
            'to',
            dest,
            'using',
            contract,
            '--file',
            file,
        ]
        args = args or []
        cmd += args
        self.run(cmd)

    def sapling_submit(
        self, file: str, fee_payer: str, contract: str, args: List[str] = None
    ) -> None:
        cmd = ['sapling', 'submit', file, 'from', fee_payer, 'using', contract]
        args = args or []
        cmd += args
        self.run(cmd)

    def sapling_list_keys(self) -> List[str]:
        cmd = ['sapling', 'list', 'keys']
        return self.run(cmd).strip().split("\n")

    def run_view(
        self,
        entrypoint: str,
        contract: str,
        parameter: str,
        args: List[str] = None,
    ) -> client_output.ViewResult:
        cmd = [
            'run',
            'tzip4',
            'view',
            entrypoint,
            'on',
            'contract',
            contract,
            'with',
            'input',
            parameter,
        ]
        args = args or []
        cmd += args
        return client_output.ViewResult(self.run(cmd))

    def frozen_deposits(self, delegate: str, level: str = None) -> int:
        """returns deposits (in mutez) held for account for given level"""
        if level:
            level_arg = f'/?level={level}'
        else:
            level_arg = ''
        return int(
            self.rpc(
                'get',
                f'/chains/main/blocks/head/context/delegates/'
                f'{delegate}/frozen_deposits{level_arg}',
            )
        )
