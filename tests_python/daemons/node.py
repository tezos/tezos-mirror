from typing import Dict, List, Tuple
import subprocess
import os
import tempfile
import shutil
from . import utils


# Timeout before killing a node which doesn't react to SIGTERM
TERM_TIMEOUT = 10


class Node:
    """Forks a tezos node and manages its persistent state.

    Typical use.

    node = Node(node_bin,
                sandbox_file,
                p2p_port=p2p_node,
                rpc_port=rpc_node,
                peers=peers_rpc,
                log_file=log_file,
                params=params,
                log_levels=log_levels)

    node.snapshot_import(snapshot) # optional, use a snapshot
    node.init_id() # generate node id
    node.init_config() # generate config file based on parameters
    node.run() # run tezos-node process
    node.terminate() # terminate process
    node.run() # re-run using same process
    node.terminate() # or node.kill()
    node.cleanup() # cleanup temp files
    """

    def __init__(self,
                 node: str,
                 sandbox_file: str = None,
                 expected_pow: float = 0.0,
                 node_dir: str = None,
                 use_tls: Tuple[str, str] = None,
                 params: List[str] = None,
                 log_file: str = None,
                 p2p_port: int = 9732,
                 rpc_port: int = 8732,
                 peers: List[int] = None,
                 log_levels: Dict[str, str] = None,
                 env: Dict[str, str] = None):

        """Creates a new Popen instance for a tezos-node, and manages context.

        args:
            use_tls (tuple): None if no tls, else couple of strings
                            (certificate, key)

        Creates a temporary node directory unless provided  by caller.
        Generate node identity.
        """
        if sandbox_file:
            assert os.path.isfile(sandbox_file), f'{sandbox_file} not a file'
        assert os.path.isfile(node), f'{node} not a file'
        assert node_dir is None or os.path.isdir(node_dir), (f'{node_dir} not '
                                                             f'a dir')
        if params is None:
            params = ["--network=sandbox"]

        self.log_file = log_file
        self._temp_dir = node_dir is None
        if node_dir is None:
            node_dir = tempfile.mkdtemp(prefix='tezos-node.')
        self.node_dir = node_dir
        self.p2p_port = p2p_port
        self.rpc_port = rpc_port
        self.expected_pow = expected_pow
        self.node = node
        self._params = params
        self._run_called_before = False

        node_run = [node,
                    'run',
                    '--data-dir', node_dir,
                    '--no-bootstrap-peers']
        if sandbox_file:
            node_run.append(f'--sandbox={sandbox_file}')
        node_run.extend(params)

        if peers is not None:
            for peer in peers:
                node_run.append('--peer')
                node_run.append(f'127.0.0.1:{peer}')

        self.use_tls = use_tls

        new_env = None
        if env is not None:
            new_env = os.environ.copy()
            new_env.update(env)
        if log_levels is not None:
            new_env = os.environ.copy() if new_env is None else new_env
            lwt_log = ";".join(f'{key} -> {values}' for key, values in
                               log_levels.items())
            new_env['TEZOS_LOG'] = lwt_log
        self._new_env = new_env
        self._node_run = node_run
        self._process = None

    def run(self):
        node_run_str = utils.format_command(self._node_run)
        print(node_run_str)
        # overwrite old log on on first invocation only
        overwrite_log = not self._run_called_before
        stdout, stderr = utils.prepare_log(self._node_run,
                                           self.log_file,
                                           overwrite_log)
        self._process = subprocess.Popen(self._node_run, stdout=stdout,
                                         stderr=stderr, env=self._new_env)
        self._run_called_before = True

    def init_config(self):
        node_config = [self.node,
                       'config',
                       'init',
                       '--data-dir', self.node_dir,
                       '--net-addr', f'127.0.0.1:{self.p2p_port}',
                       '--rpc-addr', f'127.0.0.1:{self.rpc_port}',
                       '--expected-pow', str(self.expected_pow)] + self._params

        if self.use_tls:
            # We can't create tezos.crt/tezos.key here
            # as node_dir has to be empty when we run node_config
            node_config += ['--rpc-tls',
                            (f'{self.node_dir}/tezos.crt,'
                             f'{self.node_dir}/tezos.key')]

        node_config_str = utils.format_command(node_config)
        print(node_config_str)
        subprocess.run(node_config, check=True)

    def init_id(self):
        node_identity = [self.node,
                         'identity',
                         'generate',
                         str(self.expected_pow),
                         '--data-dir', self.node_dir]
        node_identity_str = utils.format_command(node_identity)
        print(node_identity_str)
        subprocess.run(node_identity, check=True)
        if self.use_tls:
            with open(f'{self.node_dir}/tezos.crt', 'w+') as file:
                file.write(self.use_tls[0])
            with open(f'{self.node_dir}/tezos.key', 'w+') as file:
                file.write(self.use_tls[1])

    def upgrade_storage(self):
        node_upgrade = [self.node, 'upgrade', 'storage', '--data-dir',
                        self.node_dir]
        print(utils.format_command(node_upgrade))
        subprocess.run(node_upgrade, check=True)

    def snapshot_export(self, file, params=None):
        if params is None:
            params = []
        params = [f'--data-dir', self.node_dir] + params
        snapshot_cmd = ([self.node, 'snapshot',
                         'export'] + list(params) + [file])
        print(utils.format_command(snapshot_cmd))
        subprocess.run(snapshot_cmd, check=True)

    def snapshot_import(self, file, params=None):
        if params is None:
            params = []
        params = [f'--data-dir', self.node_dir] + params
        snapshot_cmd = ([self.node, 'snapshot',
                         'import'] + list(params) + [file])
        print(utils.format_command(snapshot_cmd))
        subprocess.run(snapshot_cmd, check=True)

    def reconstruct(self, params=None):
        if params is None:
            params = []
        params = [f'--data-dir', self.node_dir] + params
        reconstruct_cmd = ([self.node, 'reconstruct'] + list(params))
        print(utils.format_command(reconstruct_cmd))
        subprocess.run(reconstruct_cmd, check=True)

    def cleanup(self):
        """Remove node directory (only if generated by constructor)"""
        if self._temp_dir:
            shutil.rmtree(self.node_dir)

    def terminate(self):
        assert self._process
        return self._process.terminate()

    def kill(self):
        assert self._process
        return self._process.terminate()

    def terminate_or_kill(self):
        self._process.terminate()
        try:
            return self._process.wait(timeout=TERM_TIMEOUT)
        except subprocess.TimeoutExpired:
            return self._process.kill()

    def poll(self):
        assert self._process
        return self._process.poll()
