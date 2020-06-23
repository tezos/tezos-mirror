from typing import List, Optional
import subprocess
from client import client


class ClientRegression(client.Client):
    """Client to a Tezos node that intercepts output for regression testing.

    This subclass of Client can be associated with a `regtest` fixture
    as provided by the `pytest-regtest`, using the `set_regtest`
    method. When a `regtest` fixture is present, all output from the
    client that results from executing the `run` method is written to
    this regtest.

    If the command fails, the stderr output is also written.
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
        mode: Optional[str] = None,
        disable_disclaimer: bool = True,
    ):
        self.regtest = None
        super().__init__(
            client_path=client_path,
            admin_client_path=admin_client_path,
            host=host,
            base_dir=base_dir,
            rpc_port=rpc_port,
            use_tls=use_tls,
            endpoint=endpoint,
            mode=mode,
            disable_disclaimer=disable_disclaimer,
        )

    def set_regtest(self, regtest):
        self.regtest = regtest

    def run(
        self,
        params: List[str],
        admin: bool = False,
        check: bool = True,
        trace: bool = False,
    ):
        stderr_output = ''
        caught_exc = None
        try:
            output = super().run(params, admin, check, trace)
        except subprocess.CalledProcessError as exc:
            output = exc.args[2]
            stderr_output = exc.args[3]
            caught_exc = exc
        if self.regtest is not None:
            self.regtest.write(output)
            self.regtest.write(stderr_output)
        if caught_exc is not None:
            raise caught_exc
        return output
