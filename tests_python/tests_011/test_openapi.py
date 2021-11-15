""" Tests generating the implementation of openapi/swagger:
    https://swagger.io/

    This script launches a sandbox node, activates folder specific
    protocol, gets the RPC descriptions as JSON, and converts this JSON
    into an OpenAPI specification.

    This test mimicks src/openapi/generate.sh.
"""

import json
import subprocess
from pathlib import Path
import requests
import openapi_spec_validator
import pytest

from launchers.sandbox import Sandbox
from tools.constants import NODE_PARAMS
from . import protocol


def _get_tezos_node_version() -> str:
    cmd = ["ocaml", "../scripts/print_version.ml"]
    process_ret = subprocess.run(
        cmd, check=True, capture_output=True, text=True
    )
    version = process_ret.stdout.strip()
    assert version, "version should not be empty"
    return version


class TestOpenAPI:
    @pytest.fixture(scope="class")
    def sandbox(self, sandbox: Sandbox):
        sandbox.add_node(0, params=NODE_PARAMS)
        client = sandbox.client(0)
        protocol.activate(client)
        return sandbox

    @pytest.mark.parametrize(
        "rpc_path", ["describe", "describe/chains/main/blocks/head/"]
    )
    def test_validity(self, sandbox: Sandbox, rpc_path: str, tmp_path: Path):
        """
        Mimicks the script src/openapi/generate.sh. Generates the API
        and check it generates a valid OpenAPI specification.
        """
        node = sandbox.node(0)
        addr = f"http://localhost:{node.rpc_port}/{rpc_path}?recurse=yes"
        json_path = tmp_path / "result.json"
        with open(json_path, "w") as o_file:
            json_res = requests.get(addr).json()
            json.dump(json_res, o_file)

        # If you need to debug, insert time.sleep(15) in there,
        # to give you time to inspect generated files before the
        # enclosing 'with' block finishes or to execute the dune
        # command manually while the temporary files are still there.
        version = _get_tezos_node_version()
        cmd = [
            "dune",
            "exec",
            "../src/bin_openapi/rpc_openapi.exe",
            "--",
            version,
            str(json_path.absolute()),
        ]
        process_ret = subprocess.run(
            cmd, check=True, capture_output=True, text=True
        )
        res = json.loads(process_ret.stdout)
        openapi_spec_validator.validate_spec(res)
