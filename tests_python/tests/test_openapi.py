""" Tests generating the implementation of openapi/swagger:
    https://swagger.io/

    This script launches a sandbox node, activates Carthage,
    gets the RPC descriptions as JSON, and converts this JSON into
    an OpenAPI specification.

    This test mimicks src/openapi/generate.sh.
"""

import json
import os
import subprocess
import tempfile
import requests
from launchers.sandbox import Sandbox
from tools.constants import CARTHAGE, NODE_PARAMS
from tools import utils


def _get_version() -> str:
    cmd = ["ocaml", "../scripts/print_version.ml"]
    print(" ".join(cmd))
    process_ret = subprocess.run(cmd, check=True,
                                 capture_output=True, text=True)
    return process_ret.stdout.strip()


def _write_file(path: str, content: str):
    with open(path, 'w') as handle:
        handle.write(content)


def _try_json_loads(addr: str, content: str):
    """
        `addr` is the address from which `content` was obtained. `addr`
        is only used for generating a faulty's assert message.
    """
    try:
        json.loads(content)
    except json.JSONDecodeError:
        assert False, f"{addr} did not return valid json"


def test_openapi(sandbox: Sandbox):
    """ Mimicks the script src/openapi/generate.sh. Generates the API
        and check it generates valid json. """
    node_idx = 0
    sandbox.add_node(0, params=NODE_PARAMS)
    node = sandbox.node(node_idx)

    client = sandbox.client(0)
    utils.activate_alpha(client, proto=CARTHAGE)

    api_addr = f"http://localhost:{node.rpc_port}/describe/?recurse=yes"
    api = requests.get(api_addr).text
    _try_json_loads(api_addr, api)

    proto_api_addr = f"http://localhost:{node.rpc_port}" + \
                     "/describe/chains/main/blocks/head?recurse=yes"
    proto_api = requests.get(proto_api_addr).text
    _try_json_loads(proto_api_addr, proto_api)

    version = _get_version()
    assert version, "version should not be empty"

    with tempfile.TemporaryDirectory(prefix='tezos-openapi-') as base_dir:
        api_path = os.path.join(base_dir, "api.json")
        proto_api_path = os.path.join(base_dir, "proto_api.json")

        for (path, content) in [(api_path, api),
                                (proto_api_path, proto_api)]:
            _write_file(path, content)

        for input_ in [api_path, proto_api_path]:
            # If you need to debug, insert time.sleep(15) in there,
            # to give you time to inspect generated files before the
            # enclosing 'with' block finishes or to execute the dune
            # command manually while the temporary files are still there.
            cmd = ["dune", "exec", "../src/openapi/rpc_openapi.exe",
                   "--", version, input_]
            print(" ".join(cmd))
            process_ret = subprocess.run(cmd, check=True,
                                         capture_output=True, text=True)
            _try_json_loads(" ".join(cmd), process_ret.stdout)
