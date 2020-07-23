"""Node-less tests for the client.

Some client tests do not require a node running, nor a
persistent mockup environment. These can be placed here.
"""
import json
import os
import tempfile
from typing import List, Optional
import pytest
from client.client import Client

# Note that specifying "endpoint" and "web_port" is required
# for the final assertion of test_config_init_roundtrip to pass. That's because
# `config init -o` writes them even if unspecified by `--config-file`
# (it's a fine behavior, I just wanted to highlight it).
_INPUT_CONFIG_FILE = {"confirmations": 1,
                      "endpoint": "http://127.0.0.1:8732",
                      "remote_signer": "http://127.0.0.2",
                      "web_port": 8080,
                      "password_filename": "/tmp/doesnt_exist"}
_INPUT_CONFIG_FILES = [None, _INPUT_CONFIG_FILE]


@pytest.mark.client
class TestChainId:

    def test_chain_id_block_hash(self, simple_client):
        block_hash = 'BKyFui5WPY1n3e9aKF3qd2kGBKBtHu3rtm5miYFnUagJC1BdHTF'
        prms = ['compute', 'chain', 'id', 'from', 'block', 'hash', block_hash]
        assert simple_client.run(prms).strip() == 'NetXuwrXPL4VeX5'

    def test_chain_id_seed(self, simple_client):
        seed = 'choucroute'
        prms = ['compute', 'chain', 'id', 'from', 'seed', seed]
        assert simple_client.run(prms).strip() == 'NetXLGmPi3c5DXf'


def _write_config_file(client: Client, filename: str, config_dict: dict):
    """ Writes `config_dict` to `filename`. Returns the json effectively
        written """
    assert client is not None
    assert filename is not None
    assert config_dict is not None

    augmented_dict = dict(config_dict)  # Copy for safety
    # We need to set base_dir, it's required in the config file
    augmented_dict["base_dir"] = client.base_dir
    with open(filename, 'w') as handle:
        json.dump(augmented_dict, handle)

    return augmented_dict


def _with_config_file_cmd(config_file: Optional[str], cmd: List[str]):
    """ Prefixes `cmd` with ["--config-file", config_file] if
        config_file is not None """
    return (["--config-file", config_file] if config_file else []) + cmd


@pytest.mark.client
class TestConfigInit:

    @pytest.mark.parametrize('config_dict', _INPUT_CONFIG_FILES)
    def test_config_init(self, simple_client, config_dict):
        """
            Tests that calling
            `[--config-file config_dict]? config init -o tmp_file`
            works and yields valid json.
        """
        try:
            out_file = tempfile.mktemp(prefix='tezos-client.config_file')
            in_file = None

            if config_dict is not None:
                in_file = tempfile.mktemp(prefix='tezos-client.config_file')
                _write_config_file(simple_client, in_file, config_dict)

            cmd = _with_config_file_cmd(in_file,
                                        ["config", "init", "-o", out_file])
            simple_client.run(cmd)
            # Try loading the file as json, to check it is valid
            with open(out_file) as handle:
                json.load(handle)
        finally:
            if in_file is not None:
                os.remove(in_file)
            os.remove(out_file)

    def test_config_init_roundtrip(self, simple_client):
        """ Tests that calling `config init -o tmp_file` and
            feeding its result to `tezos-client --config-file` works
            and yields the same result (i.e. calling `tezos-client
            --config-file tmp_file config init -o tmp_file2 yields
            a `tmp_file2` that is similar to `tmp_file`).
        """
        try:
            tmp_file1 = tempfile.mktemp(prefix='tezos-client.config_file')
            tmp_file2 = tempfile.mktemp(prefix='tezos-client.config_file')
            cmd = ["config", "init", "-o", tmp_file1]
            simple_client.run(cmd)
            with open(tmp_file1) as handle:
                json1 = json.load(handle)

            flag = "--config-file"

            # Execute an arbitrary effectless command
            cmd = _with_config_file_cmd(tmp_file1,
                                        ["list", "understood", "protocols"])
            simple_client.run(cmd)  # Should work

            cmd = _with_config_file_cmd(tmp_file1,
                                        ["config", "init", "-o", tmp_file2])
            simple_client.run(cmd)  # Generate new config file

            with open(tmp_file2) as handle:
                json2 = json.load(handle)

            def _gen_assert_msg(flag, sent, received):
                result = f"Json sent with --{flag} differs from"
                result += " json received"
                result += f"\nJson sent is:\n{sent}"
                result += f"\nwhile json received is:\n{received}"

            # and finally check that the json object received is the same
            # as the one given as input
            assert json1 == json2,\
                _gen_assert_msg(flag, json1, json2)
        finally:
            os.remove(tmp_file1)
            os.remove(tmp_file2)


@pytest.mark.client
@pytest.mark.parametrize('config_dict', _INPUT_CONFIG_FILES)
class TestConfigShow:
    """ Tests of `tezos-client config show` """

    def test_config_show(self, simple_client: Client, config_dict: dict):
        """
            Tests that calling `config show` works, with or without
            specifying `--config-file`
        """
        try:
            tmp_file = None
            if config_dict is not None:
                tmp_file = tempfile.mktemp(prefix='tezos-client.config_file')
                _write_config_file(simple_client, tmp_file, config_dict)

            cmd = _with_config_file_cmd(tmp_file, ["config", "show"])
            simple_client.run(cmd)
        finally:
            if tmp_file is not None:
                os.remove(tmp_file)
