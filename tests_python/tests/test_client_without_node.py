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
# We cannot specify --endpoint in _CMD_LINE_ARGS, because the fixture
# for obtaining a client in this file (simple_client) uses Client's
# constructor default endpoint value. Hence if we specify --endpoint here
# it'll end up being specified twice in commands, and the value specified here
# will be ignored.
_CMD_LINE_ARGS = {"--wait": "3",
                  "--remote-signer": "http://10.0.0.2",
                  "--password-filename": "/tmp/doesnt_exist_either"}
_CONFIG_FILE_FLAG = "--config-file"


@pytest.mark.client
class TestChainId:

    def test_chain_id_block_hash(self, simple_client: Client):
        block_hash = 'BKyFui5WPY1n3e9aKF3qd2kGBKBtHu3rtm5miYFnUagJC1BdHTF'
        prms = ['compute', 'chain', 'id', 'from', 'block', 'hash', block_hash]
        assert simple_client.run(prms).strip() == 'NetXuwrXPL4VeX5'

    def test_chain_id_seed(self, simple_client: Client):
        seed = 'choucroute'
        prms = ['compute', 'chain', 'id', 'from', 'seed', seed]
        assert simple_client.run(prms).strip() == 'NetXLGmPi3c5DXf'


def _write_config_file(client: Client, filename: str,
                       config_dict: Optional[dict]):
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
    return ([_CONFIG_FILE_FLAG, config_file] if config_file else []) + cmd


def _gen_assert_msg(flag, sent, received):
    result = f"Json sent with --{flag} differs from"
    result += " json received"
    result += f"\nJson sent is:\n{sent}"
    result += f"\nwhile json received is:\n{received}"


@pytest.mark.client
@pytest.mark.parametrize('config_dict', _INPUT_CONFIG_FILES)
class TestConfigInit:

    def test_config_init(self, simple_client: Client,
                         config_dict: Optional[dict]):
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

    def test_config_init_roundtrip(self, simple_client: Client,
                                   config_dict: Optional[dict]):
        """ Tests that calling `config init -o tmp_file` and
            feeding its result to `tezos-client --config-file` works
            and yields the same result (i.e. calling `tezos-client
            --config-file tmp_file config init -o tmp_file2 yields
            a `tmp_file2` that is similar to `tmp_file`).

            `config_dict` specifies the content of the initial config file
            to use or None not to specify one.
        """
        try:
            if config_dict is None:
                # Take initial json from default output of `config init`

                tmp_file1 = tempfile.mktemp(prefix='tezos-client.config_file')
                cmd = ["config", "init", "-o", tmp_file1]
                simple_client.run(cmd)
                with open(tmp_file1) as handle:
                    json1 = json.load(handle)

                # Execute an arbitrary effectless command:
                list_protos = ["list", "understood", "protocols"]
                # This checks that
                # `--config-file tmp_file1 arbitrary command` works
                cmd = _with_config_file_cmd(tmp_file1, list_protos)
                simple_client.run(cmd)
            else:
                # Take initial json from config_dict

                # Write config_dict to a file
                tmp_file1 = tempfile.mktemp(prefix='tezos-client.config_file')
                json1 = _write_config_file(simple_client,
                                           tmp_file1,
                                           config_dict)

            # Execute `config init -o`
            tmp_file2 = tempfile.mktemp(prefix='tezos-client.config_file')
            cmd = _with_config_file_cmd(tmp_file1,
                                        ["config", "init", "-o", tmp_file2])
            simple_client.run(cmd)

            # Load file generated by `config init -o`
            with open(tmp_file2) as handle:
                json2 = json.load(handle)

            # and finally check that the json generated by `config init -o`
            # matches the input data (either the default one or the one
            # specified with --config-file)
            assert json1 == json2, _gen_assert_msg(_CONFIG_FILE_FLAG,
                                                   json1, json2)
        finally:
            os.remove(tmp_file1)
            os.remove(tmp_file2)


def _cmd_line_flag_to_json_field(cmd_line_flag: str):
    if cmd_line_flag == "--wait":
        return "confirmations"
    result = cmd_line_flag
    if cmd_line_flag.startswith("--"):
        result = result[2:]
    return result.replace("-", "_")


@pytest.mark.client
@pytest.mark.parametrize('config_dict', _INPUT_CONFIG_FILES)
class TestConfigShow:
    """ Tests of `tezos-client config show` """

    def test_config_show(self, simple_client: Client,
                         config_dict: Optional[dict]):
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

    @pytest.mark.parametrize('cmd_line_args', [{}, _CMD_LINE_ARGS])
    def test_config_show_roundtrip(self, simple_client: Client,
                                   config_dict: Optional[dict],
                                   cmd_line_args: dict):
        """
            Tests calling `config show` with or without `--config-file`
            and with some command line parameters (`cmd_line_arg`). It
            then parses the output to check its valid json and to check
            that command line parameters were honored.

            Then it feeds this output to a new call to `--config-file file
            config show` and checks that the json returned by this second call
            agrees with what was specified by `file`.

            This is a roundtrip test using a small matrix.
        """
        try:
            in_file1 = None
            in_file2 = None
            if config_dict is not None:
                in_file1 = tempfile.mktemp(prefix='tezos-client.config_file')
                _write_config_file(simple_client, in_file1, config_dict)

            cmd = []
            # Pass command line parameters
            for (flag, value) in cmd_line_args.items():
                cmd += [flag, value]
            cmd += ["config", "show"]
            cmd = _with_config_file_cmd(in_file1, cmd)
            # Take standard output
            (stdout, _, _) = simple_client.run_generic(cmd)

            output_json1 = json.loads(stdout)
            # Verify that command line parameters were honored
            for (flag, value) in cmd_line_args.items():
                input_value = cmd_line_args[flag]
                assert isinstance(input_value, str)
                output_value = output_json1[_cmd_line_flag_to_json_field(flag)]
                output_value = str(output_value)
                err_msg = f"Value of command line flag {flag} is not honored:"\
                          f" passed {input_value} but"\
                          f" config show yielded {output_value}"
                assert output_value == input_value, err_msg
            in_file2 = tempfile.mktemp(prefix='tezos-client.config_file')
            # Write output of first call to `config show` to disk,
            # to pass it to second call below
            with open(in_file2, 'w') as handle:
                handle.write(json.dumps(output_json1))

            # Use previous ouput file as input now
            cmd = _with_config_file_cmd(in_file2, ["config", "show"])
            (stdout, _, _) = simple_client.run_generic(cmd)

            output_json2 = json.loads(stdout)

            # And finally check that the final output matches the input
            assert output_json1 == output_json2,\
                _gen_assert_msg(_CONFIG_FILE_FLAG, output_json1, output_json2)
        finally:
            for in_file in [in_file1, in_file2]:
                if in_file is not None:
                    os.remove(in_file)
