"""Node-less tests for the client.

Some client tests do not require a node running, nor a
persistent mockup environment. These can be placed here.
"""
import json
import os
import subprocess
import tempfile
from typing import List, Optional
import pytest
from client.client import Client
from tools.utils import assert_run_failure

# Note that specifying "endpoint" and "web_port" is required
# for the final assertion of test_config_init_roundtrip to pass. That's because
# `config init -o` writes them even if unspecified by `--config-file`
# (it's a fine behavior, I just wanted to highlight it).
_INPUT_CONFIG_FILE = {
    "confirmations": 1,
    "endpoint": "http://127.0.0.1:8732",
    "remote_signer": "http://127.0.0.2",
    "web_port": 8080,
    "password_filename": "/tmp/doesnt_exist",
}
_INPUT_CONFIG_FILES = [None, _INPUT_CONFIG_FILE]
_CMD_LINE_ARGS = {
    "--endpoint": "http://127.0.0.1:9732",
    "--wait": "3",
    "--remote-signer": "http://10.0.0.2",
    "--password-filename": "/tmp/doesnt_exist_either",
}
_CONFIG_FILE_FLAG = "--config-file"


@pytest.mark.client
class TestImportKeyMnemonic:
    """Checks that the keys are correctly imported from a mnemonic."""

    @pytest.fixture
    def mnemonic(self):
        return (
            "seek paddle siege sting siege sick kidney "
            + "detect coral because comfort long enforce napkin enter"
        )

    @pytest.fixture
    def passphrase(self):
        return "very_secure_passphrase"

    def test_import_simple(self, client: Client):
        """Tests a simple import."""
        mnemonic = "release easy pulp drop select attack false math cook \
angry spin ostrich round dress acoustic"
        prms = ["import", "keys", "from", "mnemonic", "zebra", "--force"]
        stdin = mnemonic + "\n\n"
        client.run_generic(prms, stdin=stdin)
        addr = client.get_known_addresses()
        assert addr.wallet["zebra"] == "tz1aGUKE72eN21iWztoDEeH4FeKaxWb7SAUb"

    def test_import_already_present_alias(self, client: Client, mnemonic):
        """Tests that importing fails if the alias is already present."""
        prms = [
            "import",
            "keys",
            "from",
            "mnemonic",
            "super_original",
            "--force",
        ]
        stdin = mnemonic + "\n\n"
        client.run_generic(prms, stdin=stdin)
        prms = ["import", "keys", "from", "mnemonic", "super_original"]
        expected_error = "The secret_key alias super_original already exists."
        with assert_run_failure(expected_error):
            client.run_generic(prms, stdin=stdin)

    def test_import_passphrase(self, client: Client, mnemonic, passphrase):
        """Tests an import where the user specifies a passphrase."""
        stdin = mnemonic + "\n" + passphrase + "\n"
        prms = ["import", "keys", "from", "mnemonic", "key", "--force"]
        client.run_generic(prms, stdin=stdin)
        addr = client.get_known_addresses()
        assert addr.wallet["key"] == "tz1QSF4TSVzaosgbaxnFJpRbs7798Skeb8Re"

    def test_encrypted(self, client: Client, mnemonic, passphrase):
        """Tests an import where the user wants to encrypt the key."""
        encrypt_pwd = "imgonnaencryptthiskeysohard"
        stdin = (
            mnemonic
            + "\n"
            + passphrase
            + "\n"
            + encrypt_pwd
            + "\n"
            + encrypt_pwd
            + "\n"
        )
        prms = [
            "import",
            "keys",
            "from",
            "mnemonic",
            "cryptkey",
            "--encrypt",
            "--force",
        ]
        client.run_generic(prms, stdin=stdin)
        addr = client.get_known_addresses()
        pkh = addr.wallet["cryptkey"]
        secret_key = client.show_address(
            "cryptkey", show_secret=True
        ).secret_key
        assert secret_key is not None
        assert secret_key.startswith("encrypted:")
        assert pkh == "tz1QSF4TSVzaosgbaxnFJpRbs7798Skeb8Re"

    def test_gen_key_from_menmonic_bad_mnemonic(self, client: Client):
        """Tests that the command fails if the user gives a bad mnemonic."""
        prms = ["import", "keys", "from", "mnemonic", "alias", "--force"]
        stdin = "hello\n\n"
        expected_error = '"hello" is not a valid BIP39 mnemonic.'
        with assert_run_failure(expected_error):
            client.run_generic(prms, stdin=stdin)


@pytest.mark.usefixtures("encrypted_account_with_tez")
class TestStopLoopPassword:
    """Tests that the client stops asking for the password after
    three erroneous passwords given by the user"""

    @pytest.mark.parametrize('stdin', ["\n\n\n", "\n\n\nign", "\n\n\npassword"])
    def test_stops_after_three_tries(self, client: Client, stdin):
        with assert_run_failure("3 incorrect password attempt"):
            client.transfer(0.1, 'encrypted_account', 'bootstrap1', stdin=stdin)

    @pytest.mark.parametrize(
        'stdin', ["password\n", "\npassword\n", "\n\npassword\n"]
    )
    def test_password_succeed_before_three_tries(self, client: Client, stdin):
        client.transfer(0.1, 'encrypted_account', 'bootstrap1', stdin=stdin)


@pytest.mark.client
class TestChainId:
    def test_chain_id_block_hash(self, nodeless_client: Client):
        block_hash = 'BKyFui5WPY1n3e9aKF3qd2kGBKBtHu3rtm5miYFnUagJC1BdHTF'
        prms = ['compute', 'chain', 'id', 'from', 'block', 'hash', block_hash]
        assert nodeless_client.run(prms).strip() == 'NetXuwrXPL4VeX5'

    def test_chain_id_seed(self, nodeless_client: Client):
        seed = 'choucroute'
        prms = ['compute', 'chain', 'id', 'from', 'seed', seed]
        assert nodeless_client.run(prms).strip() == 'NetXLGmPi3c5DXf'


def _write_config_file(
    client: Client, filename: str, config_dict: Optional[dict]
):
    """Writes `config_dict` to `filename`. Returns the json effectively
    written"""
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
    """Prefixes `cmd` with ["--config-file", config_file] if
    config_file is not None"""
    return ([_CONFIG_FILE_FLAG, config_file] if config_file else []) + cmd


def _gen_assert_msg(flag, sent, received):
    result = f"Json sent with --{flag} differs from"
    result += " json received"
    result += f"\nJson sent is:\n{sent}"
    result += f"\nwhile json received is:\n{received}"


@pytest.mark.client
@pytest.mark.parametrize('config_dict', _INPUT_CONFIG_FILES)
class TestConfigInit:
    def test_config_init(
        self, nodeless_client: Client, config_dict: Optional[dict]
    ):
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
                _write_config_file(nodeless_client, in_file, config_dict)

            cmd = _with_config_file_cmd(
                in_file, ["config", "init", "-o", out_file]
            )
            nodeless_client.run(cmd)
            # Try loading the file as json, to check it is valid
            with open(out_file) as handle:
                json.load(handle)
        finally:
            if in_file is not None:
                os.remove(in_file)
            os.remove(out_file)

    def test_config_init_roundtrip(
        self, nodeless_client: Client, config_dict: Optional[dict]
    ):
        """Tests that calling `config init -o tmp_file` and
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
                nodeless_client.run(cmd)
                with open(tmp_file1) as handle:
                    json1 = json.load(handle)

                # Execute an arbitrary effectless command:
                list_protos = ["list", "understood", "protocols"]
                # This checks that
                # `--config-file tmp_file1 arbitrary command` works
                cmd = _with_config_file_cmd(tmp_file1, list_protos)
                nodeless_client.run(cmd)
            else:
                # Take initial json from config_dict

                # Write config_dict to a file
                tmp_file1 = tempfile.mktemp(prefix='tezos-client.config_file')
                json1 = _write_config_file(
                    nodeless_client, tmp_file1, config_dict
                )

            # Execute `config init -o`
            tmp_file2 = tempfile.mktemp(prefix='tezos-client.config_file')
            cmd = _with_config_file_cmd(
                tmp_file1, ["config", "init", "-o", tmp_file2]
            )
            nodeless_client.run(cmd)

            # Load file generated by `config init -o`
            with open(tmp_file2) as handle:
                json2 = json.load(handle)

            # and finally check that the json generated by `config init -o`
            # matches the input data (either the default one or the one
            # specified with --config-file)
            assert json1 == json2, _gen_assert_msg(
                _CONFIG_FILE_FLAG, json1, json2
            )
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
    """Tests of `tezos-client config show`"""

    def test_config_show(
        self, nodeless_client: Client, config_dict: Optional[dict]
    ):
        """
        Tests that calling `config show` works, with or without
        specifying `--config-file`
        """
        try:
            tmp_file = None
            if config_dict is not None:
                tmp_file = tempfile.mktemp(prefix='tezos-client.config_file')
                _write_config_file(nodeless_client, tmp_file, config_dict)

            cmd = _with_config_file_cmd(tmp_file, ["config", "show"])
            nodeless_client.run(cmd)
        finally:
            if tmp_file is not None:
                os.remove(tmp_file)

    @pytest.mark.parametrize('cmd_line_args', [{}, _CMD_LINE_ARGS])
    def test_config_show_roundtrip(
        self,
        nodeless_client: Client,
        config_dict: Optional[dict],
        cmd_line_args: dict,
    ):
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
                _write_config_file(nodeless_client, in_file1, config_dict)

            cmd = []
            # Pass command line parameters
            for (flag, value) in cmd_line_args.items():
                cmd += [flag, value]
            cmd += ["config", "show"]
            cmd = _with_config_file_cmd(in_file1, cmd)
            # Take standard output
            (stdout, _, _) = nodeless_client.run_generic(cmd)

            output_json1 = json.loads(stdout)
            # Verify that command line parameters were honored
            for (flag, value) in cmd_line_args.items():
                input_value = cmd_line_args[flag]
                assert isinstance(input_value, str)
                output_value = output_json1[_cmd_line_flag_to_json_field(flag)]
                output_value = str(output_value)
                err_msg = (
                    f"Value of command line flag {flag} is not honored:"
                    f" passed {input_value} but"
                    f" config show yielded {output_value}"
                )
                assert output_value == input_value, err_msg
            in_file2 = tempfile.mktemp(prefix='tezos-client.config_file')
            # Write output of first call to `config show` to disk,
            # to pass it to second call below
            with open(in_file2, 'w') as handle:
                handle.write(json.dumps(output_json1))

            # Use previous ouput file as input now
            cmd = _with_config_file_cmd(in_file2, ["config", "show"])
            (stdout, _, _) = nodeless_client.run_generic(cmd)

            output_json2 = json.loads(stdout)

            # And finally check that the final output matches the input
            assert output_json1 == output_json2, _gen_assert_msg(
                _CONFIG_FILE_FLAG, output_json1, output_json2
            )
        finally:
            for in_file in [in_file1, in_file2]:
                if in_file is not None:
                    os.remove(in_file)


@pytest.mark.client
class TestConfigValid:
    """Tests of validity of tezos-client config"""

    def test_config_node_port(self, nodeless_client: Client):
        """
        Tests that calling `config show` works, with a valid node port
        """
        self._run_config_show_with_node_port(nodeless_client, 8732)
        self._run_config_show_with_node_port(nodeless_client, 58732)
        pytest.raises(
            subprocess.CalledProcessError,
            self._run_config_show_with_node_port,
            nodeless_client,
            158732,
        )
        pytest.raises(
            subprocess.CalledProcessError,
            self._run_config_show_with_node_port,
            nodeless_client,
            -8732,
        )

    def test_config_web_port(self, nodeless_client: Client):
        """
        Tests that calling `config show` works, with a valid node port
        """
        self._run_config_show_with_web_port(nodeless_client, 8732)
        self._run_config_show_with_web_port(nodeless_client, 58732)
        pytest.raises(
            subprocess.CalledProcessError,
            self._run_config_show_with_web_port,
            nodeless_client,
            158732,
        )
        pytest.raises(
            subprocess.CalledProcessError,
            self._run_config_show_with_web_port,
            nodeless_client,
            -8732,
        )

    def _run_config_show_with_temp_config_file(
        self, nodeless_client: Client, config_dict: dict
    ):
        try:
            tmp_file = tempfile.mktemp(prefix='tezos-client.config_file')
            _write_config_file(nodeless_client, tmp_file, config_dict)

            cmd = _with_config_file_cmd(tmp_file, ["config", "show"])
            nodeless_client.run(cmd)
        finally:
            if tmp_file is not None:
                os.remove(tmp_file)

    def _run_config_show_with_node_port(
        self, nodeless_client: Client, port: int
    ):
        config_dict = {"node_port": port}
        self._run_config_show_with_temp_config_file(
            nodeless_client, config_dict
        )

    def _run_config_show_with_web_port(
        self, nodeless_client: Client, port: int
    ):
        config_dict = {"web_port": port}
        self._run_config_show_with_temp_config_file(
            nodeless_client, config_dict
        )
