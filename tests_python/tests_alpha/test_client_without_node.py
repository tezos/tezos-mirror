"""Node-less tests for the client.

Some client tests do not require a node running, nor a
persistent mockup environment. These can be placed here.
"""
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
