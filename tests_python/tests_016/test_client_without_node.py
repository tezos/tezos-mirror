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
