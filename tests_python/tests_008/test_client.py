import pytest
from client.client import Client
from tools.utils import assert_run_failure, bake


@pytest.mark.client
@pytest.mark.incremental
class TestSimulation:
    """ Tests the behavior of the --simulation flag. """

    @pytest.mark.usefixtures("encrypted_account")
    def test_transfer_simulation(self, client: Client):
        """ Tests that --simulation does not ask for the key password. """
        client.transfer(
            1, "bootstrap1", "encrypted_account", ["--burn-cap", "1.0"]
        )
        bake(client, bake_for="bootstrap1")
        client.transfer(
            0.1, "encrypted_account", "bootstrap1", ["--simulation"]
        )

    @pytest.mark.usefixtures("encrypted_account")
    def test_transfer_without_simulation(self, client: Client):
        """ Tests that the client asks for the password w/o --simulation. """
        with assert_run_failure("End_of_file", mode="stdout"):
            client.transfer(0.1, "encrypted_account", "bootstrap1")

    @pytest.mark.usefixtures("encrypted_account")
    def test_delegate_simulation(self, client: Client):
        """ Tests that --simulation does not ask for the key password. """
        client.gen_key("delegate")
        client.transfer(100, "bootstrap1", "delegate", ["--burn-cap", "1.0"])
        bake(client, bake_for="bootstrap1")
        client.transfer(
            100, "bootstrap1", "encrypted_account", ["--burn-cap", "1.0"]
        )
        bake(client, bake_for="bootstrap2")
        client.register_delegate("delegate")
        bake(client, bake_for="bootstrap1")
        client.set_delegate("encrypted_account", "delegate", ["--simulation"])

    @pytest.mark.usefixtures("encrypted_account")
    def test_delegate_without_simulation(self, client: Client):
        """ Tests that the client asks for the password w/o --simulation. """
        with assert_run_failure("End_of_file", mode="stdout"):
            client.set_delegate("encrypted_account", "delegate")
