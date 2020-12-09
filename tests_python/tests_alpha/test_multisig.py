import pytest
from tools import utils
from client.client import Client


@pytest.mark.incremental
class TestMultisig:
    def test_gen_keys(self, client: Client, session: dict):
        session['keys'] = ['foo', 'bar', 'boo']
        sigs = [None, 'secp256k1', 'ed25519']
        for key, sig in zip(session['keys'], sigs):
            args = [] if sig is None else ['--sig', sig]
            client.gen_key(key, args)

    def test_deploy_multisig(self, client: Client, session: dict):
        keys = session['keys']
        client.deploy_msig(
            'msig', 100, 'bootstrap1', 2, keys, ['--burn-cap', '100']
        )
        utils.bake(client)

    def test_transfer(self, client: Client, session: dict):
        key = session['keys'][0]
        session['sig0'] = client.msig_sign_transfer(
            'msig', 10, 'bootstrap2', key
        )

    def test_prepare_msig_transfer(self, client: Client):
        client.msig_prepare_transfer('msig', 10, 'bootstrap2')

    def test_prepare_msig_sign(self, client: Client, session: dict):
        to_sign = client.msig_prepare_transfer(
            'msig', 10, 'bootstrap2', ['--bytes-only']
        )
        session['sig1'] = client.sign_bytes(to_sign, session['keys'][1])
        session['sig2'] = client.sign_bytes(to_sign, session['keys'][2])

    def test_transfer_failure(self, client: Client, session: dict):
        error_pattern = (
            r"Not enough signatures: "
            + r"only 1 signatures were given "
            + r"but the threshold is currently 2"
        )

        with utils.assert_run_failure(error_pattern):
            client.msig_transfer(
                'msig', 10, 'bootstrap2', 'bootstrap1', [session['sig2']]
            )

    def test_transfer_success(self, client: Client, session: dict):
        client.msig_transfer(
            'msig',
            10,
            'bootstrap2',
            'bootstrap1',
            [session['sig0'], session['sig2']],
        )
        utils.bake(client)

    def test_delegate_change(self, client: Client, session: dict):
        sig0 = client.msig_sign_set_delegate(
            'msig', 'bootstrap5', session['keys'][0]
        )
        to_sign = client.msig_prepare_set_delegate(
            'msig', 'bootstrap5', ['--bytes-only']
        )
        sig2 = client.sign_bytes(to_sign, session['keys'][2])
        client.msig_set_delegate(
            'msig', 'bootstrap5', 'bootstrap1', [sig0, sig2]
        )
        utils.bake(client)

    def test_delegate_withdraw(self, client: Client, session: dict):
        sig0 = client.msig_sign_withdrawing_delegate('msig', session['keys'][0])
        to_sign = client.msig_prepare_withdrawing_delegate(
            'msig', ['--bytes-only']
        )

        sig1 = client.sign_bytes(to_sign, session['keys'][1])
        client.msig_withdrawing_delegate('msig', 'bootstrap1', [sig0, sig1])
        utils.bake(client)

    def test_run_transaction_change_keys_and_threshold(
        self, client: Client, session: dict
    ):
        # test changing the keys and threshold with `run transaction` command
        keys = session['keys']
        sig0 = client.msig_sign_setting_threshold(
            'msig', keys[0], 2, [keys[0], keys[2]]
        )
        to_sign = client.msig_prepare_setting_threshold(
            'msig', 2, [keys[0], keys[2]], ['--bytes-only']
        )
        sig2 = client.sign_bytes(to_sign, session['keys'][2])
        client.msig_run_transaction('msig', to_sign, 'bootstrap1', [sig0, sig2])
        utils.bake(client)

    def test_change_keys_and_threshold(self, client: Client, session: dict):
        # test changing the keys and threshold with `set threshold of multisig`
        keys = session['keys']
        new_keys = [keys[0], keys[2]]
        sig0 = client.msig_sign_setting_threshold('msig', keys[0], 2, new_keys)
        to_sign = client.msig_prepare_setting_threshold(
            'msig', 2, new_keys, ['--bytes-only']
        )
        sig2 = client.sign_bytes(to_sign, session['keys'][2])
        client.msig_set_threshold(
            'msig', 2, new_keys, 'bootstrap1', [sig0, sig2]
        )
        utils.bake(client)


@pytest.mark.incremental
class TestMultisigFromAddress:
    def test_gen_keys(self, client: Client, session: dict):
        session['keys'] = ['foo', 'bar', 'boo']
        sigs = [None, 'secp256k1', 'ed25519']
        for key, sig in zip(session['keys'], sigs):
            args = [] if sig is None else ['--sig', sig]
            client.gen_key(key, args)

    def test_deploy_multisig(self, client: Client, session: dict):
        keys = session['keys']
        deployment = client.deploy_msig(
            'msig2', 100, 'bootstrap1', 2, keys, ['--burn-cap', '100']
        )
        session['msig'] = deployment.contract
        utils.bake(client)

    def test_transfer(self, client: Client, session: dict):
        key = session['keys'][0]
        msig = session['msig']
        session['sig0'] = client.msig_sign_transfer(msig, 10, 'bootstrap2', key)

    def test_prepare_msig_transfer(self, client: Client, session: dict):
        msig = session['msig']
        client.msig_prepare_transfer(msig, 10, 'bootstrap2')

    def test_prepare_msig_sign(self, client: Client, session: dict):
        msig = session['msig']
        to_sign = client.msig_prepare_transfer(
            msig, 10, 'bootstrap2', ['--bytes-only']
        )
        session['sig1'] = client.sign_bytes(to_sign, session['keys'][1])
        session['sig2'] = client.sign_bytes(to_sign, session['keys'][2])

    def test_transfer_failure(self, client: Client, session: dict):
        msig = session['msig']

        def cmd():
            client.msig_transfer(
                msig, 10, 'bootstrap2', 'bootstrap1', [session['sig2']]
            )

        error_pattern = (
            r"Not enough signatures: "
            + r"only 1 signatures were given "
            + r"but the threshold is currently 2"
        )

        with utils.assert_run_failure(error_pattern):
            cmd()

    def test_transfer_success(self, client: Client, session: dict):
        msig = session['msig']
        client.msig_transfer(
            msig,
            10,
            'bootstrap2',
            'bootstrap1',
            [session['sig0'], session['sig2']],
        )
        utils.bake(client)

    def test_delegate_change(self, client: Client, session: dict):
        msig = session['msig']
        sig0 = client.msig_sign_set_delegate(
            msig, 'bootstrap5', session['keys'][0]
        )
        to_sign = client.msig_prepare_set_delegate(
            msig, 'bootstrap5', ['--bytes-only']
        )
        sig2 = client.sign_bytes(to_sign, session['keys'][2])
        client.msig_set_delegate(msig, 'bootstrap5', 'bootstrap1', [sig0, sig2])
        utils.bake(client)

    def test_delegate_withdraw(self, client: Client, session: dict):
        msig = session['msig']
        sig0 = client.msig_sign_withdrawing_delegate(msig, session['keys'][0])
        to_sign = client.msig_prepare_withdrawing_delegate(
            msig, ['--bytes-only']
        )

        sig1 = client.sign_bytes(to_sign, session['keys'][1])
        client.msig_withdrawing_delegate(msig, 'bootstrap1', [sig0, sig1])
        utils.bake(client)

    def test_run_transaction_change_keys_and_threshold(
        self, client: Client, session: dict
    ):
        # test changing the keys and threshold with `run transaction` command
        msig = session['msig']
        keys = session['keys']
        sig0 = client.msig_sign_setting_threshold(
            msig, keys[0], 2, [keys[0], keys[2]]
        )
        to_sign = client.msig_prepare_setting_threshold(
            msig, 2, [keys[0], keys[2]], ['--bytes-only']
        )
        sig2 = client.sign_bytes(to_sign, session['keys'][2])
        client.msig_run_transaction(msig, to_sign, 'bootstrap1', [sig0, sig2])
        utils.bake(client)

    def test_change_keys_and_threshold(self, client: Client, session: dict):
        # test changing the keys and threshold with `set threshold of multisig`
        msig = session['msig']
        keys = session['keys']
        new_keys = [keys[0], keys[2]]
        sig0 = client.msig_sign_setting_threshold(msig, keys[0], 2, new_keys)
        to_sign = client.msig_prepare_setting_threshold(
            msig, 2, new_keys, ['--bytes-only']
        )
        sig2 = client.sign_bytes(to_sign, session['keys'][2])
        client.msig_set_threshold(msig, 2, new_keys, 'bootstrap1', [sig0, sig2])
        utils.bake(client)
