import pytest
from tools import utils

BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']


@pytest.mark.incremental
class TestMultisig:

    def test_gen_keys(self, client, session):
        session['keys'] = ['foo', 'bar', 'boo']
        sig = [None, 'secp256k1', 'ed25519']
        for key, sig in zip(session['keys'], sig):
            args = [] if sig is None else ['--sig', sig]
            client.gen_key(key, args)

    def test_deploy_multisig(self, client, session):
        keys = session['keys']
        client.deploy_msig('msig',
                           100,
                           'bootstrap1', 2, keys,
                           ['--burn-cap', '100'])
        client.bake('bootstrap1', BAKE_ARGS)

    def test_transfer(self, client, session):
        key = session['keys'][0]
        session['sig0'] = client.msig_sign_transfer('msig', 10,
                                                    'bootstrap2', key)

    def test_prepare_msig_transfer(self, client):
        client.msig_prepare_transfer('msig', 10, 'bootstrap2')

    def test_prepare_msig_sign(self, client, session):
        to_sign = client.msig_prepare_transfer('msig', 10,
                                               'bootstrap2',
                                               ['--bytes-only'])
        sign = client.sign_bytes(to_sign, session['keys'][1])
        session['sig1'] = sign.signature
        sign = client.sign_bytes(to_sign, session['keys'][2])
        session['sig2'] = sign.signature

    def test_transfer_failure(self, client, session):
        def cmd():
            client.msig_transfer('msig', 10, 'bootstrap2',
                                 'bootstrap1',
                                 [session['sig2']])

        error_pattern = (r"Not enough signatures: " +
                         r"only 1 signatures were given " +
                         r"but the threshold is currently 2")

        utils.assert_run_failure(cmd, error_pattern)

    def test_transfer_success(self, client, session):
        client.msig_transfer('msig', 10, 'bootstrap2', 'bootstrap1',
                             [session['sig0'], session['sig2']])
        client.bake('bootstrap1', BAKE_ARGS)

    def test_delegate_change(self, client, session):
        sig0 = client.msig_sign_set_delegate('msig', 'bootstrap5',
                                             session['keys'][0])
        to_sign = client.msig_prepare_set_delegate('msig', 'bootstrap5',
                                                   ['--bytes-only'])
        sign = client.sign_bytes(to_sign, session['keys'][2])
        sig2 = sign.signature
        client.msig_set_delegate('msig', 'bootstrap5', 'bootstrap1',
                                 [sig0, sig2])
        client.bake('bootstrap1', BAKE_ARGS)

    def test_delegate_withdraw(self, client, session):
        sig0 = client.msig_sign_withdrawing_delegate('msig',
                                                     session['keys'][0])
        to_sign = client.msig_prepare_withdrawing_delegate('msig',
                                                           ['--bytes-only'])

        sign = client.sign_bytes(to_sign, session['keys'][1])
        sig1 = sign.signature
        client.msig_withdrawing_delegate('msig', 'bootstrap1',
                                         [sig0, sig1])
        client.bake('bootstrap1', BAKE_ARGS)

    def test_change_keys_and_threshold(self, client, session):
        keys = session['keys']
        sig0 = client.msig_sign_setting_threshold('msig', keys[0],
                                                  2, [keys[0], keys[2]])
        to_sign = client.msig_prepare_setting_threshold('msig', 2,
                                                        [keys[0], keys[2]],
                                                        ['--bytes-only'])
        sign = client.sign_bytes(to_sign, session['keys'][2])
        sig2 = sign.signature
        client.msig_run_transaction('msig', to_sign,
                                    'bootstrap1',
                                    [sig0, sig2])
        client.bake('bootstrap1', BAKE_ARGS)


@pytest.mark.incremental
class TestMultisigFromAddress:

    def test_gen_keys(self, client, session):
        session['keys'] = ['foo', 'bar', 'boo']
        sig = [None, 'secp256k1', 'ed25519']
        for key, sig in zip(session['keys'], sig):
            args = [] if sig is None else ['--sig', sig]
            client.gen_key(key, args)

    def test_deploy_multisig(self, client, session):
        keys = session['keys']
        deployment = client.deploy_msig('msig2',
                                        100,
                                        'bootstrap1', 2, keys,
                                        ['--burn-cap', '100'])
        session['msig'] = deployment.contract
        client.bake('bootstrap1', BAKE_ARGS)

    def test_transfer(self, client, session):
        key = session['keys'][0]
        msig = session['msig']
        session['sig0'] = client.msig_sign_transfer(msig, 10,
                                                    'bootstrap2', key)

    def test_prepare_msig_transfer(self, client, session):
        msig = session['msig']
        client.msig_prepare_transfer(msig, 10, 'bootstrap2')

    def test_prepare_msig_sign(self, client, session):
        msig = session['msig']
        to_sign = client.msig_prepare_transfer(msig, 10,
                                               'bootstrap2',
                                               ['--bytes-only'])
        sign = client.sign_bytes(to_sign, session['keys'][1])
        session['sig1'] = sign.signature
        sign = client.sign_bytes(to_sign, session['keys'][2])
        session['sig2'] = sign.signature

    def test_transfer_failure(self, client, session):
        msig = session['msig']

        def cmd():
            client.msig_transfer(msig, 10, 'bootstrap2',
                                 'bootstrap1',
                                 [session['sig2']])

        error_pattern = (r"Not enough signatures: " +
                         r"only 1 signatures were given " +
                         r"but the threshold is currently 2")

        utils.assert_run_failure(cmd, error_pattern)

    def test_transfer_success(self, client, session):
        msig = session['msig']
        client.msig_transfer(msig, 10, 'bootstrap2', 'bootstrap1',
                             [session['sig0'], session['sig2']])
        client.bake('bootstrap1', BAKE_ARGS)

    def test_delegate_change(self, client, session):
        msig = session['msig']
        sig0 = client.msig_sign_set_delegate(msig, 'bootstrap5',
                                             session['keys'][0])
        to_sign = client.msig_prepare_set_delegate(msig, 'bootstrap5',
                                                   ['--bytes-only'])
        sign = client.sign_bytes(to_sign, session['keys'][2])
        sig2 = sign.signature
        client.msig_set_delegate(msig, 'bootstrap5', 'bootstrap1',
                                 [sig0, sig2])
        client.bake('bootstrap1', BAKE_ARGS)

    def test_delegate_withdraw(self, client, session):
        msig = session['msig']
        sig0 = client.msig_sign_withdrawing_delegate(msig,
                                                     session['keys'][0])
        to_sign = client.msig_prepare_withdrawing_delegate(msig,
                                                           ['--bytes-only'])

        sign = client.sign_bytes(to_sign, session['keys'][1])
        sig1 = sign.signature
        client.msig_withdrawing_delegate(msig, 'bootstrap1',
                                         [sig0, sig1])
        client.bake('bootstrap1', BAKE_ARGS)

    def test_change_keys_and_threshold(self, client, session):
        msig = session['msig']
        keys = session['keys']
        sig0 = client.msig_sign_setting_threshold(msig, keys[0],
                                                  2, [keys[0], keys[2]])
        to_sign = client.msig_prepare_setting_threshold(msig, 2,
                                                        [keys[0], keys[2]],
                                                        ['--bytes-only'])
        sign = client.sign_bytes(to_sign, session['keys'][2])
        sig2 = sign.signature
        client.msig_run_transaction(msig, to_sign,
                                    'bootstrap1',
                                    [sig0, sig2])
        client.bake('bootstrap1', BAKE_ARGS)
