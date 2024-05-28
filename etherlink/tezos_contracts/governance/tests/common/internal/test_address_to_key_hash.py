from tests.base import BaseTestCase
from tests.helpers.errors import NOT_IMPLICIT_ADDRESS

class PayloadDecorationTestCase(BaseTestCase):
    def test_address_to_key_hash(self) -> None:
        proxy = self.deploy_internal_test_proxy()

        assert proxy.address_to_key_hash('tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx') == 'tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx'
        assert proxy.address_to_key_hash('tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc') == 'tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc'
        assert proxy.address_to_key_hash('tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA') == 'tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA'
        assert proxy.address_to_key_hash('tz4Jxn8MpRndqWUzkuZbQKmE3aNWJzYsSEso') == 'tz4Jxn8MpRndqWUzkuZbQKmE3aNWJzYsSEso'

        with self.raisesMichelsonError(NOT_IMPLICIT_ADDRESS):
            proxy.address_to_key_hash('KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq')

        with self.raisesMichelsonError(NOT_IMPLICIT_ADDRESS):
            proxy.address_to_key_hash('sr1EStimadnRRA3vnjpWV1RwNAsDbM3JaDt6')