from tests.base import BaseTestCase

class PayloadDecorationTestCase(BaseTestCase):
    def test_kernel_upgrade_payload_decoration(self) -> None:
        proxy = self.deploy_internal_test_proxy()

        kernel_root_hash = bytes.fromhex('009279df4982e47cf101e2525b605fa06cd3ccc0f67d1c792a6a3ea56af9606abc')
        activation_timestamp = 1708444800
        
        expected_upgrade_payload = 'eba1009279df4982e47cf101e2525b605fa06cd3ccc0f67d1c792a6a3ea56af9606abc8880ccd46500000000'
        assert proxy.get_kernel_upgrade_payload(kernel_root_hash, activation_timestamp).hex() == expected_upgrade_payload

    def test_sequencer_upgrade_payload_decoration(self) -> None:
        proxy = self.deploy_internal_test_proxy()

        sequencer_pk = 'edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav'
        pool_address = '71c7656ec7ab88b098defb751b7401b5f6d8976f'
        activation_timestamp = 1709355827
        expected_upgrade_payload = 'f855b66564706b75426b6e5732386e5737324b4736526f48745957377031325436474b63376e4162775958356d3857643973445643397961769471c7656ec7ab88b098defb751b7401b5f6d8976f8833b3e26500000000'
        assert proxy.get_sequencer_upgrade_payload(sequencer_pk, pool_address, activation_timestamp).hex() == expected_upgrade_payload
    
        sequencer_pk = 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X'
        pool_address = 'B7A97043983f24991398E5a82f63F4C58a417185'
        activation_timestamp = 1709089500
        expected_upgrade_payload = 'f855b66564706b7572636761665a3255527942367a736d35643159716d4c743972314c6b38394a38314e364b70794d61557a5857457376315894b7a97043983f24991398e5a82f63f4c58a41718588dca2de6500000000'
        assert proxy.get_sequencer_upgrade_payload(sequencer_pk, pool_address, activation_timestamp).hex() == expected_upgrade_payload
    
        sequencer_pk = 'sppk7ZZ62L7f1dep7ddYNhM2vsNuzouGzCrv9CRsxmJ6q4Cavys2fsk'
        pool_address = 'B7A97043983f24991398E5a82f63F4C58a417185'
        activation_timestamp = 1709175900
        expected_upgrade_payload = 'f856b77370706b375a5a36324c376631646570376464594e684d3276734e757a6f75477a43727639435273786d4a36713443617679733266736b94b7a97043983f24991398e5a82f63f4c58a417185885cf4df6500000000'
        assert proxy.get_sequencer_upgrade_payload(sequencer_pk, pool_address, activation_timestamp).hex() == expected_upgrade_payload
    
        sequencer_pk = 'p2pk65aQziqHmuSpoEtEfaMRL4MwBMrSYQYoF3BCebocBKkg1shLepb'
        pool_address = 'B7A97043983f24991398E5a82f63F4C58a417185'
        activation_timestamp = 1909175900
        expected_upgrade_payload = 'f856b77032706b363561517a6971486d7553706f45744566614d524c344d77424d72535951596f4633424365626f63424b6b673173684c65706294b7a97043983f24991398e5a82f63f4c58a417185885cb6cb7100000000'
        assert proxy.get_sequencer_upgrade_payload(sequencer_pk, pool_address, activation_timestamp).hex() == expected_upgrade_payload
    