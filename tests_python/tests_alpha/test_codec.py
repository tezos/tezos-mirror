from typing import Any
import pytest
from codec.codec import Codec
from tools import paths

CODEC_BIN = paths.TEZOS_HOME + "tezos-codec"

ENCODINGS = [
    (
        "network_version",
        {"p2p_version": 0, "distributed_db_version": 1, "chain_name": "main"},
    )
]


@pytest.mark.codec
class TestCodec:
    @pytest.mark.parametrize("encoding_name, data", ENCODINGS)
    def test_codec_encode_decode(self, encoding_name: str, data: Any):
        codec = Codec(CODEC_BIN)
        data_encoded = codec.encode(encoding_name, data_json=data)
        data_decoded = codec.decode(encoding_name, data=data_encoded)
        assert data_decoded == data
