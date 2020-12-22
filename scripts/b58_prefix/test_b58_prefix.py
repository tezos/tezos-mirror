import pytest
import b58_prefix

@pytest.mark.parametrize("prefix,length,expected_result", [
    ("edpk", 32, (54, [13, 15, 37, 191])),
    ("tz2", 20, (36, [6, 161, 161])),
    ("tz1", 20, (36, [6, 161, 159])),
])
def test_compute_version_bytes(prefix, length, expected_result):
    assert b58_prefix.compute_version_bytes(prefix, length) == expected_result

@pytest.mark.parametrize("word,expected_output", [
    ("1", 0),
    ("tz1", 174870),
])
def test_b58dec(word, expected_output):
    assert b58_prefix.b58dec(word) == expected_output

@pytest.mark.parametrize("val,expected_output", [
    (434591, [6, 161, 159]),
    (797373, [12, 42, 189]),
])
def test_asciidec(val, expected_output):
    assert b58_prefix.asciidec(val) == expected_output

