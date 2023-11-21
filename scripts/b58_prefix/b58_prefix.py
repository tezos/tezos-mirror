import base58
import fire

ALPHABET = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'


def b58dec(word):
    """
    Get the decimal representation of the base58 encoded parameter

    Examples:
    - b58dec("1") = 0 = 0 * 58 ** 0
    - b58dec("tz1") = 174870 = 51 * 58**2 + 57 * 58 ** 1 + 0 * 58**0
                             =     t            z             1
    """
    x = 0
    for c in word:
        x *= 58
        x += ALPHABET.find(c)
    return x


def asciidec(val):
    """
    Convert an integer to ascii, i.e. a list of integers in base 256.
    Examples:
    - asciidec(434591) = 6 * 256**2 + 161 * 256**1 + 159 * 256**0
    - asciidec(797373) = 12 * 256**2 + 42 * 256**1 + 189 * 256**0
    """
    word = []
    while val > 0:
        word.append(val % 256)
        val = val // 256
    return word[-1::-1]


def compute_version_bytes(prefix, length, maximal_base58_size=1000):
    """
    Compute the version bytes to add to the class of payloads of the given
    length to get a base58 encoding starting with the given prefix.
    Additionally, return the size of the base58 encoded values.
    """
    length = int(length)
    target = b58dec(prefix)

    # 4 is the checksum we add.
    shift = 8 * (length + 4)

    # We will increase a m which will be the length of the base58 encoding
    # (hence the exponentiation by 58) until we find that the m is high enough
    # to encode a payload of the given length (see 1 << shift). The idea is to
    # find an interval of decimal numbers [lo, hi].
    for m in range(1, maximal_base58_size):
        # The first bytes in base58 must be the given target. lo is the version
        # byte we want to compute.
        lo = target * 58**m
        lo = lo >> shift
        # (a >> b) << b == a iff a = 2**b. Then this line is equivalent to add 1
        # if lo is not a power of 2.
        lo += (0 if lo == ((lo >> shift) << shift) else 1)
        # hi will be negative because of the minus if m is not big enough to
        # encode a payload of [shift] bits.
        hi = (target + 1) * 58**m - (1 << shift) + 1
        # if b is negative (i.e. m not big enough to encode a payload of the
        # given length), b >> a is always strictly negative
        hi = hi >> shift
        if hi >= lo:
            # We test that any sequence of bytes of the given length starts with
            # the prefix and has the same length.
            to_encode = bytearray(asciidec(lo) + [0] * length)
            base58_encoded_minimal_value = base58.\
                b58encode_check(bytes(to_encode))
            assert base58_encoded_minimal_value.startswith(prefix.encode())
            assert len(base58_encoded_minimal_value) == m + len(prefix)

            to_encode = bytearray(asciidec(lo) + [255] * length)
            base58_encoded_maximal_value = base58.\
                b58encode_check(bytes(to_encode))
            assert base58_encoded_maximal_value.startswith(prefix.encode())
            assert len(base58_encoded_maximal_value) == m + len(prefix)
            break

    return m + len(prefix), asciidec(lo)


def main(prefix, length, maximal_base58_size=1000):
    base58_size, version_bytes = compute_version_bytes(prefix, length,
                                                       maximal_base58_size)
    print(f"Base58 size: {base58_size}. Version bytes: {version_bytes}")

if __name__ == "__main__":
    fire.Fire(main)
