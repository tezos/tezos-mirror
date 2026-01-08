# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

from tezos import Entrypoint, Error


def test_build_entrypoint():
    raw_entrypoint = "set_delegate_parameters"
    entrypoint = Entrypoint(raw_entrypoint)
    assert str(entrypoint) == raw_entrypoint


def test_build_wrong_formatted_entrypoint():
    try:
        Entrypoint('a' * 32)
        raise AssertionError("Converting wrong formatted entrypoint must fail")
    except Error.Entrypoint as e:
        expected_error = Error.Entrypoint('Entrypoint failure: Format(WrongFormat("entrypoint name must be at most 31 characters long, but it is 32 characters long"))')
        assert str(e) == str(expected_error)
