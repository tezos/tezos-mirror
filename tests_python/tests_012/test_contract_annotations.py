import pytest
from tools.utils import assert_typecheck_data_failure, assert_typecheck_failure
from client.client import Client


@pytest.mark.slow
@pytest.mark.contract
class TestAnnotations:
    """Tests of Michelson annotations."""

    def test_annotation_length_success(self, client: Client):
        client.typecheck_data('3', f"(int :{'a' * 254})")

    def test_annotation_length_failure(self, client: Client):
        assert_typecheck_data_failure(
            client,
            '3',
            f"(int :{'a' * 255})",
            r'annotation exceeded maximum length \(255 chars\)',
        )

    def test_field_annotation_in_type_alphabetic(self, client):
        client.typecheck_data('Pair 0 0', 'pair (nat %x) (int %y)')

    def test_field_annotation_in_type_numeral(self, client):
        client.typecheck_data('Pair 0 0', 'pair (nat %1) (int %2)')

    def test_field_annotation_in_type_invalid_character(self, client):
        assert_typecheck_data_failure(
            client,
            'Pair 0 0',
            'pair (nat %.) (int %.)',
            'unexpected annotation',
        )

    def test_field_annotation_in_instruction_alphabetic(self, client):
        client.typecheck_data(
            '{ CAR %x }', 'lambda (pair (nat %x) (int %y)) nat'
        )

    def test_field_annotation_in_instruction_numeral(self, client):
        client.typecheck_data(
            '{ CAR %1 }', 'lambda (pair (nat %1) (int %2)) nat'
        )

    def test_field_annotation_in_instruction_invalid_character(self, client):
        assert_typecheck_data_failure(
            client,
            '{ CAR %. }',
            'lambda (pair (nat %.) (int %.)) nat',
            'unexpected annotation',
        )

    def test_field_annotation_in_root_alphabetic(self, client):
        client.typecheck(
            'parameter %r unit; storage unit; code {FAILWITH}', file=False
        )

    def test_field_annotation_in_root_numeral(self, client):
        client.typecheck(
            'parameter %1 unit; storage unit; code {FAILWITH}', file=False
        )

    def test_field_annotation_in_root_invalid_character(self, client):
        assert_typecheck_failure(
            client,
            'parameter %. unit; storage unit; code {FAILWITH}',
            'unexpected annotation',
            file=False,
        )

    def test_field_annotation_in_root_type_alphabetic(self, client):
        client.typecheck(
            'parameter (unit %r); storage unit; code {FAILWITH}', file=False
        )

    def test_field_annotation_in_root_type_numeral(self, client):
        client.typecheck(
            'parameter (unit %1); storage unit; code {FAILWITH}', file=False
        )

    def test_field_annotation_in_root_type_invalid_character(self, client):
        assert_typecheck_failure(
            client,
            'parameter (unit %.); storage unit; code {FAILWITH}',
            'unexpected annotation',
            file=False,
        )
