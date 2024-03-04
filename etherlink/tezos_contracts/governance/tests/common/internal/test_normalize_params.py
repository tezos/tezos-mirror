from tests.base import BaseTestCase
from tests.helpers.utility import normalize_params

class NormalizeParamsTestCase(BaseTestCase):
    def test_normalize_params(self) -> None:
        assert normalize_params([100, 1, 2, 3]) == [100, 1, 2, 3]
        assert normalize_params([100, 1.0, 2.00, 3.000]) == [100, 1, 2, 3]
        assert normalize_params([100, 1.0, 2, 3]) == [100, 1, 2, 3]
        assert normalize_params([100, 0.1, 2, 3]) == [1000, 1, 20, 30]
        assert normalize_params([100, 5.1, 25, 33.123]) == [100000, 5100, 25000, 33123]
        assert normalize_params([100, 1, 0.02, 3]) == [10000, 100, 2, 300]
        assert normalize_params([100, 1, 2, 0.003]) == [100000, 1000, 2000, 3]
        assert normalize_params([100, 0.1, 0.02, 0.003]) == [100000, 100, 20, 3]
