import json
from pytezos.operation.result import OperationResult

class OperationResultRecorder:
    def __init__(self):
        self.data = {}

    def add_element(self, key, op):
        value = {
            'consumed_gas': OperationResult.consumed_gas(op),
            'paid_storage_size_diff': OperationResult.paid_storage_size_diff(op),
        }
        self.data[key] = value

    def write_to_file(self, filename):
        with open(filename, 'w') as f:
            json.dump(self.data, f, indent=4)
