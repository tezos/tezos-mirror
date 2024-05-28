from pytezos.client import PyTezosClient
from pytezos.sandbox.node import SandboxedNodeTestCase
from tests.helpers.contracts import (
    KernelGovernance,
)
from typing import Optional
from pytezos.rpc import RpcError
from contextlib import contextmanager
from tests.helpers.contracts.internal_test_proxy import InternalTestProxy
from tests.helpers.contracts.rollup_mock import RollupMock
from tests.helpers.contracts.sequencer_governance import SequencerGovernance
from tests.helpers.utility import pkh
from pytezos.contract.result import ContractCallResult
from pytezos.operation.group import OperationGroup

class BaseTestCase(SandboxedNodeTestCase):
    accounts: list = []
    
    def setUp(self) -> None:
        self.accounts = []
        self.manager = self.bootstrap_baker()

    def get_current_level(self) -> int:
        return self.client.shell.head.header()['level']

    def bootstrap_baker(self, n: Optional[int] = None) -> PyTezosClient:
        """Creates baker with given number"""

        accounts_count = n or len(self.accounts)
        bootstrap: PyTezosClient = self.client.using(key=f'bootstrap{accounts_count + 1}')
        bootstrap.reveal()
        self.accounts.append(bootstrap)
        return bootstrap
    
    def bootstrap_no_baker(self) -> PyTezosClient:
        """Creates no baker account"""

        no_baker = self.client.using(key='alice')
        if no_baker.balance() == 0:
            donor = self.manager
            donor.transaction(destination=pkh(no_baker), amount=100000000000).autofill().sign().inject()
            self.bake_block()
            no_baker.reveal().autofill().sign().inject()
        return no_baker

    def deploy_rollup_mock(self) -> RollupMock:
        """Deploys Rollup Mock contract"""

        opg = RollupMock.originate(self.manager).send()
        self.bake_block()
        return RollupMock.from_opg(self.manager, opg)

    def deploy_internal_test_proxy(self) -> InternalTestProxy:
        """Deploys Internal Test Proxy contract"""

        opg = InternalTestProxy.originate(self.manager).send()
        self.bake_block()
        return InternalTestProxy.from_opg(self.manager, opg)

    def deploy_kernel_governance(self, custom_config=None) -> KernelGovernance:
        """Deploys Kernel Governance contract"""

        opg = KernelGovernance.originate(self.manager, custom_config).send()
        self.bake_block()
        return KernelGovernance.from_opg(self.manager, opg)

    def deploy_sequencer_governance(self, custom_config=None) -> SequencerGovernance:
        """Deploys Committee Governance contract"""

        opg = SequencerGovernance.originate(self.manager, custom_config).send()
        self.bake_block()
        return SequencerGovernance.from_opg(self.manager, opg)

    @contextmanager
    def raisesMichelsonError(self, error_message):
        """Asserts that instruction fails in smart contract with the specified error"""
        with self.assertRaises(RpcError) as r:
            yield r

        failed_with = self.extract_runtime_failwith(r.exception)
        self.assertEqual(error_message, failed_with)

    def extract_runtime_failwith(self, e: RpcError):
        return e.args[-1]['with']['string']

    def bake_block_and_get_operation_result(self, opg: OperationGroup) -> ContractCallResult:
        self.bake_block()
        opg = self.client.shell.blocks['head':].find_operation(opg.hash())
        return ContractCallResult.from_operation_group(opg)[0]
    
    def bake_blocks(self, count: int):
        for _ in range(count):
            self.bake_block()