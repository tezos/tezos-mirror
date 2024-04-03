from pytezos.contract.interface import ContractInterface
from pytezos.client import PyTezosClient
from pytezos.operation.group import OperationGroup
from dataclasses import dataclass, replace
from tests.helpers.utility import (
    load_contract_from_address,
    find_op_by_hash,
    get_address_from_op,
)
from typing import TypeVar, Type, Any
from abc import ABC


T = TypeVar('T', bound='ContractHelper')


@dataclass
class ContractHelper(ABC):
    contract: ContractInterface
    client: PyTezosClient
    address: str

    @classmethod
    def from_opg(
        cls: Type[T],
        client: PyTezosClient,
        opg: OperationGroup,
        **init_params: Any,
    ) -> T:
        """Creates ContractHelper from given operation group
        with given client"""

        op = find_op_by_hash(client, opg)
        address = get_address_from_op(op)
        print(f'found contract address: {address}')

        return cls(
            contract=load_contract_from_address(client, address),
            client=client,
            address=address,
            **init_params,
        )

    def using(self: T, client: PyTezosClient) -> T:
        """Returns new ContractHelper with updated client"""

        return replace(
            self,
            client=client,
            contract=client.contract(self.contract.address),
        )

    @classmethod
    def from_address(
        cls: Type[T],
        client: PyTezosClient,
        address: str,
        **init_params: Any,
    ) -> T:
        """Loads contract from given address using given client"""

        return cls(
            contract=client.contract(address),
            client=client,
            address=address,
            **init_params,
        )
