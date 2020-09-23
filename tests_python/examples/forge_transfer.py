"""This example show how to forge and inject a transaction in a node
using RPCs."""
import math
from tools import constants, paths, utils
from launchers.sandbox import Sandbox

SENDER = 'bootstrap1'
RECEIVER = 'bootstrap2'
SENDER_ID = constants.IDENTITIES['bootstrap1']['identity']
RECEIVER_ID = constants.IDENTITIES['bootstrap2']['identity']
SENDER_SK = constants.IDENTITIES['bootstrap1']['secret'][len('unencrypted:') :]


def scenario():
    with Sandbox(paths.TEZOS_HOME, constants.IDENTITIES) as sandbox:
        # Launch node running protocol alpha
        sandbox.add_node(0, params=constants.NODE_PARAMS)
        utils.activate_alpha(sandbox.client(0))
        port = sandbox.node(0).rpc_port

        chain_id = utils.rpc(
            'localhost', port, 'get', '/chains/main/chain_id'
        ).json()

        counter_path = (
            f'/chains/main/blocks/head/context/contracts/'
            f'{SENDER_ID}/counter'
        )
        counter = utils.rpc('localhost', port, 'get', counter_path).json()
        head_hash = utils.rpc(
            'localhost', port, 'get', '/chains/main/blocks/head/hash'
        ).json()

        operation_json = {
            "branch": head_hash,
            "contents": [
                {
                    "kind": "transaction",
                    "source": SENDER_ID,
                    "fee": "0",
                    "counter": str(int(counter) + 1),
                    "gas_limit": "1040000",
                    "storage_limit": "60000",
                    "amount": '1000',
                    "destination": RECEIVER_ID,
                }
            ],
        }

        run_json = {'operation': operation_json, 'chain_id': chain_id}
        # call run_operation
        dummy_sig = (
            'edsigtkpiSSschcaCt9pUVrpNPf7TTcgvgDEDD6NCEHMy8NNQJCGnMfL'
            'ZzYoQj74yLjo9wx6MPVV29CvVzgi7qEcEUok3k7AuMg'
        )
        operation_json['signature'] = dummy_sig
        run_operation_path = (
            '/chains/main/blocks/head/helpers/scripts/run_operation'
        )
        res = utils.rpc(
            'localhost', port, 'post', run_operation_path, data=run_json
        ).json()

        # update fields before forging
        gas_limit = res['contents'][0]['metadata']['operation_result'][
            'consumed_gas'
        ]
        milligas_limit = res['contents'][0]['metadata']['operation_result'][
            'consumed_milligas'
        ]
        gas_from_milligas = math.ceil(float(milligas_limit) / 1000)
        safer_gas_limit_through_milligas = int(gas_from_milligas) + 100
        safer_gas_limit = int(gas_limit) + 100
        assert safer_gas_limit == safer_gas_limit_through_milligas
        operation_json['contents'][0]['gas_limit'] = str(safer_gas_limit)
        operation_json['contents'][0]['storage_limit'] = '0'
        operation_json['contents'][0]['fee'] = '1300'  # arbitrary fee
        del operation_json['signature']

        # forge operation
        path_forge = '/chains/main/blocks/head/helpers/forge/operations'
        operation_hex_string = utils.rpc(
            'localhost', port, 'post', path_forge, data=operation_json
        ).json()

        # sign operation
        watermarked_operation = b'\x03' + bytes.fromhex(operation_hex_string)
        sender_sk_hex = utils.b58_key_to_hex(SENDER_SK)
        sender_sk_bin = bytes.fromhex(sender_sk_hex)
        sig_hex = utils.sign(watermarked_operation, sender_sk_bin)
        sig_b58 = utils.hex_sig_to_b58(sig_hex)

        # change operation structure as expected by preapply
        operation_json['signature'] = sig_b58  # preapply require b58 signature

        # Hard-coded but we could get the protocol hash using an RPC
        operation_json['protocol'] = constants.ALPHA
        operation_json_list = [operation_json]

        preapply_path = '/chains/main/blocks/head/helpers/preapply/operations'
        preapply_res = utils.rpc(
            'localhost', port, 'post', preapply_path, data=operation_json_list
        ).json()

        preapply_status = preapply_res[0]['contents'][0]['metadata'][
            'operation_result'
        ]['status']
        assert preapply_status == 'applied'

        # injection require hex signature
        signed_op = operation_hex_string + sig_hex
        op_hash = utils.rpc(
            'localhost', port, 'post', 'injection/operation', signed_op
        ).json()

        mempool = utils.rpc(
            'localhost', port, 'get', '/chains/main/mempool/pending_operations'
        ).json()
        assert op_hash == mempool['applied'][0]['hash']


if __name__ == "__main__":
    scenario()
