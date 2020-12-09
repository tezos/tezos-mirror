import time
from tools import constants, paths
from tools.constants import PROTO_DEMO_NOOPS, PROTO_GENESIS
from launchers.sandbox import Sandbox


PROTO_DEMO = PROTO_DEMO_NOOPS
PARAMS = ['-p', PROTO_GENESIS]


def forge_block_header_data(protocol_data):
    """
    Returns a binary encoding for a dict of the form
    `{'block_header_data: string}`, as expected by the protocol.

    This corresponds to the encoding given by
    `data_encoding.(obj1 (req "block_header_data" string))`. See
    `lib_data_encoding/data_encoding.mli` for the spec.
    """
    assert len(protocol_data) == 1 and 'block_header_data' in protocol_data
    string = protocol_data['block_header_data']
    tag = '0000'
    padded_hex_len = f'{len(string):#06x}'[2:]
    return tag + padded_hex_len + bytes(string, 'utf-8').hex()


def main():
    with Sandbox(
        paths.TEZOS_HOME, constants.IDENTITIES, log_dir='tmp'
    ) as sandbox:
        # launch a sandbox node
        sandbox.add_node(0, params=constants.NODE_PARAMS)
        client = sandbox.client(0)

        protocols = client.list_protocols()
        assert PROTO_DEMO in protocols

        parameters = {}  # type: dict
        client.activate_protocol_json(
            PROTO_DEMO, parameters, key='activator', fitness='1'
        )

        head = client.rpc('get', '/chains/main/blocks/head/', params=PARAMS)
        # current protocol is still genesis and level == 1
        assert head['header']['level'] == 1
        assert head['protocol'] == PROTO_GENESIS

        time.sleep(1)

        # bake a block for new protocol, using fours RPCs:
        # - helpers/preapply/block builds the block
        # - helpers/forge_block_header encodes the whole block header
        # - /injection/block injects it
        message = "hello world"

        data = {
            "protocol_data": {
                "protocol": PROTO_DEMO,
                "block_header_data": message,
            },
            "operations": [],
        }
        block = client.rpc(
            'post',
            '/chains/main/blocks/head/helpers/preapply/block',
            data=data,
            params=PARAMS,
        )

        protocol_data = {'block_header_data': message}
        encoded = forge_block_header_data(protocol_data)

        shell_header = block['shell_header']
        shell_header['protocol_data'] = encoded
        encoded = client.rpc(
            'post',
            '/chains/main/blocks/head/helpers/forge_block_header',
            data=shell_header,
            params=PARAMS,
        )

        inject = {'data': encoded['block'], 'operations': []}
        client.rpc('post', '/injection/block', data=inject, params=PARAMS)

        head = client.rpc('get', '/chains/main/blocks/head/', params=PARAMS)
        assert head['header']['level'] == 2


if __name__ == "__main__":
    main()
