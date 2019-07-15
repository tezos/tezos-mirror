"""This example show how to perform a simple RPC on an alphanet node"""

from tools import utils

# Replace this with your node address
SERVER = '127.0.0.1'
PORT = 8732


def main():
    resp = utils.rpc(SERVER, PORT, 'get', '/chains/main/blocks/head')
    utils.pprint(resp.json())


if __name__ == "__main__":
    main()
