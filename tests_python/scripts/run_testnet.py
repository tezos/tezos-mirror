#!/usr/bin/env python3
import time
import argparse
import os
from tools import constants, paths
from launchers.sandbox import Sandbox
from tests_alpha import protocol


# KEYS used in the testnet
# TODO Could be generated in the test instead
KEYS = [
    (
        "bootstrap1",
        "unencrypted:edsk4K663GCFQwpP9A8KmB1apLti9QdymaLSfDpZJYxF6FU7RDxTf7",
    ),
    (
        "bootstrap2",
        "unencrypted:edsk3TQC5LiubMxjRHPtG9C6nm36naBc48HRyCutpg7EvKEjbDXWwg",
    ),
    (
        "bootstrap3",
        "unencrypted:edsk4R5vSo6TcJDKzHyEpBN4J43Zoq5hqgRxZR6CfxavprMVLsx3XH",
    ),
    (
        "bootstrap4",
        "unencrypted:edsk2ofibH3K3t9aMZkabhNYSx84K8tSpDt8WDKrAZvcRS6PEsEbZx",
    ),
    (
        "bootstrap5",
        "unencrypted:edsk3hqvSQqWqcT9qcEoKDCajFxPhD3Kis2NNsdbjKZHpe5PyBZp2E",
    ),
    (
        "bootstrap6",
        "unencrypted:edsk3XyGzQCT9k55Yp1TVBEvXnfW8mfBVp63LmT4J7TTerS1QFtmbm",
    ),
    (
        "bootstrap7",
        "unencrypted:edsk4X5cGKWGTGjoaQiAKSgr686F6aQoiaqizRiGZJkt31zjkfxveH",
    ),
    (
        "bootstrap8",
        "unencrypted:edsk4UEfVoTEwHaZ29WgR9jU2WuzKksYDSQkMokpBHRMNtRuczamjg",
    ),
    (
        "bootstrap9",
        "unencrypted:edsk37M9nY8GwNjfBVHp4ATEfNcRB6NqU3yhB4Lsh5EMgowfzL6v5R",
    ),
    (
        "bootstrap10",
        "unencrypted:edsk3koctiTD89vDaWmJWUwCdKfcWkNCgQrHGQpfdDBdheQRBweRvf",
    ),
    (
        "bootstrap11",
        "unencrypted:edsk3zp6hca1iDEwYe7gjF4rJfAsxmgq8bCS3hmce8LxaKzH6AcCJx",
    ),
    (
        "bootstrap12",
        "unencrypted:edsk4cWrRPMewAnQPPgMsQKtypjYc1iUFkB55J8yLtQRFYp6tYPMBi",
    ),
    (
        "bootstrap13",
        "unencrypted:edsk2vzWWn2ZHqfnecJD2sRjPCe351qXgDSJoRiBsnDALzA3Hp7gy4",
    ),
    (
        "bootstrap14",
        "unencrypted:edsk2jTnjJBD7XqXLj7zmfkfJ7fcp5SSn67spQRUe5nkSMQ6mFfzFQ",
    ),
    (
        "bootstrap15",
        "unencrypted:edsk3LT9AUuuhRRQ7DBEiB8qLD1DFZmnYANATxnaMiZU2kUW4WX5dT",
    ),
    (
        "bootstrap16",
        "unencrypted:edsk3yGpeunWHVojExQid55fAWHL9GN9R1bV1W9GsgmrkfjS2PkCDY",
    ),
    (
        "bootstrap17",
        "unencrypted:edsk3jnsvuFPkRAXLVB67fJWLSrk7pFjL1dw7xK8jQrGR5hpQnid9X",
    ),
    (
        "bootstrap18",
        "unencrypted:edsk4Nt3os53mSjqBdeFLFTRxX4L1oRLp1GMacesQzwSUgbzvs8swy",
    ),
    (
        "bootstrap19",
        "unencrypted:edsk3kHrZT1GW759Fq8qRKdv8fpf2DhvCqnhLhjSAHJ4bsD7ajTqQ9",
    ),
    (
        "bootstrap20",
        "unencrypted:edsk2pxjeZzoWx5noS3Cg5sVgcnGb2YYji6bGjwFhonZYYHbujSaDP",
    ),
    (
        "bootstrap21",
        "unencrypted:edsk4YQfPpKmUYvbenAdrcLqWCA56tgjZmVAv8E3Er3t18gDe9nKrJ",
    ),
    (
        "bootstrap22",
        "unencrypted:edsk3BJASkPA6bvSwvu8n4JH8Hd4s4m7S6njejGkE4L86hMVzkTqA1",
    ),
    (
        "bootstrap23",
        "unencrypted:edsk3bTfXmJnbeFov2BdSzTfLq92m4m7qnsVtxFMu4nP3tdXchqLP7",
    ),
    (
        "bootstrap24",
        "unencrypted:edsk3Co7CigtiCHZHe5w81GtfFZVJx97FjWBkEHpe8SXSAkLeTnthh",
    ),
    (
        "bootstrap25",
        "unencrypted:edsk3Yk8RAm9SnDRmkV1Bw2YSAaE29ZGCsf6siXR4pS1RvX6XRb15o",
    ),
    (
        "bootstrap26",
        "unencrypted:edsk3eG4Mb7bfdxwkD1yHxCQMqWSEJULwnqMJchza493dCMY6xe9XZ",
    ),
    (
        "bootstrap27",
        "unencrypted:edsk3d1R7mkh5ioRiDC97dWSUNMd379hj3ezrGs8Rdnm6eZx8iAq4L",
    ),
    (
        "bootstrap28",
        "unencrypted:edsk3gRsMw3rARu8tTdSdfL93uVDkeg9YoNV32vCmm6Kx5u36zgGQX",
    ),
    (
        "bootstrap29",
        "unencrypted:edsk3u8EayRjqEPuhb4J88SzBePieHZR4h7iHcYZXkU3jMges3XN8n",
    ),
    (
        "bootstrap30",
        "unencrypted:edsk4HnaFh65N2fSwi3wHB4BqXUm4haLMNCe4TiWkVqwzm4kuLeQZH",
    ),
    (
        "bootstrap31",
        "unencrypted:edsk3b1iWD1J3fiQKm4WCK1LvqrRiDxwjsfczCwiQK7vWFGaT6RzbL",
    ),
    (
        "bootstrap32",
        "unencrypted:edsk3yQXEuyKCAwBsojSsYudcUB4PqEPEyw4pyjVTXNgiiS3PKZyiL",
    ),
    (
        "bootstrap33",
        "unencrypted:edsk3aTbg7Qka2G2MrBcPv8kivhjpUX1phiZHBnkTsZE8kWd14yo4x",
    ),
    (
        "bootstrap34",
        "unencrypted:edsk3p9cRQ1EDWXdzCwwSGDGCQ4VK4fr1PAcxcYTzN1spYAEwsiBLX",
    ),
    (
        "bootstrap35",
        "unencrypted:edsk3t15XnxHBzbitxnSQZoc3HEYm45iAgHQWSd31UrQxY2twbAT7C",
    ),
    (
        "bootstrap36",
        "unencrypted:edsk496KFUos2tL8qgPjbwmyFtosJ7oF9C7b5J3snRiZuEeP2cetis",
    ),
    (
        "bootstrap37",
        "unencrypted:edsk2fvvnkEJYd1ZkVxUQDSKXixSiRMBwXCz9HRHRdhcsUii6EBvdg",
    ),
    (
        "bootstrap38",
        "unencrypted:edsk3ekA9BmGMCCCnV5pd3tvmQETd43B4CyzsgsFPUyHKdaZiwDAPk",
    ),
    (
        "bootstrap39",
        "unencrypted:edsk2phuUApj9ggF8yfaCAU6eL8yFs7WyS12auy1qyvNYjDGyevKc5",
    ),
    (
        "bootstrap40",
        "unencrypted:edsk35nQxhJ7jj7Fp5mFLReJWmeoWMoapCYXG3tFS9ynRqT5QiD1p7",
    ),
    (
        "bootstrap41",
        "unencrypted:edsk3tLjUDNutzrS4JNnhm5MVavFkixuFCZJgUJCRHFfSUdPBp88pY",
    ),
    (
        "bootstrap42",
        "unencrypted:edsk49ZwCsFc5u9f4q1FCHCzqnHnA2wQo8EfN6z3qCFj358vHXRAzK",
    ),
    (
        "bootstrap43",
        "unencrypted:edsk3Gmf2uaXpfTrVrCcHcGBmvenX2UmWuAKuEdFy8sg9kgZyKCxv7",
    ),
    (
        "bootstrap44",
        "unencrypted:edsk4RRLgSdwQyBhCHKEMDJsM5MGaEdpLURqwyyhg34Ez6tuzdje1f",
    ),
    (
        "bootstrap45",
        "unencrypted:edsk3zgorCTqzceNyrcgcEKSCSuxo97NiLJakzX6RBmfCBqF5v9KZh",
    ),
    (
        "bootstrap46",
        "unencrypted:edsk43WzdjDukBGt1wmx8wDEhwCiicUit5vT5hhbWGmKqmq1HZrFoJ",
    ),
    (
        "bootstrap47",
        "unencrypted:edsk3FTeK3uWfeenLpvdpopbYswW8HcTSBcLAu6jA65pNhGbjr2kdz",
    ),
    (
        "bootstrap48",
        "unencrypted:edsk2zSv9QxyZKZMyBmPWDtF1puFUrQTtVUqbpryjLfhFQrjzQxWTp",
    ),
    (
        "bootstrap49",
        "unencrypted:edsk3LsymG5cohGRvnheztubWgZ5Y6SjSVjBwAELNFByA2zKAEtDYw",
    ),
    (
        "bootstrap50",
        "unencrypted:edsk36tF1LdVJVwikbzPF5dPVm1Nj9NLA1zK9qnWMCbpWjcyhe9cgr",
    ),
    (
        "bootstrap51",
        "unencrypted:edsk3hB4hMcGSMthz24CYUxwNeCZaATLkT2bShsHtj5YcexN6pvxNx",
    ),
    (
        "bootstrap52",
        "unencrypted:edsk4HFCzvavH3m3aJ9pUCQAyAe3HdFpMAepx1KRumPtYo7PsVjMNu",
    ),
    (
        "bootstrap53",
        "unencrypted:edsk3XRDMyfLFsUHfqq6A4FG4D1akM4dZan4oQeVuFgNjm1w2WcC2v",
    ),
    (
        "bootstrap54",
        "unencrypted:edsk3RinEQ7SrvUrYtj6hpDyEKdTaZYbt77b2LHLtVrRDnPCiGveVn",
    ),
    (
        "bootstrap55",
        "unencrypted:edsk46KNd7H1HdFjMrjdphConYNrejn8B18Dyfb2Y1WqRNittBQvdv",
    ),
    (
        "bootstrap56",
        "unencrypted:edsk3fp6cAsLbXidNHnuZ9wMX1ViK9ncXUEjPvu1WP4DXKsXwrDA2H",
    ),
    (
        "bootstrap57",
        "unencrypted:edsk4Q21mB7NKn9Kd9cHaPUFLo6qq1BMVXBoe64mn1StCCff8iW3HK",
    ),
    (
        "bootstrap58",
        "unencrypted:edsk2qz6LeFB558sJLDzgyhgrRMzvEiJKFiCLDsxXLekXLtrmP6YCT",
    ),
    (
        "bootstrap59",
        "unencrypted:edsk32dVUUmWXFB2GeYY6ufDGJqqYzvpSs1TmdDuuAMbdRgxgsePWK",
    ),
    (
        "bootstrap60",
        "unencrypted:edsk4KgbLFCuQzq6XqA5khWE9JDJtMvm7R5ijMJR7DV8JJKwVZV2DE",
    ),
    (
        "bootstrap61",
        "unencrypted:edsk4XiewdSWTf8qfV8m5e8jTsu1v6e3p5uuAm3yjkZcz9QkVk7BF4",
    ),
    (
        "bootstrap62",
        "unencrypted:edsk3kxZt9ix9pQpC436d8AoNhfSDCqnMVvDhp8efeEiwWe7xVEc84",
    ),
    (
        "bootstrap63",
        "unencrypted:edsk3PKUeiG4zFveJDFFUfX564eeV9LD4FCLmNP3C9hWpdGaJX6GiM",
    ),
    (
        "bootstrap64",
        "unencrypted:edsk3y8FtXJbc2GzUXirn3ZQhJPEargPckHwrCseyUachRps1vX8Go",
    ),
    (
        "bootstrap65",
        "unencrypted:edsk4DRKcrDx38tTeL2WYSGnENwoVdeoU7DedrSUL89GeSobsfjVxQ",
    ),
    (
        "bootstrap66",
        "unencrypted:edsk31xEsCpdFban4vZCupk9XMvc5jgEHD1oZshWMQ9rcZjB687P67",
    ),
    (
        "bootstrap67",
        "unencrypted:edsk4A3WX8WjyrPEWKKk9miZ4AD44499p83FpWpmftPAjPa6oGVAF3",
    ),
    (
        "bootstrap68",
        "unencrypted:edsk4GSVoVwsbivkYTTnQiq2dCrdFmaQsoXiHgRnYZvhNhHdbirmG9",
    ),
    (
        "bootstrap69",
        "unencrypted:edsk43AXSQAsv1mMmCujV8b5xZdXyeKaLNV3BismoWhRGFoYoG7E7D",
    ),
    (
        "bootstrap70",
        "unencrypted:edsk4LmTCHCHTq9ejF1XoCgcXMzKVhcvvrto9SW4wpL3aNxvM92mRo",
    ),
    (
        "bootstrap71",
        "unencrypted:edsk3RaXQohk3ytaccf4ovvacQzGFuxhqDj1XPM8VucGmzom3tQ5Ao",
    ),
    (
        "bootstrap72",
        "unencrypted:edsk43WCHdyQVbvrCSf4jH3wrGthKmk4d9AscPVYr6JddfWBpgj3S2",
    ),
    (
        "bootstrap73",
        "unencrypted:edsk3fdWsZVELiLE92nJD4XBHeUJCKecLy4cmtpyreT33VBUM8pqhJ",
    ),
    (
        "bootstrap74",
        "unencrypted:edsk37XA3rRkYL1SHkGuaMWsXXtKCJmcWgTFraz8TrUNQre9s9s9nh",
    ),
    (
        "bootstrap75",
        "unencrypted:edsk35mupp2njv9xbjLKN9jmAeUoZwvbVkfzkTuinjpMwyVuQVv3kE",
    ),
    (
        "bootstrap76",
        "unencrypted:edsk4AborPRpuR36GCmCzcZzGJn8HxU8SbfsQ1fcb3grR6NaUBAVWC",
    ),
    (
        "bootstrap77",
        "unencrypted:edsk3Ew2RYFFw2X6roHtioieKaPoW3DjoruGH53jaNSoaX6emGMzWt",
    ),
    (
        "bootstrap78",
        "unencrypted:edsk2yTsm39smYfYG5xarLT6U7pHry1aqm6Q3rYfbYLs3Y6LoJbTuU",
    ),
    (
        "bootstrap79",
        "unencrypted:edsk3W1nnD39CxtPTH1opNK297YN9ymGbHifg732oKfJQY9tE4ao1V",
    ),
    (
        "bootstrap80",
        "unencrypted:edsk47UY9j65f3SqrBTy1pzbAxhj8RFVSy22nev6FtefiksFvfKwBh",
    ),
    (
        "bootstrap81",
        "unencrypted:edsk3caYottvwZk6RfaZUe5qaMuknHJGeAZWBbDfwnH8wWwLnbpyj4",
    ),
    (
        "bootstrap82",
        "unencrypted:edsk2gYSqge2obXUpUKgtuoPcbJSqzxcMoc1NXRvWhtNm3N4t1g1cX",
    ),
    (
        "bootstrap83",
        "unencrypted:edsk4LtDdcBJnJjs8p51h4J3W9s3XRkSvLvvVHriGdqJucrRZkprrH",
    ),
    (
        "bootstrap84",
        "unencrypted:edsk4LTGr15vS9zwuVszH3VP3JJUxGShdnWFyPvQAWo4qKk6jwVPSQ",
    ),
    (
        "bootstrap85",
        "unencrypted:edsk3Ue2qpscNaQz9dX65jU6Rpnu3uR6kv7Ka1iQtwyY53mQQKNKmC",
    ),
    (
        "bootstrap86",
        "unencrypted:edsk3Xe437dhiaNxQJbQHxkaN59PLijnr4vB5Twd4iKiTXR2C3YByP",
    ),
    (
        "bootstrap87",
        "unencrypted:edsk2sNKLcSWM6qx82PCMhrUmggMdsepupqmve66uHiwCZ1v4b4CWY",
    ),
    (
        "bootstrap88",
        "unencrypted:edsk2kbo1PxmNQAJZYPKCs8PJoWD66rzDukCRDaK3ZXEVNc6Ytixrv",
    ),
    (
        "bootstrap89",
        "unencrypted:edsk3b2MQQ2y1wejsAXk7tVH1JiAPdpyveiSapeHJiDQtZaxAyEEns",
    ),
    (
        "bootstrap90",
        "unencrypted:edsk34uJZesZn7a9KTZi5gGitp5ZjAX8L3Fi94txZr6gvjbs9hx8pr",
    ),
    (
        "bootstrap91",
        "unencrypted:edsk31oFoMdGJU15RGnogkTV5wM8sURkuhz1ASxXo35YykA9AGh9vK",
    ),
    (
        "bootstrap92",
        "unencrypted:edsk3pJVD6aTjDuaEBd84etSDHERFnk5yNEacxqUYVWcASiukFufyR",
    ),
    (
        "bootstrap93",
        "unencrypted:edsk4NMSS2CixsRmrbFY8KUn1XAeXHhCnQFbCdQb9Dfkw7Ew1yHTbt",
    ),
    (
        "bootstrap94",
        "unencrypted:edsk3jF6vVJry3jL4RVHXvGN88ahi9cTgv6LLo7SoUywgCRkm6MmCX",
    ),
    (
        "bootstrap95",
        "unencrypted:edsk4KrsynXVeAzcr3PP6d95FtGdcwaHmK1CTZtcwz7DFhLQXJBW2e",
    ),
    (
        "bootstrap96",
        "unencrypted:edsk2tmfb4bgB6DMzevo1H7r8aqQAPurUY6dv93JBuSkQW6QzyAQio",
    ),
    (
        "bootstrap97",
        "unencrypted:edsk3Xf1gv4k4YsUfsw9BoBgh4BdbouJqv3BvAmns1A2sPEPFeAVbJ",
    ),
    (
        "bootstrap98",
        "unencrypted:edsk4JkYQJ5X1BGCRnEeGo7EfunriNpfK4tFj1LsLonrguUTyWMZMN",
    ),
    (
        "bootstrap99",
        "unencrypted:edsk3yKcPeV8MWP7G2ZBpShcLL8JZH8k9Y8qsywuKk7MUh5RzctkXP",
    ),
    (
        "bootstrap100",
        "unencrypted:edsk4R7i4PiNU4baYSzCVzcVeZvYNHAGwzBaHpZUdzuRXE74pchtvN",
    ),
    ("activator", constants.IDENTITIES['activator']['secret']),
]


NUM_ACCOUNTS = len(KEYS) - 1


# Split accounts into groups of even length
def accounts(node, num_nodes):
    quotient = NUM_ACCOUNTS // num_nodes
    start = node * quotient + 1
    end = (node + 1) * quotient + 1
    return [f'bootstrap{i}' for i in range(start, end)]


def scenario(round_duration, num_nodes, log_dir):
    with Sandbox(
        paths.TEZOS_HOME, constants.IDENTITIES, log_dir=log_dir
    ) as sandbox:
        for i in range(num_nodes):
            sandbox.add_node(
                i, params=constants.NODE_PARAMS, config_client=False
            )
            for account, key in KEYS:
                sandbox.client(i).import_secret_key(account, key)

        proto_testnet_params = dict(protocol.TENDERBAKE_PARAMETERS)
        parameters = dict(proto_testnet_params)
        parameters['minimal_block_delay'] = str(round_duration)
        parameters['delay_increment_per_round'] = str(round_duration)
        protocol.activate(sandbox.client(0), parameters=parameters)

        for i in range(num_nodes):
            sandbox.add_baker(
                i,
                accounts(i, num_nodes),
                proto=constants.ALPHA_DAEMON,
                log_levels=constants.TENDERBAKE_BAKER_LOG_LEVELS,
                run_params=['--liquidity-baking-toggle-vote', 'pass'],
            )

        while 1:
            sandbox.client(0).get_head()
            time.sleep(round_duration)


DESCRIPTION = '''
Utility script to run a tenderbake testnet
'''


def main():
    description = DESCRIPTION
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument(
        '--round-duration',
        dest='round_duration',
        metavar='TIME',
        help='round_duration (seconds), default=5',
        required=False,
        default='5',
    )
    parser.add_argument(
        '--num-nodes',
        dest='num_nodes',
        metavar='NUM',
        help='num_nodes, default=1',
        required=False,
        default='1',
    )
    parser.add_argument(
        '--log-dir',
        dest='log_dir',
        metavar='PATH',
        help='log dir',
        required=False,
        default=None,
    )
    args = parser.parse_args()
    log_dir = args.log_dir
    if not (log_dir is None or os.path.isdir(log_dir)):
        assert 0, "log_dir isn't a valid path"
    scenario(
        int(args.round_duration),
        int(args.num_nodes),
        args.log_dir,
    )


if __name__ == "__main__":
    main()
