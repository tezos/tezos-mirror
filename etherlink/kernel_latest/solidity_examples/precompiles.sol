// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.2 <0.9.0;

contract PrecompileCaller {
    function callPrecompiles()
        public
        view
        returns (
            address recovered,
            bytes32 sha256Hash,
            bytes20 ripemd160Hash,
            bytes memory identityResult,
            bytes memory modexpResult,
            uint256[2] memory ecAddResult,
            uint256[2] memory ecMulResult,
            bool ecPairingResult,
            bytes memory blake2fResult
        )
    {
        recovered = callEcRecover();
        sha256Hash = callSha256();
        ripemd160Hash = callRipemd160();
        identityResult = callIdentity();
        modexpResult = callModexp();
        ecAddResult = callEcAdd();
        ecMulResult = callEcMul();
        ecPairingResult = callEcPairing();
        blake2fResult = callBlake2f();
    }

    function callEcRecover() public pure returns (address) {
        bytes32 hash = 0x5e806a52647a95f9e0f46d31cbe51e1a99f531dbe3a5f4de7d7a2be1d781c4af;
        uint8 v = 27;
        bytes32 r = 0x0b5e42292e5fd622f034cb6cbd7749e8deefee81a9d569cb092e7e51d5365a6a;
        bytes32 s = 0x2a2d65a201b41662b81b59edafbc2588f97f7156d7f7b7e37a878cc195b4236d;

        return ecrecover(hash, v, r, s);
    }

    function callSha256() public pure returns (bytes32) {
        bytes memory data = "Fast";
        return sha256(data);
    }

    function callRipemd160() public pure returns (bytes20) {
        bytes memory data = "Fair";
        return ripemd160(data);
    }

    function callIdentity() public view returns (bytes memory) {
        bytes memory data = "Free";
        bytes memory result = new bytes(data.length);
        assembly {
            let success := staticcall(
                gas(),
                0x04,
                add(data, 0x20),
                mload(data),
                add(result, 0x20),
                mload(data)
            )
            if iszero(success) {
                revert(0, 0)
            }
        }
        return result;
    }

    function callModexp() public view returns (bytes memory) {
        bytes memory input = abi.encodePacked(
            uint256(0x02), // base
            uint256(0x03), // exponent
            uint256(0x05) // modulus
        );
        bytes memory result = new bytes(32);
        assembly {
            let success := staticcall(
                gas(),
                0x05,
                add(input, 0x20),
                mload(input),
                add(result, 0x20),
                mload(result)
            )
            if iszero(success) {
                revert(0, 0)
            }
        }
        return result;
    }

    function callEcAdd() public view returns (uint256[2] memory) {
        uint8[2] memory input1 = [
            0x0000000000000000000000000000000000000000000000000000000000000000,
            0x0000000000000000000000000000000000000000000000000000000000000000
        ];

        uint8[2] memory input2 = [
            0x0000000000000000000000000000000000000000000000000000000000000000,
            0x0000000000000000000000000000000000000000000000000000000000000000
        ];

        uint256[2] memory result;
        assembly {
            let input := mload(0x40)
            mstore(input, mload(add(input1, 0x20)))
            mstore(add(input, 0x20), mload(add(input1, 0x40)))
            mstore(add(input, 0x40), mload(add(input2, 0x20)))
            mstore(add(input, 0x60), mload(add(input2, 0x40)))
            let success := staticcall(gas(), 0x06, input, 0x80, result, 0x40)
            if iszero(success) {
                revert(0, 0)
            }
        }
        return result;
    }

    function callEcMul() public view returns (uint256[2] memory) {
        // Point (1,2) on the elliptic curve and scalar 2
        uint256[3] memory input = [uint256(1), uint256(2), uint256(2)];
        uint256[2] memory result;
        assembly {
            let success := staticcall(
                gas(),
                0x07,
                input,
                0x60, // 96 bytes: 3 * 32-byte words
                result,
                0x40 // 64 bytes: 2 * 32-byte words
            )
            if iszero(success) {
                revert(0, 0)
            }
        }
        return result;
    }

    function callEcPairing() public view returns (bool) {
        uint8[1] memory inputPairing = [
            0x0000000000000000000000000000000000000000000000000000000000000000
        ];
        bool result;
        assembly {
            let success := staticcall(
                gas(),
                0x08,
                inputPairing,
                0x180,
                result,
                0x20
            )
            if iszero(success) {
                revert(0, 0)
            }
        }
        return result;
    }

    function callBlake2f() public view returns (bytes memory) {
        bytes memory blake2fInput = abi.encodePacked(
            hex"0000000048c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b61626300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000300000000000000000000000000000001"
        );
        bytes memory result = new bytes(64);
        assembly {
            let success := staticcall(
                gas(),
                0x09,
                add(blake2fInput, 0x20),
                mload(blake2fInput),
                add(result, 0x20),
                64
            )
            if iszero(success) {
                revert(0, 0)
            }
        }
        return result;
    }
}
