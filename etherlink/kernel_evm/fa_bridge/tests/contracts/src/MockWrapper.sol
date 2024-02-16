// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.19;

function calcTicketHash(bytes22 ticketer, bytes memory content)
    pure
    returns (uint256)
{
    return uint256(keccak256(abi.encodePacked(ticketer, content)));
}

/**
 * MockWrapper is a mock token contract which represents a L1 token on L2.
 */
contract MockWrapper {
    uint256 private _ticketHash;
    address private _kernel;

    /**
     * @param ticketer_ whitelisted Tezos L1 address of the ticketer contract
     * @param content_ identifier of the L1 token (ticket id)
     * @param kernel_ address of the rollup kernel which is responsible for
     *        minting tokens
     * @param flag_ debug flag for tracking storage changes
     */
    constructor(
        bytes22 ticketer_,
        bytes memory content_,
        address kernel_,
        uint256 flag_
    ) {
        _ticketHash = calcTicketHash(ticketer_, content_);
        _kernel = kernel_;
        setFlag(flag_);
        this;
    }

    function setFlag(uint256 value) internal {
        bytes32 slot = keccak256(abi.encodePacked("FLAG_TAG"));
        assembly {
            sstore(slot, value)
        }
    }

    event Mint (
        address indexed receiver,
        uint256 amount
    );

    event Burn (
        address indexed sender,
        uint256 amount
    );

    /**
     * Mints `amount` tokens for `to` address if provided `ticket`hash`
     * is correct.
     *
     * Requirements:
     *
     * - only kernel address allowed to mint tokens.
     * - `ticketHash` must be equal to the token hash of the ticketer
     * and identifier used during deployment.
     */
    function deposit(address receiver, uint256 amount, uint256 ticketHash)
        public
    {
        require(
            _kernel == msg.sender,
            "MockWrapper: only kernel allowed to mint tokens"
        );
        require(_ticketHash == ticketHash, "MockWrapper: wrong ticket hash");
        emit Mint(receiver, amount);
        setFlag(amount);
    }

    /**
     * Burns `amount` tokens for `to` address if provided `ticket hash`
     * is correct.
     *
     * Requirements:
     *
     * - only kernel address allowed to burn tokens.
     * - `ticketHash` must be equal to the token hash of the ticketer
     * and identifier used during deployment.
     */
    function withdraw(address sender, uint256 amount, uint256 ticketHash)
        public
    {
        require(
            _kernel == msg.sender,
            "MockWrapper: only kernel allowed to mint tokens"
        );
        require(_ticketHash == ticketHash, "MockWrapper: wrong ticket hash");
        emit Burn(sender, amount);
        setFlag(amount);
    }
}