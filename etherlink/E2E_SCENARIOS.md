# E2E Real World scenarios

Milestone: https://gitlab.com/tezos/tezos/-/milestones/310#end-to-end-real-world-scenarios
Repo: https://github.com/trilitech/live-testing-protocols/tree/main

## Scenario 1: ERC-20 (token)

Basic scenario for using the ERC-20 token: cryptocurrency.

The code for the actions can be found in [BasicTestToken.ts](https://github.com/trilitech/live-testing-protocols/blob/main/test/ERC20/BasicTestToken.ts).

### Actions:

 1. Owner can create tokens
    * The creation event is emitted
    * The owner balance is increased
 2. Non-owner can't create tokens
    * Revert
    * The revert message should be compatible: [#6744](https://gitlab.com/tezos/tezos/-/issues/6744)
    * The non-owner balance is not increased
 3. Owner can burn tokens
    * The burn event is emitted
    * The owner balance is decreased
 4. Non-owner can't burn tokens
    * Revert
    * The revert message should be compatible: [#6744](https://gitlab.com/tezos/tezos/-/issues/6744)
    * The non-owner balance is not decreased
 5. Usage of the permit protocol (EIP-2612)
    * First user sign an ERC2612 permission off-chain
    * Second user can use permit and transfer tokens from first user
    * Balance of the second user increased

### Executing scenario

Follow [these instructions](https://github.com/trilitech/live-testing-protocols/tree/main#tests) specialized with the ERC-20 test.
* Local: `npx hardhat test test/ERC20/BasicTestToken.ts`.
* Ghostnet: `npx hardhat test --network etherlink test/ERC20/BasicTestToken.ts`.

## Scenario 2: ERC-721 (NFT)

## Scenario 3: ERC-1967 (transparent proxy pattern)

## Scenario 4: conventional NFT dApp

## Scenario 5: the Uniswap v2 DeFi protocol

## Scenario 6: interactions with Foundry

## Scenario 7: interactions with Hardhat

## Scenario 8: interactions with Remix

## Scenario 9: interactions with MetaMask

## Scenario 10: interactions with ThirdWeb
