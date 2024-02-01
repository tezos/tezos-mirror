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

Basic scenario for using the ERC-721 token: NFT (Non-Fungible Token).

The code for the scenario can be found in [BasicNFT.sol](https://github.com/trilitech/live-testing-protocols/blob/main/contracts/ERC721/BasicNFT.sol).

### Actions

1. Owner can create a token
    * The creation event is emitted
    * The owner balance is increased
2. Owner can transfer a token
    * The transfer event is emitted
    * The receiver balance is increased
    * The sender balance is decreased

The code for the actions can be found in [BasicNFT.ts](https://github.com/trilitech/live-testing-protocols/blob/main/test/ERC721/BasicNFT.ts).

### Testing

Follow [these instructions](https://github.com/trilitech/live-testing-protocols/tree/main#tests) specialized with the ERC-721 test.

* Local: `npx hardhat test test/ERC721/BasicNFT.ts`.
* Ghostnet: `npx hardhat test --network etherlink test/ERC721/BasicNFT.ts`.

## Scenario 3: ERC-1967 (transparent proxy pattern)

Basic scenario for using the [ERC-1967](https://eips.ethereum.org/EIPS/eip-1967): the transparent proxy pattern by Openzeppelin.

The code for the scenario can be found in [Logic_positive.sol](https://github.com/trilitech/live-testing-protocols/blob/main/contracts/proxies/transparent/Logic_positive.sol) and [Logic_negative.sol](https://github.com/trilitech/live-testing-protocols/blob/main/contracts/proxies/transparent/Logic_negative.sol).

This is almost the same code but with a small difference: one increase the storage, the other decrease it. The goal is to use them to test the proxy system and "upgrade" it by switching between these 2 versions.

### Actions:
1. Deployment of Transparent Proxy Pattern
   * The proxy (storage) contract is deployed by the Openzeppelin's plugin
   * The implementation (logic) contract is deployed by the Openzeppelin's plugin
   * The admin (manage upgrade) contract is deployed by the Openzeppelin's plugin
2. Delegate calls works
   * The number in the proxy (storage) is modify
   * The number in the implementation (logic) is not modify
3. Upgrade version on Transparent Proxy
   * The proxy contract address is the same after upgrade
   * The proxy contract storage is the same after upgrade
   * The implementation (logic) contract is the new version
4. Delegate calls works after upgrade
   * The number in the proxy (storage) is modify by the new version
   * The number in the implementation (logic) is not modify

The code for the actions can be found in [logicPositiveAndNegative.ts](https://github.com/trilitech/live-testing-protocols/blob/main/test/proxies/transparent/logicPositiveAndNegative.ts).

The code for the deployment using Openzeppelin's plugin can be found in [01-deploy-proxyLogicPositive.ts](https://github.com/trilitech/live-testing-protocols/blob/main/deploy/proxies/transparent/01-deploy-proxyLogicPositive.ts).

Important to know:
- The objective here is to test a proxy system. These proxies rely on a logic contract (also known as implementation contract or master copy) that is called using delegatecall. This allows proxies to keep a persistent state (storage and balance) while the code is delegated to the logic contract.
- The deployment process is also important here because the tool do not simply deploy the smart contract used for the test. Instead, the plugin deploy 3 different contracts: the Admin, the Proxy and the Logic. The Admin is responsible for the upgrade part. The Proxy is the contract storing all the modification and sending the request with delegatecall and the Logic is the contract containing the logic itself and will be called by the Proxy to modify directly it storage.

### Testing

Follow [these instructions](https://github.com/trilitech/live-testing-protocols/tree/main#tests) specialized with the ERC-1967 test.
* Local: `npx hardhat test test/proxies/transparent/logicPositiveAndNegative.ts`.
* Ghostnet: `npx hardhat test --network etherlink test/proxies/transparent/logicPositiveAndNegative.ts`.

The test should go through, even if the interaction actually fails because of some issues in Etherlink, not the scenario. (Fixing Etherlink would be the purpose of the next task.)

## Scenario 4: conventional NFT dApp

## Scenario 5: the Uniswap v2 DeFi protocol

## Scenario 6: interactions with Foundry

## Scenario 7: interactions with Hardhat

## Scenario 8: interactions with Remix

## Scenario 9: interactions with MetaMask

## Scenario 10: interactions with ThirdWeb
