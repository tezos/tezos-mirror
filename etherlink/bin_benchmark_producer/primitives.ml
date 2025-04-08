(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let evm_version = Evm_version.Cancun

let current = "etherlink/bin_benchmark_producer"

let general_contracts = Filename.concat current "general_contracts"

let problematic_opcodes = Filename.concat current "problematic_opcodes"

let gas_sinks = Filename.concat current "gas_sinks"

let addr1 = Eth_account.bootstrap_accounts.(1).address

let addr2 = Eth_account.bootstrap_accounts.(2).address

let registered_general_contracts =
  [
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "erc20tok.sol")
        ~label:"erc20tok"
        ~contract:"ERC20"
        evm_version,
      [
        ("transfer(address,uint256)", [addr1; "1000"]);
        ("approve(address,uint256)", [addr1; "500"]);
        ("transferFrom(address,address,uint256)", [addr1; addr2; "250"]);
        ("mint(uint256)", ["10000"]);
        ("burn(uint256)", ["1000"]);
      ] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "erc1155.sol")
        ~label:"erc1155_mint"
        ~contract:"MyMultiToken"
        evm_version,
      [
        ("mint(uint256,uint256,bytes)", ["1"; "100"; "0x"]);
        ("batchMint(uint256[],uint256[],bytes)", ["[1,2]"; "[100,200]"; "0x"]);
        ("burn(uint256,uint256)", ["1"; "50"]);
        ("batchBurn(uint256[],uint256[])", ["[1,2]"; "[50,100]"]);
      ] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "gas_guzzler.sol")
        ~label:"gas_guzzler"
        ~contract:"GasGuzzler"
        evm_version,
      [
        ("incrementLocalSum(uint256)", ["100"]);
        ("incrementGlobalSum(uint256)", ["100"]);
      ] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "keccak.sol")
        ~label:"keccak_guess"
        ~contract:"GuessTheMagicWord"
        evm_version,
      [("guess(string)", ["Solidity"])] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "loop.sol")
        ~label:"loop"
        ~contract:"Loop"
        evm_version,
      [("loop(uint256)", ["1000"])] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "read_info.sol")
        ~label:"read_info"
        ~contract:"ReadInfo"
        evm_version,
      [
        ("timestamp()", []);
        ("gas_price()", []);
        ("coin_base()", []);
        ("origin()", []);
        ("gas_limit()", []);
        ("chain_id()", []);
        ("block_number()", []);
        ("balance()", []);
        ("base_fee()", []);
        ("extcodehash()", []);
        ("msize()", []);
        ("get_code()", []);
      ] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "revert_computation.sol")
        ~label:"dummy_computation"
        ~contract:"DummyComputation"
        evm_version,
      [
        ("addToFive(uint256)", ["3"]);
        ("addToFive(uint256)", ["6"]);
        ("addToFive(uint256)", ["10"]);
      ] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "selfdestruct.sol")
        ~label:"self_destruct_example"
        ~contract:"SelfDestructExample"
        evm_version,
      [("close()", [])] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "storage.sol")
        ~label:"simple_storage"
        ~contract:"SimpleStorage"
        evm_version,
      [("set(uint256)", ["42"]); ("get()", [])] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "verify_signature.sol")
        ~label:"verify_signature"
        ~contract:"VerifySignature"
        evm_version,
      [
        ( "getMessageHash(address,uint256,string,uint256)",
          [addr1; "123"; "\"coffee and donuts\""; "1"] );
        ( "getEthSignedMessageHash(bytes32)",
          ["0xcf36ac4f97dc10d91fc2cbb20d718e94a8cbfe0f82eaedc6a4aa38946fb797cd"]
        );
        ( "verify(address,address,uint256,string,uint256,bytes)",
          [
            addr2;
            addr1;
            "123";
            "\"coffee and donuts\"";
            "1";
            "0x993dab3dd91f5c6dc28e17439be475478f5635c92a56e17e82349d3fb2f166196f466c0b4e0c146f285204f0dcb13e5ae67bc33f4b888ec32dfe0a063e8f3f781b";
          ] );
        ( "recoverSigner(bytes32,bytes)",
          [
            "0x65ba8777d4d56ae3e119b1b3f390c44fa24cdac2a2ef41c75be6352d3dd2dbfb";
            "0x993dab3dd91f5c6dc28e17439be475478f5635c92a56e17e82349d3fb2f166196f466c0b4e0c146f285204f0dcb13e5ae67bc33f4b888ec32dfe0a063e8f3f781b";
          ] );
        ( "splitSignature(bytes)",
          [
            "0x993dab3dd91f5c6dc28e17439be475478f5635c92a56e17e82349d3fb2f166196f466c0b4e0c146f285204f0dcb13e5ae67bc33f4b888ec32dfe0a063e8f3f781b";
          ] );
      ] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "rec_call.sol")
        ~label:"rec_call"
        ~contract:"A"
        evm_version,
      [("call()", [])] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat general_contracts "external_recursive.sol")
        ~label:"external_recursive"
        ~contract:"RecursiveCaller"
        evm_version,
      [("recurse()", [])] );
  ]

let registered_problematic_opcodes =
  [
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat problematic_opcodes "0x20.sol")
        ~label:"use_sha3"
        ~contract:"UseSHA3"
        evm_version,
      [("hashSomething(bytes)", ["0x123456"])] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat problematic_opcodes "0x37.sol")
        ~label:"use_calldata_copy"
        ~contract:"UseCalldataCopy"
        evm_version,
      [("echo()", [])] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat problematic_opcodes "0x55.sol")
        ~label:"use_sstore"
        ~contract:"UseSStore"
        evm_version,
      [("store(uint256)", ["42"])] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat problematic_opcodes "0xf5.sol")
        ~label:"use_create2"
        ~contract:"Deployer"
        evm_version,
      [
        ( "deploy(bytes,bytes32)",
          [
            "0x600060005560016000f3";
            "0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef";
          ] );
      ] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat problematic_opcodes "mcopy.sol")
        ~label:"use_mcopy"
        ~contract:"UseMCopy"
        evm_version,
      [("mcopy()", [])] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat problematic_opcodes "tstore_tload.sol")
        ~label:"use_transient_storage"
        ~contract:"UseTransientStorage"
        evm_version,
      [("store(uint256,uint256)", ["1"; "42"]); ("load(uint256)", ["1"])] );
  ]

let registered_gas_sinks =
  [
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat gas_sinks "create2_loop.sol")
        ~label:"create2_loop"
        ~contract:"DeployerLoop"
        evm_version,
      [
        ( "deployMany(bytes,bytes32)",
          [
            "0x60006000556001600055";
            "0x0000000000000000000000000000000000000000000000000000000000000042";
          ] );
      ] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat gas_sinks "mstore_loop.sol")
        ~label:"mstore_loop"
        ~contract:"MemoryBlower"
        evm_version,
      [("expandMemory()", [""])] );
    ( Solidity_contracts.compile_contract
        ~source:(Filename.concat gas_sinks "sha3_loop.sol")
        ~label:"sha3_loop"
        ~contract:"UseSHA3Loop"
        evm_version,
      [("hashManyTimes(bytes)", ["0x1234"])] );
  ]

let registered_precompiled =
  [
    ( Solidity_contracts.precompiles evm_version,
      [
        ("callEcRecover()", [""]);
        ("callSha256()", [""]);
        ("callRipemd160()", [""]);
        ("callIdentity()", [""]);
        ("callModexp()", [""]);
        ("callEcAdd()", [""]);
        ("callEcMul()", [""]);
        ("callEcPairing()", [""]);
        ("callBlake2f()", [""]);
      ] );
  ]
