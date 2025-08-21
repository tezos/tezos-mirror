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

let solidity_basics = Filename.concat current "solidity_basics"

let addr1 = Eth_account.bootstrap_accounts.(1).address

let addr2 = Eth_account.bootstrap_accounts.(2).address

type contract_info = {
  contract : Solidity_contracts.contract Lwt.t;
  constructor_arg : string option;
  calls : (string * string list) trace;
}

let registered_general_contracts =
  [
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:
            (Filename.concat general_contracts "call_imbricated_revert.sol")
          ~label:"call_imbricated_revert"
          ~contract:"TestDepthCall"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("startDepth(uint256)", ["0"]);
          ("startDepth(uint256)", ["5"]);
          ("startDepth(uint256)", ["6"]);
          ("startDepth(uint256)", ["11"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "call_tracer_depth.sol")
          ~label:"call_tracer_depth"
          ~contract:"TestDepthCall"
          evm_version;
      constructor_arg = None;
      calls = [("startDepth()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "erc20tok.sol")
          ~label:"erc20tok"
          ~contract:"ERC20"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("transfer(address,uint256)", [addr1; "1000"]);
          ("approve(address,uint256)", [addr1; "500"]);
          ("transferFrom(address,address,uint256)", [addr1; addr2; "250"]);
          ("mint(uint256)", ["10000"]);
          ("burn(uint256)", ["1000"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "erc1155.sol")
          ~label:"erc1155_mint"
          ~contract:"MyMultiToken"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("mint(uint256,uint256,bytes)", ["1"; "100"; "0x"]);
          ("batchMint(uint256[],uint256[],bytes)", ["[1,2]"; "[100,200]"; "0x"]);
          ("burn(uint256,uint256)", ["1"; "50"]);
          ("batchBurn(uint256[],uint256[])", ["[1,2]"; "[50,100]"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "gas_guzzler.sol")
          ~label:"gas_guzzler"
          ~contract:"GasGuzzler"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("incrementLocalSum(uint256)", ["100"]);
          ("incrementGlobalSum(uint256)", ["100"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "keccak.sol")
          ~label:"keccak_guess"
          ~contract:"GuessTheMagicWord"
          evm_version;
      constructor_arg = None;
      calls = [("guess(string)", ["Solidity"])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "loop.sol")
          ~label:"loop"
          ~contract:"Loop"
          evm_version;
      constructor_arg = None;
      calls = [("loop(uint256)", ["1000"])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "read_info.sol")
          ~label:"read_info"
          ~contract:"ReadInfo"
          evm_version;
      constructor_arg = None;
      calls =
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
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "revert_computation.sol")
          ~label:"dummy_computation"
          ~contract:"DummyComputation"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("addToFive(uint256)", ["3"]);
          ("addToFive(uint256)", ["6"]);
          ("addToFive(uint256)", ["10"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "selfdestruct.sol")
          ~label:"self_destruct_example"
          ~contract:"SelfDestructExample"
          evm_version;
      constructor_arg = None;
      calls = [("close()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "storage.sol")
          ~label:"simple_storage"
          ~contract:"SimpleStorage"
          evm_version;
      constructor_arg = None;
      calls = [("set(uint256)", ["42"]); ("get()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "verify_signature.sol")
          ~label:"verify_signature"
          ~contract:"VerifySignature"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "getMessageHash(address,uint256,string,uint256)",
            [addr1; "123"; "\"coffee and donuts\""; "1"] );
          ( "getEthSignedMessageHash(bytes32)",
            [
              "0xcf36ac4f97dc10d91fc2cbb20d718e94a8cbfe0f82eaedc6a4aa38946fb797cd";
            ] );
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
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "rec_call.sol")
          ~label:"rec_call"
          ~contract:"A"
          evm_version;
      constructor_arg = None;
      calls = [("call()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat general_contracts "external_recursive.sol")
          ~label:"external_recursive"
          ~contract:"RecursiveCaller"
          evm_version;
      constructor_arg = None;
      calls = [("recurse()", [])];
    };
  ]

let registered_problematic_opcodes =
  [
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat problematic_opcodes "0x20.sol")
          ~label:"use_sha3"
          ~contract:"UseSHA3"
          evm_version;
      constructor_arg = None;
      calls = [("hashSomething(bytes)", ["0x123456"])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat problematic_opcodes "0x37.sol")
          ~label:"use_calldata_copy"
          ~contract:"UseCalldataCopy"
          evm_version;
      constructor_arg = None;
      calls = [("echo()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat problematic_opcodes "0x55.sol")
          ~label:"use_sstore"
          ~contract:"UseSStore"
          evm_version;
      constructor_arg = None;
      calls = [("store(uint256)", ["42"])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat problematic_opcodes "0xf5.sol")
          ~label:"use_create2"
          ~contract:"Deployer"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "deploy(bytes,bytes32)",
            [
              "0x600060005560016000f3";
              "0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef";
            ] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat problematic_opcodes "mcopy.sol")
          ~label:"use_mcopy"
          ~contract:"UseMCopy"
          evm_version;
      constructor_arg = None;
      calls = [("mcopy()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat problematic_opcodes "tstore_tload.sol")
          ~label:"use_transient_storage"
          ~contract:"UseTransientStorage"
          evm_version;
      constructor_arg = None;
      calls =
        [("store(uint256,uint256)", ["1"; "42"]); ("load(uint256)", ["1"])];
    };
  ]

let registered_gas_sinks =
  [
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat gas_sinks "create2_loop.sol")
          ~label:"create2_loop"
          ~contract:"DeployerLoop"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "deployMany(bytes,bytes32)",
            [
              "0x60006000556001600055";
              "0x0000000000000000000000000000000000000000000000000000000000000042";
            ] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat gas_sinks "mstore_loop.sol")
          ~label:"mstore_loop"
          ~contract:"MemoryBlower"
          evm_version;
      constructor_arg = None;
      calls = [("expandMemory()", [""])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat gas_sinks "sha3_loop.sol")
          ~label:"sha3_loop"
          ~contract:"UseSHA3Loop"
          evm_version;
      constructor_arg = None;
      calls = [("hashManyTimes(bytes)", ["0x1234"])];
    };
  ]

let registered_precompiled =
  [
    {
      contract = Solidity_contracts.precompiles evm_version;
      constructor_arg = None;
      calls =
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
        ];
    };
  ]

let registered_solidity_basics =
  [
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "abi_decode.sol")
          ~label:"abi_decode"
          ~contract:"AbiDecode"
          evm_version;
      constructor_arg = None;
      calls = [("test()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "abi_encode.sol")
          ~label:"abi_encode"
          ~contract:"AbiEncode"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "test(address,address,uint256)",
            [
              "0x0000000000000000000000000000000000000001";
              "0x0000000000000000000000000000000000000002";
              "1000";
            ] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "array.sol")
          ~label:"array"
          ~contract:"Array"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("push(uint256)", ["1"]);
          ("push(uint256)", ["42"]);
          ("push(uint256)", ["100"]);
          ("get(uint256)", ["0"]);
          ("get(uint256)", ["1"]);
          ("getLength()", []);
          ("getArr()", []);
          ("remove(uint256)", ["1"]);
          ("pop()", []);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "assembly_error.sol")
          ~label:"assembly_error"
          ~contract:"AssemblyError"
          evm_version;
      constructor_arg = None;
      calls = [("yul_revert(uint256)", ["5"]); ("yul_revert(uint256)", ["11"])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "assembly_loop.sol")
          ~label:"assembly_loop"
          ~contract:"AssemblyLoop"
          evm_version;
      constructor_arg = None;
      calls = [("yul_for_loop()", []); ("yul_while_loop()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "assembly_variable.sol")
          ~label:"assembly_variable"
          ~contract:"AssemblyVariable"
          evm_version;
      constructor_arg = None;
      calls = [("yul_let()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "binary_exponentiation.sol")
          ~label:"binary_exponentiation"
          ~contract:"AssemblyBinExp"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("rpow(uint256,uint256,uint256)", ["0"; "0"; "10"]);
          ("rpow(uint256,uint256,uint256)", ["0"; "3"; "10"]);
          ("rpow(uint256,uint256,uint256)", ["2"; "5"; "10"]);
          ("rpow(uint256,uint256,uint256)", ["5"; "3"; "1000"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "bitwise_op.sol")
          ~label:"bitwise_op"
          ~contract:"BitwiseOps"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("and(uint256,uint256)", ["14"; "11"]);
          ("or(uint256,uint256)", ["12"; "9"]);
          ("xor(uint256,uint256)", ["12"; "5"]);
          ("not(uint8)", ["12"]);
          ("shiftLeft(uint256,uint256)", ["3"; "2"]);
          ("shiftRight(uint256,uint256)", ["12"; "1"]);
          ("getLastNBits(uint256,uint256)", ["13"; "3"]);
          ("getLastNBitsUsingMod(uint256,uint256)", ["13"; "3"]);
          ("mostSignificantBit(uint256)", ["12"]);
          ("getFirstNBits(uint256,uint256,uint256)", ["14"; "2"; "4"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "bytecode_contract.sol")
          ~label:"bytecode_factory"
          ~contract:"Factory"
          evm_version;
      constructor_arg = None;
      calls = [("deploy()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "counter.sol")
          ~label:"counter"
          ~contract:"Counter"
          evm_version;
      constructor_arg = None;
      calls = [("inc()", []); ("dec()", []); ("get()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "create_contract.sol")
          ~label:"car_factory"
          ~contract:"CarFactory"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "create(address,string)",
            ["0x0000000000000000000000000000000000000001"; "Renault"] );
          ( "createAndSendEther(address,string)",
            ["0x0000000000000000000000000000000000000001"; "Renault"] );
          ( "create2(address,string,bytes32)",
            [
              "0x0000000000000000000000000000000000000001";
              "Renault";
              "0x0000000000000000000000000000000000000000000000000000000000000042";
            ] );
          ( "create2AndSendEther(address,string,bytes32)",
            [
              "0x0000000000000000000000000000000000000001";
              "Renault";
              "0x0000000000000000000000000000000000000000000000000000000000000042";
            ] );
          ("getCar(uint256)", ["0"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "create2.sol")
          ~label:"create2"
          ~contract:"Factory"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "deploy(address,uint256,bytes32)",
            [
              "0x0000000000000000000000000000000000000001";
              "42";
              "0x0000000000000000000000000000000000000000000000000000000000000042";
            ] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "create2.sol")
          ~label:"create2_assembly"
          ~contract:"FactoryAssembly"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "getBytecode(address,uint256)",
            ["0x0000000000000000000000000000000000000001"; "42"] );
          ( "getAddress(bytes,uint256)",
            [
              "0x608060405234801561001057600080fd5b5060405161010038038061010083398101604081905261002f91610036565b6001600081905550506100dc806100486000396000f3fe608060405260043610601f5760003560e01c8063c6052f4a146024578063fc0c546a14603e575b600080fd5b602a6048565b604051604591906075565b60405180910390f35b60446050565b005b60008054905090565b6000813590506073816090565b92915050565b600060208284031215608857600080fd5b60006094848285016062565b9150509291505056fea26469706673582212200c1a5e8438d75511336f3c02120c44f4b2dd2bc197f2155c996b3207b50273d264736f6c63430008140033";
              "66";
            ] );
          ( "deploy(bytes,uint256)",
            [
              "0x608060405234801561001057600080fd5b5060405161010038038061010083398101604081905261002f91610036565b6001600081905550506100dc806100486000396000f3fe608060405260043610601f5760003560e01c8063c6052f4a146024578063fc0c546a14603e575b600080fd5b602a6048565b604051604591906075565b60405180910390f35b60446050565b005b60008054905090565b6000813590506073816090565b92915050565b600060208284031215608857600080fd5b60006094848285016062565b9150509291505056fea26469706673582212200c1a5e8438d75511336f3c02120c44f4b2dd2bc197f2155c996b3207b50273d264736f6c63430008140033";
              "66";
            ] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "delegatecall.sol")
          ~label:"delegatecall"
          ~contract:"Delegator"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "setVars(address,uint256)",
            ["0x0000000000000000000000000000000000000001"; "42"] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "enum.sol")
          ~label:"enum"
          ~contract:"Enum"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("get()", []);
          ("set(uint8)", ["1"]);
          ("set(uint8)", ["3"]);
          ("cancel()", []);
          ("reset()", []);
          ("get()", []);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "erc721.sol")
          ~label:"erc721"
          ~contract:"MyNFT"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "mint(address,uint256)",
            ["0x0000000000000000000000000000000000000001"; "1"] );
          ( "approve(address,uint256)",
            ["0x0000000000000000000000000000000000000002"; "1"] );
          ( "transferFrom(address,address,uint256)",
            [
              "0x0000000000000000000000000000000000000001";
              "0x0000000000000000000000000000000000000002";
              "1";
            ] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "ether_wallet.sol")
          ~label:"ether_wallet"
          ~contract:"EtherWallet"
          evm_version;
      constructor_arg = None;
      calls = [("getBalance()", []); ("withdraw(uint256)", ["1"])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "event.sol")
          ~label:"event"
          ~contract:"Event"
          evm_version;
      constructor_arg = None;
      calls = [("test()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "fallback.sol")
          ~label:"fallback"
          ~contract:"Fallback"
          evm_version;
      constructor_arg = None;
      calls = [("getBalance()", [])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "function_modifier.sol")
          ~label:"function_modifier"
          ~contract:"FunctionModifier"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "changeOwner(address)",
            ["0x0000000000000000000000000000000000000002"] );
          ("decrement(uint256)", ["3"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "function_selector.sol")
          ~label:"function_selector"
          ~contract:"FunctionSelector"
          evm_version;
      constructor_arg = None;
      calls = [("getSelector(string)", ["\"transfer(address,uint256)\""])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "immutable.sol")
          ~label:"immutable"
          ~contract:"Immutable"
          evm_version;
      constructor_arg = Some "[\"123\"]";
      calls = [("getMyAddress()", []); ("getMyUint()", [])];
    };
    (* {
         contract =
           Solidity_contracts.compile_contract
             ~source:(Filename.concat solidity_basics "iterable_map.sol")
             ~label:"iterable_map"
             ~contract:"TestIterableMap"
             evm_version;
         constructor_arg = None;
         calls = [("testIterableMap()", [])];
       }; *)
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "merkle_tree.sol")
          ~label:"merkle_proof"
          ~contract:"TestMerkleProof"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("getRoot()", []);
          ( "verify(bytes32[],bytes32,bytes32,uint256)",
            [
              "[0x8da9e1c820f9dbd1589fd6585872bc1063588625729e7ab0797cfc63a00bd950,0x995788ffc103b987ad50f5e5707fd094419eb12d9552cc423bd0cd86a3861433]";
              "0xcc086fcc038189b4641db2cc4f1de3bb132aefbd65d510d817591550937818c7";
              "0xdca3326ad7e8121bf9cf9c12333e6b2271abe823ec9edfe42f813b1e768fa57b";
              "2";
            ] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "minimal_proxy.sol")
          ~label:"minimal_proxy"
          ~contract:"MinimalProxy"
          evm_version;
      constructor_arg = None;
      calls =
        [("clone(address)", ["0x0000000000000000000000000000000000000001"])];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "multi_sig_wallet.sol")
          ~label:"multi_sig_wallet"
          ~contract:"MultiSigWallet"
          evm_version;
      constructor_arg = Some (Printf.sprintf "[[\"%s\",\"%s\"],2]" addr1 addr2);
      calls =
        [
          ("getOwners()", []);
          ("getTransactionCount()", []);
          ("submitTransaction(address,uint256,bytes)", [addr1; "0"; "0x"]);
          ("confirmTransaction(uint256)", ["0"]);
          ("revokeConfirmation(uint256)", ["0"]);
          ("getTransaction(uint256)", ["0"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "payable.sol")
          ~label:"payable"
          ~contract:"Payable"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("deposit()", []);
          ("notPayable()", []);
          ("withdraw()", []);
          ( "transfer(address,uint256)",
            ["0x0000000000000000000000000000000000000001"; "1"] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "send_ether.sol")
          ~label:"send_ether"
          ~contract:"SendEther"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "sendViaTransfer(address)",
            ["0x0000000000000000000000000000000000000001"] );
          ( "sendViaSend(address)",
            ["0x0000000000000000000000000000000000000001"] );
          ( "sendViaCall(address)",
            ["0x0000000000000000000000000000000000000001"] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "send_to_fallback.sol")
          ~label:"send_to_fallback"
          ~contract:"SendToFallback"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "transferToFallback(address)",
            ["0x0000000000000000000000000000000000000001"] );
          ( "callFallback(address)",
            ["0x0000000000000000000000000000000000000001"] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "struct.sol")
          ~label:"struct"
          ~contract:"Todos"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("create(string)", ["\"Write tests\""]);
          ("get(uint256)", ["0"]);
          ("updateText(uint256,string)", ["0"; "\"Finish docs\""]);
          ("toggleCompleted(uint256)", ["0"]);
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "upgradeable_proxy.sol")
          ~label:"upgradeable_proxy"
          ~contract:"ProxyAdmin"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ( "getProxyAdmin(address)",
            ["0x0000000000000000000000000000000000000001"] );
          ( "getProxyImplementation(address)",
            ["0x0000000000000000000000000000000000000001"] );
        ];
    };
    {
      contract =
        Solidity_contracts.compile_contract
          ~source:(Filename.concat solidity_basics "upgradeable_proxy.sol")
          ~label:"upgradeable_proxy_workflow"
          ~contract:"ProxyAdmin"
          evm_version;
      constructor_arg = None;
      calls =
        [
          ("upgrade(address,address)", [addr1; addr2]);
          ("getProxyImplementation(address)", [addr1]);
          ("getProxyAdmin(address)", [addr1]);
          ( "upgrade(address,address)",
            [addr1; "0x0000000000000000000000000000000000000003"] );
          ("getProxyImplementation(address)", [addr1]);
        ];
    };
  ]
