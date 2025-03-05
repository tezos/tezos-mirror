// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::error::Error;
use std::path::Path;

use base64::Engine;
use base64::engine::general_purpose::URL_SAFE;
use bincode::config::Configuration;
use bincode::config::Fixint;
use bincode::config::LittleEndian;
use http::HeaderMap;
use http::Method;
use http::Uri;
use jstz_crypto::hash::Hash;
use jstz_crypto::keypair_from_passphrase;
use jstz_crypto::public_key::PublicKey;
use jstz_crypto::public_key_hash::PublicKeyHash;
use jstz_crypto::secret_key::SecretKey;
use jstz_crypto::smart_function_hash::SmartFunctionHash;
use jstz_proto::context::account::Address;
use jstz_proto::context::account::Addressable;
use jstz_proto::context::account::Nonce;
use jstz_proto::context::account::ParsedCode;
use jstz_proto::operation::Content;
use jstz_proto::operation::DeployFunction;
use jstz_proto::operation::Operation;
use jstz_proto::operation::RunFunction;
use jstz_proto::operation::SignedOperation;
use serde::Serialize;
use serde::Serializer;
use tezos_data_encoding::enc::BinWriter;
use tezos_smart_rollup::inbox::ExternalMessageFrame;
use tezos_smart_rollup::types::SmartRollupAddress;
use tezos_smart_rollup::utils::inbox::file::InboxFile;
use tezos_smart_rollup::utils::inbox::file::Message;

const BINCODE_CONFIGURATION: Configuration<LittleEndian, Fixint> = bincode::config::legacy();

const FA2: &str = include_str!("../../fa2.js");

const DEFAULT_GAS_LIMIT: u32 = 100_000;

// tag + 20 byte address
const EXTERNAL_FRAME_SIZE: usize = 21;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

/// Generate the requested 'FA2 transfers', writing to `./inbox.json`.
///
/// This includes setup (contract deployment/minting) as well as balance checks at the end.
/// The transfers are generated with a 'follow on' strategy. For example 'account 0' will
/// have `num_accounts` minted of 'token 0'. It will then transfer all of them to 'account 1',
/// which will transfer `num_accounts - 1` to the next account, etc.
pub fn handle_generate(rollup_addr: &str, inbox_file: &Path, transfers: usize) -> Result<()> {
    let inbox = generate_inbox(rollup_addr, transfers)?;
    inbox.save(inbox_file)?;
    Ok(())
}

/// Like [`handle_generate`] but writes the inbox as a shell script.
pub fn handle_generate_script(
    rollup_addr: &str,
    script_file: &Path,
    transfers: usize,
) -> Result<()> {
    let inbox = generate_inbox(rollup_addr, transfers)?;
    inbox.save_script(script_file)?;
    Ok(())
}

/// Generate the inbox for the given rollup address and number of transfers.
fn generate_inbox(rollup_addr: &str, transfers: usize) -> Result<InboxFile> {
    let rollup_addr = SmartRollupAddress::from_b58check(rollup_addr)?;

    let accounts = accounts_for_transfers(transfers);

    if accounts == 0 {
        return Err("--transfers must be greater than zero".into());
    }

    let mut accounts = gen_keys(accounts)?;

    // Part 1 - setup
    let (fa2_address, deploy) = deploy_fa2(rollup_addr.clone(), accounts.first_mut().unwrap())?;
    let batch_mint = batch_mint(rollup_addr.clone(), &mut accounts, &fa2_address)?;

    let mut messages = vec![deploy, batch_mint];

    // Part 2 - transfers
    let len = accounts.len();
    let expected_len = messages.len() + transfers;

    'outer: for token_id in 0..len {
        for (from, amount) in (token_id..(token_id + len)).zip(1..len) {
            if expected_len == messages.len() {
                break 'outer;
            }

            let to = accounts[(from + 1) % len].address.clone();
            let transfer = Transfer {
                token_id,
                amount: len - amount,
                to,
            };

            let account = &mut accounts[from % len];
            let op = transfer_op(rollup_addr.clone(), account, &fa2_address, &transfer)?;

            messages.push(op);
        }
    }

    // Part 3 - checking
    let tokens = 0..accounts.len();
    for balance in accounts
        .iter_mut()
        .map(|a| balance(rollup_addr.clone(), a, &fa2_address, tokens.clone()))
    {
        messages.push(balance?);
    }

    // Output inbox file
    let inbox = InboxFile(vec![messages]);
    Ok(inbox)
}

#[derive(Debug, Serialize)]
struct MintNew<'a> {
    token_id: usize,
    #[serde(serialize_with = "address_ser")]
    owner: &'a Address,
    amount: usize,
}

#[derive(Debug, Serialize)]
struct BalanceRequest<'a> {
    token_id: usize,
    #[serde(serialize_with = "address_ser")]
    owner: &'a Address,
}

#[derive(Debug, Serialize)]
struct Transfer {
    token_id: usize,
    amount: usize,
    #[serde(serialize_with = "address_ser")]
    to: Address,
}

#[derive(Debug, Serialize)]
struct TransferToken<'a> {
    #[serde(serialize_with = "address_ser")]
    from: &'a Address,
    transfers: &'a [&'a Transfer],
}

fn address_ser<S>(address: &Address, ser: S) -> std::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let address = address.to_base58();
    String::serialize(&address, ser)
}

fn transfer_op(
    rollup_addr: SmartRollupAddress,
    account: &mut Account,
    fa2: &Address,
    transfer: &Transfer,
) -> Result<Message> {
    let transfer = [TransferToken {
        from: &account.address,
        transfers: &[transfer],
    }];

    let body = serde_json::ser::to_vec(&transfer)?;

    let content = Content::RunFunction(RunFunction {
        uri: Uri::try_from(format!("tezos://{fa2}/transfer"))?,
        method: Method::POST,
        headers: HeaderMap::default(),
        body: Some(body),
        gas_limit: DEFAULT_GAS_LIMIT.try_into()?,
    });

    account.operation_to_message(rollup_addr, content)
}

fn balance(
    rollup_addr: SmartRollupAddress,
    account: &mut Account,
    fa2: &Address,
    tokens: std::ops::Range<usize>,
) -> Result<Message> {
    let reqs: Vec<_> = tokens
        .map(|i| BalanceRequest {
            owner: &account.address,
            token_id: i,
        })
        .collect();
    let query = serde_json::ser::to_vec(&reqs)?;
    let query = URL_SAFE.encode(query);

    let content = Content::RunFunction(RunFunction {
        uri: Uri::try_from(format!("tezos://{fa2}/balance_of?requests={query}"))?,
        method: Method::GET,
        headers: HeaderMap::default(),
        body: None,
        gas_limit: DEFAULT_GAS_LIMIT.try_into()?,
    });

    account.operation_to_message(rollup_addr, content)
}

fn batch_mint(
    rollup_addr: SmartRollupAddress,
    accounts: &mut [Account],
    fa2: &Address,
) -> Result<Message> {
    let amount = accounts.len() + 1;
    let mints: Vec<_> = accounts
        .iter()
        .enumerate()
        .map(|(i, a)| MintNew {
            token_id: i,
            owner: &a.address,
            amount,
        })
        .collect();

    let body = serde_json::ser::to_vec(&mints)?;
    let account = &mut accounts[0];

    let content = Content::RunFunction(RunFunction {
        uri: Uri::try_from(format!("tezos://{fa2}/mint_new"))?,
        method: Method::POST,
        headers: HeaderMap::default(),
        body: Some(body),
        gas_limit: DEFAULT_GAS_LIMIT.try_into()?,
    });

    account.operation_to_message(rollup_addr, content)
}

fn deploy_fa2(
    rollup_addr: SmartRollupAddress,
    account: &mut Account,
) -> Result<(Address, Message)> {
    let code: ParsedCode = FA2.to_string().try_into()?;

    let address = Address::SmartFunction(SmartFunctionHash::digest(
        format!("{}{}{}", &account.address, code, account.nonce.next()).as_bytes(),
    )?);

    let content = Content::DeployFunction(DeployFunction {
        function_code: code,
        account_credit: 0,
    });

    let message = account.operation_to_message(rollup_addr, content)?;

    Ok((address, message))
}

fn gen_keys(num: usize) -> Result<Vec<Account>> {
    let mut res = Vec::with_capacity(num);

    for i in 0..num {
        let (sk, pk) = keypair_from_passphrase(&i.to_string())?;
        let address = Address::User(PublicKeyHash::from(&pk));
        let account = Account {
            address,
            sk,
            pk,
            nonce: Default::default(),
        };
        res.push(account)
    }

    Ok(res)
}

struct Account {
    nonce: Nonce,
    sk: SecretKey,
    pk: PublicKey,
    address: Address,
}

impl Account {
    fn operation_to_message(
        &mut self,
        rollup_addr: SmartRollupAddress,
        content: Content,
    ) -> Result<Message> {
        let Address::User(source) = &self.address else {
            return Err("Expected a user address".into());
        };

        let op = Operation {
            source: source.clone(),
            nonce: self.nonce,
            content,
        };

        let hash = op.hash();
        let signed_op = SignedOperation::new(self.pk.clone(), self.sk.sign(hash)?, op);

        let bytes = bincode::encode_to_vec(signed_op, BINCODE_CONFIGURATION)?;
        let mut external = Vec::with_capacity(bytes.len() + EXTERNAL_FRAME_SIZE);

        let frame = ExternalMessageFrame::Targetted {
            contents: bytes,
            address: rollup_addr,
        };

        frame.bin_write(&mut external)?;

        self.nonce = self.nonce.next();
        let message = Message::External { external };

        Ok(message)
    }
}

/// The generation strategy supports up to `num_accounts ^ 2` transfers,
/// find the smallest number of accounts which will allow for this.
fn accounts_for_transfers(transfers: usize) -> usize {
    f64::sqrt(transfers as f64).ceil() as usize + 1
}
