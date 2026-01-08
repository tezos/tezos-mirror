// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Definitions for the TZT runner.

mod expectation;

use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt;
use typed_arena::Arena;

use crate::ast::big_map::{BigMapId, InMemoryLazyStorage, MapInfo};
use crate::ast::michelson_address::AddressHash;
use crate::ast::*;
use crate::context::*;
use crate::gas::{self, Gas};
use crate::interpreter::*;
use crate::irrefutable_match::irrefutable_match;
use crate::lexer::Prim;
use crate::parser::spanned_lexer;
use crate::parser::Parser;
use crate::stack::*;
use crate::syntax::tztTestEntitiesParser;
use crate::typechecker::*;
use crate::tzt::expectation::*;

/// Test's input stack represented as a [Vec] of pairs of type and typechecked
/// value. The top of the stack is the _leftmost_ element.
pub type TestStack<'a> = Vec<(Type, TypedValue<'a>)>;

/// The TZT execution didn't succeed, the expectation is not fulfilled.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TztTestError<'a> {
    /// Expected and actual output stacks don't match.
    StackMismatch(
        (FailingTypeStack, IStack<'a>),
        (FailingTypeStack, IStack<'a>),
    ),
    /// An error happened, when the test expected a success.
    UnexpectedError(TestError<'a>),
    /// Execution completed successfully, when the test expected an error.
    UnexpectedSuccess(ErrorExpectation<'a>, IStack<'a>),
    /// Expected one error, but got another.
    ExpectedDifferentError(Box<(ErrorExpectation<'a>, TestError<'a>)>),
}

impl fmt::Display for TztTestError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TztTestError::*;
        match self {
            StackMismatch(e, r) => {
                write!(f, "Stack mismatch: Expected {e:?}, Real {r:?}")
            }
            UnexpectedError(e) => {
                write!(f, "Unexpected error during test code execution: {e}")
            }
            UnexpectedSuccess(e, stk) => {
                write!(
                    f,
                    "Expected an error but none occurred. Expected {e} but ended with stack {stk:?}."
                )
            }
            ExpectedDifferentError(box_tuple) => {
                let (e, r) = &**box_tuple;
                write!(
                    f,
                    "Expected an error but got a different one.\n expected: {e}\n got: {r}."
                )
            }
        }
    }
}

/// Represent one Tzt test.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TztTest<'a> {
    /// Test code, the content of the `code` field.
    pub code: Micheline<'a>,
    /// Test input, as defined by the `input` field.
    pub input: TestStack<'a>,
    /// Expected output, as defined by the `output` field.
    pub output: TestExpectation<'a>,
    /// Transfer amount, as defined by the `amount` field.
    pub amount: Option<i64>,
    /// Contract balance, as defined by the `balance` field.
    pub balance: Option<i64>,
    /// Current chain identifier, as defined by the `chain_id` field.
    pub chain_id: Option<ChainId>,
    /// Self parameter entrypoints, as defined by the type in the `parameter`
    /// field.
    pub parameter: Option<Entrypoints>,
    /// Self address, as defined by the `self` field.
    pub self_addr: Option<AddressHash>,
    /// Other known contracts, as defined by `other_contracts` field.
    pub other_contracts: Option<HashMap<AddressHash, Entrypoints>>,
    /// mapping between integers representing big_map indices and descriptions of big maps
    /// as defined by the `big_maps` field.
    pub big_maps: Option<InMemoryLazyStorage<'a>>,
    /// mapping from address to storage value
    pub storages: Option<HashMap<AddressHash, (Type, TypedValue<'a>)>>,
    /// Initial value for "now" in the context.
    pub now: Option<BigInt>,
    /// Address that directly or indirectly initiated the current transaction
    pub source: Option<PublicKeyHash>,
    /// Address that directly initiated the current transaction
    pub sender: Option<AddressHash>,
    /// Views in test
    pub views: Option<HashMap<AddressHash, HashMap<String, View<'a>>>>,
}

fn populate_ctx_with_known_contracts(
    _ctx: &mut Ctx,
    self_param: Option<(AddressHash, Option<Entrypoints>)>,
    m_other_contracts: Option<HashMap<AddressHash, Entrypoints>>,
) {
    // If other_contracts is not provided, then initialize with empty map,
    // or else initialize with the provided list of known contracts.
    let mut known_contracts = m_other_contracts.unwrap_or_default();

    // If self address is provided, include that to the list of known contracts as well.
    // Use a default type of Unit, if parameter type is not provided.
    match self_param {
        None => {}
        Some((ah, eps)) => {
            known_contracts.insert(
                ah,
                eps.unwrap_or(HashMap::from([(Entrypoint::default(), Type::Unit)])),
            );
        }
    }

    // Set known contracts in context.
    _ctx.set_known_contracts(known_contracts);
}

fn typecheck_stack<'a>(
    stk: Vec<(Micheline<'a>, Micheline<'a>)>,
    self_param: Option<(AddressHash, Option<Entrypoints>)>,
    m_other_contracts: Option<HashMap<AddressHash, Entrypoints>>,
    m_big_maps: Option<InMemoryLazyStorage<'a>>,
) -> Result<Vec<(Type, TypedValue<'a>)>, TcError> {
    let mut ctx = Ctx::default();
    populate_ctx_with_known_contracts(&mut ctx, self_param, m_other_contracts);
    ctx.set_big_map_storage(m_big_maps.unwrap_or_default());

    stk.into_iter()
        .map(|(t, v)| {
            let t = parse_ty(ctx.gas(), &t)?;
            let tc_val = typecheck_value(&v, &mut ctx, &t)?;
            Ok((t, tc_val))
        })
        .collect()
}

impl<'a> Parser<'a> {
    /// Parse top-level definition of a TZT test.
    pub fn parse_tzt_test(&'a self, src: &'a str) -> Result<TztTest<'a>, Box<dyn Error + 'a>> {
        tztTestEntitiesParser::new()
            .parse(&self.arena, spanned_lexer(src))?
            .try_into()
    }
}

// Check if the option argument value is none, and raise an error if it is not.
// If it is none, then fill it with the provided value.
fn set_tzt_field<T>(field_name: &str, t: &mut Option<T>, v: T) -> Result<(), String> {
    match t {
        Some(_) => Err(format!("Duplicate field '{field_name}' in test")),
        None => {
            *t = Some(v);
            Ok(())
        }
    }
}

use std::error::Error;
impl<'a> TryFrom<Vec<TztEntity<'a>>> for TztTest<'a> {
    type Error = Box<dyn Error>;
    fn try_from(tzt: Vec<TztEntity<'a>>) -> Result<Self, Self::Error> {
        use TestExpectation::*;
        use TztEntity::*;
        use TztOutput::*;
        let mut m_code: Option<Micheline> = None;
        let mut m_amount: Option<i64> = None;
        let mut m_balance: Option<i64> = None;
        let mut m_chain_id: Option<Micheline> = None;
        let mut m_parameter: Option<Micheline> = None;
        let mut m_self: Option<Micheline> = None;
        let mut m_other_contracts: Option<Vec<(Micheline, Micheline)>> = None;
        let mut m_big_maps: Option<Vec<(Micheline, Micheline, Micheline, Micheline)>> = None;
        let mut m_now: Option<BigInt> = None;
        let mut m_source: Option<Micheline> = None;
        let mut m_sender: Option<Micheline> = None;
        let mut m_storages: Option<Vec<(Micheline, Micheline, Micheline)>> = None;
        let mut m_views: Option<Vec<(micheline::Micheline<'_>, Vec<RawNamedView<'_>>)>> = None;

        // This would hold the untypechecked, expected output value. This is because If the self
        // and parameters values are specified, then we need to fetch them and populate the context
        // first, before we can typecheck output stack using it and properly construct the typed
        // output stack expectation.
        let mut m_output_inter: Option<TztOutput> = None;

        let mut input_stk_backup: Option<Vec<(Micheline, Micheline)>> = None;

        for e in tzt {
            match e {
                Code(ib) => set_tzt_field("code", &mut m_code, ib)?,
                Input(stk) => {
                    // Save input to treat it last, after we have self_param and other_contracts
                    if input_stk_backup.is_some() {
                        return Err("Duplicate field 'input' in test".into());
                    }
                    input_stk_backup = Some(stk);
                }
                Output(tzt_output) => set_tzt_field("output", &mut m_output_inter, tzt_output)?,
                Amount(m) => set_tzt_field("amount", &mut m_amount, m)?,
                Balance(m) => set_tzt_field("balance", &mut m_balance, m)?,
                ChainId(id) => set_tzt_field("chain_id", &mut m_chain_id, id)?,
                Parameter(ty) => set_tzt_field("parameter", &mut m_parameter, ty)?,
                SelfAddr(v) => set_tzt_field("self", &mut m_self, v)?,
                OtherContracts(v) => set_tzt_field("other_contracts", &mut m_other_contracts, v)?,
                Now(n) => set_tzt_field("now", &mut m_now, n.into())?,
                Source(v) => set_tzt_field("source", &mut m_source, v)?,
                SenderAddr(v) => set_tzt_field("sender", &mut m_sender, v)?,
                BigMaps(v) => set_tzt_field("big_maps", &mut m_big_maps, v)?,
                Storages(v) => set_tzt_field("storages", &mut m_storages, v)?,
                Views(v) => set_tzt_field("views", &mut m_views, v)?,
            }
        }

        // We process self address and parameter fields first because if specified, we need them to
        // populate the context for type checking the output stack.
        let self_addr = match m_self {
            Some(s) => Some(
                irrefutable_match!(
                typecheck_value(&s, &mut Ctx::default(), &Type::Address)?;
                TypedValue::Address
                )
                .hash,
            ),
            None => None,
        };

        let parameter = match m_parameter {
            Some(p) => Some(p.get_entrypoints(&mut Gas::default())?),
            None => None,
        };

        let source = match m_source {
            Some(s) => Some(irrefutable_match!(
            typecheck_value(&s, &mut Ctx::default(), &Type::KeyHash)?;
            TypedValue::KeyHash
            )),
            None => None,
        };

        let sender = match m_sender {
            Some(s) => Some(
                irrefutable_match!(
                typecheck_value(&s, &mut Ctx::default(), &Type::Address)?;
                TypedValue::Address
                )
                .hash,
            ),
            None => None,
        };

        let other_contracts = match m_other_contracts {
            Some(oc) => {
                let mut a = HashMap::new();
                for (ahm, ctm) in oc {
                    let address_hash = irrefutable_match!(
                        typecheck_value(&ahm, &mut Ctx::default(), &Type::Address)?;
                        TypedValue::Address)
                    .hash;
                    let entrypoints = ctm.get_entrypoints(&mut Gas::default()).unwrap();
                    match a.get(&address_hash) {
                        None => {
                            a.insert(address_hash, entrypoints);
                        }
                        Some(_) => {
                            return Err("Address cannot repeat in 'other_contracts'".into());
                        }
                    }
                }
                Some(a)
            }
            None => None,
        };

        let big_maps = match m_big_maps {
            Some(bm) => {
                let mut a = BTreeMap::new();
                for (idx, key_ty, val_ty, elts) in bm {
                    let idx: BigMapId = irrefutable_match!(
                        typecheck_value(&idx, &mut Ctx::default(), &Type::Int)?;
                        TypedValue::Int)
                    .clone()
                    .into();
                    let key_ty = parse_ty(&mut Gas::default(), &key_ty)?;
                    let val_ty = parse_ty(&mut Gas::default(), &val_ty)?;
                    let elts = match elts {
                        Micheline::Seq(elts) => elts,
                        _ => return Err("Big map elements must be a sequence".into()),
                    };
                    let descr: BTreeMap<TypedValue<'a>, TypedValue<'a>> = elts
                        .iter()
                        .map(|elt| {
                            match elt {
                                // If Micheline::App stores its arguments in a Vec,
                                // pattern match with a condition to ensure length is 2
                                Micheline::App(Prim::Elt, kv, _) if kv.len() == 2 => {
                                    let (k_raw, v_raw) = (&kv[0], &kv[1]);
                                    let k = typecheck_value(k_raw, &mut Ctx::default(), &key_ty)
                                        .unwrap();
                                    let v = typecheck_value(v_raw, &mut Ctx::default(), &val_ty)
                                        .unwrap();
                                    Ok((k, v))
                                }
                                _ => Err(
                                    "Each big map element must be of the form `Elt <key> <value>`.",
                                ),
                            }
                        })
                        .collect::<Result<BTreeMap<_, _>, _>>()?;

                    a.insert(idx, MapInfo::new(descr, key_ty, val_ty));
                }
                let storage = InMemoryLazyStorage::with_big_maps(a);
                Some(storage)
            }
            None => None,
        };

        // Once we have self_addr and parameter, we typecheck the output stack
        // after populating the context's known_contracts with the self address.
        // Later we may add the Tzt specs `other_contracts` fields. At that point
        // we ll have to include those contracts here as well.
        let m_output = match m_output_inter {
            Some(x) => match x {
                TztSuccess(stk) => Some(ExpectSuccess(typecheck_stack(
                    stk,
                    self_addr.clone().map(|x| (x, parameter.clone())),
                    other_contracts.clone(),
                    big_maps.clone(),
                )?)),
                TztError(error_exp) => Some(ExpectError(error_exp)),
            },
            None => None,
        };

        // Now we can set the input stack.
        let m_input = match input_stk_backup {
            Some(stk) => Some(typecheck_stack(
                stk,
                self_addr.clone().map(|x| (x, parameter.clone())),
                other_contracts.clone(),
                big_maps.clone(),
            )?),
            None => None,
        };

        let storages = match m_storages {
            Some(tzt_storages) => {
                let tzt_storages: HashMap<AddressHash, (Type, TypedValue)> = tzt_storages
                    .into_iter()
                    .map(|(address_raw, storage_type_raw, storage_raw)| {
                        let typed_address =
                            typecheck_value(&address_raw, &mut Ctx::default(), &Type::Address)?;
                        let storage_type = parse_ty(Ctx::default().gas(), &storage_type_raw)?;
                        let storage =
                            typecheck_value(&storage_raw, &mut Ctx::default(), &storage_type)?;

                        let address = match typed_address {
                            TypedValue::Address(Address {
                                hash,
                                entrypoint: _,
                            }) => hash,
                            _ => return Err("Invalid address for storage".into()),
                        };

                        Ok((address, (storage_type, storage)))
                    })
                    .collect::<Result<HashMap<_, _>, Self::Error>>()?;

                Some(tzt_storages)
            }
            None => None,
        };

        let views = match m_views {
            Some(tzt_views) => {
                let tzt_views: HashMap<AddressHash, HashMap<String, View>> = tzt_views
                    .into_iter()
                    .map(|(address_raw, views)| {
                        let typed_address =
                            typecheck_value(&address_raw, &mut Ctx::default(), &Type::Address)?;
                        let views: HashMap<String, View> = views
                            .into_iter()
                            .map(|(name_raw, arg_type_raw, return_type_raw, code)| {
                                let name = match name_raw {
                                    Micheline::String(name) => name,
                                    _ => return Err("View does not have a valid name.".into()),
                                };

                                let input_type = parse_ty(Ctx::default().gas(), &arg_type_raw)?;
                                let output_type = parse_ty(Ctx::default().gas(), &return_type_raw)?;
                                Ok((
                                    name,
                                    View {
                                        input_type,
                                        output_type,
                                        code,
                                    },
                                ))
                            })
                            .collect::<Result<HashMap<_, _>, Self::Error>>()?;

                        let address = match typed_address {
                            TypedValue::Address(Address {
                                hash,
                                entrypoint: _,
                            }) => hash,
                            _ => return Err("Invalid address for view".into()),
                        };

                        Ok((address, views))
                    })
                    .collect::<Result<HashMap<_, _>, Self::Error>>()?;

                Some(tzt_views)
            }
            None => None,
        };

        if let Some(v) = views.clone() {
            if let Some(s) = storages.clone() {
                for view_key in v.keys() {
                    if !s.contains_key(view_key) {
                        return Err(format!(
                            "The {view_key:?} appears in `views` but not in `storages`."
                        ))?;
                    }
                }
            } else {
                return Err("The `storages` tzt primitive is missing but mandatory when using the `views` tzt primitive.".to_owned())?;
            }
        }

        Ok(TztTest {
            code: m_code.ok_or("code section not found in test")?,
            input: m_input.ok_or("input section not found in test")?,
            output: m_output.ok_or("output section not found in test")?,
            amount: m_amount,
            balance: m_balance,
            chain_id: m_chain_id
                .map(|v| {
                    Ok::<_, TcError>(irrefutable_match!(
                    typecheck_value(&v, &mut Ctx::default(), &Type::ChainId)?;
                    TypedValue::ChainId
                    ))
                })
                .transpose()?,
            parameter,
            self_addr,
            other_contracts,
            big_maps,
            storages,
            views,
            now: m_now,
            source,
            sender,
        })
    }
}

/// This represents possibilities in which the execution of
/// the code in a test can fail.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum TestError<'a> {
    /// Error happened during typechecking.
    #[error(transparent)]
    TypecheckerError(#[from] TcError),
    /// Error happened during interpretation.
    #[error(transparent)]
    InterpreterError(InterpretError<'a>),
}

impl<'a> From<InterpretError<'a>> for TestError<'a> {
    fn from(x: InterpretError<'a>) -> Self {
        Self::InterpreterError(x)
    }
}

/// This represents the outcome that we expect from interpreting
/// the code in a test.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TestExpectation<'a> {
    /// Expecting the test code to finish with the given output stack.
    ExpectSuccess(TestStack<'a>),
    /// Expecting the test code to fail with the given error.
    ExpectError(ErrorExpectation<'a>),
}

/// Expected test error.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorExpectation<'a> {
    /// Typechecker error, with an optional string.
    TypecheckerError(Option<String>),
    /// Interpreter error.
    InterpreterError(InterpreterErrorExpectation<'a>),
}

impl fmt::Display for ErrorExpectation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ErrorExpectation::*;
        match self {
            TypecheckerError(None) => write!(f, "some typechecker error"),
            TypecheckerError(Some(err)) => write!(f, "typechecker error: {err}"),
            InterpreterError(err) => write!(f, "interpreter error: {err}"),
        }
    }
}

/// Interpreter errors we can expect.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InterpreterErrorExpectation<'a> {
    /// GeneralOverflow error, which can happen with bit-shift arithmetic.
    GeneralOverflow(BigInt, BigInt),
    /// MutezOverflow error, which can happen with mutez arithmetic.
    MutezOverflow(i64, i64),
    /// Overflow error, which can happen with arithmetic operations.
    Overflow,
    /// OutOfGas error, which happens when execution runs out of gas.
    OutOfGas(gas::OutOfGas),
    /// FailedWith error, which happens when execution reaches `FAILWITH`
    /// instruction.
    FailedWith(Micheline<'a>),
}

impl fmt::Display for InterpreterErrorExpectation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use InterpreterErrorExpectation::*;
        match self {
            GeneralOverflow(a1, a2) => write!(f, "General Overflow {a1} {a2}"),
            Overflow => write!(f, "Overflow"),
            MutezOverflow(a1, a2) => write!(f, "MutezOverflow {a1} {a2}"),
            OutOfGas(_) => write!(f, "OutOfGas"),
            FailedWith(v) => write!(f, "FailedWith {v:?}"),
        }
    }
}

type RawNamedView<'a> = (Micheline<'a>, Micheline<'a>, Micheline<'a>, Micheline<'a>);

/// Helper type for use during parsing, represent a single
/// line from the test file.
pub(crate) enum TztEntity<'a> {
    Code(Micheline<'a>),
    Input(Vec<(Micheline<'a>, Micheline<'a>)>),
    Output(TztOutput<'a>),
    Amount(i64),
    Balance(i64),
    ChainId(Micheline<'a>),
    Parameter(Micheline<'a>),
    SelfAddr(Micheline<'a>),
    OtherContracts(Vec<(Micheline<'a>, Micheline<'a>)>),
    Now(i64),
    Source(Micheline<'a>),
    SenderAddr(Micheline<'a>),
    BigMaps(Vec<(Micheline<'a>, Micheline<'a>, Micheline<'a>, Micheline<'a>)>),
    Storages(Vec<(Micheline<'a>, Micheline<'a>, Micheline<'a>)>),
    Views(Vec<(Micheline<'a>, Vec<RawNamedView<'a>>)>),
}

/// Possible values for the "output" expectation field in a Tzt test. This is a
/// [Micheline] ("untyped") version of [TestExpectation].
pub(crate) enum TztOutput<'a> {
    /// Expecting the test code to finish with the given output stack.
    TztSuccess(Vec<(Micheline<'a>, Micheline<'a>)>),
    /// Expecting the test code to fail with the given error.
    TztError(ErrorExpectation<'a>),
}

fn execute_tzt_test_code<'a>(
    code: Micheline<'a>,
    ctx: &mut Ctx<'a>,
    arena: &'a Arena<Micheline<'a>>,
    m_parameter: Option<Entrypoints>,
    input: Vec<(Type, TypedValue<'a>)>,
) -> Result<(FailingTypeStack, IStack<'a>), TestError<'a>> {
    // Build initial stacks (type and value) for running the test from the test input
    // stack.
    let (typs, vals): (Vec<Type>, Vec<TypedValue>) = input.into_iter().unzip();

    let mut t_stack: FailingTypeStack = FailingTypeStack::Ok(TopIsFirst::from(typs).0);

    let parameter = m_parameter.unwrap_or(Entrypoints::from([(Entrypoint::default(), Type::Unit)]));

    // Run the code and save the status of the
    // final result as a Result<(), TestError>.
    //
    // This value along with the test expectation
    // from the test file will be used to decide if
    // the test was a success or a fail.
    let typechecked_code = typecheck_instruction(&code, ctx.gas(), Some(&parameter), &mut t_stack)?;
    let mut i_stack: IStack = TopIsFirst::from(vals).0;
    typechecked_code.interpret(ctx, arena, &mut i_stack)?;
    Ok((t_stack, i_stack))
}

/// Run a [TztTest]. If the test is successful, the result is `Ok(())`.
/// Otherwise, it returns [TztTestError]. An [Arena] must be supplied, it will
/// be used for storing the results of `UNPACK`, which may end up as part of the
/// error.
pub fn run_tzt_test<'a>(
    test: TztTest<'a>,
    arena: &'a Arena<Micheline<'a>>,
) -> Result<(), TztTestError<'a>> {
    // Here we compare the outcome of the interpreting with the
    // expectation from the test, and declare the result of the test
    // accordingly.
    let mut ctx = Ctx::default();
    ctx.gas = Gas::default();
    ctx.amount = test.amount.unwrap_or_default();
    ctx.balance = test.balance.unwrap_or_default();
    ctx.chain_id = test.chain_id.unwrap_or(Ctx::default().chain_id);
    ctx.self_address = test
        .self_addr
        .clone()
        .unwrap_or(Ctx::default().self_address);
    ctx.source = test.source.clone().unwrap_or(Ctx::default().source);
    ctx.sender = test.sender.clone().unwrap_or(Ctx::default().sender);

    populate_ctx_with_known_contracts(
        &mut ctx,
        test.self_addr.clone().map(|x| (x, test.parameter.clone())),
        test.other_contracts.clone(),
    );

    ctx.storage = test.storages.unwrap_or_default();

    ctx.views = test.views.unwrap_or_default();

    ctx.set_big_map_storage(test.big_maps.unwrap_or_default());

    ctx.now = test.now.clone().unwrap_or(Ctx::default().now);

    let execution_result =
        execute_tzt_test_code(test.code, &mut ctx, arena, test.parameter, test.input);
    check_expectation(&mut ctx, test.output, execution_result)
}
