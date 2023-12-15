/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

mod expectation;

use num_bigint::BigInt;
use std::collections::HashMap;
use std::fmt;
use typed_arena::Arena;

use crate::ast::michelson_address::entrypoint::Entrypoints;
use crate::ast::michelson_address::AddressHash;
use crate::ast::*;
use crate::context::*;
use crate::interpreter::*;
use crate::irrefutable_match::irrefutable_match;
use crate::parser::spanned_lexer;
use crate::parser::Parser;
use crate::stack::*;
use crate::syntax::tztTestEntitiesParser;
use crate::typechecker::*;
use crate::tzt::expectation::*;

pub type TestStack<'a> = Vec<(Type, TypedValue<'a>)>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TztTestError<'a> {
    StackMismatch(
        (FailingTypeStack, IStack<'a>),
        (FailingTypeStack, IStack<'a>),
    ),
    UnexpectedError(TestError<'a>),
    UnexpectedSuccess(ErrorExpectation<'a>, IStack<'a>),
    ExpectedDifferentError(ErrorExpectation<'a>, TestError<'a>),
}

impl fmt::Display for TztTestError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TztTestError::*;
        match self {
            StackMismatch(e, r) => {
                write!(f, "Stack mismatch: Expected {:?}, Real {:?}", e, r)
            }
            UnexpectedError(e) => {
                write!(f, "Unexpected error during test code execution: {}", e)
            }
            UnexpectedSuccess(e, stk) => {
                write!(
                    f,
                    "Expected an error but none occured. Expected {} but ended with stack {:?}.",
                    e, stk
                )
            }
            ExpectedDifferentError(e, r) => {
                write!(
                    f,
                    "Expected an error but got a different one.\n expected: {}\n got: {}.",
                    e, r
                )
            }
        }
    }
}

/// Represent one Tzt test.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TztTest<'a> {
    pub code: Micheline<'a>,
    pub input: TestStack<'a>,
    pub output: TestExpectation<'a>,
    pub amount: Option<i64>,
    pub balance: Option<i64>,
    pub chain_id: Option<ChainId>,
    pub parameter: Option<Entrypoints>,
    pub self_addr: Option<AddressHash>,
    pub other_contracts: Option<HashMap<AddressHash, Entrypoints>>,
}

fn populate_ctx_with_known_contracts(
    ctx: &mut Ctx,
    self_param: Option<(AddressHash, Option<Entrypoints>)>,
    m_other_contracts: Option<HashMap<AddressHash, Entrypoints>>,
) {
    // If other_contracts is not provided, then initialize with empty map,
    // or else initialize with the provided list of known contracts.
    let mut known_contracts = m_other_contracts.unwrap_or(HashMap::new());

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
    ctx.set_known_contracts(known_contracts);
}

fn typecheck_stack<'a>(
    stk: Vec<(Micheline<'a>, Micheline<'a>)>,
    self_param: Option<(AddressHash, Option<Entrypoints>)>,
    m_other_contracts: Option<HashMap<AddressHash, Entrypoints>>,
) -> Result<Vec<(Type, TypedValue<'a>)>, TcError> {
    let mut ctx = Ctx::default();
    populate_ctx_with_known_contracts(&mut ctx, self_param, m_other_contracts);

    stk.into_iter()
        .map(|(t, v)| {
            let t = parse_ty(&mut ctx, &t)?;
            let tc_val = typecheck_value(&v, &mut ctx, &t)?;
            Ok((t, tc_val))
        })
        .collect()
}

impl<'a> Parser<'a> {
    pub fn parse_tzt_test(&'a self, src: &'a str) -> Result<TztTest, Box<dyn Error + '_>> {
        tztTestEntitiesParser::new()
            .parse(&self.arena, spanned_lexer(src))?
            .try_into()
    }
}

// Check if the option argument value is none, and raise an error if it is not.
// If it is none, then fill it with the provided value.
fn set_tzt_field<T>(field_name: &str, t: &mut Option<T>, v: T) -> Result<(), String> {
    match t {
        Some(_) => Err(format!("Duplicate field '{}' in test", field_name)),
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
        let mut m_input: Option<TestStack> = None;
        let mut m_amount: Option<i64> = None;
        let mut m_balance: Option<i64> = None;
        let mut m_chain_id: Option<Micheline> = None;
        let mut m_parameter: Option<Micheline> = None;
        let mut m_self: Option<Micheline> = None;
        let mut m_other_contracts: Option<Vec<(Micheline, Micheline)>> = None;

        // This would hold the untypechecked, expected output value. This is because If the self
        // and parameters values are specified, then we need to fetch them and populate the context
        // first, before we can typecheck output stack using it and properly construct the typed
        // output stack expectation.
        let mut m_output_inter: Option<TztOutput> = None;

        for e in tzt {
            match e {
                Code(ib) => set_tzt_field("code", &mut m_code, ib)?,
                Input(stk) => {
                    set_tzt_field("input", &mut m_input, typecheck_stack(stk, None, None)?)?
                }
                Output(tzt_output) => set_tzt_field("output", &mut m_output_inter, tzt_output)?,
                Amount(m) => set_tzt_field("amount", &mut m_amount, m)?,
                Balance(m) => set_tzt_field("balance", &mut m_balance, m)?,
                ChainId(id) => set_tzt_field("chain_id", &mut m_chain_id, id)?,
                Parameter(ty) => set_tzt_field("parameter", &mut m_parameter, ty)?,
                SelfAddr(v) => set_tzt_field("self", &mut m_self, v)?,
                OtherContracts(v) => set_tzt_field("other_contracts", &mut m_other_contracts, v)?,
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
            Some(p) => Some(p.get_entrypoints(&mut Ctx::default())?),
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
                    let entrypoints = ctm.get_entrypoints(&mut Ctx::default()).unwrap();
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
                )?)),
                TztError(error_exp) => Some(ExpectError(error_exp)),
            },
            None => None,
        };

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
        })
    }
}

/// This represents possibilities in which the execution of
/// the code in a test can fail.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum TestError<'a> {
    #[error(transparent)]
    TypecheckerError(#[from] TcError),
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
    ExpectSuccess(Vec<(Type, TypedValue<'a>)>),
    ExpectError(ErrorExpectation<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorExpectation<'a> {
    TypecheckerError(Option<String>),
    InterpreterError(InterpreterErrorExpectation<'a>),
}

impl fmt::Display for ErrorExpectation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ErrorExpectation::*;
        match self {
            TypecheckerError(None) => write!(f, "some typechecker error"),
            TypecheckerError(Some(err)) => write!(f, "typechecker error: {}", err),
            InterpreterError(err) => write!(f, "interpreter error: {}", err),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InterpreterErrorExpectation<'a> {
    GeneralOverflow(BigInt, BigInt),
    MutezOverflow(i64, i64),
    FailedWith(Micheline<'a>),
}

impl fmt::Display for InterpreterErrorExpectation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use InterpreterErrorExpectation::*;
        match self {
            GeneralOverflow(a1, a2) => write!(f, "General Overflow {} {}", a1, a2),
            MutezOverflow(a1, a2) => write!(f, "MutezOverflow {} {}", a1, a2),
            FailedWith(v) => write!(f, "FailedWith {:?}", v),
        }
    }
}

/// Helper type for use during parsing, represent a single
/// line from the test file.
pub enum TztEntity<'a> {
    Code(Micheline<'a>),
    Input(Vec<(Micheline<'a>, Micheline<'a>)>),
    Output(TztOutput<'a>),
    Amount(i64),
    Balance(i64),
    ChainId(Micheline<'a>),
    Parameter(Micheline<'a>),
    SelfAddr(Micheline<'a>),
    OtherContracts(Vec<(Micheline<'a>, Micheline<'a>)>),
}

/// Possible values for the "output" expectation field in a Tzt test
pub enum TztOutput<'a> {
    TztSuccess(Vec<(Micheline<'a>, Micheline<'a>)>),
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
    let typechecked_code = typecheck_instruction(&code, ctx, Some(&parameter), &mut t_stack)?;
    let mut i_stack: IStack = TopIsFirst::from(vals).0;
    typechecked_code.interpret(ctx, arena, &mut i_stack)?;
    Ok((t_stack, i_stack))
}

pub fn run_tzt_test<'a>(
    test: TztTest<'a>,
    arena: &'a Arena<Micheline<'a>>,
) -> Result<(), TztTestError<'a>> {
    // Here we compare the outcome of the interpreting with the
    // expectation from the test, and declare the result of the test
    // accordingly.
    let mut ctx = Ctx::default();
    ctx.gas = crate::gas::Gas::default();
    ctx.amount = test.amount.unwrap_or_default();
    ctx.balance = test.balance.unwrap_or_default();
    ctx.chain_id = test.chain_id.unwrap_or(Ctx::default().chain_id);
    ctx.self_address = test
        .self_addr
        .clone()
        .unwrap_or(Ctx::default().self_address);

    populate_ctx_with_known_contracts(
        &mut ctx,
        test.self_addr.clone().map(|x| (x, test.parameter.clone())),
        test.other_contracts.clone(),
    );

    let execution_result =
        execute_tzt_test_code(test.code, &mut ctx, arena, test.parameter, test.input);
    check_expectation(&mut ctx, test.output, execution_result)
}
