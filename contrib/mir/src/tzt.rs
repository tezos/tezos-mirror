/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use crate::ast::*;
use crate::interpreter::*;
use crate::stack::*;
use crate::typechecker::*;

pub type TestStack = Vec<(Type, TypedValue)>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TztTestError {
    StackMismatch(
        (FailingTypeStack, Stack<Value>),
        (FailingTypeStack, Stack<Value>),
    ),
    UnexpectedError(TestError),
    UnexpectedSuccess(IStack),
    ExpectedDifferentError(ErrorExpectation, TestError),
}

/// Represent one Tzt test. The output attribute is a Result to include
/// expectation of failure.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TztTest {
    pub code: ParsedInstructionBlock,
    pub input: TestStack,
    pub output: TestExpectation,
    pub amount: Option<i64>,
}

/// This represents possibilities in which the execution of
/// the code in a test can fail.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum TestError {
    #[error(transparent)]
    TypecheckerError(#[from] TcError),
    #[error(transparent)]
    InterpreterError(#[from] InterpretError),
}

/// This represents the outcome that we expect from interpreting
/// the code in a test.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TestExpectation {
    ExpectSuccess(Vec<(Type, Value)>),
    ExpectError(ErrorExpectation),
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorExpectation {
    TypecheckerError(TcError),
    InterpreterError(InterpreterErrorExpectation),
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InterpreterErrorExpectation {
    GeneralOverflow(i128, i128),
    MutezOverflow(i64, i64),
    FailedWith(Value),
}

/// Helper type for use during parsing, represent a single
/// line from the test file.
pub enum TztEntity {
    Code(ParsedInstructionBlock),
    Input(Vec<(Type, Value)>),
    Output(TztOutput),
    Amount(i64),
}

/// Possible values for the "output" field in a Tzt test
pub enum TztOutput {
    Success(Vec<(Type, Value)>),
    Fail(Value),
    MutezOverflow(i64, i64),
    GeneralOverflow(i128, i128),
}
