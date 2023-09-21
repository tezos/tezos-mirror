/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use crate::ast::*;
use crate::stack::*;

pub fn interpret(ast: &AST, stack: &mut IStack) {
    for i in ast {
        interpret_one(&i, stack);
    }
}

fn unreachable_state() -> ! {
    // If the typechecking of the program being interpreted was successful and if this is reached
    // during interpreting, then the typechecking should be broken, and needs to be fixed.
    panic!("Unreachable state reached during interpreting, possibly broken typechecking!")
}

fn interpret_one(i: &Instruction, stack: &mut IStack) {
    use Instruction::*;
    use Value::*;

    match i {
        Add => match stack.make_contiguous() {
            [NumberValue(o1), NumberValue(o2), ..] => {
                let sum = *o1 + *o2;
                stack.pop_front();
                stack.pop_front();
                stack.push_front(NumberValue(sum));
            }
            _ => unimplemented!(),
        },
        Dip(opt_height, nested) => {
            let protected_height: usize = opt_height.unwrap_or(1);
            let mut live = stack.split_off(protected_height);
            interpret(nested, &mut live);
            stack.append(&mut live);
        }
        Drop(opt_height) => {
            let drop_height: usize = opt_height.unwrap_or(1);
            *stack = stack.split_off(drop_height);
        }
        Dup(opt_height) => {
            let dup_height: usize = opt_height.unwrap_or(1);
            stack.push_front(stack.get(dup_height - 1).unwrap().clone());
        }
        Gt => match stack.make_contiguous() {
            [NumberValue(i), ..] => {
                stack[0] = BooleanValue(*i > 0);
            }
            _ => unreachable_state(),
        },
        If(nested_t, nested_f) => {
            if let Some(BooleanValue(b)) = stack.pop_front() {
                if b {
                    interpret(nested_t, stack);
                } else {
                    interpret(nested_f, stack);
                }
            } else {
                unreachable_state();
            }
        }
        Instruction::Int => match stack.make_contiguous() {
            [NumberValue(_), ..] => {}
            _ => {
                unreachable_state();
            }
        },
        Loop(nested) => loop {
            if let Some(BooleanValue(b)) = stack.pop_front() {
                if b {
                    interpret(nested, stack);
                } else {
                    break;
                }
            } else {
                unreachable_state();
            }
        },
        Push(_, v) => {
            stack.push_front(v.clone());
        }
        Swap => {
            stack.swap(0, 1);
        }
    }
}

#[cfg(test)]
mod interpreter_tests {
    use crate::interpreter::*;
    use crate::parser::*;
    use std::collections::VecDeque;
    use Instruction::*;
    use Value::*;

    #[test]
    fn test_add() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(30)]);
        interpret_one(&Add, &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_dip() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(5), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(10), NumberValue(25)]);
        interpret_one(&Dip(None, parse("{ADD}").unwrap()), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_dip2() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(5), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(10), NumberValue(5)]);
        interpret_one(&Dip(Some(2), parse("{DROP}").unwrap()), &mut stack);
       assert!(stack == expected_stack);
    }

    #[test]
    fn test_drop() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(5), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(5), NumberValue(20)]);
        interpret_one(&Drop(None), &mut stack);
        assert!(stack == expected_stack);
    }

   #[test]
    fn test_drop2() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(5), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(20)]);
        interpret_one(&Drop(Some(2)), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_dup() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(5), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(10), NumberValue(10), NumberValue(5), NumberValue(20)]);
        interpret_one(&Dup(None), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_dup2() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(5), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(5), NumberValue(10), NumberValue(5), NumberValue(20)]);
        interpret_one(&Dup(Some(2)), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_gt() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(20)]);
        let expected_stack = VecDeque::from([BooleanValue(true), NumberValue(20)]);
        interpret_one(&Gt, &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_if_t() {
        let mut stack = VecDeque::from([BooleanValue(true), NumberValue(5), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(20)]);
        interpret_one(&If(parse("{DROP}").unwrap(), parse("{ADD}").unwrap()), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_if_f() {
        let mut stack = VecDeque::from([BooleanValue(false), NumberValue(5), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(25)]);
        interpret_one(&If(parse("{DROP}").unwrap(), parse("{ADD}").unwrap()), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_int() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(10), NumberValue(20)]);
        interpret_one(&Int, &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_push() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(0), NumberValue(10), NumberValue(20)]);
        interpret_one(&Push(Type::Nat, NumberValue(0)), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_loop_0() {
        let mut stack = VecDeque::from([BooleanValue(false), NumberValue(10), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(10), NumberValue(20)]);
        interpret_one(&Loop(parse("{PUSH nat 1; ADD; PUSH bool False}").unwrap()), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_loop_1() {
        let mut stack = VecDeque::from([BooleanValue(true), NumberValue(10), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(11), NumberValue(20)]);
        interpret_one(&Loop(parse("{PUSH nat 1; ADD; PUSH bool False}").unwrap()), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_loop_many() {
        let mut stack = VecDeque::from([BooleanValue(true), NumberValue(10), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(0), NumberValue(20)]);
        interpret_one(&Loop(parse("{PUSH int -1; ADD; DUP; GT}").unwrap()), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_swap() {
        let mut stack = VecDeque::from([NumberValue(10), NumberValue(20)]);
        let expected_stack = VecDeque::from([NumberValue(20), NumberValue(10)]);
        interpret_one(&Swap, &mut stack);
        assert!(stack == expected_stack);
    }
}
