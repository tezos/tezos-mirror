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
            let protected_height: usize = opt_height.unwrap_or(1);;
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
            _ => panic!("Unexpected stack during instruction!"),
        },
        If(nested_t, nested_f) => {
            if let Some(BooleanValue(b)) = stack.pop_front() {
                if b {
                    interpret(nested_t, stack);
                } else {
                    interpret(nested_f, stack);
                }
            } else {
                panic!("Unexpected stack during instruction!");
            }
        }
        Instruction::Int => match stack.make_contiguous() {
            [NumberValue(_), ..] => {}
            _ => {
                panic!("Unexpected stack during instruction!");
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
                panic!("Unexpected stack during instruction!");
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
}
