/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use crate::ast::*;
use crate::stack::*;
use std::collections::VecDeque;

pub fn typecheck(ast: &AST, stack: &mut TypeStack) -> bool {
    for i in ast {
        if !typecheck_instruction(&i, stack) {
            return false;
        }
    }
    return true;
}

fn typecheck_instruction(i: &Instruction, stack: &mut TypeStack) -> bool {
    use Instruction::*;
    use Type::*;

    match i {
        Add => match stack.make_contiguous() {
            [Type::Nat, Type::Nat, ..] => {
                stack.pop_front();
                true
            }
            [Type::Int, Type::Int, ..] => {
                stack.pop_front();
                true
            }
            _ => unimplemented!(),
        },
        DipN(_, nested) | Dip(nested) => {
            let h = {
                // Extract the height of the protected stack if the
                // instruction is DipN, or default to 1 if it is Dip.
                if let DipN(h, _) = i {
                    *h
                } else {
                    1
                }
            };
            let protected_height: usize = usize::try_from(h).unwrap();

            if ensure_stack_len(stack, protected_height) {
                // Here we split the stack into protected and live segments, and after typechecking
                // nested code with the live segment, we append the protected and the potentially
                // modified live segment as the result stack.
                let mut live = stack.split_off(protected_height);
                if typecheck(nested, &mut live) {
                    stack.append(&mut live);
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
        DropN(_) | Drop => {
            let h = {
                if let DropN(h) = i {
                    *h
                } else {
                    1
                }
            };
            let drop_height: usize = usize::try_from(h).unwrap();
            if ensure_stack_len(&stack, drop_height) {
                *stack = stack.split_off(drop_height);
                true
            } else {
                false
            }
        }
        DupN(0) => {
            // DUP instruction requires an argument that is > 0.
            false
        }
        DupN(_) | Dup => {
            let h = {
                if let DupN(h) = i {
                    *h
                } else {
                    1
                }
            };
            let dup_height: usize = usize::try_from(h).unwrap();
            if ensure_stack_len(stack, dup_height) {
                stack.push_front(stack.get(dup_height - 1).unwrap().to_owned());
                true
            } else {
                false
            }
        }
        Gt => {
            match stack.make_contiguous() {
                [Type::Int, ..] => {
                    stack[0] = Type::Bool;
                    true
                }
                _ => false,
            }
        }
        If(nested_t, nested_f) => match stack.make_contiguous() {
            // Check if top is bool and bind the tail to `t`.
            [Type::Bool, t @ ..] => {
                // Clone the stack so that we have two stacks to run
                // the two branches with.
                let mut t_stack: TypeStack = VecDeque::from(t.to_owned());
                let mut f_stack: TypeStack = VecDeque::from(t.to_owned());
                if typecheck(nested_t, &mut t_stack) && typecheck(nested_f, &mut f_stack) {
                    // If both stacks are same after typecheck, then make result
                    // stack using one of them and return success.
                    if t_stack == f_stack {
                        *stack = t_stack;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            _ => false,
        },
        Instruction::Int => match stack.make_contiguous() {
            [val @ Type::Nat, ..] => {
                *val = Type::Int;
                true
            }
        }
        Loop(nested) => match stack.make_contiguous() {
            // Check if top is bool and bind the tail to `t`.
            [Bool, t @ ..] => {
                let mut live: TypeStack = VecDeque::from(t.to_owned());
                // Clone the tail and typecheck the nested body using it.
                if typecheck(nested, &mut live) {
                    match live.make_contiguous() {
                        // ensure the result stack has a bool on top.
                        [Bool, r @ ..] => {
                            // If the starting tail and result tail match
                            // then the typecheck is complete. pop the bool
                            // off the original stack to form the final result.
                            if t == r {
                                stack.pop_front();
                                true
                            } else {
                                false
                            }
                        }
                        _ => false,
                    }
                } else {
                    false
                }
            }
            _ => false,
        },
        Push(t, v) => {
            if typecheck_value(&t, &v) {
                stack.push_front(t.to_owned());
                true
            } else {
                false
            }
        }
        Swap => {
            if stack.len() > 1 {
                stack.swap(0, 1);
                true
            } else {
                false
            }
        }
    }
}

fn typecheck_value(t: &Type, v: &Value) -> bool {
    use Type::*;
    use Value::*;
    match (t, v) {
        (Nat, NumberValue(n)) => n >= &0,
        (Int, NumberValue(_)) => true,
        (Bool, BooleanValue(_)) => true,
        _ => false,
    }
}

#[cfg(test)]
mod typecheck_tests {
    use std::collections::VecDeque;

    use crate::parser::*;
    use crate::typechecker::*;
    use Instruction::*;

    #[test]
    fn test_dup() {
        let mut stack = VecDeque::from([Type::Nat]);
        let expected_stack = VecDeque::from([Type::Nat, Type::Nat]);
        typecheck_instruction(&DupN(1), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_dup_n() {
        let mut stack = VecDeque::from([Type::Nat, Type::Int]);
        let expected_stack = VecDeque::from([Type::Int, Type::Nat, Type::Int]);
        typecheck_instruction(&DupN(2), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_swap() {
        let mut stack = VecDeque::from([Type::Nat, Type::Int]);
        let expected_stack = VecDeque::from([Type::Int, Type::Nat]);
        typecheck_instruction(&Swap, &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_int() {
        let mut stack = VecDeque::from([Type::Nat]);
        let expected_stack = VecDeque::from([Type::Int]);
        typecheck_instruction(&Int, &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_drop() {
        let mut stack = VecDeque::from([Type::Nat]);
        let expected_stack = VecDeque::from([]);
        typecheck(&parse("{DROP}").unwrap(), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_drop_n() {
        let mut stack = VecDeque::from([Type::Nat, Type::Int]);
        let expected_stack = VecDeque::from([]);
        typecheck_instruction(&DropN(2), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_push() {
        let mut stack = VecDeque::from([Type::Nat]);
        let expected_stack = VecDeque::from([Type::Int, Type::Nat]);
        typecheck_instruction(&Push(Type::Int, Value::NumberValue(1)), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_gt() {
        let mut stack = VecDeque::from([Type::Int]);
        let expected_stack = VecDeque::from([Type::Bool]);
        typecheck_instruction(&Gt, &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_dip() {
        let mut stack = VecDeque::from([Type::Int, Type::Bool]);
        let expected_stack = VecDeque::from([Type::Int, Type::Nat, Type::Bool]);
        typecheck_instruction(&DipN(1, parse("{PUSH nat 6}").unwrap()), &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_add() {
        let mut stack = VecDeque::from([Type::Int, Type::Int]);
        let expected_stack = VecDeque::from([Type::Int]);
        typecheck_instruction(&Add, &mut stack);
        assert!(stack == expected_stack);
    }

    #[test]
    fn test_loop() {
        let mut stack = VecDeque::from([Type::Bool, Type::Int]);
        let expected_stack = VecDeque::from([Type::Int]);
        assert!(typecheck_instruction(
            &Loop(parse("{PUSH bool True}").unwrap()),
            &mut stack
        ));
        assert!(stack == expected_stack);
    }
}
