//
// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//

//! Instruments a WASM module so that every function maintains a global
//! call-depth counter and traps once it exceeds [`DEFAULT_MAX_CALL_DEPTH`].
//!
//! A native stack overflow puts the Tezos WASM PVM into `Stuck`; a WASM trap
//! is handled gracefully instead. See `instrument` in the kernel SDK README.

use std::convert::Infallible;
use std::error::Error;

use wasm_encoder::reencode::{utils, Reencode};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, Function, GlobalSection, GlobalType, Instruction,
    Module, ValType,
};
use wasmparser::{
    CompositeInnerType, FunctionBody, GlobalSectionReader, Operator, Parser, Payload,
    TypeRef,
};

#[derive(Clone)]
enum RetType {
    Void,
    Ret(wasmparser::ValType),
    NotAFunction,
}

pub struct CallDepthGuard {
    /// Index of the injected `call_depth` global in the module's global index space.
    depth: u32,
    /// Number of functions instrumented (for reporting / self-check).
    pub instrumented: u32,
    max_call_depth: u32,
    /// Result types of every defined function, in code-section order. Used to
    /// give the per-function wrapper block the right result type.
    func_results: Vec<RetType>,
    /// Index (code-section order) of the function currently being rebuilt.
    current_func: usize,
}

impl CallDepthGuard {
    fn inject_prologue(&self, f: &mut Function) {
        f.instruction(&Instruction::GlobalGet(self.depth));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::GlobalSet(self.depth));
        f.instruction(&Instruction::GlobalGet(self.depth));
        f.instruction(&Instruction::I32Const(self.max_call_depth as i32));
        f.instruction(&Instruction::I32GeU);
        f.instruction(&Instruction::If(BlockType::Empty));
        f.instruction(&Instruction::Unreachable);
        f.instruction(&Instruction::End);
    }

    /// Increment the stack depth global counter, and check it remains within the targeted limit
    fn instrument_body(
        &mut self,
        wrapper_bt: BlockType,
        f: &mut Function,
        func: FunctionBody<'_>,
    ) -> Result<(), wasm_encoder::reencode::Error<Infallible>> {
        // Open the wrapper block that every exit funnels through.
        f.instruction(&Instruction::Block(wrapper_bt));

        let mut done = false;
        let mut ctrl_depth: u32 = 0;
        let mut reader = func.get_operators_reader()?;
        while !reader.eof() {
            // `End` for the top-level function block was not read yet
            assert!(!done);
            let op = reader.read()?;
            match op {
                Operator::Return => {
                    f.instruction(&Instruction::Br(ctrl_depth));
                }
                Operator::Block { .. } | Operator::Loop { .. } | Operator::If { .. } => {
                    let instr = self.instruction(op)?;
                    f.instruction(&instr);
                    ctrl_depth += 1;
                }
                Operator::End => {
                    f.instruction(&Instruction::End);
                    if let Some(new_depth) = ctrl_depth.checked_sub(1) {
                        ctrl_depth = new_depth;
                    } else {
                        // This was the `End` operator for the top-level function block. We can only
                        // have one of them
                        done = true;
                    }
                }
                Operator::Throw { .. }
                | Operator::ThrowRef
                | Operator::Rethrow { .. }
                | Operator::Try { .. }
                | Operator::TryTable { .. }
                | Operator::Catch { .. }
                | Operator::CatchAll
                | Operator::Delegate { .. }
                | Operator::ReturnCall { .. }
                | Operator::ReturnCallIndirect { .. }
                | Operator::ReturnCallRef { .. } => {
                    panic!(
                        "unsupported opcode: exception-handling / tail-call \
                         operators cannot be depth-instrumented"
                    )
                }
                other => {
                    let instr = self.instruction(other)?;
                    f.instruction(&instr);
                }
            }
        }

        // `ctrl_depth` being strictly positive would mean we have unclosed blocks
        assert!(ctrl_depth == 0);
        Ok(())
    }

    /// Decrement the stack depth global counter
    fn inject_epilogue(&self, f: &mut Function) {
        f.instruction(&Instruction::GlobalGet(self.depth));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Sub);
        f.instruction(&Instruction::GlobalSet(self.depth));
        f.instruction(&Instruction::End);
    }
}

impl Reencode for CallDepthGuard {
    type Error = Infallible;

    /// Copy the existing globals, then append our mutable `call_depth = 0`.
    fn parse_global_section(
        &mut self,
        globals: &mut GlobalSection,
        reader: GlobalSectionReader<'_>,
    ) -> Result<(), wasm_encoder::reencode::Error<Self::Error>> {
        utils::parse_global_section(self, globals, reader)?;
        globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const(0),
        );
        Ok(())
    }

    fn parse_function_body(
        &mut self,
        code: &mut CodeSection,
        func: FunctionBody<'_>,
    ) -> Result<(), wasm_encoder::reencode::Error<Self::Error>> {
        let mut f = self.new_function_with_parsed_locals(&func)?;

        // Derive the wrapper block type from the result type of the function
        let wrapper_bt = match &self.func_results[self.current_func] {
            RetType::Void => BlockType::Empty,
            RetType::Ret(ty) => BlockType::Result(self.val_type(*ty)?),
            RetType::NotAFunction => {
                panic!("indexes mismatch, expected a function type",)
            }
        };

        self.current_func += 1;

        self.inject_prologue(&mut f);
        self.instrument_body(wrapper_bt, &mut f, func)?;
        self.inject_epilogue(&mut f);

        code.function(&f);

        self.instrumented += 1;

        Ok(())
    }
}

pub fn prepare_pass(
    bytes: &[u8],
    max_call_depth: u32,
) -> Result<CallDepthGuard, Box<dyn Error>> {
    let mut has_global = false;
    let mut imported = 0u32;
    let mut defined = 0u32;
    // Result types of every declared type, indexed by type index (non-func
    // types occupy a slot they never reference).
    let mut type_results: Vec<RetType> = Vec::new();
    // Type index of each defined function, in code-section order.
    let mut func_type_idx: Vec<u32> = Vec::new();
    for payload in Parser::new(0).parse_all(bytes) {
        match payload? {
            Payload::ImportSection(reader) => {
                // One section entry (`Imports`) can expand to several `Import`s in the
                // compact encoding, hence the nested loop.
                for imports in reader {
                    for item in imports? {
                        let (_, import) = item?;
                        if matches!(import.ty, TypeRef::Global(_)) {
                            imported += 1;
                        }
                    }
                }
            }
            Payload::GlobalSection(reader) => {
                has_global = true;
                defined = reader.count();
            }
            Payload::TypeSection(reader) => {
                for rec_group in reader {
                    for sub_type in rec_group?.types() {
                        let result = match &sub_type.composite_type.inner {
                            CompositeInnerType::Func(ft) => match ft.results() {
                                [] => RetType::Void,
                                [ty] => RetType::Ret(*ty),
                                many => {
                                    return Err(format!(
                                        "function type has {} results; \
                                             multi-value returns are unsupported",
                                        many.len()
                                    )
                                    .into())
                                }
                            },
                            _ => RetType::NotAFunction,
                        };
                        type_results.push(result);
                    }
                }
            }
            Payload::FunctionSection(reader) => {
                for ty_idx in reader {
                    func_type_idx.push(ty_idx?);
                }
            }
            _ => {}
        }
    }

    // The Reencode `parse_global_section` hook only fires when a global section
    // exists; without one the injected `call_depth` global is never appended,
    // yet `depth` below still points at its index.
    assert!(
        has_global,
        "module has no global section; cannot inject the call_depth global"
    );

    let func_results = func_type_idx
        .iter()
        .map(|&ti| type_results[ti as usize].clone())
        .collect();

    Ok(CallDepthGuard {
        depth: imported + defined,
        instrumented: 0,
        max_call_depth,
        func_results,
        current_func: 0,
    })
}

impl CallDepthGuard {
    /// Index of the injected `call_depth` global in the global index space.
    pub fn depth(&self) -> u32 {
        self.depth
    }

    /// Number of functions the module declares, as counted by [`prepare_pass`].
    /// Used by [`instrument`] to self-check that every one was rewritten.
    pub fn declared_functions(&self) -> usize {
        self.func_results.len()
    }
}

/// Call-depth ceiling enforced in every instrumented function's prologue.
///
/// Chosen well below the WASM PVM's own 60_000-frame `stack_size_limit`
/// (`src/lib_scoru_wasm/wasm_vm.ml`) so that the WASM trap always precedes both
/// the interpreter's counter and Wasmer's native limit: the kernel then traps at
/// the same depth under either engine, keeping execution deterministic.
pub const DEFAULT_MAX_CALL_DEPTH: u32 = 2000;

/// An instrumented module and the number of functions rewritten to produce it.
pub struct Instrumented {
    pub module: Vec<u8>,
    pub functions: u32,
}

/// Instrument every function of `bytes` with the call-depth guard.
///
/// The output is validated before being returned, and the pass self-checks that
/// it rewrote exactly as many functions as the module declares.
pub fn instrument(
    bytes: &[u8],
    max_call_depth: u32,
) -> Result<Instrumented, Box<dyn Error>> {
    let mut pass = prepare_pass(bytes, max_call_depth)?;
    let mut module = Module::new();
    pass.parse_core_module(&mut module, Parser::new(0), bytes)?;
    let out_bytes = module.finish();

    // Prove the instrumented module is still well-formed.
    wasmparser::validate(&out_bytes)?;

    // Every declared function must have been rewritten: the wrapper block types
    // are derived from `func_results` in code-section order, so any drift between
    // that order and the code section would silently mistype a wrapper.
    if pass.instrumented as usize != pass.declared_functions() {
        return Err(format!(
            "instrumented {} functions but module declares {}",
            pass.instrumented,
            pass.declared_functions()
        )
        .into());
    }

    Ok(Instrumented {
        module: out_bytes,
        functions: pass.instrumented,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasmi::{Engine, Linker, Store, TrapCode};

    /// Count occurrences of each operator name in every function body.
    fn census(wasm: &[u8]) -> std::collections::HashMap<String, usize> {
        let mut out = std::collections::HashMap::new();
        for payload in Parser::new(0).parse_all(wasm) {
            if let Payload::CodeSectionEntry(body) = payload.unwrap() {
                let mut reader = body.get_operators_reader().unwrap();
                while !reader.eof() {
                    let op = reader.read().unwrap();
                    // `Operator` has no stable name accessor; the Debug prefix is
                    // the variant name, which is all we need to count by kind.
                    let dbg = format!("{op:?}");
                    let name = dbg
                        .split(['(', ' ', '{'])
                        .next()
                        .unwrap_or_default()
                        .to_string();
                    *out.entry(name).or_default() += 1;
                }
            }
        }
        out
    }

    fn count(c: &std::collections::HashMap<String, usize>, k: &str) -> usize {
        c.get(k).copied().unwrap_or(0)
    }

    /// Instantiate and call an exported function taking/returning i32.
    fn run(wasm: &[u8], func: &str, arg: i32) -> Result<i32, wasmi::Error> {
        let engine = Engine::default();
        let module = wasmi::Module::new(&engine, wasm)?;
        let mut store = Store::new(&engine, ());
        let linker = <Linker<()>>::new(&engine);
        let instance = linker.instantiate_and_start(&mut store, &module)?;
        let f = instance.get_typed_func::<i32, i32>(&store, func)?;
        f.call(&mut store, arg)
    }

    fn trap_code(e: &wasmi::Error) -> Option<TrapCode> {
        e.as_trap_code()
    }

    /// Exercises `return` at top level, inside `if`/`else`, inside a `loop`, and
    /// through a `br_table` — i.e. every shape whose rewrite depends on
    /// `ctrl_depth` being tracked correctly.
    const RETURNS: &str = r#"
    (module
      (global $g (mut i32) (i32.const 7))
      ;; return at top level
      (func (export "top") (param i32) (result i32)
        (return (i32.add (local.get 0) (i32.const 1))))
      ;; return nested inside if/else
      (func (export "nested_if") (param i32) (result i32)
        (if (i32.eqz (local.get 0))
          (then (return (i32.const 100)))
          (else (return (i32.const 200))))
        (i32.const 300))
      ;; return from inside a loop, two control frames deep
      (func (export "in_loop") (param i32) (result i32)
        (local $i i32)
        (block $out
          (loop $lp
            (br_if $out (i32.ge_s (local.get $i) (local.get 0)))
            (if (i32.eq (local.get $i) (i32.const 3))
              (then (return (i32.const 42))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $lp)))
        (local.get $i))
      ;; return under a br_table
      (func (export "table") (param i32) (result i32)
        (block $b2 (block $b1 (block $b0
          (br_table $b0 $b1 $b2 (local.get 0)))
          (return (i32.const 10)))
          (return (i32.const 11)))
        (i32.const 12))
      ;; a void function (wrapper block type must be Empty, not Result)
      (func (export "void_fn") (param i32)
        (if (i32.eqz (local.get 0)) (then (return)))
        (global.set $g (local.get 0)))
      ;; reads the global so `void_fn`'s effect is observable
      (func (export "get_g") (param i32) (result i32) (global.get $g)))
    "#;

    #[test]
    fn every_return_is_retargeted_and_nothing_else_changes() {
        let orig = wat::parse_str(RETURNS).unwrap();
        let instr = instrument(&orig, DEFAULT_MAX_CALL_DEPTH).unwrap();

        let (a, b) = (census(&orig), census(&instr.module));
        let funcs = instr.functions as usize;

        // No `return` survives; each became exactly one `br`.
        assert_eq!(count(&b, "Return"), 0, "a Return survived the rewrite");
        assert_eq!(
            count(&b, "Br") - count(&a, "Br"),
            count(&a, "Return"),
            "Δbr must equal the number of rewritten returns"
        );
        // Nothing else is added or dropped.
        for op in ["Call", "LocalGet", "BrTable", "BrIf", "LocalSet"] {
            assert_eq!(count(&a, op), count(&b, op), "{op} count changed");
        }
        // Exactly one guard per function.
        for op in ["Block", "If", "Unreachable", "I32GeU"] {
            assert_eq!(
                count(&b, op) - count(&a, op),
                funcs,
                "{op} must be injected exactly once per function"
            );
        }
    }

    #[test]
    fn semantics_are_preserved() {
        let orig = wat::parse_str(RETURNS).unwrap();
        let instr = instrument(&orig, DEFAULT_MAX_CALL_DEPTH).unwrap();

        for (func, args) in [
            ("top", vec![0, 1, -5]),
            ("nested_if", vec![0, 1]),
            ("in_loop", vec![0, 2, 3, 9]),
            ("table", vec![0, 1, 2, 7]),
        ] {
            for a in args {
                let want = run(&orig, func, a).unwrap();
                let got = run(&instr.module, func, a).unwrap();
                assert_eq!(want, got, "{func}({a}) diverged after instrumentation");
            }
        }
    }

    #[test]
    fn void_function_side_effect_is_preserved() {
        let orig = wat::parse_str(RETURNS).unwrap();
        let instr = instrument(&orig, DEFAULT_MAX_CALL_DEPTH).unwrap();

        // `void_fn(0)` returns early and leaves the global at 7;
        // `void_fn(5)` falls through and sets it to 5.
        for arg in [0, 5] {
            let engine = Engine::default();
            let module = wasmi::Module::new(&engine, &instr.module[..]).unwrap();
            let mut store = Store::new(&engine, ());
            let linker = <Linker<()>>::new(&engine);
            let inst = linker.instantiate_and_start(&mut store, &module).unwrap();
            inst.get_typed_func::<i32, ()>(&store, "void_fn")
                .unwrap()
                .call(&mut store, arg)
                .unwrap();
            let g = inst
                .get_typed_func::<i32, i32>(&store, "get_g")
                .unwrap()
                .call(&mut store, 0)
                .unwrap();
            assert_eq!(
                g,
                if arg == 0 { 7 } else { 5 },
                "void_fn({arg}) side effect"
            );
        }
    }

    /// `rec(n)` recurses n deep. With the guard at `LIMIT`, a call needing more
    /// than LIMIT frames must trap with `unreachable`, and one needing fewer
    /// must succeed.
    const RECURSE: &str = r#"
    (module
      (global $g (mut i32) (i32.const 0))
      (func $rec (export "rec") (param i32) (result i32)
        (if (i32.le_s (local.get 0) (i32.const 0))
          (then (return (i32.const 0))))
        (i32.add (i32.const 1)
          (call $rec (i32.sub (local.get 0) (i32.const 1))))))
    "#;

    #[test]
    fn guard_traps_beyond_the_limit_and_not_below_it() {
        const LIMIT: u32 = 20;
        let orig = wat::parse_str(RECURSE).unwrap();
        let instr = instrument(&orig, LIMIT).unwrap();

        // Uninstrumented, deep recursion is fine — proving the trap below is
        // the guard firing and not an artefact of the fixture.
        assert_eq!(run(&orig, "rec", 100).unwrap(), 100);

        // The entry call is frame 1, so `rec(n)` uses n+1 frames.
        let deepest_ok = LIMIT as i32 - 2;
        assert_eq!(
            run(&instr.module, "rec", deepest_ok).unwrap(),
            deepest_ok,
            "recursion just under the limit must still succeed"
        );

        let err = run(&instr.module, "rec", LIMIT as i32).unwrap_err();
        assert_eq!(
            trap_code(&err),
            Some(TrapCode::UnreachableCodeReached),
            "exceeding the limit must trap via the guard's `unreachable`"
        );
    }

    #[test]
    fn counter_does_not_leak_across_calls() {
        const LIMIT: u32 = 20;
        let instr = instrument(&wat::parse_str(RECURSE).unwrap(), LIMIT).unwrap();

        let engine = Engine::default();
        let module = wasmi::Module::new(&engine, &instr.module[..]).unwrap();
        let mut store = Store::new(&engine, ());
        let linker = <Linker<()>>::new(&engine);
        let inst = linker.instantiate_and_start(&mut store, &module).unwrap();
        let rec = inst.get_typed_func::<i32, i32>(&store, "rec").unwrap();

        // Each call returns normally, so every prologue must be matched by its
        // epilogue. If the counter leaked even one per call, the guard would
        // fire well before the 500th.
        for i in 0..500 {
            let n = LIMIT as i32 - 2;
            assert_eq!(
                rec.call(&mut store, n).unwrap(),
                n,
                "call #{i} leaked depth"
            );
        }
    }

    #[test]
    fn injected_global_lands_after_imported_and_defined_globals() {
        // 2 imported + 1 defined => call_depth must be global index 3.
        let src = r#"
        (module
          (import "env" "a" (global i32))
          (import "env" "b" (global i32))
          (import "env" "f" (func))
          (global $own (mut i32) (i32.const 5))
          (func (export "id") (param i32) (result i32) (local.get 0)))
        "#;
        let instr =
            instrument(&wat::parse_str(src).unwrap(), DEFAULT_MAX_CALL_DEPTH).unwrap();

        let c = census(&instr.module);
        // The guard uses GlobalGet/GlobalSet on the injected index only.
        assert!(count(&c, "GlobalGet") >= 3 && count(&c, "GlobalSet") >= 2);

        let mut globals = 0;
        for payload in Parser::new(0).parse_all(&instr.module) {
            if let Payload::GlobalSection(r) = payload.unwrap() {
                globals = r.count();
            }
        }
        assert_eq!(
            globals, 2,
            "the injected global must be appended to the defined ones"
        );

        // Index space: 2 imported + 1 defined => injected index 3.
        let pass =
            prepare_pass(&wat::parse_str(src).unwrap(), DEFAULT_MAX_CALL_DEPTH).unwrap();
        assert_eq!(pass.depth(), 3);
    }

    #[test]
    fn output_is_deterministic() {
        let orig = wat::parse_str(RETURNS).unwrap();
        let a = instrument(&orig, DEFAULT_MAX_CALL_DEPTH).unwrap();
        let b = instrument(&orig, DEFAULT_MAX_CALL_DEPTH).unwrap();
        assert_eq!(a.module, b.module, "instrumentation must be reproducible");
    }
}
