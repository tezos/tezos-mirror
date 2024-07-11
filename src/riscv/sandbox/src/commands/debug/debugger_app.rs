// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{errors, tui};
use color_eyre::Result;
use crossterm::event::{self, Event, KeyCode, KeyEventKind};
use octez_riscv::{
    bits::Bits64,
    kernel_loader,
    machine_state::{
        bus::{main_memory::MainMemoryLayout, Address},
        csregisters::satp::{Satp, SvLength, TranslationAlgorithm},
        mode::Mode,
        AccessType,
    },
    program::Program,
    pvm::PvmHooks,
    state_backend::CellRead,
    stepper::{pvm::PvmStepper, test::TestStepper, Stepper},
};
use ratatui::{prelude::*, style::palette::tailwind, widgets::*};
use std::collections::{BTreeMap, HashMap, HashSet};
use tezos_smart_rollup::utils::inbox::Inbox;

mod render;
mod updates;

const GREEN: Color = tailwind::GREEN.c400;
const YELLOW: Color = tailwind::YELLOW.c400;
const RED: Color = tailwind::RED.c500;
const BLUE: Color = tailwind::BLUE.c400;
const ORANGE: Color = tailwind::ORANGE.c500;
const GRAY: Color = tailwind::GRAY.c500;
const SELECTED_STYLE_FG: Color = BLUE;
const NEXT_STYLE_FG: Color = GREEN;
const MAX_STEPS: usize = 1_000_000;
const PC_CONTEXT: u64 = 12;

#[derive(Debug, Clone)]
pub struct Instruction {
    address: u64,
    pub text: String,
    jump: Option<(u64, Option<String>)>,
}

impl Instruction {
    fn new(address: u64, text: String, symbols: &HashMap<u64, &str>) -> Self {
        let jump = match text
            .split(' ')
            .next()
            .expect("Unexpected instruction format")
        {
            "jal" | "beq" | "bne" | "blt" | "bge" | "bltu" | "bgeu" => {
                text.split(',').last().map(|jump_address| {
                    let addr = address.wrapping_add(jump_address.parse::<i64>().unwrap() as u64);
                    (addr, symbols.get(&addr).map(|s| s.to_string()))
                })
            }
            _ => None,
        };
        Self {
            address,
            text,
            jump,
        }
    }
}

enum EffectiveTranslationState {
    Off,
    On,
    Faulting,
}

impl EffectiveTranslationState {
    pub fn text(&self) -> &'static str {
        match self {
            Self::Off => "Off",
            Self::On => "On",
            Self::Faulting => "Faulting",
        }
    }

    pub fn fg(&self) -> Color {
        match self {
            Self::Off => GRAY,
            Self::On => GREEN,
            Self::Faulting => RED,
        }
    }
}

enum SATPModeState {
    // Translation is BARE mode
    Bare,
    // Translation is SvXY mode
    Sv(SvLength),
}

struct TranslationState {
    mode: SATPModeState,
    base: Address,
    effective: EffectiveTranslationState,
}

impl TranslationState {
    pub(super) fn update(
        &mut self,
        faulting: bool,
        effective_mode: TranslationAlgorithm,
        satp_val: Satp,
    ) {
        self.effective = if faulting {
            EffectiveTranslationState::Faulting
        } else {
            match effective_mode {
                Bare => EffectiveTranslationState::Off,
                Sv39 | Sv48 | Sv57 => EffectiveTranslationState::On,
            }
        };

        self.base = satp_val.ppn().to_bits();

        use TranslationAlgorithm::*;
        self.mode = match satp_val.mode() {
            Bare => SATPModeState::Bare,
            Sv39 => SATPModeState::Sv(SvLength::Sv39),
            Sv48 => SATPModeState::Sv(SvLength::Sv48),
            Sv57 => SATPModeState::Sv(SvLength::Sv57),
        }
    }
}

struct DebuggerState<R> {
    pub result: R,
    pub prev_pc: Address,
    pub translation: TranslationState,
}

struct ProgramView<'a> {
    state: ListState,
    instructions: Vec<Instruction>,
    next_instr: usize,
    breakpoints: HashSet<u64>,
    symbols: HashMap<u64, &'a str>,
}

pub struct DebuggerApp<'a, S: Stepper> {
    title: &'a str,
    stepper: &'a mut S,
    program: ProgramView<'a>,
    state: DebuggerState<S::StepResult>,
}

impl<'a, ML: MainMemoryLayout> DebuggerApp<'a, TestStepper<'a, ML>> {
    pub fn launch(
        fname: &str,
        program: &[u8],
        initrd: Option<&[u8]>,
        exit_mode: Mode,
    ) -> Result<()> {
        let mut backend = TestStepper::<'_, ML>::create_backend();
        let (mut interpreter, prog) =
            TestStepper::new_with_parsed_program(&mut backend, program, initrd, exit_mode)?;
        let symbols = kernel_loader::get_elf_symbols::<ML>(program)?;
        errors::install_hooks()?;
        let terminal = tui::init()?;
        DebuggerApp::new(&mut interpreter, fname, &prog, symbols).run_debugger(terminal)?;
        tui::restore()?;
        Ok(())
    }
}

impl<'backend, 'hooks, ML: MainMemoryLayout>
    DebuggerApp<'backend, PvmStepper<'backend, 'hooks, ML>>
{
    /// Launch the Debugger app for a PVM.
    pub fn launch(
        fname: &str,
        program: &[u8],
        initrd: Option<&[u8]>,
        inbox: Inbox,
        rollup_address: [u8; 20],
        origination_level: u32,
    ) -> Result<()> {
        let hooks = PvmHooks::new(|_| {});

        let mut backend = PvmStepper::<'backend, 'hooks, ML>::create_backend();
        let mut stepper = PvmStepper::new(
            &mut backend,
            program,
            initrd,
            inbox,
            hooks,
            rollup_address,
            origination_level,
        )?;

        let symbols = kernel_loader::get_elf_symbols::<ML>(program)?;
        let program = Program::<ML>::from_elf(program)?.parsed();

        errors::install_hooks()?;
        let terminal = tui::init()?;
        DebuggerApp::new(&mut stepper, fname, &program, symbols).run_debugger(terminal)?;

        tui::restore()?;
        Ok(())
    }
}

impl<'a, S> DebuggerApp<'a, S>
where
    S: Stepper,
{
    fn new(
        stepper: &'a mut S,
        title: &'a str,
        program: &'a BTreeMap<u64, String>,
        symbols: HashMap<u64, &'a str>,
    ) -> Self {
        Self {
            title,
            stepper,
            program: ProgramView::with_items(
                program
                    .iter()
                    .map(|x| Instruction::new(*x.0, x.1.to_string(), &symbols))
                    .collect::<Vec<Instruction>>(),
                symbols,
            ),
            state: DebuggerState {
                result: S::StepResult::default(),
                prev_pc: 0,
                translation: TranslationState {
                    mode: SATPModeState::Bare,
                    base: 0,
                    effective: EffectiveTranslationState::Off,
                },
            },
        }
    }

    fn run_debugger(&mut self, mut terminal: Terminal<impl Backend>) -> Result<()> {
        loop {
            self.draw(&mut terminal)?;
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    use KeyCode::*;
                    match key.code {
                        Char('q') | Esc => return Ok(()),
                        Char('s') => self.step(1),
                        Char('b') => self.program.set_breakpoint(),
                        Char('r') => self.step_until_breakpoint(),
                        Char('n') => self.step_until_next_symbol(),
                        Char('j') | Down => {
                            self.program.next();
                            self.update_selected_context();
                        }
                        Char('k') | Up => {
                            self.program.previous();
                            self.update_selected_context();
                        }
                        Char('g') | Home => {
                            self.program.go_top();
                            self.update_selected_context();
                        }
                        Char('G') | End => {
                            self.program.go_bottom();
                            self.update_selected_context();
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    fn draw(&mut self, terminal: &mut Terminal<impl Backend>) -> Result<()> {
        terminal.draw(|f| f.render_widget(self, f.size()))?;
        Ok(())
    }

    fn step(&mut self, max_steps: usize) {
        let result = self.stepper.step_max(max_steps);
        self.update_after_step(result);
    }

    fn step_until_breakpoint(&mut self) {
        // perform at least a step to progress if already on a breakpoint
        let result = self.stepper.step_range_while(1..=MAX_STEPS, |m| {
            let raw_pc = m.hart.pc.read();
            let pc = m
                .translate(raw_pc, AccessType::Instruction)
                .unwrap_or(raw_pc);
            !self.program.breakpoints.contains(&pc)
        });
        self.update_after_step(result);
    }

    fn step_until_next_symbol(&mut self) {
        // perform at least a step to progress if already on a breakpoint/symbol
        let result = self.stepper.step_range_while(1..=MAX_STEPS, |m| {
            let raw_pc = m.hart.pc.read();
            let pc = m
                .translate(raw_pc, AccessType::Instruction)
                .unwrap_or(raw_pc);

            !(self.program.breakpoints.contains(&pc) || self.program.symbols.contains_key(&pc))
        });
        self.update_after_step(result);
    }
}

impl<'a> ProgramView<'a> {
    fn with_items(
        instructions: Vec<Instruction>,
        symbols: HashMap<u64, &'a str>,
    ) -> ProgramView<'a> {
        ProgramView {
            state: ListState::default().with_selected(Some(0)),
            instructions,
            next_instr: 0,
            breakpoints: HashSet::new(),
            symbols,
        }
    }

    fn set_breakpoint(&mut self) {
        let instr = self.state.selected().unwrap_or(self.next_instr);
        let address = self.instructions[instr].address;
        if self.breakpoints.contains(&address) {
            self.breakpoints.remove(&address);
        } else {
            self.breakpoints.insert(address);
        }
    }

    fn next(&mut self) {
        let i = match self.state.selected() {
            Some(i) => {
                if i >= self.instructions.len() - 1 {
                    0
                } else {
                    i + 1
                }
            }
            None => self.next_instr,
        };
        self.state.select(Some(i));
    }

    fn previous(&mut self) {
        let i = match self.state.selected() {
            Some(i) => {
                if i == 0 {
                    self.instructions.len() - 1
                } else {
                    i - 1
                }
            }
            None => self.next_instr,
        };
        self.state.select(Some(i));
    }

    fn go_top(&mut self) {
        self.state.select(Some(0));
    }

    fn go_bottom(&mut self) {
        self.state.select(Some(self.instructions.len() - 1));
    }

    pub fn partial_update(&mut self, mut new_instructions: Vec<Instruction>) {
        // Update / Insert new_instructions to existing instructions.
        self.instructions.retain(|i| {
            new_instructions
                .iter()
                .all(|new_i| i.address != new_i.address)
        });
        self.instructions.append(&mut new_instructions);
        self.instructions.sort_by(|a, b| a.address.cmp(&b.address));
    }
}
