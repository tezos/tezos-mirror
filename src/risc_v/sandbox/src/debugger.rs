// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use color_eyre::Result;
use crossterm::event::{self, Event, KeyCode, KeyEventKind};
use ratatui::{
    prelude::*,
    style::{palette::tailwind, Stylize},
    symbols::border,
    widgets::{block::*, *},
};
use risc_v_interpreter::{
    machine_state::{
        bus::Address,
        csregisters::{
            self,
            satp::{self, SvLength, TranslationAlgorithm},
            xstatus::{ExtensionValue, MPPValue, SPPValue},
            CSRRepr, CSRegister,
        },
        mode::Mode,
        registers, AccessType,
    },
    Interpreter, InterpreterResult,
};
use std::collections::{BTreeMap, HashMap, HashSet};

mod errors;
mod tui;

const GREEN: Color = tailwind::GREEN.c400;
const YELLOW: Color = tailwind::YELLOW.c400;
const RED: Color = tailwind::RED.c500;
const BLUE: Color = tailwind::BLUE.c400;
const ORANGE: Color = tailwind::ORANGE.c500;
const GRAY: Color = tailwind::GRAY.c500;
const SELECTED_STYLE_FG: Color = BLUE;
const NEXT_STYLE_FG: Color = GREEN;
const MAX_STEPS: usize = 1_000_000;

#[derive(Debug, Clone)]
struct Instruction<'a> {
    address: u64,
    text: &'a str,
    jump: Option<(u64, Option<&'a str>)>,
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
    // Reserved / Unsupported
    Invalid,
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
    fn update(
        &mut self,
        faulting: bool,
        effective_mode: Option<TranslationAlgorithm>,
        satp_val: CSRRepr,
    ) {
        self.effective = if faulting {
            EffectiveTranslationState::Faulting
        } else {
            match effective_mode {
                None => EffectiveTranslationState::Off,
                Some(_alg) => EffectiveTranslationState::On,
            }
        };

        self.base = satp::get_PPN(satp_val);

        self.mode = match satp::get_MODE(satp_val) {
            None => SATPModeState::Invalid,
            Some(alg) => match alg {
                TranslationAlgorithm::Bare => SATPModeState::Bare,
                TranslationAlgorithm::Sv(length) => SATPModeState::Sv(length),
            },
        }
    }
}

impl SATPModeState {
    pub fn text(&self) -> &'static str {
        match self {
            Self::Invalid => "Invalid",
            Self::Bare => "Bare",
            Self::Sv(length) => match length {
                SvLength::Sv39 => "Sv39",
                SvLength::Sv48 => "Sv48",
                SvLength::Sv57 => "Sv57",
            },
        }
    }

    pub fn fg(&self) -> Color {
        match self {
            Self::Invalid => GRAY,
            Self::Bare => BLUE,
            Self::Sv(_) => GREEN,
        }
    }
}

struct DebuggerState {
    pub interpreter: InterpreterResult,
    pub prev_pc: Address,
    pub translation: TranslationState,
}

struct ProgramView<'a> {
    state: ListState,
    instructions: Vec<Instruction<'a>>,
    next_instr: usize,
    breakpoints: HashSet<u64>,
    symbols: HashMap<u64, &'a str>,
}

pub struct DebuggerApp<'a> {
    title: &'a str,
    interpreter: &'a mut Interpreter<'a>,
    program: ProgramView<'a>,
    state: DebuggerState,
}

macro_rules! xregister_line {
    ($self: ident, $reg: ident) => {
        Line::from(vec![
            format!("   {0} ({0:?}): ", $reg).into(),
            format!("{} ", $self.interpreter.read_xregister($reg)).fg(YELLOW),
            format!("0x{:x}", $self.interpreter.read_xregister($reg)).fg(ORANGE),
        ])
    };
}

macro_rules! fregister_line {
    ($self: ident, $reg: ident) => {
        Line::from(vec![
            format!("   {0} ({0:?}): ", $reg).into(),
            format!("{} ", u64::from($self.interpreter.read_fregister($reg))).fg(YELLOW),
            format!("0x{:x}", u64::from($self.interpreter.read_fregister($reg))).fg(ORANGE),
        ])
    };
}

impl<'a> Instruction<'a> {
    fn new(address: u64, text: &'a str, symbols: &HashMap<u64, &'a str>) -> Self {
        let jump = match text
            .split(' ')
            .next()
            .expect("Unexpected instruction format")
        {
            "jal" | "beq" | "bne" | "blt" | "bge" | "bltu" | "bgeu" => {
                text.split(',').last().map(|jump_address| {
                    let addr = address.wrapping_add(jump_address.parse::<i64>().unwrap() as u64);
                    (addr, symbols.get(&addr).copied())
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

impl<'a> DebuggerApp<'a> {
    pub fn launch(fname: &str, contents: &[u8], exit_mode: Mode) -> Result<()> {
        let mut backend = Interpreter::create_backend();
        let (mut interpreter, prog) =
            Interpreter::new_with_parsed_program(&mut backend, contents, None, exit_mode)?;
        let symbols = kernel_loader::get_elf_symbols(contents)?;
        errors::install_hooks()?;
        let terminal = tui::init()?;
        DebuggerApp::new(&mut interpreter, fname, &prog, symbols).run_debugger(terminal)?;
        tui::restore()?;
        Ok(())
    }

    fn new(
        interpreter: &'a mut Interpreter<'a>,
        title: &'a str,
        program: &'a BTreeMap<u64, String>,
        symbols: HashMap<u64, &'a str>,
    ) -> Self {
        Self {
            title,
            interpreter,
            program: ProgramView::with_items(
                program
                    .iter()
                    .map(|x| Instruction::new(*x.0, x.1, &symbols))
                    .collect::<Vec<Instruction>>(),
                symbols,
            ),
            state: DebuggerState {
                interpreter: InterpreterResult::Running(0),
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
                        Char('j') | Down => self.program.next(),
                        Char('k') | Up => self.program.previous(),
                        Char('g') | Home => self.program.go_top(),
                        Char('G') | End => self.program.go_bottom(),
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

    /// Returns the physical program counter, and if the translation algorithm is faulting
    fn update_pc_after_step(&mut self) -> (Address, bool) {
        let raw_pc = self.interpreter.read_pc();
        let res @ (pc, _faulting) = match self.interpreter.translate_instruction_address(raw_pc) {
            Err(_e) => (self.state.prev_pc, true),
            Ok(pc) => (pc, false),
        };

        self.state.prev_pc = pc;

        res
    }

    fn update_translation_after_step(&mut self, faulting: bool) {
        let effective_mode = self
            .interpreter
            .effective_translation_alg(&AccessType::Instruction);
        let satp_val = self.interpreter.read_csregister(CSRegister::satp);
        self.state
            .translation
            .update(faulting, effective_mode, satp_val)
    }

    fn update_after_step(&mut self, result: InterpreterResult) {
        let (pc, faulting) = self.update_pc_after_step();
        self.update_translation_after_step(faulting);

        self.program.next_instr = self
            .program
            .instructions
            .iter()
            .position(|instr| instr.address == pc)
            .unwrap_or_else(|| {
                panic!(
                    "pc {:x} does not correspond to any instruction's address",
                    pc
                )
            });
        self.program.state.select(Some(self.program.next_instr));
        self.state.interpreter = result;
    }

    fn step(&mut self, max_steps: usize) {
        let result = self.interpreter.run(max_steps);
        self.update_after_step(result);
    }

    fn step_until_breakpoint(&mut self) {
        self.step(1);
        let result = self.interpreter.step_many(MAX_STEPS, |m| {
            let raw_pc = m.hart.pc.read();
            let pc = m
                .translate(raw_pc, AccessType::Instruction)
                .unwrap_or(raw_pc);
            !self.program.breakpoints.contains(&pc)
        });
        self.update_after_step(result);
    }

    fn render_program_pane(&mut self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(format!(" {} ", self.title).bold());
        let block = Block::default()
            .title(title.alignment(Alignment::Left))
            .borders(Borders::ALL)
            .border_set(border::THICK);
        let instructions: Vec<ListItem> = self
            .program
            .instructions
            .iter()
            .enumerate()
            .map(|(i, instr)| {
                instr.to_list_item(
                    i == self.program.next_instr,
                    i == self
                        .program
                        .state
                        .selected()
                        .unwrap_or(self.program.next_instr),
                    self.program.breakpoints.contains(&instr.address),
                    self.program.symbols.get(&instr.address).copied(),
                )
            })
            .collect();

        let list = List::new(instructions).block(block);
        StatefulWidget::render(list, area, buf, &mut self.program.state)
    }

    fn render_xregisters_pane(&mut self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" X Registers ".bold());
        let block = Block::default()
            .title(title.alignment(Alignment::Left))
            .borders(Borders::ALL)
            .border_set(border::THICK);

        use registers::*;
        let registers_text = Text::from(vec![
            xregister_line!(self, ra),
            xregister_line!(self, sp),
            xregister_line!(self, gp),
            xregister_line!(self, tp),
            xregister_line!(self, t0),
            xregister_line!(self, t1),
            xregister_line!(self, t2),
            xregister_line!(self, s0),
            xregister_line!(self, s1),
            xregister_line!(self, a0),
            xregister_line!(self, a1),
            xregister_line!(self, a2),
            xregister_line!(self, a3),
            xregister_line!(self, a4),
            xregister_line!(self, a5),
            xregister_line!(self, a6),
            xregister_line!(self, a7),
            xregister_line!(self, s2),
            xregister_line!(self, s3),
            xregister_line!(self, s4),
            xregister_line!(self, s5),
            xregister_line!(self, s6),
            xregister_line!(self, s7),
            xregister_line!(self, s8),
            xregister_line!(self, s9),
            xregister_line!(self, s10),
            xregister_line!(self, s11),
            xregister_line!(self, t3),
            xregister_line!(self, t4),
            xregister_line!(self, t5),
            xregister_line!(self, t6),
        ]);

        Paragraph::new(registers_text)
            .left_aligned()
            .block(block)
            .render(area, buf)
    }

    fn render_mstatus_pane(&mut self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" MSTATUS ".bold());
        let block = Block::default()
            .title(title.alignment(Alignment::Left))
            .borders(Borders::ALL)
            .border_set(border::THICK);

        use csregisters::*;
        use CSRegister as CSR;

        let mstatus = self.interpreter.read_csregister(CSR::mstatus);
        let mbe = xstatus::get_MBE(mstatus);
        let sbe = xstatus::get_SBE(mstatus);
        let tvm = xstatus::get_TVM(mstatus);
        let tsr = xstatus::get_TSR(mstatus);
        let sum = xstatus::get_SUM(mstatus);
        let mxr = xstatus::get_MXR(mstatus);
        let mie = xstatus::get_MIE(mstatus);
        let sie = xstatus::get_SIE(mstatus);
        let mpie = xstatus::get_MPIE(mstatus);
        let spie = xstatus::get_SPIE(mstatus);
        let mpp = xstatus::get_MPP(mstatus);
        let spp = xstatus::get_SPP(mstatus);
        let mprv = xstatus::get_MPRV(mstatus);
        let tw = xstatus::get_TW(mstatus);
        let ube = xstatus::get_UBE(mstatus);
        let sd = xstatus::get_SD(mstatus);
        let xs = xstatus::get_XS(mstatus);
        let fs = xstatus::get_FS(mstatus);
        let vs = xstatus::get_VS(mstatus);

        let bool_field = |name: &'static str, toggled| match toggled {
            false => name.fg(GRAY),
            true => name.bold(),
        };

        let spp_field = |spp_val: SPPValue| match spp_val {
            SPPValue::User => "U".bold(),
            SPPValue::Supervisor => "S".fg(BLUE),
        };

        let mpp_field = |mpp_val: MPPValue| match mpp_val {
            MPPValue::User => "U".bold(),
            MPPValue::Supervisor => "S".fg(BLUE),
            MPPValue::Machine => "M".fg(RED),
        };

        let ext_field = |ext_val: ExtensionValue| match ext_val {
            ExtensionValue::Off => "O".fg(GRAY),
            ExtensionValue::Initial => "I".fg(GREEN),
            ExtensionValue::Clean => "C".fg(BLUE),
            ExtensionValue::Dirty => "D".fg(RED),
        };

        let mstatus_text = Text::from(vec![
            Line::from(vec![
                // M-level / higher-privilege settings
                " XS:".fg(GRAY),
                ext_field(xs),
                bool_field(" MBE", mbe),
                bool_field(" MIE", mie),
                bool_field(" MPIE", mpie),
                " MPP:".into(),
                mpp_field(mpp),
            ]),
            Line::from(vec![
                // S-level / lower-privilege settings
                " FS:".fg(GRAY),
                ext_field(fs),
                bool_field(" SBE", sbe),
                bool_field(" SIE", sie),
                bool_field(" SPIE", spie),
                " SPP:".into(),
                spp_field(spp),
            ]),
            Line::from(vec![
                // Misc
                " VS:".fg(GRAY),
                ext_field(vs),
                bool_field(" TVM", tvm),
                bool_field(" SUM", sum),
                bool_field(" TSR", tsr),
                bool_field(" MPRV", mprv),
                bool_field(" MXR", mxr),
                bool_field(" TW", tw),
                bool_field(" UBE", ube),
                bool_field(" SD", sd),
            ]),
        ]);

        Paragraph::new(mstatus_text)
            .left_aligned()
            .block(block)
            .render(area, buf)
    }

    fn render_fregisters_pane(&mut self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" F Registers ".bold());
        let block = Block::default()
            .title(title.alignment(Alignment::Left))
            .borders(Borders::ALL)
            .border_set(border::THICK);

        use registers::*;
        let registers_text = Text::from(vec![
            fregister_line!(self, ft0),
            fregister_line!(self, ft1),
            fregister_line!(self, ft2),
            fregister_line!(self, ft3),
            fregister_line!(self, ft4),
            fregister_line!(self, ft5),
            fregister_line!(self, ft6),
            fregister_line!(self, ft7),
            fregister_line!(self, fs0),
            fregister_line!(self, fs1),
            fregister_line!(self, fa0),
            fregister_line!(self, fa1),
            fregister_line!(self, fa2),
            fregister_line!(self, fa3),
            fregister_line!(self, fa4),
            fregister_line!(self, fa5),
            fregister_line!(self, fa6),
            fregister_line!(self, fa7),
            fregister_line!(self, fs2),
            fregister_line!(self, fs3),
            fregister_line!(self, fs4),
            fregister_line!(self, fs5),
            fregister_line!(self, fs6),
            fregister_line!(self, fs7),
            fregister_line!(self, fs8),
            fregister_line!(self, fs9),
            fregister_line!(self, fs10),
            fregister_line!(self, ft8),
            fregister_line!(self, ft9),
            fregister_line!(self, ft10),
            fregister_line!(self, ft11),
        ]);

        Paragraph::new(registers_text)
            .left_aligned()
            .block(block)
            .render(area, buf)
    }

    fn render_fcsr_pane(&mut self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" FCSR ".bold());
        let block = Block::default()
            .title(title.alignment(Alignment::Left))
            .borders(Borders::ALL)
            .border_set(border::THICK);

        use csregisters::*;

        let frm = self.interpreter.read_csregister(CSRegister::frm);
        let fflags = self.interpreter.read_csregister(CSRegister::fflags);

        fn rounding_mode(rm: u64) -> &'static str {
            match rm {
                0b000 => "RNE",
                0b001 => "RTZ",
                0b010 => "RDN",
                0b011 => "RUP",
                0b100 => "RMM",
                0b101..=0b111 => "invalid",
                rm => unreachable!("Rounding mode must only be 3bits, got {rm:b}"),
            }
        }

        let flagged = |name: &'static str, bit: usize| {
            if ((1_u64 << bit) & fflags) == 0 {
                name.fg(GRAY)
            } else {
                name.bold()
            }
        };

        let registers_text = Text::from(vec![
            Line::from(vec![
                format!(" {0:>6}: ", CSRegister::fcsr).into(),
                format!(
                    "0x{:02x}",
                    self.interpreter.read_csregister(CSRegister::fcsr)
                )
                .fg(ORANGE),
            ]),
            Line::from(vec![
                format!(" {0:>6}: ", CSRegister::frm).into(),
                format!("0x{0:02x} ", frm).fg(ORANGE),
                format!("0b{0:03b}   ", frm).fg(YELLOW),
                rounding_mode(frm).bold(),
            ]),
            Line::from(vec![
                format!(" {0:>6}: ", CSRegister::fflags).into(),
                format!("0x{0:02x} ", fflags).fg(ORANGE),
                format!("0b{0:05b}", fflags).fg(YELLOW),
                flagged(" NV", 4),
                flagged(" DZ", 3),
                flagged(" OF", 2),
                flagged(" UF", 1),
                flagged(" NX", 0),
            ]),
        ]);

        Paragraph::new(registers_text)
            .left_aligned()
            .block(block)
            .render(area, buf)
    }

    fn render_status_pane(&mut self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" Status ".bold());
        let block = Block::default()
            .title(title.alignment(Alignment::Left))
            .borders(Borders::ALL)
            .border_set(border::THICK);
        let pc_line = Line::from(vec![
            "   PC: ".into(),
            format!("{:x}", self.interpreter.read_pc()).fg(ORANGE),
        ]);
        let TranslationState {
            mode,
            base,
            effective,
        } = &self.state.translation;

        let virt_line = Line::from(vec![
            "   Address Translation: ".into(),
            effective.text().to_string().fg(effective.fg()),
            " | ".into(),
            mode.text().to_string().fg(mode.fg()),
            ":".fg(GRAY),
            format!("{:x}", base).fg(ORANGE),
        ]);
        let mode_line = Line::from(vec![
            "   Mode: ".into(),
            format!("{:?}", self.interpreter.read_mode()).fg(BLUE),
        ]);
        let status_text = match &self.state.interpreter {
            InterpreterResult::Running(steps) => vec![
                Line::from(vec!["   Running".bold().fg(GREEN)]),
                Line::from(vec![format!("   Steps executed: {}", steps).into()]),
                pc_line,
                virt_line,
                mode_line,
            ],
            InterpreterResult::Exit { code, steps } => {
                let color = if *code == 0 { YELLOW } else { RED };
                vec![
                    Line::from(vec![format!("   Exit with code {}", code).bold().fg(color)]),
                    Line::from(vec![format!("   Steps executed: {}", steps).into()]),
                    pc_line,
                    virt_line,
                ]
            }
            InterpreterResult::Exception(exc, steps) => vec![
                Line::from(vec![format!("   Exception: {:?}", exc).bold().fg(RED)]),
                Line::from(vec![format!("   Steps executed: {}", steps).into()]),
                pc_line,
                virt_line,
            ],
        };
        Paragraph::new(Text::from(status_text))
            .left_aligned()
            .block(block)
            .render(area, buf)
    }

    fn render_bottom_bar(&mut self, area: Rect, buf: &mut Buffer) {
        Line::from(vec![
            " Step ".into(),
            "<s>  ".fg(BLUE).bold(),
            " Navigate ".into(),
            "<↑/↓>  ".fg(BLUE).bold(),
            " Place breakpoint ".into(),
            "<b>  ".fg(BLUE).bold(),
            " Run ".into(),
            "<r>  ".fg(BLUE).bold(),
            " Quit ".into(),
            "<q> ".fg(BLUE).bold(),
        ])
        .render(area, buf);
    }
}

impl Widget for &mut DebuggerApp<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        // Split main layout from bottom instructions bar
        use Constraint::{Fill, Length, Percentage};
        let outer_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Fill(1), Length(1)])
            .split(area);

        let main_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Percentage(50), Percentage(50)]);
        let [program_area, rhs_area] = main_layout.areas(outer_layout[0]);

        let rhs_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Fill(1), Length(7)]);
        let [registers_area, status_area] = rhs_layout.areas(rhs_area);

        let registers_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Fill(1), Constraint::Percentage(50)]);
        let [x_area, f_area] = registers_layout.areas(registers_area);

        let x_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Fill(1), Constraint::Length(5)]);
        let [xregisters_area, mstatus_area] = x_layout.areas(x_area);

        let f_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Fill(1), Length(5)]);
        let [fregisters_area, fcsr_area] = f_layout.areas(f_area);

        self.render_program_pane(program_area, buf);
        self.render_xregisters_pane(xregisters_area, buf);
        self.render_mstatus_pane(mstatus_area, buf);
        self.render_fregisters_pane(fregisters_area, buf);
        self.render_fcsr_pane(fcsr_area, buf);
        self.render_status_pane(status_area, buf);
        self.render_bottom_bar(outer_layout[1], buf);
    }
}

impl<'a> Instruction<'a> {
    fn to_list_item(
        &self,
        next: bool,
        selected: bool,
        breakpoint: bool,
        symbol: Option<&'a str>,
    ) -> ListItem {
        let color = if next {
            Style::default()
                .add_modifier(Modifier::BOLD)
                .add_modifier(Modifier::REVERSED)
                .fg(NEXT_STYLE_FG)
        } else if selected {
            Style::default()
                .add_modifier(Modifier::BOLD)
                .add_modifier(Modifier::REVERSED)
                .fg(SELECTED_STYLE_FG)
        } else {
            Style::default()
        };
        let mut line = Vec::new();
        line.push(
            format!(
                " {} {:x}:   ",
                if breakpoint { "⦿" } else { " " },
                self.address
            )
            .into(),
        );
        for p in self.text.split(',') {
            line.push(p.into());
            line.push(" ".into());
        }
        line.pop();

        // If the instruction is a jump address and it points to a known symbol,
        // display the address and the symbol
        if let Some((address, label)) = self.jump {
            let label = label.map(|x| format!("({})", x)).unwrap_or_default();
            line.push(format!(" # {:x} {}", address, label).into())
        }

        // If the address of the instruction is also the address of a known symbol,
        // display the symbol
        match symbol {
            Some(name) => {
                let mut symbol_line = Vec::new();
                symbol_line.push(format!(" {}", name).into());
                ListItem::new(vec![
                    Line::from(""),
                    Line::from(symbol_line),
                    Line::from(line).style(color),
                ])
            }
            None => ListItem::new(Line::from(line).style(color)),
        }
    }
}

impl<'a> ProgramView<'a> {
    fn with_items(
        instructions: Vec<Instruction<'a>>,
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
}
