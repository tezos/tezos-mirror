// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{
    DebuggerApp, Instruction, SATPModeState, TranslationState, BLUE, GRAY, GREEN, NEXT_STYLE_FG,
    ORANGE, RED, SELECTED_STYLE_FG, YELLOW,
};
use octez_riscv::{
    machine_state::{
        csregisters::{
            self,
            satp::SvLength,
            xstatus::{ExtensionValue, MPPValue, MStatus, SPPValue},
            CSRegister,
        },
        registers,
    },
    stepper::{Stepper, StepperStatus},
};
use ratatui::{
    prelude::*,
    style::Stylize,
    symbols::border,
    widgets::{block::*, *},
};

macro_rules! xregister_line {
    ($self: ident, $reg: ident) => {
        Line::from(vec![
            format!("   {0} ({0:?}): ", $reg).into(),
            format!(
                "{} ",
                $self.stepper.machine_state().hart.xregisters.read($reg)
            )
            .fg(super::YELLOW),
            format!(
                "0x{:x}",
                $self.stepper.machine_state().hart.xregisters.read($reg)
            )
            .fg(super::ORANGE),
        ])
    };
}

macro_rules! fregister_line {
    ($self: ident, $reg: ident) => {
        Line::from(vec![
            format!("   {0} ({0:?}): ", $reg).into(),
            format!(
                "{} ",
                u64::from($self.stepper.machine_state().hart.fregisters.read($reg))
            )
            .fg(super::YELLOW),
            format!(
                "0x{:x}",
                u64::from($self.stepper.machine_state().hart.fregisters.read($reg))
            )
            .fg(super::ORANGE),
        ])
    };
}

impl SATPModeState {
    pub fn text(&self) -> &'static str {
        match self {
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
            Self::Bare => BLUE,
            Self::Sv(_) => GREEN,
        }
    }
}

impl Instruction {
    fn to_list_item(
        &self,
        next: bool,
        selected: bool,
        breakpoint: bool,
        symbol: Option<String>,
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
        if let Some((address, label)) = &self.jump {
            let label = label
                .as_ref()
                .map(|x| format!("({})", x))
                .unwrap_or_default();
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

impl<'a, S> DebuggerApp<'a, S>
where
    S: Stepper,
{
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
                    self.program
                        .symbols
                        .get(&instr.address)
                        .map(|s| s.to_string()),
                )
            })
            .collect();

        let list = List::new(instructions).block(block);
        StatefulWidget::render(list, area, buf, &mut self.program.state)
    }

    fn render_xregisters_pane(&self, area: Rect, buf: &mut Buffer) {
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

    fn render_mstatus_pane(&self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" MSTATUS ".bold());
        let block = Block::default()
            .title(title.alignment(Alignment::Left))
            .borders(Borders::ALL)
            .border_set(border::THICK);

        use csregisters::*;
        use CSRegister as CSR;

        let mstatus: MStatus = self
            .stepper
            .machine_state()
            .hart
            .csregisters
            .read(CSR::mstatus);
        let mbe = mstatus.mbe();
        let sbe = mstatus.sbe();
        let tvm = mstatus.tvm();
        let tsr = mstatus.tsr();
        let sum = mstatus.sum();
        let mxr = mstatus.mxr();
        let mie = mstatus.mie();
        let sie = mstatus.sie();
        let mpie = mstatus.mpie();
        let spie = mstatus.spie();
        let mpp = mstatus.mpp();
        let spp = mstatus.spp();
        let mprv = mstatus.mprv();
        let tw = mstatus.tw();
        let ube = mstatus.ube();
        let sd = mstatus.sd();
        let xs = mstatus.xs();
        let fs = mstatus.fs();

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

    fn render_fregisters_pane(&self, area: Rect, buf: &mut Buffer) {
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

    fn render_fcsr_pane(&self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" FCSR ".bold());
        let block = Block::default()
            .title(title.alignment(Alignment::Left))
            .borders(Borders::ALL)
            .border_set(border::THICK);

        use csregisters::*;

        let frm: CSRRepr = self
            .stepper
            .machine_state()
            .hart
            .csregisters
            .read(CSRegister::frm);
        let fflags: CSRRepr = self
            .stepper
            .machine_state()
            .hart
            .csregisters
            .read(CSRegister::fflags);

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
                    self.stepper
                        .machine_state()
                        .hart
                        .csregisters
                        .read::<CSRRepr>(CSRegister::fcsr)
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

    fn render_status_pane(&self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" Status ".bold());
        let block = Block::default()
            .title(title.alignment(Alignment::Left))
            .borders(Borders::ALL)
            .border_set(border::THICK);
        let pc_line = Line::from(vec![
            "   PC: ".into(),
            format!("{:x}", self.stepper.machine_state().hart.pc.read()).fg(ORANGE),
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
            format!("{:?}", self.stepper.machine_state().hart.mode.read()).fg(BLUE),
        ]);
        let status_text = match &self.state.result {
            StepperStatus::Running { steps } => vec![
                Line::from(vec!["   Running".bold().fg(GREEN)]),
                Line::from(vec![format!("   Steps executed: {}", steps).into()]),
                pc_line,
                virt_line,
                mode_line,
            ],
            StepperStatus::Exited {
                status,
                success,
                steps,
            } => {
                let color = if *success { YELLOW } else { RED };
                vec![
                    Line::from(vec![format!("   Exited with: {}", status).bold().fg(color)]),
                    Line::from(vec![format!("   Steps executed: {}", steps).into()]),
                    pc_line,
                    virt_line,
                ]
            }
            StepperStatus::Errored {
                cause,
                message,
                steps,
            } => vec![
                Line::from(vec![format!("   Exception: {}", cause).bold().fg(RED)]),
                Line::from(vec![format!("   Message: {}", message).bold().fg(RED)]),
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

    fn render_bottom_bar(&self, area: Rect, buf: &mut Buffer) {
        Line::from(vec![
            " Step ".into(),
            "<s>  ".fg(BLUE).bold(),
            " Navigate ".into(),
            "<↑/↓>  ".fg(BLUE).bold(),
            " Place breakpoint ".into(),
            "<b>  ".fg(BLUE).bold(),
            " Run ".into(),
            "<r>  ".fg(BLUE).bold(),
            " Next symbol ".into(),
            "<n>  ".fg(BLUE).bold(),
            " Quit ".into(),
            "<q> ".fg(BLUE).bold(),
        ])
        .render(area, buf);
    }
}

impl<'a, S> Widget for &mut DebuggerApp<'a, S>
where
    S: Stepper,
{
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
