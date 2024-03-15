// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use color_eyre::Result;
use crossterm::event::{self, Event, KeyCode, KeyEventKind};
use ratatui::{
    prelude::*,
    style::palette::tailwind,
    symbols::border,
    widgets::{block::*, *},
};
use risc_v_interpreter::{
    machine_state::{mode::Mode, registers},
    Interpreter, InterpreterResult,
};
use std::collections::{BTreeMap, HashSet};

mod errors;
mod tui;

const GREEN: Color = tailwind::GREEN.c400;
const YELLOW: Color = tailwind::YELLOW.c400;
const RED: Color = tailwind::RED.c500;
const BLUE: Color = tailwind::BLUE.c400;
const ORANGE: Color = tailwind::ORANGE.c500;
const SELECTED_STYLE_FG: Color = BLUE;
const NEXT_STYLE_FG: Color = GREEN;
const MAX_STEPS: usize = 1_000_000;

#[derive(Debug)]
struct Instruction<'a> {
    address: u64,
    text: &'a str,
}

struct ProgramView<'a> {
    state: ListState,
    instructions: Vec<Instruction<'a>>,
    next_instr: usize,
    breakpoints: HashSet<u64>,
}

pub struct DebuggerApp<'a> {
    title: &'a str,
    interpreter: &'a mut Interpreter<'a>,
    program: ProgramView<'a>,
    status: InterpreterResult,
}

macro_rules! register_line {
    ($self: ident, $reg: ident) => {
        Line::from(vec![
            format!("   {0} ({0:?}): ", $reg).into(),
            format!("{} ", $self.interpreter.read_register($reg)).fg(YELLOW),
            format!("0x{:x}", $self.interpreter.read_register($reg)).fg(ORANGE),
        ])
    };
}

impl<'a> DebuggerApp<'a> {
    pub fn launch(fname: &str, contents: &[u8]) -> Result<()> {
        let mut backend = Interpreter::create_backend();
        let (mut interpreter, prog) =
            Interpreter::new_with_parsed_program(&mut backend, contents, None, Mode::User)?;
        errors::install_hooks()?;
        let terminal = tui::init()?;
        DebuggerApp::new(&mut interpreter, fname, &prog).run_debugger(terminal)?;
        tui::restore()?;
        Ok(())
    }

    fn new(
        interpreter: &'a mut Interpreter<'a>,
        title: &'a str,
        program: &'a BTreeMap<u64, String>,
    ) -> Self {
        Self {
            title,
            interpreter,
            program: ProgramView::with_items(
                program
                    .iter()
                    .map(|x| Instruction {
                        address: *x.0,
                        text: x.1,
                    })
                    .collect::<Vec<Instruction>>(),
            ),
            status: InterpreterResult::Running(0),
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

    fn update_after_step(&mut self, result: InterpreterResult) {
        let pc = self.interpreter.read_pc();
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
        self.status = result;
    }

    fn step(&mut self, max_steps: usize) {
        let result = self.interpreter.run(max_steps);
        self.update_after_step(result);
    }

    fn step_until_breakpoint(&mut self) {
        self.step(1);
        let result = self.interpreter.step_many(MAX_STEPS, |m| {
            !self.program.breakpoints.contains(&m.hart.pc.read())
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
                    self.program.breakpoints.contains(&instr.address),
                )
            })
            .collect();

        let list = List::new(instructions).block(block).highlight_style(
            Style::default()
                .add_modifier(Modifier::BOLD)
                .add_modifier(Modifier::REVERSED)
                .fg(SELECTED_STYLE_FG),
        );
        StatefulWidget::render(list, area, buf, &mut self.program.state)
    }

    fn render_registers_pane(&mut self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" Registers ".bold());
        let block = Block::default()
            .title(title.alignment(Alignment::Left))
            .borders(Borders::ALL)
            .border_set(border::THICK);

        use registers::*;
        let registers_text = Text::from(vec![
            register_line!(self, ra),
            register_line!(self, sp),
            register_line!(self, gp),
            register_line!(self, tp),
            register_line!(self, t0),
            register_line!(self, t1),
            register_line!(self, t2),
            register_line!(self, s0),
            register_line!(self, s1),
            register_line!(self, a0),
            register_line!(self, a1),
            register_line!(self, a2),
            register_line!(self, a3),
            register_line!(self, a4),
            register_line!(self, a5),
            register_line!(self, a6),
            register_line!(self, a7),
            register_line!(self, s2),
            register_line!(self, s3),
            register_line!(self, s4),
            register_line!(self, s5),
            register_line!(self, s6),
            register_line!(self, s7),
            register_line!(self, s8),
            register_line!(self, s9),
            register_line!(self, s10),
            register_line!(self, s11),
            register_line!(self, t3),
            register_line!(self, t4),
            register_line!(self, t5),
            register_line!(self, t6),
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
        let status_text = match &self.status {
            InterpreterResult::Running(steps) => vec![
                Line::from(vec!["   Running".bold().fg(GREEN)]),
                Line::from(vec![format!("   Steps executed: {}", steps).into()]),
                pc_line,
            ],
            InterpreterResult::Exit { code, steps } => {
                let color = if *code == 0 { YELLOW } else { RED };
                vec![
                    Line::from(vec![format!("   Exit with code {}", code).bold().fg(color)]),
                    Line::from(vec![format!("   Steps executed: {}", steps).into()]),
                    pc_line,
                ]
            }
            InterpreterResult::Exception(exc, steps) => vec![
                Line::from(vec![format!("   Exception: {:?}", exc).bold().fg(RED)]),
                Line::from(vec![format!("   Steps executed: {}", steps).into()]),
                pc_line,
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
        let outer_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Fill(1), Constraint::Length(1)])
            .split(area);

        let main_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)]);
        let [program_area, rhs_area] = main_layout.areas(outer_layout[0]);

        let rhs_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Fill(1), Constraint::Length(5)]);
        let [registers_area, status_area] = rhs_layout.areas(rhs_area);

        self.render_program_pane(program_area, buf);
        self.render_registers_pane(registers_area, buf);
        self.render_status_pane(status_area, buf);
        self.render_bottom_bar(outer_layout[1], buf);
    }
}

impl Instruction<'_> {
    fn to_list_item(&self, next: bool, breakpoint: bool) -> ListItem {
        let color = if next {
            Style::default()
                .add_modifier(Modifier::BOLD)
                .add_modifier(Modifier::REVERSED)
                .fg(NEXT_STYLE_FG)
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
        ListItem::new(Line::from(line).style(color))
    }
}

impl ProgramView<'_> {
    fn with_items(instructions: Vec<Instruction<'_>>) -> ProgramView<'_> {
        ProgramView {
            state: ListState::default().with_selected(Some(0)),
            instructions,
            next_instr: 0,
            breakpoints: HashSet::new(),
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
