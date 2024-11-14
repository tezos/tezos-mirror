// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![allow(unused)]

use gdbstub::arch::Arch;
use gdbstub::common::Signal;
use gdbstub::conn::{Connection, ConnectionExt};
use gdbstub::stub::{run_blocking, DisconnectReason, GdbStub, SingleThreadStopReason};
use gdbstub::target::ext::base::singlethread::{
    SingleThreadBase, SingleThreadResume, SingleThreadResumeOps, SingleThreadSingleStep,
    SingleThreadSingleStepOps,
};
use gdbstub::target::ext::base::BaseOps;
use gdbstub::target::ext::breakpoints::{
    Breakpoints, BreakpointsOps, SwBreakpoint, SwBreakpointOps,
};
use gdbstub::target::{Target, TargetError, TargetResult};
use gdbstub_arch::riscv::reg::RiscvCoreRegs;
use octez_riscv::machine_state::bus::main_memory::M1G;
use octez_riscv::machine_state::bus::OutOfBounds;
use octez_riscv::machine_state::mode::Mode;
use octez_riscv::machine_state::registers::XRegister;
use octez_riscv::stepper::test::TestStepper;
use octez_riscv::stepper::Stepper;

use crate::cli::GdbServerOptions;
use std::error::Error;
use std::marker::PhantomData;
use std::net::{TcpListener, TcpStream};
use std::{fs, io};

/// Run a gdb server that can be used for debugging RISC-V programs.
pub fn gdb_server(opts: GdbServerOptions) -> Result<(), Box<dyn Error>> {
    let fname = opts
        .input
        .file_name()
        .ok_or("Invalid program path")?
        .to_str()
        .ok_or("File name cannot be converted to string")?;
    let program = fs::read(&opts.input)?;
    let (mut stepper, _symbols) =
        TestStepper::<M1G>::new_with_parsed_program(program.as_slice(), None, Mode::User)?;

    // loop
    let mut target = RiscvGdb {
        stepper: &mut stepper,
    };

    let connection = wait_for_gdb_connection(opts.port)?;
    let connection: Box<dyn ConnectionExt<Error = std::io::Error>> = Box::new(connection);
    let mut debugger = GdbStub::new(connection);

    let disconnect = debugger.run_blocking::<RiscvEventLoop<_>>(&mut target)?;

    eprintln!("{:?}", disconnect);
    Ok(())
}

/// Blocks until a GDB client connects via TCP.
/// i.e: Running `target remote localhost:<port>` from the GDB prompt.
fn wait_for_gdb_connection(port: u16) -> io::Result<TcpStream> {
    let sockaddr = format!("localhost:{}", port);
    eprintln!("Waiting for a GDB connection on {:?}...", sockaddr);
    let sock = TcpListener::bind(sockaddr)?;
    let (stream, addr) = sock.accept()?;

    eprintln!("Debugger connected from {}", addr);
    Ok(stream) // `TcpStream` implements `gdbstub::Connection`
}

struct RiscvGdb<'a, S: Stepper> {
    stepper: &'a mut S,
}

impl<'a, S: Stepper> RiscvGdb<'a, S> {
    fn run_and_check_for_incoming_data(
        &mut self,
        conn: &mut <RiscvEventLoop<S> as run_blocking::BlockingEventLoop>::Connection,
    ) -> RiscvGdbEvent {
        let mut poll_incoming_data = || {
            // gdbstub takes ownership of the underlying connection, so the `borrow_conn`
            // method is used to borrow the underlying connection back from the stub to
            // check for incoming data.
            conn.peek().map(|b| b.is_some()).unwrap_or(true)
        };

        while !poll_incoming_data() {}

        RiscvGdbEvent::IncomingData
    }
}

enum RiscvGdbEvent {
    IncomingData,
}

impl<'a, S: Stepper> Target for RiscvGdb<'a, S> {
    type Error = String;
    type Arch = gdbstub_arch::riscv::Riscv64;

    #[inline(always)]
    fn base_ops(&mut self) -> BaseOps<Self::Arch, Self::Error> {
        BaseOps::SingleThread(self)
    }

    // opt-in to support for setting/removing breakpoints
    #[inline(always)]
    fn support_breakpoints(&mut self) -> Option<BreakpointsOps<Self>> {
        Some(self)
    }
}

impl<'a, S: Stepper> SingleThreadBase for RiscvGdb<'a, S> {
    fn read_registers(&mut self, regs: &mut RiscvCoreRegs<u64>) -> TargetResult<(), Self> {
        let state = self.stepper.machine_state();
        regs.pc = state.hart.pc.read();

        let array = state.hart.xregisters.struct_ref();
        for i in 0..31 {
            regs.x[i + 1] = array.read(i);
        }
        Ok(())
    }

    fn write_registers(
        &mut self,
        regs: &<Self::Arch as Arch>::Registers,
    ) -> TargetResult<(), Self> {
        todo!()
    }

    fn read_addrs(&mut self, start_addr: u64, data: &mut [u8]) -> TargetResult<usize, Self> {
        const EFAULT: u8 = 14;
        eprintln!("read_addrs: {start_addr} -> {}", data.len());

        let state = self.stepper.machine_state();

        use octez_riscv::machine_state::bus::AddressableRead;

        match state.main_memory.read_all(start_addr, data) {
            Ok(()) => Ok(data.len()),
            Err(OutOfBounds) => Err(TargetError::Errno(EFAULT)),
        }
    }

    fn write_addrs(&mut self, start_addr: u64, data: &[u8]) -> TargetResult<(), Self> {
        todo!()
    }

    #[inline(always)]
    fn support_resume(&mut self) -> Option<SingleThreadResumeOps<Self>> {
        Some(self)
    }
}

impl<'a, S: Stepper> SingleThreadResume for RiscvGdb<'a, S> {
    fn resume(&mut self, signal: Option<Signal>) -> Result<(), Self::Error> {
        todo!()
    }

    #[inline(always)]
    fn support_single_step(&mut self) -> Option<SingleThreadSingleStepOps<'_, Self>> {
        Some(self)
    }
}

impl<'a, S: Stepper> SingleThreadSingleStep for RiscvGdb<'a, S> {
    fn step(&mut self, signal: Option<Signal>) -> Result<(), Self::Error> {
        todo!()
    }
}

impl<'a, S: Stepper> Breakpoints for RiscvGdb<'a, S> {
    // there are several kinds of breakpoints - this target uses software breakpoints
    // We implement support for Software Breakpoints, in a similar fashion to
    // the debugger.
    #[inline(always)]
    fn support_sw_breakpoint(&mut self) -> Option<SwBreakpointOps<Self>> {
        Some(self)
    }
}

impl<'a, S: Stepper> SwBreakpoint for RiscvGdb<'a, S> {
    fn add_sw_breakpoint(
        &mut self,
        addr: u64,
        kind: <Self::Arch as Arch>::BreakpointKind,
    ) -> TargetResult<bool, Self> {
        todo!()
    }

    fn remove_sw_breakpoint(
        &mut self,
        addr: u64,
        kind: <Self::Arch as Arch>::BreakpointKind,
    ) -> TargetResult<bool, Self> {
        todo!()
    }
}

struct RiscvEventLoop<'a, S: Stepper>(PhantomData<&'a S>);

// The `run_blocking::BlockingEventLoop` groups together various callbacks
// the `GdbStub::run_blocking` event loop requires you to implement.
impl<'a, S: Stepper> run_blocking::BlockingEventLoop for RiscvEventLoop<'a, S> {
    type Target = RiscvGdb<'a, S>;
    type Connection = Box<dyn ConnectionExt<Error = std::io::Error>>;

    // or MultiThreadStopReason on multi threaded targets
    type StopReason = SingleThreadStopReason<u64>;

    // Invoked immediately after the target's `resume` method has been
    // called. The implementation should block until either the target
    // reports a stop reason, or if new data was sent over the connection.
    fn wait_for_stop_reason(
        target: &mut Self::Target,
        conn: &mut Self::Connection,
    ) -> Result<
        run_blocking::Event<SingleThreadStopReason<u64>>,
        run_blocking::WaitForStopReasonError<
            <Self::Target as Target>::Error,
            <Self::Connection as Connection>::Error,
        >,
    > {
        // the specific mechanism to "select" between incoming data and target
        // events will depend on your project's architecture.
        //
        // some examples of how you might implement this method include: `epoll`,
        // `select!` across multiple event channels, periodic polling, etc...
        //
        // in this example, lets assume the target has a magic method that handles
        // this for us.
        let event = match target.run_and_check_for_incoming_data(conn) {
            RiscvGdbEvent::IncomingData => {
                let byte = conn
                    .read() // method provided by the `ConnectionExt` trait
                    .map_err(run_blocking::WaitForStopReasonError::Connection)?;

                run_blocking::Event::IncomingData(byte)
            }
        };

        Ok(event)
    }

    // Invoked when the GDB client sends a Ctrl-C interrupt.
    fn on_interrupt(
        target: &mut Self::Target,
    ) -> Result<Option<SingleThreadStopReason<u64>>, <Self::Target as Target>::Error> {
        // notify the target that a ctrl-c interrupt has occurred.
        // target.stop_in_response_to_ctrl_c_interrupt()?;

        // a pretty typical stop reason in response to a Ctrl-C interrupt is to
        // report a "Signal::SIGINT".
        Ok(Some(SingleThreadStopReason::Signal(Signal::SIGINT).into()))
    }
}
