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
use gdbstub::target::{Target, TargetResult};

use crate::cli::GdbServerOptions;
use std::error::Error;
use std::io;
use std::net::{TcpListener, TcpStream};

/// Run a gdb server that can be used for debugging RISC-V programs.
pub fn gdb_server(opts: GdbServerOptions) -> Result<(), Box<dyn Error>> {
    let mut target = RiscvGdb;
    let connection = wait_for_gdb_connection(opts.port)?;
    let connection: Box<dyn ConnectionExt<Error = std::io::Error>> = Box::new(connection);
    let mut debugger = GdbStub::new(connection);

    let disconnect = debugger.run_blocking::<RiscVEventLoop>(&mut target)?;

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

struct RiscvGdb;

impl RiscvGdb {
    fn run_and_check_for_incoming_data(
        &mut self,
        conn: &mut <RiscVEventLoop as run_blocking::BlockingEventLoop>::Connection,
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

impl Target for RiscvGdb {
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

impl SingleThreadBase for RiscvGdb {
    fn read_registers(
        &mut self,
        regs: &mut <Self::Arch as Arch>::Registers,
    ) -> TargetResult<(), Self> {
        todo!()
    }

    fn write_registers(
        &mut self,
        regs: &<Self::Arch as Arch>::Registers,
    ) -> TargetResult<(), Self> {
        todo!()
    }

    fn read_addrs(&mut self, start_addr: u64, data: &mut [u8]) -> TargetResult<usize, Self> {
        todo!()
    }

    fn write_addrs(&mut self, start_addr: u64, data: &[u8]) -> TargetResult<(), Self> {
        todo!()
    }

    #[inline(always)]
    fn support_resume(&mut self) -> Option<SingleThreadResumeOps<Self>> {
        Some(self)
    }
}

impl SingleThreadResume for RiscvGdb {
    fn resume(&mut self, signal: Option<Signal>) -> Result<(), Self::Error> {
        todo!()
    }

    #[inline(always)]
    fn support_single_step(&mut self) -> Option<SingleThreadSingleStepOps<'_, Self>> {
        Some(self)
    }
}

impl SingleThreadSingleStep for RiscvGdb {
    fn step(&mut self, signal: Option<Signal>) -> Result<(), Self::Error> {
        todo!()
    }
}

impl Breakpoints for RiscvGdb {
    // We implement support for Software Breakpoints, in a similar fashion to
    // the debugger.
    #[inline(always)]
    fn support_sw_breakpoint(&mut self) -> Option<SwBreakpointOps<Self>> {
        Some(self)
    }
}

impl SwBreakpoint for RiscvGdb {
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

enum RiscVEventLoop {}

// The `run_blocking::BlockingEventLoop` groups together various callbacks
// the `GdbStub::run_blocking` event loop requires you to implement.
impl run_blocking::BlockingEventLoop for RiscVEventLoop {
    type Target = RiscvGdb;
    type Connection = Box<dyn ConnectionExt<Error = std::io::Error>>;

    // or MultiThreadStopReason on multi threaded targets
    type StopReason = SingleThreadStopReason<u64>;

    // Invoked immediately after the target's `resume` method has been
    // called. The implementation should block until either the target
    // reports a stop reason, or if new data was sent over the connection.
    fn wait_for_stop_reason(
        target: &mut RiscvGdb,
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
        target: &mut RiscvGdb,
    ) -> Result<Option<SingleThreadStopReason<u64>>, <RiscvGdb as Target>::Error> {
        // notify the target that a ctrl-c interrupt has occurred.
        // target.stop_in_response_to_ctrl_c_interrupt()?;

        // a pretty typical stop reason in response to a Ctrl-C interrupt is to
        // report a "Signal::SIGINT".
        Ok(Some(SingleThreadStopReason::Signal(Signal::SIGINT).into()))
    }
}

fn gdb_event_loop_thread(
    debugger: GdbStub<RiscvGdb, Box<dyn ConnectionExt<Error = std::io::Error>>>,
    mut target: RiscvGdb,
) {
    match debugger.run_blocking::<RiscVEventLoop>(&mut target) {
        Ok(disconnect_reason) => match disconnect_reason {
            DisconnectReason::Disconnect => {
                println!("Client disconnected")
            }
            DisconnectReason::TargetExited(code) => {
                println!("Target exited with code {}", code)
            }
            DisconnectReason::TargetTerminated(sig) => {
                println!("Target terminated with signal {}", sig)
            }
            DisconnectReason::Kill => println!("GDB sent a kill command"),
        },
        Err(e) => {
            if e.is_target_error() {
                println!(
                    "target encountered a fatal error: {:?}",
                    e.into_target_error().unwrap()
                )
            } else if e.is_connection_error() {
                let (e, kind) = e.into_connection_error().unwrap();
                println!("connection error: {:?} - {}", kind, e,)
            } else {
                println!("gdbstub encountered a fatal error: {:?}", e)
            }
        }
    }
}
