// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Initial support for debugging RISC-V programs using gdb.
//!
//! This is achieved by implementing 'just enough' of the gdb server protocol to be useful.

use std::collections::HashSet;
use std::error::Error;
use std::fs;
use std::io;
use std::marker::PhantomData;
use std::net::TcpListener;
use std::net::TcpStream;
use std::ops::Bound;

use gdbstub::arch::Arch;
use gdbstub::common::Signal;
use gdbstub::conn::Connection;
use gdbstub::conn::ConnectionExt;
use gdbstub::stub::GdbStub;
use gdbstub::stub::SingleThreadStopReason;
use gdbstub::stub::run_blocking;
use gdbstub::target::Target;
use gdbstub::target::TargetError;
use gdbstub::target::TargetResult;
use gdbstub::target::ext::base::BaseOps;
use gdbstub::target::ext::base::singlethread::SingleThreadBase;
use gdbstub::target::ext::base::singlethread::SingleThreadResume;
use gdbstub::target::ext::base::singlethread::SingleThreadResumeOps;
use gdbstub::target::ext::base::singlethread::SingleThreadSingleStep;
use gdbstub::target::ext::base::singlethread::SingleThreadSingleStepOps;
use gdbstub::target::ext::breakpoints::Breakpoints;
use gdbstub::target::ext::breakpoints::BreakpointsOps;
use gdbstub::target::ext::breakpoints::SwBreakpoint;
use gdbstub::target::ext::breakpoints::SwBreakpointOps;
use gdbstub::target::ext::exec_file::ExecFile;
use gdbstub_arch::riscv::reg::RiscvCoreRegs;
use octez_riscv::machine_state::block_cache::block::InterpretedBlockBuilder;
use octez_riscv::machine_state::memory::BadMemoryAccess;
use octez_riscv::machine_state::memory::M1G;
use octez_riscv::machine_state::memory::Memory;
use octez_riscv::pvm::PvmHooks;
use octez_riscv::state_backend::FnManagerIdent;
use octez_riscv::stepper::StepResult;
use octez_riscv::stepper::Stepper;
use octez_riscv::stepper::StepperStatus;
use octez_riscv::stepper::pvm::PvmStepper;
use tezos_smart_rollup::utils::inbox::InboxBuilder;
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;

use crate::cli::GdbServerOptions;

/// Run a gdb server that can be used for debugging RISC-V programs.
pub fn gdb_server(opts: GdbServerOptions) -> Result<(), Box<dyn Error>> {
    let fname = fs::canonicalize(opts.input).map_err(|_| "Invalid program path")?;
    let fname = fname
        .to_str()
        .ok_or("File name cannot be converted to string")?;

    let program = fs::read(fname)?;

    let initrd = opts.initrd.as_ref().map(fs::read).transpose()?;

    let mut inbox = InboxBuilder::new();
    if let Some(inbox_file) = opts.inbox.file {
        inbox.load_from_file(inbox_file)?;
    }
    let inbox = inbox.build();

    let rollup_address = SmartRollupAddress::from_b58check(opts.inbox.address.as_str())?;

    let mut stepper = PvmStepper::<M1G>::new(
        program.as_slice(),
        initrd.as_deref(),
        inbox,
        PvmHooks::default(),
        rollup_address.into_hash().as_ref().try_into()?,
        opts.inbox.origination_level,
        opts.preimage.preimages_dir,
        InterpretedBlockBuilder,
    )?;

    // loop
    let mut target = RiscvGdb {
        fname,
        stepper: &mut stepper,
        breakpoints: HashSet::new(),
        single_step: false,
    };

    let connection = wait_for_gdb_connection(opts.port)?;
    let connection: Box<dyn ConnectionExt<Error = std::io::Error>> = Box::new(connection);
    let debugger = GdbStub::new(connection);

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
    Ok(stream)
}

struct RiscvGdb<'a, S: Stepper> {
    fname: &'a str,
    stepper: &'a mut S,
    breakpoints: HashSet<u64>,
    single_step: bool,
}

impl<'a, S: Stepper> RiscvGdb<'a, S> {
    fn run_and_check_for_incoming_data(
        &mut self,
        conn: &mut <RiscvEventLoop<'a, S> as run_blocking::BlockingEventLoop>::Connection,
    ) -> RiscvGdbEvent {
        let mut poll_incoming_data = || {
            // borrow the connection from gdbstub to check for pending input
            conn.peek().map(|b| b.is_some()).unwrap_or(true)
        };

        if self.single_step {
            self.single_step = false;
            return RiscvGdbEvent::DoneStep;
        }

        loop {
            match self
                .stepper
                .step_max(Bound::Included(1))
                .to_stepper_status()
            {
                StepperStatus::Running { .. } => {
                    if poll_incoming_data() {
                        return RiscvGdbEvent::IncomingData;
                    }

                    if self.at_breakpoint() {
                        return RiscvGdbEvent::BreakpointHit;
                    }
                }
                _ => continue,
            }
        }
    }

    fn at_breakpoint(&self) -> bool {
        let pc = self.stepper.machine_state().hart.pc.read();
        self.breakpoints.contains(&pc)
    }
}

enum RiscvGdbEvent {
    IncomingData,
    BreakpointHit,
    DoneStep,
}

impl<S: Stepper> Target for RiscvGdb<'_, S> {
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

    #[inline(always)]
    fn support_exec_file(
        &mut self,
    ) -> Option<gdbstub::target::ext::exec_file::ExecFileOps<'_, Self>> {
        Some(self)
    }
}

impl<S: Stepper> SingleThreadBase for RiscvGdb<'_, S> {
    fn read_registers(&mut self, regs: &mut RiscvCoreRegs<u64>) -> TargetResult<(), Self> {
        let state = self.stepper.machine_state();
        regs.pc = state.hart.pc.read();

        let array = state.hart.xregisters.struct_ref::<FnManagerIdent>();
        for i in 0..31 {
            // first is x0 == zero, only x1..=x31 are set
            regs.x[i + 1] = array.read(i);
        }
        Ok(())
    }

    fn write_registers(
        &mut self,
        _regs: &<Self::Arch as Arch>::Registers,
    ) -> TargetResult<(), Self> {
        unimplemented!()
    }

    fn read_addrs(&mut self, start_addr: u64, data: &mut [u8]) -> TargetResult<usize, Self> {
        const DEFAULT: u8 = 14;

        let state = self.stepper.machine_state();

        match state.main_memory.read_all(start_addr, data) {
            Ok(()) => Ok(data.len()),
            Err(BadMemoryAccess) => Err(TargetError::Errno(DEFAULT)),
        }
    }

    fn write_addrs(&mut self, _start_addr: u64, _data: &[u8]) -> TargetResult<(), Self> {
        unimplemented!()
    }

    #[inline(always)]
    fn support_resume(&mut self) -> Option<SingleThreadResumeOps<Self>> {
        Some(self)
    }
}

impl<S: Stepper> SingleThreadResume for RiscvGdb<'_, S> {
    fn resume(&mut self, _signal: Option<Signal>) -> Result<(), Self::Error> {
        Ok(())
    }

    #[inline(always)]
    fn support_single_step(&mut self) -> Option<SingleThreadSingleStepOps<'_, Self>> {
        Some(self)
    }
}

impl<S: Stepper> SingleThreadSingleStep for RiscvGdb<'_, S> {
    fn step(&mut self, signal: Option<Signal>) -> Result<(), Self::Error> {
        self.single_step = true;

        let None = signal else { return Ok(()) };

        // handle status
        let _status = self
            .stepper
            .step_max(Bound::Included(1))
            .to_stepper_status();

        Ok(())
    }
}

impl<S: Stepper> Breakpoints for RiscvGdb<'_, S> {
    // there are several kinds of breakpoints - this target uses software breakpoints
    // We implement support for Software Breakpoints, in a similar fashion to
    // the debugger.
    #[inline(always)]
    fn support_sw_breakpoint(&mut self) -> Option<SwBreakpointOps<Self>> {
        Some(self)
    }
}

impl<S: Stepper> SwBreakpoint for RiscvGdb<'_, S> {
    fn add_sw_breakpoint(
        &mut self,
        addr: u64,
        _kind: <Self::Arch as Arch>::BreakpointKind,
    ) -> TargetResult<bool, Self> {
        self.breakpoints.insert(addr);

        Ok(true)
    }

    fn remove_sw_breakpoint(
        &mut self,
        addr: u64,
        _kind: <Self::Arch as Arch>::BreakpointKind,
    ) -> TargetResult<bool, Self> {
        Ok(self.breakpoints.remove(&addr))
    }
}

struct RiscvEventLoop<'a, S: Stepper>(PhantomData<&'a S>);

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
            RiscvGdbEvent::BreakpointHit => {
                run_blocking::Event::TargetStopped(gdbstub::stub::BaseStopReason::SwBreak(()))
            }
            RiscvGdbEvent::DoneStep => {
                run_blocking::Event::TargetStopped(gdbstub::stub::BaseStopReason::DoneStep)
            }
        };

        Ok(event)
    }

    // Invoked when the GDB client sends a Ctrl-C interrupt.
    fn on_interrupt(
        _target: &mut Self::Target,
    ) -> Result<Option<SingleThreadStopReason<u64>>, <Self::Target as Target>::Error> {
        // notify the target that a ctrl-c interrupt has occurred.
        // target.stop_in_response_to_ctrl_c_interrupt()?;

        // a pretty typical stop reason in response to a Ctrl-C interrupt is to
        // report a "Signal::SIGINT".
        Ok(Some(SingleThreadStopReason::Signal(Signal::SIGINT)))
    }
}

impl<S: Stepper> ExecFile for RiscvGdb<'_, S> {
    // Return the executable file name to the GDB client. This allows it to fetch the file itself,
    // if it wasn't specified in the client already.
    fn get_exec_file(
        &self,
        _pid: Option<gdbstub::common::Pid>,
        offset: u64,
        _length: usize,
        buf: &mut [u8],
    ) -> TargetResult<usize, Self> {
        let offset = offset as usize;
        let bytes = self.fname.as_bytes();

        if offset >= bytes.len() {
            return Ok(0);
        }

        let read = usize::min(bytes.len() - offset, buf.len());
        let slice = &bytes[offset..offset + read];

        buf[..read].copy_from_slice(slice);

        Ok(read)
    }
}
