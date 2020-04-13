use std::cell::RefCell;
use std::path::Path;
use std::net::TcpListener;
use std::borrow::Cow;

mod cpu;
use cpu::*;

use gdb_remote_protocol::*;

pub struct GdbState {
    pub cpu: RefCell<Cpu>,
    pub breakpoints: RefCell<Vec<u32>>
}

impl GdbState {
    pub fn new(cpu: Cpu) -> GdbState {
        GdbState {
            cpu: RefCell::new(cpu),
            breakpoints: RefCell::new(vec![]),
        }
    }
}

impl Handler for GdbState {
    fn attached(&self, _pid: Option<u64>) -> Result<ProcessType, Error> {
        Ok(ProcessType::Attached)
    }

    fn halt_reason(&self) -> Result<StopReason, Error> {
        Ok(StopReason::Exited(23, 0))
    }

    fn set_address_randomization(&self, _enable: bool) -> Result<(), Error> {
        Ok(())
    }

    fn read_general_registers(&self) -> Result<Vec<u8>, Error> {
        let mut result = Vec::new();
        for reg in self.cpu.borrow().regs().iter() {
            result.extend_from_slice(&reg.to_le_bytes());
        }

        result.extend_from_slice(&self.cpu.borrow().sr().to_le_bytes());
        result.extend_from_slice(&self.cpu.borrow().lo().to_le_bytes());
        result.extend_from_slice(&self.cpu.borrow().hi().to_le_bytes());
        result.extend_from_slice(&self.cpu.borrow().badvaddr().to_le_bytes());
        result.extend_from_slice(&self.cpu.borrow().cause().to_le_bytes());
        result.extend_from_slice(&self.cpu.borrow().pc().to_le_bytes());

        Ok(result)
    }

    fn read_memory(&self, region: MemoryRegion) -> Result<Vec<u8>, Error> {
        let mut result = vec![];
        for i in 0..region.length {
            result.push(self.cpu.borrow().load((region.address + i) as u32));
        }

        Ok(result)
    }

    fn query_supported_vcont(&self) -> Result<Cow<'static, [VContFeature]>, Error> {
        Ok(Cow::from(
            &[
                VContFeature::Continue,
                VContFeature::ContinueWithSignal,
            ][..]
        ))
    }

    fn vcont(&self, request: Vec<(VCont, Option<ThreadId>)>) -> Result<StopReason, Error> {
        let req = request.first().unwrap();
        
        match req.0 {
            VCont::Continue => {
                let mut cpu_ref = self.cpu.borrow_mut();
                cpu_ref.run_next_instruction();

                while !self.breakpoints.borrow().contains(&cpu_ref.pc()) {
                    cpu_ref.run_next_instruction();
                }
                Ok(StopReason::Signal(5))
            },
            _ => Err(Error::Unimplemented)
        }
    }

    fn insert_software_breakpoint(&self, breakpoint: Breakpoint) -> Result<(), Error> {
        let addr = breakpoint.addr as u32;
        if !self.breakpoints.borrow_mut().contains(&addr) {
            self.breakpoints.borrow_mut().push(addr);
        }

        Ok(())
    }

    fn remove_software_breakpoint(&self, breakpoint: Breakpoint) -> Result<(), Error> {
        self.breakpoints.borrow_mut().retain(|addr| *addr != (breakpoint.addr as u32));

        Ok(())
    }
}

fn main() {
    let mut args: Vec<String> = std::env::args().collect();
    args.remove(0); // exe name

    if args.is_empty() {
        let exe_name = std::env::current_exe().unwrap();
        let exe_string = exe_name.file_name().unwrap().to_string_lossy();
        println!("usage: {} rom.bin", exe_string);
        println!("");
        println!("where rom.bin - path to binary program");
        return;
    }

    let mut path = None;

    for arg in args.iter() {
        match arg {
            arg if arg.starts_with("--") => {

            }
            file_arg => path = Some(Path::new(file_arg))
        }
    }

    let mut cpu = Cpu::new(path.expect("filepath error"));

    let listener = TcpListener::bind("0.0.0.0:2424").unwrap();
    println!("Listening on port 2424");
    
    let debugger = GdbState::new(cpu);
    if let Ok((stream, _addr)) = listener.accept() {
        println!("Got connection");
        process_packets_from(stream.try_clone().unwrap(), stream, debugger);
    }
    println!("Connection closed");
}

