use std::path::Path;

mod cpu;
use cpu::*;

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

    loop {
        cpu.run_next_instruction();
    }
}

