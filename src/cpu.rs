use std::io;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

pub struct Program {
    data: Vec<u8>
}

impl Program {
    pub fn new(path: &Path) -> io::Result<Program> {
        let mut file = File::open(path).unwrap();

        let mut data = Vec::new();

        match file.read_to_end(&mut data) {
            Ok(_)    => Ok(Program { data }),
            Err(err) => Err(err),
        }
    }

    pub fn load<T: Addressable>(&self, offset: u32) -> T {
        let offset = offset as usize;

        let mut v = 0;

        for i in 0..T::width() as usize {
            v |= (self.data[offset + i] as u32) << (i * 8)
        }

        Addressable::from_u32(v)
    }
}

enum Exception {
    LoadAddressError = 0x4,
    StoreAddressError = 0x5,
    SysCall = 0x8,
    Break = 0x9,
    Overflow = 0xc
}

pub struct Cpu {
    pc: u32,
    next_pc: u32,
    regs: [u32; 32],
    out_regs: [u32; 32],
    load: (RegisterIndex, u32),
    hi: u32,
    lo: u32,
    sr: u32,
    current_pc: u32,
    cause: u32,
    epc: u32,
    branch: bool,
    delay_slot: bool,
    inter: Interconnect,
}

#[derive(Clone,Copy)]
struct RegisterIndex(u32);

impl Cpu {
    pub fn new(program_path: &Path) -> Cpu {
        let mut regs = [0xcafebabe; 32];
        regs[0] = 0;

        let program = Program::new(program_path).unwrap();
        let ram = Ram::new();
        
        let inter = Interconnect::new(program, ram);

        let pc = 0xbfc00000;
        Cpu {
            pc: pc,
            next_pc: pc.wrapping_add(4),
            regs: regs,
            out_regs: regs,
            load: (RegisterIndex(0), 0),
            hi: 0xcafebabe,
            lo: 0xcafebabe,
            sr: 0xcafebabe,
            current_pc: 0xcafebabe,
            cause: 0xcafebabe,
            epc: 0xcafebabe,
            branch: false,
            delay_slot: false,
            inter: inter,
        }
    }

    pub fn pc(&self) -> u32 {
        self.pc
    }

    pub fn regs(&self) -> [u32; 32] {
        self.regs
    }

    pub fn lo(&self) -> u32 {
        self.lo
    }

    pub fn hi(&self) -> u32 {
        self.hi
    }

    pub fn sr(&self) -> u32 {
        self.sr
    }

    pub fn badvaddr(&self) -> u32 {
        0
    }

    pub fn cause(&self) -> u32 {
        self.cause
    }

    pub fn run_next_instruction(&mut self) {
        let pc = self.pc;

        self.current_pc = self.pc;

        if self.current_pc % 4 != 0 {
            self.exception(Exception::LoadAddressError);
            return;
        }

        let instruction = Instruction{op: self.load(pc)};
        self.pc = self.next_pc;
        self.next_pc = self.next_pc.wrapping_add(4);

        let (reg, val) = self.load;
        self.set_reg(reg, val);

        self.load = (RegisterIndex(0), 0);

        self.delay_slot = self.branch;
        self.branch = false;

        self.decode_and_execute(instruction);
        self.regs = self.out_regs;
    }

    fn reg(&self, index: RegisterIndex) -> u32 {
        let RegisterIndex(index) = index;
        self.regs[index as usize]
    }

    fn set_reg(&mut self, index: RegisterIndex, val: u32) {
        let RegisterIndex(index) = index;
        self.out_regs[index as usize] = val;
        self.out_regs[0] = 0;
    }

    pub fn load<T: Addressable>(&self, addr: u32) -> T {
        self.inter.load(addr)
    }

    pub fn store<T: Addressable + PartialEq + std::fmt::Debug>(&mut self, addr: u32, val: T) {
        if self.sr & 0x10000 != 0 {
            println!("ignoring store while cache is isolated");
            return;
        }

        self.inter.store(addr, val);
    }

    fn decode_and_execute(&mut self, insn: Instruction) {
        match insn.function() {
            0b000000 => match insn.subfunction() {
                0b000000 => self.op_sll(insn),
                0b000010 => self.op_srl(insn),
                0b000011 => self.op_sra(insn),
                0b000100 => self.op_sllv(insn),
                0b000110 => self.op_srlv(insn),
                0b000111 => self.op_srav(insn),
                0b001100 => self.op_syscall(insn),
                0b001101 => self.op_break(insn),
                0b011001 => self.op_multu(insn),
                0b011010 => self.op_div(insn),
                0b011011 => self.op_divu(insn),
                0b100000 => self.op_add(insn),
                0b010000 => self.op_mfhi(insn),
                0b010001 => self.op_mthi(insn),
                0b010010 => self.op_mflo(insn),
                0b010011 => self.op_mtlo(insn),
                0b100001 => self.op_addu(insn),
                0b100011 => self.op_subu(insn),
                0b101010 => self.op_slt(insn),
                0b101011 => self.op_sltu(insn),
                0b100100 => self.op_and(insn),
                0b100101 => self.op_or(insn),
                0b100110 => self.op_xor(insn),
                0b100111 => self.op_nor(insn),
                0b001000 => self.op_jr(insn),
                0b001001 => self.op_jalr(insn),
                unknown => panic!("unhandled instruction {:08x}, {:x}", unknown, insn.op)
            } 
            0b000001 => self.op_bxx(insn),
            0b000010 => self.op_j(insn),
            0b000011 => self.op_jal(insn),
            0b000100 => self.op_beq(insn),
            0b000101 => self.op_bne(insn),
            0b000110 => self.op_blez(insn),
            0b000111 => self.op_bgtz(insn),
            0b001000 => self.op_addi(insn),
            0b001001 => self.op_addiu(insn),
            0b001010 => self.op_slti(insn),
            0b001011 => self.op_sltiu(insn),
            0b001111 => self.op_lui(insn),
            0b001100 => self.op_andi(insn),
            0b001101 => self.op_ori(insn),
            0b010000 => self.op_cop0(insn),
            0b100010 => self.op_lwl(insn),
            0b100110 => self.op_lwr(insn),
            0b101010 => self.op_swl(insn),
            0b101110 => self.op_swr(insn),
            0b100000 => self.op_lb(insn),
            0b100001 => self.op_lh(insn),
            0b100100 => self.op_lbu(insn),
            0b100101 => self.op_lhu(insn),
            0b101000 => self.op_sb(insn),
            0b100011 => self.op_lw(insn),
            0b101011 => self.op_sw(insn),
            0b101001 => self.op_sh(insn),
            unknown => panic!("unhandled instruction {:08x}, {:x}", unknown, insn.op)
        }
    }

    fn op_sll(&mut self, insn: Instruction) {
        let i = insn.shift();
        let t = insn.t();
        let d = insn.d();

        let v = self.reg(t) << i;

        self.set_reg(d, v);
    }

    fn op_srl(&mut self, insn: Instruction) {
        let i = insn.shift();
        let t = insn.t();
        let d = insn.d();

        let v = self.reg(t) >> i;

        self.set_reg(d, v);
    }

    fn op_sra(&mut self, insn: Instruction) {
        let i = insn.shift();
        let t = insn.t();
        let d = insn.d();

        let v = (self.reg(t) as i32) >> i;

        self.set_reg(d, v as u32);
    }

    fn op_sllv(&mut self, insn: Instruction) {
        let d = insn.d();
        let s = insn.s();
        let t = insn.t();

        let v = self.reg(t) << (self.reg(s) & 0x1f);

        self.set_reg(d, v);
    }

    fn op_srlv(&mut self, insn: Instruction) {
        let d = insn.d();
        let s = insn.s();
        let t = insn.t();

        let v = self.reg(t) >> (self.reg(s) & 0x1f);

        self.set_reg(d, v);
    }

    fn op_srav(&mut self, insn: Instruction) {
        let d = insn.d();
        let s = insn.s();
        let t = insn.t();

        let v = (self.reg(t) as i32) >> (self.reg(s) & 0x1f);

        self.set_reg(d, v as u32);
    }


    fn exception(&mut self, cause: Exception) {
        let handler = match self.sr & (1 << 22) != 0 {
            true  => 0xbfc00180,
            false => 0x80000080
        };

        let mode = self.sr & 0x3f;
        self.sr &= !0x3f;
        self.sr |= (mode << 2) & 0x3f;

        self.cause = (cause as u32) << 2;
        self.epc = self.current_pc;

        if self.delay_slot {
            self.epc = self.epc.wrapping_sub(4);
            self.cause |= 1 << 31;
        }

        self.pc = handler;
        self.next_pc = self.pc.wrapping_add(4);
    }

    fn op_syscall(&mut self, _: Instruction) {
        self.exception(Exception::SysCall);
    }

    fn op_break(&mut self, _: Instruction) {
        self.exception(Exception::Break);
    }

    fn op_mfhi(&mut self, insn: Instruction) {
        let d = insn.d();

        let hi = self.hi;

        self.set_reg(d, hi);
    }

    fn op_mthi(&mut self, insn: Instruction) {
        let s = insn.s();

        self.hi = self.reg(s);
    }

    fn op_mflo(&mut self, insn: Instruction) {
        let d = insn.d();

        let lo = self.lo;

        self.set_reg(d, lo);
    }

    fn op_mtlo(&mut self, insn: Instruction) {
        let s = insn.s();

        self.lo = self.reg(s);
    }

    fn op_multu(&mut self, insn: Instruction) {
        let s = insn.s();
        let t = insn.t();

        let a = self.reg(s) as u64;
        let b = self.reg(t) as u64;

        let v = a * b;

        self.hi = (v >> 32) as u32;
        self.lo = v as u32;
    }

    fn op_div(&mut self, insn: Instruction) {
        let s = insn.s();
        let t = insn.t();

        let n = self.reg(s) as i32;
        let d = self.reg(t) as i32;

        if d == 0 {
            self.hi = n as u32;

            if n >= 0 {
                self.lo = 0xffffffff;
            }
            else {
                self.lo = 1;
            }
        }
        else if n as u32 == 0x80000000 && d == -1 {
            self.hi = 0;
            self.lo = 0x80000000;
        }
        else {
            self.hi = (n % d) as u32;
            self.lo = (n / d) as u32;
        }
    }

    fn op_divu(&mut self, insn: Instruction) {
        let s = insn.s();
        let t = insn.t();

        let n = self.reg(s);
        let d = self.reg(t);

        if d == 0 {
            self.hi = n;
            self.lo = 0xffffffff;
        }
        else {
            self.hi = n % d;
            self.lo = n / d;
        }
    }

    fn op_slt(&mut self, insn: Instruction) {
        let d = insn.d();
        let s = insn.s();
        let t = insn.t();

        let v = (self.reg(s) as i32) < (self.reg(t) as i32);

        self.set_reg(d, v as u32);
    }

    fn op_sltu(&mut self, insn: Instruction) {
        let d = insn.d();
        let s = insn.s();
        let t = insn.t();

        let v = self.reg(s) < self.reg(t);

        self.set_reg(d, v as u32);
    }

    fn op_or(&mut self, insn: Instruction) {
        let d = insn.d();
        let s = insn.s();
        let t = insn.t();

        let v = self.reg(s) | self.reg(t);

        self.set_reg(d, v);
    }

    fn op_xor(&mut self, insn: Instruction) {
        let d = insn.d();
        let s = insn.s();
        let t = insn.t();

        let v = self.reg(s) ^ self.reg(t);

        self.set_reg(d, v);
    }

    fn op_nor(&mut self, insn: Instruction) {
        let d = insn.d();
        let s = insn.s();
        let t = insn.t();

        let v = !(self.reg(s) | self.reg(t));

        self.set_reg(d, v);
    }

    fn op_and(&mut self, insn: Instruction) {
        let d = insn.d();
        let s = insn.s();
        let t = insn.t();

        let v = self.reg(s) & self.reg(t);

        self.set_reg(d, v);
    }

    fn op_jr(&mut self, insn: Instruction) {
        let s = insn.s();

        self.branch = true;

        self.next_pc = self.reg(s);
    }

    fn op_jalr(&mut self, insn: Instruction) {
        let d = insn.d();
        let s = insn.s();

        let ra = self.next_pc;

        self.set_reg(d, ra);

        self.branch = true;

        self.next_pc = self.reg(s);
    }

    fn op_bxx(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let s = insn.s();

        let instruction = insn.op;

        let is_bgez = (instruction >> 16) & 1;
        let is_link = (instruction >> 20) & 1 != 0;

        let v = self.reg(s) as i32;

        let test = (v < 0) as u32;

        let test = test ^ is_bgez;

        if test != 0 {
            if is_link {
                let ra = self.next_pc;

                self.set_reg(RegisterIndex(31), ra);
            }

            self.branch(i);
        }
    }

    fn op_sh(&mut self, insn: Instruction) {
        if self.sr & 0x10000 != 0 {
            println!("ignoring store while cache is isolated");
            return;
        }

        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);
        let v = self.reg(t);

        if addr % 2 == 0 {
            self.store(addr, v as u16);
        }
        else {
            self.exception(Exception::StoreAddressError);
        }
    }

    fn op_j(&mut self, insn: Instruction) {
        let i = insn.imm_jump();

        self.branch = true;

        self.next_pc = (self.pc & 0xf0000000) | (i << 2);
    }

    fn op_jal(&mut self, insn: Instruction) {
        let ra = self.next_pc;

        self.set_reg(RegisterIndex(31), ra);

        self.op_j(insn);
    }

    fn op_add(&mut self, insn: Instruction) {
        let s = insn.s();
        let t = insn.t();
        let d = insn.d();

        let s = self.reg(s) as i32;
        let t = self.reg(t) as i32;

        match s.checked_add(t) {
            Some(v) => self.set_reg(d, v as u32),
            None => self.exception(Exception::Overflow)
        }
    }

    fn op_addu(&mut self, insn: Instruction) {
        let s = insn.s();
        let t = insn.t();
        let d = insn.d();

        let v = self.reg(s).wrapping_add(self.reg(t));

        self.set_reg(d, v);
    }

    fn op_subu(&mut self, insn: Instruction) {
        let s = insn.s();
        let t = insn.t();
        let d = insn.d();

        let v = self.reg(s).wrapping_sub(self.reg(t));

        self.set_reg(d, v);
    }

    fn branch(&mut self, offset: u32) {
        let offset = offset << 2;
        let mut pc = self.next_pc;

        pc = pc.wrapping_add(offset);
        pc = pc.wrapping_sub(4);

        self.branch = true;

        self.next_pc = pc;
    }

    fn op_beq(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let s = insn.s();
        let t = insn.t();

        if self.reg(s) == self.reg(t) {
            self.branch(i);
        }
    }

    fn op_bne(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let s = insn.s();
        let t = insn.t();

        if self.reg(s) != self.reg(t) {
            self.branch(i);
        }
    }

    fn op_blez(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let s = insn.s();

        let v = self.reg(s) as i32;

        if v <= 0 {
            self.branch(i);
        }
    }

    fn op_bgtz(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let s = insn.s();

        let v = self.reg(s) as i32;

        if v > 0 {
            self.branch(i);
        }
    }

    fn op_addi(&mut self, insn: Instruction) {
        let i = insn.imm_se() as i32;
        let t = insn.t();
        let s = insn.s();

        let s = self.reg(s) as i32;

        match s.checked_add(i) {
            Some(v) => self.set_reg(t, v as u32),
            None    => self.exception(Exception::Overflow)
        }
    }

    fn op_addiu(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let v = self.reg(s).wrapping_add(i);

        self.set_reg(t, v);
    }

    fn op_slti(&mut self, insn: Instruction) {
        let i = insn.imm_se() as i32;
        let s = insn.s();
        let t = insn.t();

        let v = (self.reg(s) as i32) < i;

        self.set_reg(t, v as u32);
    }

    fn op_sltiu(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let s = insn.s();
        let t = insn.t();

        let v = self.reg(s) < i;

        self.set_reg(t, v as u32);
    }

    fn op_lui(&mut self, insn: Instruction) {
        let i = insn.imm();
        let t = insn.t();

        let v = i << 16;

        self.set_reg(t, v);
    }

    fn op_andi(&mut self, insn: Instruction) {
        let i = insn.imm();
        let t = insn.t();
        let s = insn.s();

        let v = self.reg(s) & i;

        self.set_reg(t, v);
    }

    fn op_ori(&mut self, insn: Instruction) {
        let i = insn.imm();
        let t = insn.t();
        let s = insn.s();

        let v = self.reg(s) | i;

        self.set_reg(t, v);
    }

    fn op_cop0(&mut self, insn: Instruction) {
        match insn.cop_opcode() {
            0b00000 => self.op_mfc0(insn),
            0b00100 => self.op_mtc0(insn),
            0b10000 => self.op_rfe(insn),
            _ => panic!("unhandled cop0 instruction {:08x}", insn.op)
        }
    }

    fn op_lwl(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);

        let cur_v = self.out_regs[t.0 as usize];

        let aligned_addr = addr & !3;
        let aligned_word: u32 = self.load(aligned_addr);

        let v = match addr & 3 {
            0 => (cur_v & 0x00ffffff) | (aligned_word << 24),
            1 => (cur_v & 0x0000ffff) | (aligned_word << 16),
            2 => (cur_v & 0x000000ff) | (aligned_word << 8),
            3 => (cur_v & 0x00000000) | (aligned_word << 0),
            _ => unreachable!()
        };

        self.load = (t, v);
    }

    fn op_lwr(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);

        let cur_v = self.out_regs[t.0 as usize];

        let aligned_addr = addr & !3;
        let aligned_word: u32 = self.load(aligned_addr);

        let v = match addr & 3 {
            3 => (cur_v & 0xffffff00) | (aligned_word >> 24),
            2 => (cur_v & 0xffff0000) | (aligned_word >> 16),
            1 => (cur_v & 0xff000000) | (aligned_word >> 8),
            0 => (cur_v & 0x00000000) | (aligned_word >> 0),
            _ => unreachable!()
        };

        self.load = (t, v);
    }

    fn op_swl(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);
        let v = self.reg(t);

        let aligned_addr = addr & !3;
        let cur_mem = self.load::<u32>(aligned_addr);

        let mem = match addr & 3 {
            0 => (cur_mem & 0xffffff00) | (v >> 24),
            1 => (cur_mem & 0xffff0000) | (v >> 16),
            2 => (cur_mem & 0xff000000) | (v >> 8),
            3 => (cur_mem & 0x00000000) | (v >> 0),
            _ => unreachable!(),
        };

        self.store::<u32>(aligned_addr, mem);
    }

    fn op_swr(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);
        let v = self.reg(t);

        let aligned_addr = addr & !3;
        let cur_mem = self.load::<u32>(aligned_addr);

        let mem = match addr & 3 {
            0 => (cur_mem & 0x00000000) | (v << 0),
            1 => (cur_mem & 0x000000ff) | (v << 8),
            2 => (cur_mem & 0x0000ffff) | (v << 16),
            3 => (cur_mem & 0x00ffffff) | (v << 24),
            _ => unreachable!(),
        };

        self.store::<u32>(aligned_addr, mem);
    }

    fn op_lb(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);

        let v = self.load::<u8>(addr) as i8;

        self.load = (t, v as u32);
    }

    fn op_lh(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);

        let v = self.load::<u16>(addr) as i16;

        self.load = (t, v as u32);
    }

    fn op_lbu(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);
        
        let v = self.load::<u8>(addr);

        self.load = (t, v as u32);
    }

    fn op_lhu(&mut self, insn: Instruction) {
        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);

        if addr % 2 == 0 {
            let v: u16 = self.load(addr);

            self.load = (t, v as u32);
        }
        else {
            self.exception(Exception::LoadAddressError);
        }
    }

    fn op_lw(&mut self, insn: Instruction) {
        if self.sr & 0x1000 != 0 {
            println!("Ignoring load while cache is isolated");
            return;
        }

        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);

        if addr % 4 == 0 {
            let v = self.load(addr);

            self.load = (t, v);
        }
        else {
            self.exception(Exception::LoadAddressError);
        }        
    }

    fn op_sb(&mut self, insn: Instruction) {
        if self.sr & 0x10000 != 0 {
            println!("ignoring store while cache is isolated");
            return;
        }

        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);
        let v = self.reg(t);

        self.store::<u8>(addr, v as u8);
    }

    fn op_sw(&mut self, insn: Instruction) {
        if self.sr & 0x10000 != 0 {
            println!("ignoring store while cache is isolated");
            return;
        }

        let i = insn.imm_se();
        let t = insn.t();
        let s = insn.s();

        let addr = self.reg(s).wrapping_add(i);

        if addr % 4 == 0 {
            let v = self.reg(t);

            self.store(addr, v);
        }
        else {
            self.exception(Exception::StoreAddressError);
        }
        
    }

    fn op_mfc0(&mut self, insn: Instruction) {
        let cpu_r = insn.t();
        let cop_r = insn.d().0;

        let v = match cop_r {
            12 => self.sr,
            13 => self.cause,
            14 => self.epc,
            _ => panic!("unhandled read from cop0 register {}", cop_r)
        };

        self.load = (cpu_r, v)
    }

    fn op_mtc0(&mut self, insn: Instruction) {
        let cpu_r = insn.t();
        let cop_r = insn.d().0;

        let v = self.reg(cpu_r);

        match cop_r {
            3 | 5 | 6 | 7 | 9 | 11 =>
                if v != 0 {
                    panic!("unhandled write to cop0 register {}", cop_r);
                },
            12 => self.sr = v,
            13 => if v != 0 {
                panic!("unhandled write to CAUSE register");
            },
            n => panic!("unhandled cop0 register: {:08x}", n)
        }
    }

    fn op_rfe(&mut self, insn: Instruction) {
        if insn.op & 0x3f != 0b010000 {
            panic!("unhandled cop0 instruction: {:x}", insn.op);
        }

        let mode = self.sr & 0x3f;
        self.sr &= !0x3f;
        self.sr |= mode >> 2;
    }
}

pub struct Interconnect {
    program: Program,
    ram: Ram,
}

const REGION_MASK: [u32; 8] = [
    // KUSEG: 2048MB
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    // KSEG0 :
    // 512MB
    0x7fffffff,
    // KSEG1 :
    // 512MB
    0x1fffffff,
    // KSEG2 : 1024MB
    0xffffffff, 0xffffffff,
];

pub fn mask_region(addr: u32) -> u32 {
    let index = (addr >> 29) as usize;

    addr & REGION_MASK[index]
}

#[derive(Debug,PartialEq)]
pub enum AccessWidth {
    Byte = 1,
    Halfword = 2,
    Word = 4
}

pub trait Addressable {
    fn width() -> AccessWidth;
    fn from_u32(i: u32) -> Self;
    fn as_u32(&self) -> u32;
}

impl Addressable for u8 {
    fn width() -> AccessWidth {
        AccessWidth::Byte
    }

    fn from_u32(i: u32) -> u8 {
        i as u8
    }

    fn as_u32(&self) -> u32 {
        *self as u32
    }
}

impl Addressable for u16 {
    fn width() -> AccessWidth {
        AccessWidth::Halfword
    }

    fn from_u32(i: u32) -> u16 {
        i as u16
    }

    fn as_u32(&self) -> u32 {
        *self as u32
    }
}

impl Addressable for u32 {
    fn width() -> AccessWidth {
        AccessWidth::Word
    }

    fn from_u32(i: u32) -> u32 {
        i
    }

    fn as_u32(&self) -> u32 {
        *self
    }
}


impl Interconnect {
    pub fn new(program: Program, ram: Ram) -> Interconnect {
        Interconnect {
            program,
            ram,
        }
    }

    pub fn load<T: Addressable>(&self, addr: u32) -> T {
        let abs_addr = mask_region(addr);

        if let Some(offset) = map::RAM.contains(abs_addr) {
            return self.ram.load(offset);
        }
        else if let Some(offset) = map::PROGRAM.contains(abs_addr) {
            return self.program.load(offset);
        }
        else {
            println!("unhandled load at {:08x}", abs_addr);
            Addressable::from_u32(0)
        }
    }

    pub fn store<T: Addressable + PartialEq + std::fmt::Debug>(&mut self, addr: u32, val: T) {
        let abs_addr = mask_region(addr);

        if let Some(offset) = map::RAM.contains(abs_addr) {
            self.ram.store(offset, val);
        }
        else {
            println!("unhandled store at address {:08x}, {:?}", addr, val);
        }
    }
}

pub struct Ram {
    data: Vec<u8>
}

impl Ram {
    pub fn new() -> Ram {
        let data = vec![0xca; 2 * 1024 * 1024];

        Ram { data: data }
    }

    pub fn load<T: Addressable>(&self, offset: u32) -> T {
        let offset = offset as usize;

        let mut v = 0;

        for i in 0..T::width() as usize {
            v |= (self.data[offset + i] as u32) << (i * 8)
        }

        Addressable::from_u32(v)
    }

    pub fn store<T: Addressable>(&mut self, offset: u32, val: T) {
        let offset = offset as usize;

        let val = val.as_u32();

        for i in 0..T::width() as usize {
            self.data[offset + i] = (val >> (i * 8)) as u8;
        }
    }
}

mod map {
    pub struct Range(u32, u32);

    impl Range {
        pub fn contains(self, addr: u32) -> Option<u32> {
            let Range(start, length) = self;

            if addr >= start && addr < start + length {
                Some(addr - start)
            }
            else {
                None
            }
        }
    }

    pub const PROGRAM: Range = Range(0x1fc00000, 512 * 1024);
    pub const RAM: Range = Range(0x00000000, 2 * 1024 * 1024);
}

#[derive(Clone,Copy)]
struct Instruction { op: u32 }

impl Instruction {
    fn function(&self) -> u32 {
        self.op >> 26
    }

    fn t(&self) -> RegisterIndex {
        RegisterIndex((self.op >> 16) & 0x1f)
    }

    fn s(&self) -> RegisterIndex {
        RegisterIndex((self.op >> 21) & 0x1f)
    }

    fn cop_opcode(&self) -> u32 {
        self.s().0
    }

    fn imm(&self) -> u32 {
        self.op & 0xffff
    }
    
    fn imm_se(&self) -> u32 {
        let v = (self.op & 0xffff) as i16;

        v as u32
    }

    fn d(&self) -> RegisterIndex {
        RegisterIndex((self.op >> 11) & 0x1f)
    }

    fn subfunction(&self) -> u32 {
        self.op & 0x3f
    }

    fn shift(&self) -> u32 {
        (self.op >> 6) & 0x1f
    }

    fn imm_jump(&self) -> u32 {
        self.op & 0x3ffffff
    }
}
