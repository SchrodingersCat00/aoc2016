use crate::common::Day;

pub struct Day12;

type Register = u8;
type Value = i32;
type Registry = Vec::<i32>;
type Program = Vec::<Instruction>;

#[derive(Debug)]
pub enum Operand {
    Const(i32),
    Reg(Register)
}

impl Operand {
    fn get_value(&self, registry: &Registry) -> Value{
        match self {
            Self::Const(x) => *x,
            Self::Reg(r) => registry[*r as usize]
        }
    }
}

#[derive(Debug)]
pub enum Instruction {
    Copy(Operand, Register), 
    Inc(Register),
    Dec(Register),
    Jnz(Operand, Value),
}

#[derive(Debug)]
struct ProgramState {
    program: Program,
    pc: usize,
    registry: Registry
}

fn parse_operand(input: &str) -> Operand {
    match input.parse::<i32>() {
        Ok(x) => Operand::Const(x),
        Err(_) => {
            let reg = parse_reg(input);
            return Operand::Reg(reg)
        }
    }
}

fn parse_reg(input: &str) -> Register {
    match input {
        "a" => 0,
        "b" => 1,
        "c" => 2,
        "d" => 3,
        _ => panic!("Unexpected register: {}", input),
    }
}

fn parse_value(input: &str) -> Value {
    input.parse::<i32>().expect("Value is not a valid int")
}

fn parse_instruction(input: &str) -> Instruction {
    let words: Vec::<&str> = input.split(' ').collect();
    match words[0] {
        "cpy" => Instruction::Copy(parse_operand(words[1]), parse_reg(words[2])),
        "inc" => Instruction::Inc(parse_reg(words[1])),
        "dec" => Instruction::Dec(parse_reg(words[1])),
        "jnz" => Instruction::Jnz(parse_operand(words[1]), parse_value(words[2])),
        _ => panic!("Unexpected instruction")
    }
}

fn step(program_state: &mut ProgramState) -> bool {
    let instruction = &program_state.program[program_state.pc];
    match instruction {
        Instruction::Copy(op, reg) => {
            program_state.pc += 1;
            program_state.registry[reg] = op.get_value(program_state.registry);
        },
        Instruction::Inc(r) => {
            program_state.pc += 1;
            program_state.registry[r] += 1;
        },
        Instruction::Dec(r) => {
            program_state.pc += 1;
            program_state.registry[r] -= 1;
        },
        Instruction::Jnz(op, val) => {
            program_state.pc = program_state.pc.checked_add(val);
        }
    }
}

fn run_program(program: Program) {
    let mut state = ProgramState {
        program,
        pc: 0,
        registry: vec![0, 0, 0, 0]
    };

    while step(&mut state) {}
}

impl Day for Day12 {
    type Input = Vec<Instruction>;
    type Output = i32;

    fn get_input_file() -> &'static str {
        "day12"
    }
    
    fn parse(input: &str) -> Self::Input {
        input.split('\n').map(parse_instruction).collect()
    }

    fn part1(input: &Self::Input) -> Self::Output {
        println!("{:?}", input);
        0
    }

    fn part2(input: &Self::Input) -> Self::Output {
        0
    }
}