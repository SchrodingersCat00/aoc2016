use crate::common::Day;

pub struct Day12;

type Register = u8;
type Value = i32;
type Registry = Vec::<u8>;

enum Operand {
    Constant(i32),
    Register(Register)
}

pub enum Instruction {
    Copy(Operand, Register), 
    Inc(Register),
    Dec(Register),
    Jnz(Operand, Register),
}

fn parse_instruction(input: &str) -> Instruction {
    Instruction::Inc(0)
}

impl Day for Day12 {
    type Input = Vec<Instruction>;
    type Output = i32;

    fn get_input_file() -> String {
        "day12".to_string()
    }
    
    fn parse(input: &str) -> Self::Input {
        input.split('\n').map(parse_instruction).collect()
    }

    fn part1(input: &Self::Input) -> Self::Output {
        0
    }

    fn part2(input: &Self::Input) -> Self::Output {
        0
    }
}