use std::fmt::Debug;

pub trait Day {
    type Input;
    type Output: Debug;
    fn get_input_file() -> String;
    fn parse(input: &str) -> Self::Input;
    fn part1(input: &Self::Input) -> Self::Output;
    fn part2(input: &Self::Input) -> Self::Output;
}