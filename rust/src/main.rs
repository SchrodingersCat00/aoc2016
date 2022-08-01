mod day12;
pub mod common;

use std::fs;

fn run_day<D: common::Day>() {
    let content = fs::read_to_string(
        format!(
            "../data/{}.txt",
            D::get_input_file()
        )
    ).expect("Error while reading file.");
    let parsed = D::parse(&content);
    let part1_result = D::part1(&parsed);
    let part2_result = D::part2(&parsed);
    println!("Part1: {:?}", part1_result);
    println!("Part2: {:?}", part2_result);
}

fn main() {
    run_day::<day12::Day12>();
}
