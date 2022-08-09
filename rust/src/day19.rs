use crate::common::Day;

pub struct Day19;

impl Day for Day19 {
    type Input = u32;
    type Output = u32;
    
    fn get_input_file() -> &'static str {
        "day19"
    }

    fn parse(input: &str) -> Self::Input {
        input.parse::<u32>().expect("Could not parse")
    }

    fn part1(input: &Self::Input) -> Self::Output {
        let mut a: Vec<u32> = (1..*input).collect();
        a.push(0);

        let mut i: usize = 0;
        while i != a[i] as usize {
            let nb = a[a[i] as usize];
            a[i] = nb;
            i = nb as usize;
        }
        i as u32 + 1
    }

    fn part2(input: &Self::Input) -> Self::Output {
        0
    }
}