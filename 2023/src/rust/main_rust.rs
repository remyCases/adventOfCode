// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

mod day_one;
mod day_two;
mod day_three;
mod day_four;
mod day_five;
mod day_six_part1;
mod day_six_part2;
mod day_seven_part1;
mod day_seven_part2;
mod day_eight_part1;
mod day_eight_part2;
mod day_nine_part1;
mod day_nine_part2;
mod day_ten_part1;
mod day_ten_part2;

use clap::Parser;

use aoc_utils::*;

fn main() -> io::Result<()> {
    let args = Args::parse();

    match args.day {
        1 => day_one::main(args.part)?,
        2 => day_two::main(args.part)?,
        3 => day_three::main(args.part)?,
        4 => day_four::main(args.part)?,
        5 => day_five::main(args.part)?,
        6 => match args.part {
            ArgPart::PartOne => day_six_part1::main()?,
            ArgPart::PartTwo => day_six_part2::main()?,
        },
        7 => match args.part {
            ArgPart::PartOne => day_seven_part1::main()?,
            ArgPart::PartTwo => day_seven_part2::main()?,
        },
        8 => match args.part {
            ArgPart::PartOne => day_eight_part1::main()?,
            ArgPart::PartTwo => day_eight_part2::main()?,
        },
        9 => match args.part {
            ArgPart::PartOne => day_nine_part1::main()?,
            ArgPart::PartTwo => day_nine_part2::main()?,
        },
        10 => match args.part {
            ArgPart::PartOne => day_ten_part1::main()?,
            ArgPart::PartTwo => day_ten_part2::main()?,
        },
        _ => println!("Incorrect combination of day and part. Day {:} and part {:} does not exist (yet).", args.day, args.part)
    };
    Ok(())
}
