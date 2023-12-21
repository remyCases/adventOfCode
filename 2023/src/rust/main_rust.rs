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

use clap::Parser;
use std::io::{Error, ErrorKind};

#[path = "utils_io.rs"] mod utils_io;

enum Part {
    PartOne,
    PartTwo,
}

#[derive(Parser)]
struct Args {
    #[arg(short, long, required = true)]
    day: i32,
    #[arg(short, long, default_value_t = 1, required = true)]
    part: u8,
}

fn main() -> Result<(), Error> {
    let args = Args::parse();

    let part = match args.part {
        1 => Part::PartOne,
        2 => Part::PartTwo,
        _ => { return Err(Error::new(ErrorKind::InvalidInput, "invalid part")); },
    };

    match args.day {
        1 => day_one::main(args.part)?,
        2 => day_two::main(args.part)?,
        3 => day_three::main(args.part)?,
        4 => day_four::main(args.part)?,
        5 => day_five::main(args.part)?,
        6 => match part {
            Part::PartOne => day_six_part1::main()?,
            Part::PartTwo => day_six_part2::main()?,
        },
        7 => match part {
            Part::PartOne => day_seven_part1::main()?,
            Part::PartTwo => day_seven_part2::main()?,
        },
        _ => println!("{:}, {:}", args.day, args.part)
    };
    Ok(())
}