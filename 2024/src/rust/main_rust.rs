// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

mod day_one;
mod day_two;
mod day_three;
mod day_four;
mod day_five;

use aoc_utils::*;

fn main() -> io::Result<()> {
    let args = argparse::parse();

    match args.day {
        1 => day_one::main(args.part)?,
        2 => day_two::main(args.part)?,
        3 => day_three::main(args.part)?,
        4 => day_four::main(args.part)?,
        5 => day_five::main(args.part)?,
        _ => println!("Incorrect combination of day and part. Day {:} and part {:} does not exist (yet).", args.day, args.part)
    };
    Ok(())
}
