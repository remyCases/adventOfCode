// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

mod day_one;
mod day_two;

use aoc_utils::*;

fn main() -> io::Result<()> {
    let args = argparse::Args::parse();

    match args.day {
        1 => day_one::main(args.part)?,
        2 => day_two::main(args.part)?,
        _ => println!("Incorrect combination of day and part. Day {:} and part {:} does not exist (yet).", args.day, args.part)
    };
    Ok(())
}
