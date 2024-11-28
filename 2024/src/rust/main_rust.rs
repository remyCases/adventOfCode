// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use clap::Parser;
use std::io::{Error, ErrorKind};

#[path = "../../../utils/utils_io.rs"] mod utils_io;

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
        _ => println!("Incorrect combination of day and part. Day {:} and part {:} does not exist (yet).", args.day, args.part)
    };
    Ok(())
}
