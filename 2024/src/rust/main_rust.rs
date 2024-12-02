// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

mod day_one;

use clap::Parser;

#[path = "../../../utils/utils_io.rs"] mod utils_io;
use utils_io::EResult;
use utils_io::Args;

fn main() -> EResult {
    let args = Args::parse();

    match args.day {
        1 => day_one::main(args.part)?,
        _ => println!("Incorrect combination of day and part. Day {:} and part {:} does not exist (yet).", args.day, args.part)
    };
    Ok(())
}
