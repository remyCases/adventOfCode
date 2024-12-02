// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::fmt;
use std::path::Path;
use std::fs::File;
use std::io::{BufRead, Lines, BufReader, Error};
use clap::Parser;

#[derive(Clone, clap::ValueEnum, Default)]
pub enum ArgPart {
    #[default]
    #[clap(name = "1")]
    PartOne,
    #[clap(name = "2")]
    PartTwo,
}

impl fmt::Display for ArgPart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArgPart::PartOne => write!(f, "1"),
            ArgPart::PartTwo => write!(f, "2"),
        }
    }
}

#[derive(Parser)]
pub struct Args {
    #[arg(short, long, required = true)]
    pub day: i32,
    #[arg(short, long, required = true)]
    pub part: ArgPart,
}

pub type EResult = Result<(), Error>;
pub type SResult = Result<String, Error>;

pub fn line_iterator(file_path: &Path) -> Result<Lines<BufReader<File>>, Error> {
    let file = File::open(file_path)?;
    Ok(BufReader::new(file).lines())
}
