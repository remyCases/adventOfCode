// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use clap::Parser;
use std::fmt;

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
