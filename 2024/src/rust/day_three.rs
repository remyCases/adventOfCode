// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

fn read_file_and_compute_multiplications(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {
    let mut res = 0;

    parse_compute!(
        file_path, part,
        multi::many0(
            branch::alt((
                combinator::map(
                    sequence::tuple((
                        sequence::preceded(
                            bytes::complete::tag("mul("),
                            combinator::map_res(
                                character::complete::digit0::<_, NomError<_>>,
                                str::parse
                            )
                        ),
                        sequence::delimited(
                            bytes::complete::tag(","),
                            combinator::map_res(
                                character::complete::digit0::<_, NomError<_>>,
                                str::parse
                            ),
                            bytes::complete::tag(")")
                        )
                    )),
                    Some
                ),
                combinator::map(
                    character::complete::anychar,
                    |_x| None
                )
            ))
            
        ),
        |v: Vec<Option<(i32, i32)>>| res += v.iter().flatten().map(|(x, y)| x*y).sum::<i32>(),
        |v: Vec<Option<(i32, i32)>>| res += v.iter().flatten().map(|(x, y)| x*y).sum::<i32>()
    );

    println!("MULTIPLICATIONS: {:}", res);
    Ok(())
}

pub fn main(part: argparse::ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2024").join("data").join("input_day_three");
    read_file_and_compute_multiplications(&filename, part)?;
    Ok(())
}
