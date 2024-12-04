// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

enum Mul {
    Numbers((i32, i32)),
    Do,
    Dont,
    None,
}

fn read_file_and_compute_multiplications(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {
    let mut res = 0;
    let mut state = Mul::Do;

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
                    Mul::Numbers
                ),
                combinator::map(
                    bytes::complete::tag("do()"),
                    |_x| Mul::Do
                ),
                combinator::map(
                    bytes::complete::tag("don't()"),
                    |_x| Mul::Dont
                ),
                combinator::map(
                    character::complete::anychar,
                    |_x| Mul::None
                )
            ))
            
        ),
        |v: Vec<Mul>| res += v.iter()
            .map(|t| match t { 
                Mul::Numbers(inner) => Some(inner),
                _ => None,
            })
            .flatten()
            .map(|(x, y)| x*y)
            .sum::<i32>(),
        |v: Vec<Mul>| res += v.iter()
            .map(|t| match t { 
                Mul::Numbers(inner) => match state { Mul::Do => Some(inner), _ => None},
                Mul::Do => {state = Mul::Do; None},
                Mul::Dont => {state = Mul::Dont; None},
                _ => None,
            })
            .flatten()
            .map(|(x, y)| x*y)
            .sum::<i32>()
    );

    println!("MULTIPLICATIONS: {:}", res);
    Ok(())
}

pub fn main(part: argparse::ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2024").join("data").join("input_day_three");
    read_file_and_compute_multiplications(&filename, part)?;
    Ok(())
}
