// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use aoc_utils::*;

fn read_file_and_compute_calories(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {

    let mut point = 0;
    const POINTS_FROM_MOVE: [usize; 9] = [
        3, 6, 0,
        0, 3, 6,
        6, 0, 3
    ];

    const POINTS_FROM_RESULT: [usize; 9] = [
        3, 1, 2,
        1, 2, 3,
        2, 3, 1
    ];

    const A_AS_INT: usize = 'A' as usize;
    const X_AS_INT: usize = 'X' as usize;

    let compute: fn(&mut usize, usize, usize) = match part {
        argparse::ArgPart::PartOne => |p, x, y| *p += POINTS_FROM_MOVE[3 * x + y] + y + 1,
        argparse::ArgPart::PartTwo => |p, x, y| *p += POINTS_FROM_RESULT[3 * x + y] + y + 1,
    };

    parse_compute!(
        file_path,
        sequence::separated_pair(
            combinator::map_res(
                character::complete::one_of::<&str, &str, (&str, _)>("ABC"),
                |c: char| Ok::<usize, ErrorKind>(c as usize - A_AS_INT)
            ),
            character::complete::multispace0,
            combinator::map_res(
                character::complete::one_of::<&str, &str, (&str, _)>("XYZ"),
                |c: char| Ok::<usize, ErrorKind>(c as usize - X_AS_INT)
            ),
        ),
        |(x, y)| compute(&mut point, x, y)
    );

    println!("SCORE: {:?}", point);
    Ok(())
}

pub fn main(part: argparse::ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2022").join("data").join("input_day_two");
    read_file_and_compute_calories(&filename, part)?;
    Ok(())
}
