// Copyright (C) 2025 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

fn area(x1: u64, y1: u64, x2: u64, y2: u64) -> u64 {
    let dx = if x1 > x2 {
        x1 - x2 + 1
    } else {
        x2 - x1 + 1
    };
    let dy = if y1 > y2 {
        y1 - y2 + 1
    } else {
        y2 - y1 + 1
    };
    return dx * dy;
}

fn read_file_and_compute(file_path: &Path, part: argparse::ArgPart) -> io::Result<()>
{
    let mut stored_coords: Vec<(u64, u64)> = vec![];
    let mut largest_area: u64 = 0;

    parse_compute!(
        file_path, part,
        sequence::separated_pair(
            combinator::map_res(character::complete::digit1::<&str, NomError<_>>,str::parse),
            character::complete::char(','),
            combinator::map_res(character::complete::digit1::<&str, NomError<_>>,str::parse)
        ),
        |(x, y): (u64, u64)| {
            for (xx, yy) in &stored_coords {
                let a = area(x, y, *xx, *yy);
                if a > largest_area {
                    largest_area = a;
                }
            }
            stored_coords.push((x, y));
        },
        |(x, y): (u64, u64)| {
        }
    );

    println!("LARGEST AREA: {}", largest_area);
    Ok(())
}

main!("2025", "input_day_nine");
