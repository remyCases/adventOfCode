// Copyright (C) 2025 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

#[derive(Debug)]
enum Direction {
    Left,
    Right,
}

const DIAL_MAX: i32 = 100;

fn read_file_and_compute(file_path: &Path, part: argparse::ArgPart) -> io::Result<()>
{
    let mut dial: i32 = 50;
    let mut dial_at_zero: i32 = 0;

    parse_compute!(
        file_path, part,
        sequence::pair(
            combinator::map_res(
                character::complete::one_of::<&str, &str, NomError<_>>("LR"),
                |c: char| match c { 
                    'L' => Ok(Direction::Left), 
                    'R' => Ok(Direction::Right),
                    _ => Err("Invalid direction"),
                }
            ),
            combinator::map_res(
                character::complete::digit1,
                str::parse,
            ),
        ), 
        |(d, u): (Direction, i32)| {
            match d {
                Direction::Left => dial = (dial + DIAL_MAX - u % DIAL_MAX) % DIAL_MAX,
                Direction::Right => dial = (dial + u % DIAL_MAX) % DIAL_MAX,
            }
            if dial == 0 { dial_at_zero += 1; }
        },
        |(d, u): (Direction, i32)| {
            dial_at_zero += u / DIAL_MAX;
            let increment: i32 = u % DIAL_MAX;
            
            if increment == 0 { return; }
            match d {
                Direction::Left => {
                    if dial <= increment && dial != 0 { dial_at_zero += 1; }
                    dial = (dial + DIAL_MAX - increment) % DIAL_MAX;
                },
                Direction::Right => {
                    if dial + increment >= DIAL_MAX { dial_at_zero += 1; }
                    dial = (dial + increment) % DIAL_MAX;
                },
            };
        }
    );

    println!("DIAL AT ZERO: {}", dial_at_zero);
    Ok(())
}

main!("2025", "input_day_one");
