// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;
use itertools::Itertools;

fn is_safe(x: i32, y: i32, z: i32) -> bool {
    x != y && (x - y) * (y - z) > 0 && i32::abs(x - y) < 4 && i32::abs(y - z) < 4
}

fn is_safe_options(ox: Option<&i32>, oy: Option<&i32>, oz: Option<&i32>) -> bool {
    if let (Some(&x), Some(&y), Some(&z)) = (ox, oy, oz) {
        is_safe(x, y ,z)
    } else {
        false
    }
}

#[derive(PartialEq)]
enum Status {
    Start,
    FirstStartUnsafe,
    SecondStartUnsafe,
    Safe,
    FirstUnsafe,
    Unsafe
}

fn explore_with_dampener(v: Vec<i32>) -> bool {
    let mut it = v.iter();
    let mut a = it.next();
    let mut b = it.next();
    let mut b_temp = b;
    let mut c = it.next();
    let mut s = Status::Start;

    while c.is_some() {
        if !is_safe_options(a, b, c) {
            match s {
                Status::Start => {s = Status::FirstStartUnsafe; b_temp = b; b = c; c = it.next(); continue; }
                Status::FirstStartUnsafe => {s = Status::SecondStartUnsafe; a = b_temp; continue; }
                Status::SecondStartUnsafe => {s = Status::Unsafe; break; }
                Status::Safe => {s = Status::FirstUnsafe; b = c; c = it.next(); continue; }
                Status::FirstUnsafe => {s = Status::Unsafe; break; }
                Status::Unsafe => {s = Status::Unsafe; break; }
            }
        } 
        a = b; b = c; c = it.next();
        s = Status::Safe;
    }
    s != Status::Unsafe
}

fn read_file_and_compute_reports(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {
    let mut reports: Vec<bool> = Vec::new();

    parse_compute!(
        file_path, part,
        multi::many0(
            sequence::terminated(
                combinator::map_res(
                    character::complete::digit0::<_, NomError<_>>,
                    str::parse
                ),
                character::complete::multispace0
            )
        ),
        |v: Vec<i32>| {
            reports.push(
                v.iter().tuple_windows::<(_, _, _)>()
                    .all(|(&x, &y, &z)| is_safe(x, y, z))
            );
        },
        |v: Vec<i32>| {
            reports.push(explore_with_dampener(v));
        }
    );

    println!("REPORTS: {:}", match part {
        argparse::ArgPart::PartOne => {
            reports.iter().map(|&x| x as usize).sum::<usize>()
        },
        argparse::ArgPart::PartTwo => {
            reports.iter().map(|&x| x as usize).sum::<usize>()
        }
    });
    Ok(())
}

pub fn main(part: argparse::ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2024").join("data").join("input_day_two");
    read_file_and_compute_reports(&filename, part)?;
    Ok(())
}

#[cfg(test)]
mod day_two_test {
    use super::*;

    #[test]
    fn dampener_at_start() {
        assert_eq!(explore_with_dampener(vec![700, 89, 90, 93]), true);
    }

    #[test]
    fn dampener_at_end() {
        assert_eq!(explore_with_dampener(vec![93, 96, 99, 800]), true);
    }

    #[test]
    fn dampener_examples() {
        assert_eq!(explore_with_dampener(vec![7, 6, 4, 2, 1]), true);
        assert_eq!(explore_with_dampener(vec![1, 2, 7, 8, 9]), false);
        assert_eq!(explore_with_dampener(vec![9, 7, 6, 2, 1]), false);
        assert_eq!(explore_with_dampener(vec![1, 3, 2, 4, 5]), true);
        assert_eq!(explore_with_dampener(vec![8, 6, 4, 4, 1]), true);
        assert_eq!(explore_with_dampener(vec![1, 3, 6, 7, 9]), true);
    }
}
