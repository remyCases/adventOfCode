// Copyright (C) 2025 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::collections::VecDeque;
use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

#[derive(Debug, Clone, Copy)]
enum Rolls {
    Paper,
    None,
}

fn rolls(v: &[Rolls]) -> i32
{
    v.iter().fold(
        0i32,
        |acc, x| acc + match x {
            Rolls::Paper => 1,
            Rolls::None => 0,
        })
}

fn rolls_by_line(prev: &[Rolls], curr: &[Rolls], next: &[Rolls]) -> i32
{
    let mut sum_rolls = 0;
    
    for (x,(y, z)) in zip!(prev.windows(3), curr.windows(3), next.windows(3))
    {
        match y[1] {
            Rolls::None => continue,
            Rolls::Paper => {
                sum_rolls += (rolls(x) + rolls(y) + rolls(z) <= 4) as i32;
            },
        };
    }

    sum_rolls
}

fn read_file_and_compute(file_path: &Path, part: argparse::ArgPart) -> io::Result<()>
{
    let mut sum_rolls: i32 = 0;
    let mut prev: Vec<Rolls> = vec![];
    let mut curr: Vec<Rolls> = vec![];

    parse_compute!(
        file_path, part,
        multi::many0(
            combinator::map_opt(
                character::complete::one_of::<&str, &str, NomError<_>>(".@"),
                |c: char| match c {
                    '.' => Some(Rolls::None),
                    '@' => Some(Rolls::Paper),
                    _ => None,
                }
            )
        ),
        |v: Vec<Rolls>| {
            let mut next_vq = VecDeque::from(v);
            next_vq.push_back(Rolls::None);
            next_vq.push_front(Rolls::None);
            let next = next_vq.make_contiguous();

            sum_rolls += rolls_by_line(&prev, &curr, next);

            if curr.is_empty() {
                curr = vec![Rolls::None; next.len()];
            }
            prev = curr.clone();
            curr = next.to_vec();
        },
        |_v: Vec<Rolls>| {
        }
    );

    sum_rolls += rolls_by_line(&prev, &curr, &vec![Rolls::None; curr.len()]);

    println!("Rolls ACCESSED: {}", sum_rolls);
    Ok(())
}

main!("2025", "input_day_four");
