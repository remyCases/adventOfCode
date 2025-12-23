// Copyright (C) 2025 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

enum Parsing {
    Ranges,
    Ids,
}

#[derive(Debug, Clone, Copy)]
struct Ranges {
    start: u64,
    end: u64,
}

fn parser_ranges(line: &str)-> IResult<&str, (u64, u64), NomError<&str>>
{
    sequence::separated_pair(
        combinator::map_res(
            character::complete::digit1::<&str, NomError<_>>,
            str::parse::<u64>,
        ),
        bytes::complete::tag("-"), 
        combinator::map_res(
            character::complete::digit1,
            str::parse::<u64>,
        )
    )(line)
}

fn parser_ids(line: &str)-> IResult<&str, u64, NomError<&str>>
{
    combinator::map_res(
        character::complete::digit1::<&str, NomError<&str>>,
        str::parse::<u64>,
    )(line)
}

fn push_range(v: &mut Vec<Ranges>, new_r: Ranges)
{
    let (unchanged, to_update): (Vec<Ranges>, Vec<Ranges>) = v.iter().partition(|r|(r.end < new_r.start + 1) || (new_r.end < r.start + 1));
    let updated_range= to_update.iter().fold(new_r,|acc, r| Ranges{start: std::cmp::min(r.start, acc.start), end: std::cmp::max(r.end, acc.end)});
    
    *v = unchanged;
    v.push(updated_range);
}

fn read_file_and_compute(file_path: &Path, part: argparse::ArgPart) -> io::Result<()>
{
    let mut fresh_ingredients: Vec<Ranges> = vec![];
    let mut existing_fresh_ids: u64 = 0;
    let mut fresh_ids: u64 = 0;
    let mut current_status: Parsing = Parsing::Ranges;

    let lines = io::line_iterator(file_path)?;
    for (nline, line) in lines.enumerate() {
        let binding = line?;

        match current_status {
            Parsing::Ranges => {
                if binding.is_empty()
                {
                    current_status = Parsing::Ids;
                    existing_fresh_ids = fresh_ingredients.iter().fold(0, |acc, x| acc + x.end - x.start + 1);
                    continue;
                }
                let (_, (s,e)) = parser_ranges(&binding).map_err(|err|
                    Error::new(
                        ErrorKind::InvalidData,
                        err.to_string() + &format!(" in line: {:}", nline)
                    )
                )?;
                push_range(&mut fresh_ingredients, Ranges{start: s, end: e});
            },
            Parsing::Ids => {
                let (_, i) = parser_ids(&binding).map_err(|err|
                    Error::new(
                        ErrorKind::InvalidData,
                        err.to_string() + &format!(" in line: {:}", nline)
                    )
                )?;
                fresh_ids += fresh_ingredients.iter().find(|r| i >= r.start && i <= r.end).map_or_else(|| 0, |_| 1);
            },
        }
        
    }

    match part {
        argparse::ArgPart::PartOne => println!("FRESH INGREDIENTS: {}", fresh_ids),
        argparse::ArgPart::PartTwo => println!("FRESH INGREDIENTS: {}", existing_fresh_ids),
    };

    Ok(())
}

main!("2025", "input_day_five");
