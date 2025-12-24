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
    Range,
    Ids,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Range {
    start: u64,
    end: u64,
}

impl Range  {
    fn new(s: u64, e:u64) -> Self{ Range{start: s, end: e} }
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

fn push_range(v: &mut Vec<Range>, new_r: Range)
{
    let (unchanged, to_update): (Vec<Range>, Vec<Range>) = v.iter().partition(|r|(r.end < new_r.start) || (new_r.end < r.start));
    let updated_range= to_update.iter().fold(new_r,|acc, r| Range::new(std::cmp::min(r.start, acc.start), std::cmp::max(r.end, acc.end)));
    
    *v = unchanged;
    v.push(updated_range);
}

fn read_file_and_compute(file_path: &Path, part: argparse::ArgPart) -> io::Result<()>
{
    let mut fresh_ingredients: Vec<Range> = vec![];
    let mut existing_fresh_ids: u64 = 0;
    let mut fresh_ids: u64 = 0;
    let mut current_status: Parsing = Parsing::Range;

    let lines = io::line_iterator(file_path)?;
    for (nline, line) in lines.enumerate() {
        let binding = line?;

        match current_status {
            Parsing::Range => {
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
                push_range(&mut fresh_ingredients, Range::new(s, e));
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

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! make_testcase_push_range_two {
        ($value:expr, $result:expr, $testname:ident) => {
            #[test]
            fn $testname() {
                let mut ranges: Vec<Range> = vec![];
                push_range(&mut ranges, $value);
                push_range(&mut ranges, Range::new(4, 7));
                assert_eq!(ranges, $result);
            }
        }
    }

    make_testcase_push_range_two!(Range::new(0, 2), vec![Range::new(0, 2), Range::new(4, 7)], test_1);
    make_testcase_push_range_two!(Range::new(0, 5), vec![Range::new(0, 7)], test_2);
    make_testcase_push_range_two!(Range::new(0, 8), vec![Range::new(0, 8)], test_3);
    make_testcase_push_range_two!(Range::new(5, 6), vec![Range::new(4, 7)], test_4);
    make_testcase_push_range_two!(Range::new(5, 8), vec![Range::new(4, 8)], test_5);
    make_testcase_push_range_two!(Range::new(8, 9), vec![Range::new(8, 9), Range::new(4, 7)], test_6);

    macro_rules! make_testcase_push_range_three {
        ($value:expr, $result:expr, $testname:ident) => {
            #[test]
            fn $testname() {
                let mut ranges: Vec<Range> = vec![];
                push_range(&mut ranges, $value);
                push_range(&mut ranges, Range::new(4, 7));
                push_range(&mut ranges, Range::new(10, 13));
                assert_eq!(ranges, $result);
            }
        }
    }

    make_testcase_push_range_three!(Range::new(0, 2), vec![Range::new(0, 2), Range::new(4, 7), Range::new(10, 13)], test_7);
    make_testcase_push_range_three!(Range::new(0, 5), vec![Range::new(0, 7), Range::new(10, 13)], test_8);
    make_testcase_push_range_three!(Range::new(0, 8), vec![Range::new(0, 8), Range::new(10, 13)], test_9);
    make_testcase_push_range_three!(Range::new(0, 11), vec![Range::new(0, 13)], test_10);
    make_testcase_push_range_three!(Range::new(0, 14), vec![Range::new(0, 14)], test_11);
    make_testcase_push_range_three!(Range::new(5, 6), vec![Range::new(4, 7), Range::new(10, 13)], test_12);
    make_testcase_push_range_three!(Range::new(5, 8), vec![Range::new(4, 8), Range::new(10, 13)], test_13);
    make_testcase_push_range_three!(Range::new(5, 11), vec![Range::new(4, 13)], test_14);
    make_testcase_push_range_three!(Range::new(5, 14), vec![Range::new(4, 14)], test_15);
    make_testcase_push_range_three!(Range::new(8, 9), vec![Range::new(8, 9), Range::new(4, 7), Range::new(10, 13)], test_16);
    make_testcase_push_range_three!(Range::new(8, 11), vec![Range::new(4, 7), Range::new(8, 13)], test_17);
    make_testcase_push_range_three!(Range::new(8, 14), vec![Range::new(4, 7), Range::new(8, 14)], test_18);
    make_testcase_push_range_three!(Range::new(11, 12), vec![Range::new(4, 7), Range::new(10, 13)], test_19);
    make_testcase_push_range_three!(Range::new(11, 14), vec![Range::new(4, 7), Range::new(10, 14)], test_20);
    make_testcase_push_range_three!(Range::new(14, 15), vec![Range::new(14, 15), Range::new(4, 7), Range::new(10, 13)], test_21);


    #[test]
    fn test_merge_multiple_at_once() {
        let mut ranges: Vec<Range> = vec![];
        push_range(&mut ranges, Range::new(0, 3));
        push_range(&mut ranges, Range::new(10, 13));
        push_range(&mut ranges, Range::new(20, 23));
        // Now insert a range that spans and merges all three
        push_range(&mut ranges, Range::new(2, 21));
        assert_eq!(ranges, vec![Range::new(0, 23)]);
    }

    #[test]
    fn test_bridge_gap() {
        let mut ranges: Vec<Range> = vec![];
        push_range(&mut ranges, Range::new(0, 3));
        push_range(&mut ranges, Range::new(5, 8));
        // Insert range that bridges the gap at position 4
        push_range(&mut ranges, Range::new(3, 5));
        assert_eq!(ranges, vec![Range::new(0, 8)]);
    }

    #[test]
    fn test_large_numbers() {
        let mut ranges: Vec<Range> = vec![];
        push_range(&mut ranges, Range::new(1_000_000, 2_000_000));
        push_range(&mut ranges, Range::new(2_000_000, 3_000_000));
        // Adjacent, should merge
        assert_eq!(ranges, vec![Range::new(1_000_000, 3_000_000)]);
    }

    #[test]
    fn test_duplicate_range() {
        let mut ranges: Vec<Range> = vec![];
        push_range(&mut ranges, Range::new(5, 10));
        push_range(&mut ranges, Range::new(5, 10));
        assert_eq!(ranges, vec![Range::new(5, 10)]);
    }
}