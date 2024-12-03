// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::io::{Error, ErrorKind};
use std::env;
use aoc_utils::*;

const VALID_STRING: [&str; 18] = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

type FnParse = fn(io::Result<String>, &mut i32, &mut i32) -> io::Result<()>;

fn convert_text_int(text: &str, digit: &mut i32) -> io::Result<()> {
    if text == "1" || text == "one" { *digit = 1; Ok(()) }
    else if text == "2" || text == "two" { *digit = 2; Ok(()) }
    else if text == "3" || text == "three" { *digit = 3; Ok(()) }
    else if text == "4" || text == "four" { *digit = 4; Ok(()) }
    else if text == "5" || text == "five" { *digit = 5; Ok(()) }
    else if text == "6" || text == "six" { *digit = 6; Ok(()) }
    else if text == "7" || text == "seven" { *digit = 7; Ok(()) }
    else if text == "8" || text == "eight" { *digit = 8; Ok(()) }
    else if text == "9" || text == "nine" { *digit = 9; Ok(()) }
    else { Err(Error::new(ErrorKind::InvalidInput, "Invalid string")) }
}

fn parse_digits(line: io::Result<String>, first_digit: &mut i32, second_digit: &mut i32) -> io::Result<()> {
    let binding_line = line?;
    let mut parsed_line = binding_line.chars().filter_map(|l| l.to_digit(10));
    let mut parsed_reversed_line = binding_line.chars().rev().filter_map(|l| l.to_digit(10));

    *first_digit = parsed_line.next().ok_or(Error::new(ErrorKind::NotFound, "Digit not found"))? as i32;
    *second_digit = parsed_reversed_line.next().ok_or(Error::new(ErrorKind::NotFound, "Digit not found"))? as i32;
    
    Ok(())
}

fn parse_digits_and_name(line: io::Result<String>, first_digit: &mut i32, second_digit: &mut i32) -> io::Result<()> {
    let binding_line = line?;
    let it_line_for_min = VALID_STRING.iter().filter_map(|s| binding_line.find(s).map(|r| (r, s.len())));
    let it_line_for_max = VALID_STRING.iter().filter_map(|s| binding_line.rfind(s).map(|r| (r, s.len())));

    let min_index = it_line_for_min.min_by(|(x, _), (z, _)| x.cmp(z)).ok_or(Error::new(ErrorKind::NotFound, "Digit not found"))?;
    let max_index = it_line_for_max.max_by(|(x, _), (z, _)| x.cmp(z)).ok_or(Error::new(ErrorKind::NotFound, "Digit not found"))?;
 
    convert_text_int(&binding_line[min_index.0..min_index.0 + min_index.1], first_digit)?;
    convert_text_int(&binding_line[max_index.0..max_index.0 + max_index.1], second_digit)?;

    Ok(())
}

fn read_file_and_compute_calibration(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {
    let lines = io::line_iterator(file_path)?;
    let mut first_digit_calibration = 0;
    let mut second_digit_calibration = 0;
    let mut sum_calibration = 0;

    let tryparse: io::Result<FnParse> = match part {
        argparse::ArgPart::PartOne => Ok(parse_digits),
        argparse::ArgPart::PartTwo => Ok(parse_digits_and_name),
    };

    let parse = tryparse?;
    for line in lines { 
        parse(line, &mut first_digit_calibration, &mut second_digit_calibration)?;
        sum_calibration += first_digit_calibration * 10 + second_digit_calibration;
    }
  
    println!("CALIBRATION VALUE: {:}", sum_calibration);
    Ok(())
}

pub fn main(part: argparse::ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_one");
    read_file_and_compute_calibration(&filename, part)?;
    Ok(())
}
