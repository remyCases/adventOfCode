// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::io::{Error, ErrorKind};
use std::env;
use std::collections::HashSet;

use nom::*;
use nom::error::Error as NomError;

use crate::utils_io::io::Result<()>;
use crate::utils_io::line_iterator;
use crate::utils_io::ArgPart;

fn parse_scratchcards<'a>(line: &'a str, index: &'a mut usize) -> IResult<&'a str, (Vec<i32>, Vec<i32>)> {
    
    let mut parser_header = sequence::terminated(bytes::complete::take_until::<_, &str, NomError<_>>(":"), bytes::complete::tag(":"));
    let parser_whitespace = character::complete::multispace1;
    let parser_digits = character::complete::i32::<&str, NomError<_>>;

    // parser needed to store only the numbers
    let mut combined_parser = sequence::preceded(parser_whitespace, parser_digits);

    // remove the first part, called header here, ensentially "Card X:"
    let (no_header, header) = parser_header(line)?;
    let (_, index_i32) = sequence::preceded(bytes::complete::take_till(|c: char| character::is_digit(c as u8)), parser_digits)(header)?;
    *index = (index_i32 - 1) as usize ;

    // first pass until " |" is reached and remove it
    let (second_part, winning_numbers) = sequence::terminated(multi::many1(&mut combined_parser), bytes::complete::tag(" |"))(no_header)?;
    // second pass
    let (_, numbers) = multi::many1(&mut combined_parser)(second_part)?;
    Ok((line, (winning_numbers, numbers)))
}

fn read_file_and_compute_winning_numbers (file_path: &Path, part: ArgPart) -> io::Result<()> {
    let lines = line_iterator(file_path)?;
    let lines_count = line_iterator(file_path)?.count();
    let mut instances: Vec<i32> = vec![0; lines_count];
    let mut points: Vec<i32> = vec![0; lines_count];
    let mut index: usize = 0;

    for line in lines { 
        if let Ok((_, (winning_numbers, numbers))) = parse_scratchcards(&line?, &mut index) {
            // since we need the intersection between vec, convert into a set is a good idea
            // but since the struct doesnt change, comparing sorted vec must be faster
            let hash_winnig_numbers = winning_numbers.into_iter().collect::<HashSet<i32>>();
            let hash_numbers = numbers.into_iter().collect::<HashSet<i32>>();
            let intersect = hash_winnig_numbers.intersection(&hash_numbers).collect::<HashSet<&i32>>();
            
            instances[index] += 1;

            match intersect.len() {
                0 => { 
                    points[index] = 0;
                },
                // power of two using bit shifting, in the real world, we should test the value of n first to avoid overflow
                n => {
                    points[index] = 1 << (n-1);
                    for i in 1..n+1 {
                        instances[index + i] += instances[index];
                    }
                    
                },
            };
        }
        else {
            return Err(Error::new(ErrorKind::InvalidData, "Parsing error"));
        }
    }

    match part {
        ArgPart::PartOne => { println!("Pile of scratchcards worth: {:}", points.iter().sum::<i32>()); Ok(()) }, 
        ArgPart::PartTwo => { println!("Pile of scratchcards : {:}", instances.iter().sum::<i32>()); Ok(()) }, 
    }
}

pub fn main(part: ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_four");
    read_file_and_compute_winning_numbers(&filename, part)?;
    Ok(())
}
