// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::fs::File;
use std::io::{BufRead, Lines, BufReader, Error, ErrorKind};
use std::env;

use nom::*;
use nom::error::Error as NomError;

#[derive(Debug)]
enum PartValue {
    Numeral(i32),
    Symbol(char),
}

#[derive(Debug)]
struct Part {
    value: PartValue,
    // position x,y of the Part in the file
    // needed to compute distance between parts
    x: usize,
    y: usize,
    size: usize,
    gear1: Option<i32>,
    gear2: Option<i32>,
}

impl Part {
    fn new(value: PartValue, x: usize, y: usize, size: usize) -> Self {
        Part {value, x, y, size, gear1: None, gear2: None}
    }
    fn close_enough_x(&self, other: &Part) -> bool {
        (other.x == 0 && self.x <= 1) ||
        (self.x <= other.x + 1 && other.x <= self.x + self.size)
    }
    fn numeral(&self) -> Result<i32, Error> {
        match self.value {
            PartValue::Numeral(n) => Ok(n),
            PartValue::Symbol(_) => Err(Error::new(ErrorKind::InvalidData, "Cannot extract a numeral in a symbol value")),
        }
    }
    fn set_gear(&mut self, v: i32) -> Result<i32, Error> {
        if self.gear1.is_none() {
            self.gear1 = Some(v);
            Ok(1)
        }
        else if self.gear2.is_none() {
            self.gear2 = Some(v);
            Ok(2)
        }
        else {
            Err(Error::new(ErrorKind::Other, "No more gear slot available"))
        }
    }
}


fn line_iterator(file_path: &Path) -> Result<Lines<BufReader<File>>, Error> {
    let file = File::open(file_path)?;
    Ok(BufReader::new(file).lines())
}

fn parse_engine<'a>(line: &'a str, nline: usize, vec_numeral: &mut Vec<Part>, vec_symbol: &mut Vec<Part>) -> IResult<&'a str, &'a str> {

    let parser_dot = bytes::complete::take_while::<_, &str, NomError<_>>(|c: char| c == '.');
    let alt_parser = branch::alt::<&str, _, NomError<_>, _>((character::complete::digit1, bytes::complete::take(1usize)));
    let combined_parser = sequence::tuple((parser_dot, alt_parser));

    let (end, v_parsed)= multi::many1(combined_parser)(line)?;

    let mut offset = 0;
    for (dots, token) in v_parsed {
        offset += dots.len();
        match character::complete::i32::<&str, NomError<_>>(token) {
            Err(_) => vec_symbol.push(Part::new(PartValue::Symbol(character::complete::anychar(token)?.1), offset, nline, 1)),
            Ok((_, n)) => vec_numeral.push(Part::new( PartValue::Numeral(n), offset, nline, token.len()))
        }
        offset += token.len();
    }

    Ok((line, end))
}

fn read_file_and_adjacent(file_path: &Path, part: u8) -> Result<(), Error> {
    let lines = line_iterator(file_path)?;
    let mut sum_numeral_parts = 0;
    let mut product_gears = 0;
    // vec to collect data from the txt
    let mut vec_numeral: Vec<Part> = Vec::new();
    let mut vec_symbol: Vec<Part> = Vec::new();

    for (nline, line) in lines.enumerate() { 
        if let Err(nom::Err::Error(err)) = parse_engine(&line?, nline, &mut vec_numeral, &mut vec_symbol) {
            return Err(Error::new(ErrorKind::InvalidData, err.to_string()));
        }
    }

    let mut index_symbol = 0;
    let mut restart_index = 0;
    let vec_len = vec_symbol.len();

    // loop over all possible parts and find them
    for n in vec_numeral {
        while vec_symbol[index_symbol].y <= n.y + 1 {
            // remove useless symbol to speed up next loop
            if n.y > 0 && vec_symbol[index_symbol].y < n.y - 1 { restart_index = index_symbol+1; }
            // if matches, the numeral is saved
            else if n.close_enough_x(&vec_symbol[index_symbol]) { 
                let v = n.numeral()?;
                sum_numeral_parts += v;
                if let PartValue::Symbol('*') = vec_symbol[index_symbol].value {
                    vec_symbol[index_symbol].set_gear(v)?;          
                }
                break; 
            }

            // try with the next element
            index_symbol += 1;
            if index_symbol >= vec_len { break; }
        }
        
        // skip the useless symbols to shorten the range of search
        index_symbol = restart_index;
        if index_symbol >= vec_len { break; }
    }
    
    for n in vec_symbol {
        if let PartValue::Symbol('*') = n.value {
            product_gears += n.gear1.unwrap_or(0) * n.gear2.unwrap_or(0);
        }
    }

    match part {
        1 => { println!("Sum of part numbers: {:}", sum_numeral_parts); Ok(()) }, 
        2 => { println!("Sum of gears: {:}", product_gears); Ok(()) }, 
        _ => Err(Error::new(ErrorKind::InvalidInput, "invalid part"))    
    }
}

pub fn main(part: u8) -> Result<(), Error> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_three");
    read_file_and_adjacent(&filename, part)?;
    Ok(())
}