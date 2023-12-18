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
struct Range {
    destination_start: i64,
    source_start: i64,
    len: i64,
}

enum Map {
    Unmapped(i64),
    Mapped(i64),
}

impl Range {
    fn from_array(v: &[i64]) -> Option<Self> {
        if v.len() >= 3 { Some(Range{ destination_start: v[0], source_start: v[1], len: v[2] }) }
        else { None } 
    }

    fn compute_source(&self, input: i64) -> Map {
        if input < self.source_start || input >= self.source_start + self.len {
            Map::Unmapped(input)
        }
        else {
            Map::Mapped(input - self.source_start + self.destination_start)
        }
    }
}

enum State {
    Seed,
    Soil,
    Fertilizer,
    Water,
    Light,
    Temperature,
    Humidity,
    Location,
}

impl State {
    fn next(&self) -> Option<Self> {
        match self {
            State::Seed => Some(State::Soil),
            State::Soil => Some(State::Fertilizer),
            State::Fertilizer => Some(State::Water),
            State::Water => Some(State::Light),
            State::Light => Some(State::Temperature),
            State::Temperature => Some(State::Humidity),
            State::Humidity => Some(State::Location),
            State::Location => None,
        }
    }
}

#[derive(Debug)]
struct Garden {
    seeds: Vec<i64>,
    soil_map: Vec<Range>,
    fertilizer_map: Vec<Range>,
    water_map: Vec<Range>,
    light_map: Vec<Range>,
    temperature_map: Vec<Range>,
    humidity_map: Vec<Range>,
    location_map: Vec<Range>,
}

fn compute_map(vr: &[Range], input: i64) -> i64 {
    for m in vr {
        if let Map::Mapped(ouput) = (*m).compute_source(input) {
            return ouput;
        }
    }
    input
}

impl Garden {
    fn new() -> Garden {
        Garden{
            seeds: Vec::new(),
            soil_map: Vec::new(),
            fertilizer_map: Vec::new(),
            water_map: Vec::new(),
            light_map: Vec::new(),
            temperature_map: Vec::new(),
            humidity_map: Vec::new(),
            location_map: Vec::new(),
        }
    }

    fn compute_garden_min(&self) -> Option<i64> {
        let ouputs = self.seeds.iter()
            .map(|o| compute_map(&self.soil_map, *o))
            .map(|o| compute_map(&self.fertilizer_map, o))
            .map(|o| compute_map(&self.water_map, o))
            .map(|o| compute_map(&self.light_map, o))
            .map(|o| compute_map(&self.temperature_map, o))
            .map(|o| compute_map(&self.humidity_map, o))
            .map(|o| compute_map(&self.location_map, o))
            .min();

        ouputs
    }
}

fn push_in_ranges<'a>(rs: &'a mut Vec<Range>, vr: &'a[i64]) -> IResult<&'static str, &'static str> {
    if let Some(r) = Range::from_array(vr) {
        rs.push(r);
        Ok(("", ""))
    } else {
        Err(nom::Err::Error(NomError::new("", error::ErrorKind::SeparatedList)))
    }
}

fn line_iterator(file_path: &Path) -> Result<Lines<BufReader<File>>, Error> {
    let file = File::open(file_path)?;
    Ok(BufReader::new(file).lines())
}

fn parse_garden<'a>(line: &'a str, garden: &'a mut Garden, parsing_state: &mut State) -> IResult<&'a str, &'a str> {

    let mut vec_numeral = multi::separated_list0(character::complete::multispace1, character::complete::i64);
    let parse_text = sequence::terminated(
        bytes::complete::take_until::<_, &str, NomError<_>>(":"), 
        branch::alt((bytes::complete::tag(": "), bytes::complete::tag(":")))
    );
    
    // if empty new line, change State and return
    if line.is_empty() {
        if let Some(next_state) = parsing_state.next() {
            *parsing_state = next_state;
        } else {
            return Err(nom::Err::Error(NomError::new("Location", error::ErrorKind::CrLf)));
        }
    }

    // try removing the text and only keep the numerals
    let (numerals, _) = multi::many0(parse_text)(line)?;
    // if no numerals, skip
    if numerals.is_empty() {
        return Ok((line, ""));
    }
    let (_, raw_vec) = vec_numeral(numerals)?;

    match *parsing_state {
        State::Seed => { garden.seeds = raw_vec; Ok((line , numerals)) },
        State::Soil => push_in_ranges(&mut garden.soil_map, &raw_vec).map(|_| (line , numerals)),
        State::Fertilizer => push_in_ranges(&mut garden.fertilizer_map, &raw_vec).map(|_| (line , numerals)),
        State::Water => push_in_ranges(&mut garden.water_map, &raw_vec).map(|_| (line , numerals)),
        State::Light => push_in_ranges(&mut garden.light_map, &raw_vec).map(|_| (line , numerals)),
        State::Temperature => push_in_ranges(&mut garden.temperature_map, &raw_vec).map(|_| (line , numerals)),
        State::Humidity => push_in_ranges(&mut garden.humidity_map, &raw_vec).map(|_| (line , numerals)),
        State::Location => push_in_ranges(&mut garden.location_map, &raw_vec).map(|_| (line , numerals)),
    }
}

fn read_file_and_compute_garden(file_path: &Path, part: u8) -> Result<(), Error> {
    let lines = line_iterator(file_path)?;
    let mut garden = Garden::new();
    let mut parsing_state = State::Seed;

    for (nline, line) in lines.enumerate() {
        if let Err(nom::Err::Error(err)) = parse_garden(&line?, &mut garden, &mut parsing_state) {
            return Err(Error::new(ErrorKind::InvalidData, err.to_string() + &format!(" in line: {:}", nline)));
        }
    }
    
    let result = garden.compute_garden_min().ok_or(Error::new(ErrorKind::InvalidInput, "Cannot determined the min in compute garden"))?;
    println!("The lowest location number is: {:}", result);
    Ok(())
}

pub fn main(part: u8) -> Result<(), Error> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_five");
    read_file_and_compute_garden(&filename, part)?;
    Ok(())
}