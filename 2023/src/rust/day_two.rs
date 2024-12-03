// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::io::{Error, ErrorKind};
use std::env;
use nom::{
    IResult, 
    bytes::complete::{tag, take_till}, 
    character::{complete::{i32, char, alpha1}, is_alphabetic},
    sequence::{tuple, delimited},
    error::Error as NomError,
    branch::alt,
    multi::many1
};
use aoc_utils::*;

const MAX_RED: i32 = 12;
const MAX_GREEN: i32 = 13;
const MAX_BLUE: i32 = 14;

#[derive(Debug)]
struct Game {
    id: i32,
    red: i32,
    green: i32,
    blue: i32,
}

fn parse_games(line: &str) -> IResult<&str, Game> {
    let mut max_red = 0;
    let mut max_blue = 0;
    let mut max_green = 0;

    // parse the id of the Game
    let (tag_output, _) = tag("Game ")(line)?;
    let (tag_output, id) = i32(tag_output)?;
    let (tag_output, _) = tag(":")(tag_output)?;

    // parse in a tuple
    let inside_parser = tuple((i32::<&str, NomError<_>>, take_till(|c| is_alphabetic(c as u8)), alpha1));
    // seperate block with whitespace at the beginning and with ; or , at the end
    let outside_parser = delimited(char::<&str, NomError<_>>(' '), inside_parser, alt((char(','), char(';'))));
    let (_, parsed) = many1(outside_parser)(tag_output)?;

    for (v, _ , t) in parsed {
        if t == "green" { max_green = if max_green <= v { v } else { max_green } }
        else if t == "red" { max_red = if max_red <= v { v } else { max_red } }
        else if t == "blue" { max_blue = if max_blue <= v { v } else { max_blue } }
    }

    Ok((line, Game{ id, red: max_red, blue: max_blue, green: max_green }))
}

fn compute_id(game: Game, result: &mut i32) {
    if game.red <= MAX_RED && game.blue <= MAX_BLUE && game.green <= MAX_GREEN {
        *result += game.id;
    }
}

fn compute_power(game: Game, result: &mut i32) {
    *result += game.red * game.green * game.blue;
}

fn read_file_and_compute_game_id(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {
    let lines = io::line_iterator(file_path)?;
    let mut result = 0;
    
    let trycompute: Result<fn(Game, &mut i32) -> (), Error> = match part {
        argparse::ArgPart::PartOne => Ok(compute_id),
        argparse::ArgPart::PartTwo => Ok(compute_power),
    };

    let compute = trycompute?;

    for line in lines { 
        // add an ending element to make the parsing easier
        let binding_line = line? + ";";
        if let Ok((_, game)) = parse_games(&binding_line) {
            compute(game, &mut result);
        }
        else {
            return Err(Error::new(ErrorKind::InvalidInput, "Invalid parsing"));
        }
    }

    println!("POWER OF GAMES: {:}", result);
    Ok(())
}

pub fn main(part: argparse::ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_two");
    read_file_and_compute_game_id(&filename, part)?;
    Ok(())
}
