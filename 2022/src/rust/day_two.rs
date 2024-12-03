// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use aoc_utils::*;

fn read_file_and_compute_calories(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {
    let lines = io::line_iterator(file_path)?;
    let mut point = 0;
    const POINTS_FROM_MOVE: [usize; 9] = [
        3, 6, 0,
        0, 3, 6,
        6, 0, 3
    ];

    const POINTS_FROM_RESULT: [usize; 9] = [
        3, 1, 2,
        1, 2, 3,
        2, 3, 1
    ];

    const A_AS_INT: usize = 'A' as usize;
    const X_AS_INT: usize = 'X' as usize;

    for line in lines {
        let ok_line = line?;
        let mut split_line = ok_line.split_whitespace().take(2);
        if let (Some(opponent_index_str), Some(your_index_str)) = (split_line.next(), split_line.next()) {
            let your_index = your_index_str.parse::<char>()? as usize - X_AS_INT;
            let index = 3 * (opponent_index_str.parse::<char>()? as usize - A_AS_INT) + your_index;

            match part {
                1 => point += POINTS_FROM_MOVE[index] + your_index + 1,
                2 => point += POINTS_FROM_RESULT[index] + your_index * 3,
            };
        };
    }

    println!("SCORE: {:?}", point);
    Ok(())
}

pub fn main(part: argparse::ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2022").join("data").join("input_day_two");
    read_file_and_compute_calories(&filename, part)?;
    Ok(())
}
