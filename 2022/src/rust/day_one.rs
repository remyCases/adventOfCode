use std::path::Path;
use std::fs::File;
use std::io::{BufRead, Lines, BufReader, Error};
use std::env;

const BUFFER_LEN: usize = 3;

fn line_iterator(file_path: &Path) -> Result<Lines<BufReader<File>>, Error> {
    let file = File::open(file_path)?;
    Ok(BufReader::new(file).lines())
}

fn read_file_and_compute_calories(file_path: &Path, part: u8) -> Result<(), Error> {
    let lines = line_iterator(file_path)?;
    let mut max_calories = 0;
    let mut sum_calories = 0;
    let mut max_calories_buffer: Vec<i32> = vec![0; BUFFER_LEN];

    for line in lines {
        match line?.parse::<i32>() {
            Ok(calories) => sum_calories += calories,
            Err(_) => sum_calories = 0,
        };

        match part {
            1 => if max_calories < sum_calories { max_calories = sum_calories; },
            2 => if max_calories_buffer[0] < sum_calories { 
                max_calories_buffer[0] = sum_calories; max_calories_buffer.sort(); 
            },
            _ => (),
        };
        
    }
    
    if part == 2 {
        max_calories = max_calories_buffer.iter().sum();
    }
    println!("MAX CALORIES: {:}", max_calories);
    Ok(())
}

pub fn main(part: u8) -> Result<(), Error> {
    let filename = env::current_dir()?.join("2022").join("data").join("input_day_one.txt");
    read_file_and_compute_calories(&filename, part)?;
    Ok(())
}