mod day_one;
mod day_two;

use clap::Parser;
use anyhow::Result;

#[derive(Parser)]
struct Args {
    #[arg(short, long, required = true)]
    day: i32,
    #[arg(short, long, default_value_t = 1, required = true)]
    part: u8,
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args.day {
        1 => day_one::main(args.part)?,
        2 => day_two::main(args.part)?,
        _ => println!("{:}, {:}", args.day, args.part)
    };
    Ok(())
}