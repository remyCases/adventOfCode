mod dayOne;

use clap::Parser;
use std::io::Error;

#[derive(Parser)]
struct Args {
    #[arg(short, long, required = true)]
    day: i32,
    #[arg(short, long, default_value_t = 1, required = true)]
    part: u8,
}

fn main() -> Result<(), Error> {
    let args = Args::parse();
    match args.day {
        1 => dayOne::main(args.part)?,
        _ => println!("{:}, {:}", args.day, args.part)
    };
    Ok(())
}