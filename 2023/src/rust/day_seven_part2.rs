// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use crate::utils_io;
use std::path::Path;
use std::io::{Error, ErrorKind};
use std::env;

use nom::*;
use nom::error::{Error as NomError, ParseError};
use std::cmp::Ordering;


fn is_authorized_char(c: char) -> bool {
    matches!(c, 'A' | 'K' | 'Q' | 'J' | 'T' | '9' | '8' | '7' | '6' | '5' | '4' | '3' | '2')
}

fn convert_card_into_rank(c: char) -> Result<i32, Error>  {
    match c {
        'A' => Ok(12),
        'K' => Ok(11),
        'Q' => Ok(10),
        'T' => Ok(9),
        '9' => Ok(8),
        '8' => Ok(7),
        '7' => Ok(6),
        '6' => Ok(5),
        '5' => Ok(4),
        '4' => Ok(3),
        '3' => Ok(2),
        '2' => Ok(1),
        'J' => Ok(0),
        _ => Err(Error::new(ErrorKind::InvalidInput, "Incorrect card to be converted")),
    }
}

#[derive(Debug)]
struct Hand {
    cards: [char; 5],
    freq: [i8; 5],
    bid: i32,
    // since there is not a lot of cards, we can compute in a 32bits integer
    // an unique ID that encapsulate the rank of that hand 
    place: i32,
    n_joker: i8,
}

impl Hand {
    // poor man fourrier
    fn compute_freq(&mut self) {
        let mut sorted_cards = self.cards;
        sorted_cards.sort();
        
        let mut i = 0;
        self.n_joker = 0;

        for (n, &c) in sorted_cards.iter().enumerate() {
            
            self.freq[n] =  0;
            // joker does not count in frequencies since they can be whatever we want to increase the score
            if c == 'J' {
                self.n_joker += 1;
                continue;
            }

            if n > 0 && c == sorted_cards[n-1] { 
                self.freq[i] += 1; 
            }
            else if n == 0 {
                self.freq[0] = 1;
            } else {
                i += 1;
                self.freq[i] = 1;
            }
        } 
    }

    fn compute_place(&mut self) -> Result<(), Error> {
        let (smax, max) = self.freq.iter()
            .fold((0, 0), |acc, &x| 
                if x > acc.1 { (acc.1, x) }
                else if x > acc.0 { (x, acc.1) }
                else { acc }
            );

        // the best play is then to add the joker to your max
        let type_hand = match (smax, max + self.n_joker) {
            (0, 5) => 6,
            (1, 4) => 5,
            (2, 3) => 4,
            (1, 3) => 3,
            (2, 2) => 2,
            (1, 2) => 1,
            (1, 1) => 0,
            _ => { 
                return Err(Error::new(ErrorKind::InvalidData, "Incorrect max and second max found in compute_place")) 
            },
        };

        self.place = type_hand;
        for c in self.cards {
            self.place *= 13;
            self.place += convert_card_into_rank(c)?;
        }
        Ok(())
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.place == other.place
    }
}

impl Eq for Hand { }

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.place.partial_cmp(&other.place)
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        self.place.cmp(&other.place)
    }
}

fn parse_hand(line: &str) -> IResult<&str, Hand>{
    let (str_bid, cards) = bytes::complete::take_while1::<_, _ ,NomError<_>>(is_authorized_char)(line)?;
    let (_, bid) = sequence::preceded(character::complete::multispace1, character::complete::i32)(str_bid)?;

    Ok((line, Hand { 
        cards: cards.chars()
            .collect::<Vec<_>>()
            .as_slice()
            .try_into()
            .map_err(|_| Err::Error(NomError::from_error_kind(cards, nom::error::ErrorKind::Char)))?, 
        freq: [0; 5], 
        bid,
        place: 0,
        n_joker: 0,
    }))
}
 
fn read_file_and_compute_camel_poker(file_path: &Path) -> Result<(), Error> {
    let lines = utils_io::line_iterator(file_path)?;
    let mut vec_hands: Vec<Hand> = Vec::new();

    for (nline, line) in lines.enumerate() {
        let (_, mut hand) = parse_hand(&line?)
            .map_err(|err| Error::new(
                ErrorKind::InvalidData, 
                err.to_string() + &format!(" in line: {:}", nline)
            )
        )?;
        hand.compute_freq();
        hand.compute_place()?;
        vec_hands.push(hand);
    }

    vec_hands.sort();
    let result = vec_hands.iter().enumerate().fold(0usize, |acc, (n, h)| acc + (n + 1) * h.bid as usize);
    println!("Total winnings: {:}", result);

    Ok(())
}

pub fn main() -> Result<(), Error> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_seven");
    read_file_and_compute_camel_poker(&filename)?;
    Ok(())
}