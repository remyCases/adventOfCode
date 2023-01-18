# Advent of code 2022 edition, an adventure to code

*Sorry for the pun.*

This project is an attempt to solve the [advent of code](https://adventofcode.com/) challenge in various programming languages I'm learning.

Currently only a [Nim](https://nim-lang.org/) version is available. Maybe a Rust or Cobol version will emerged, depending of my spare time and my willingness to test myself.

## How to run the code

First you'll need the compiler. Since there is only a Nim version for now, you'll just need the Nim compiler. You should find tutorials about installation on the official website.

Once it's done, simplely run in the root folder: 
```
nim c ./2022/src/<theDayOfYourChoice>.nim --part:[1,2]
```

As far as I know, there is always to part about each days, so `--part:1` will run the first part, and `--part:2` the second part.

Keep in mind, I didn't spend a lot of time to develop an argument parser (nor I used an already functionnal parser), so it's quite experimental at this point, and you should expect some unexpected behaviour if you do not stick to the command.

## Wait, that's not the right solutions concidering the data I have...

In the `2022/data` folder there are the data used to solve each challenge. I'm using data I was given during the challenge, and I don't know if everyone was given the same (I expect not).