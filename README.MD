# Advent of code 2022 edition, an adventure to code

*Sorry for the pun.*

This project is an attempt to solve the [advent of code](https://adventofcode.com/) challenge in various programming languages I'm learning.

Currently only a [Nim](https://nim-lang.org/) and a [Cobol](https://gnucobol.sourceforge.io/) versions are available. Maybe a Rust version will emerged, depending of my spare time and my willingness to test myself.

## How to run the Nim code

First you'll need the Nim compiler. You should find tutorials about installation on the official website.

Once it's done, to compile, simplely run in the root folder: 
```
adventBuild.dat nim
```

To execute:
```
.\2022\bin\mainNim -d DAY -p PART
```

As far as I know, there is always two parts during each days, so `-p 1` will run the first part, and `-p 2` the second part. Currently only the six first days are solved for the Nim part.

## How to run the Cobol code

First you'll need the Cobol compiler. And depending on your plateform it can be a challenge to get there. I personnally installed GnuCobol via [msys2](https://packages.msys2.org/package/mingw-w64-x86_64-gnucobol?repo=mingw64). Good luck.

Once it's done, to compile, simplely run in the root folder: 
```
adventBuild.dat cobol
```

To execute:
```
.\2022\bin\mainCobol DAY PART
```
For instance `.\2022\bin\mainCobol 21` will run the first part of the second day.

Currently only the five first days are solved for the Cobol part.

## Wait, that's not the right solutions concidering the data I have...

In the `2022/data` folder, there are the data used to solve each challenge. I'm using data I was given during the challenge, and I don't know if everyone was given the same (I expect not).