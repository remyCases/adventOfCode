@echo off
IF %1 == nim GOTO NimBuild
IF %1 == cobol GOTO CobolBuild
GOTO End

:NimBuild
    nim c ./2022/src/nim/mainNim.nim
GOTO End

:CobolBuild
    cobc -m -o ./2022/lib/dayOne.dll -conf=./utils/default.conf ./2022/src/cobol/dayOne.cob
    cobc -x -free -o ./2022/bin/mainCob -conf=./utils/default.conf ./2022/src/cobol/mainCob.cob
GOTO End

:End