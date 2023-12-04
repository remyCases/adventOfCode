@ECHO OFF
REM Copyright (C) 2023 Rémy Cases
REM See LICENSE file for extended copyright information.
REM This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

IF %2 == 2022 GOTO NEXT
IF %2 == 2023 GOTO NEXT
GOTO END

:NEXT
SET /A DAY = %3
SET /A PART = %4
IF %1 == nim GOTO NimLaunch
IF %1 == cobol GOTO CobolLaunch
IF %1 == rust GOTO RustLaunch
IF %1 == C99 GOTO C99Launch
GOTO End

:NimLaunch
    .\build\%2\bin\mainNim -d %DAY% -p %PART%
GOTO End

:CobolLaunch
    .\build\%2\bin\mainCob %DAY%%PART%
GOTO End

:RustLaunch
    .\build\%2\bin\release\main_rust --day %DAY% --part %PART%
GOTO End

:C99Launch
    .\build\%2\bin\mainC99 %DAY% %PART%
GOTO End

:End