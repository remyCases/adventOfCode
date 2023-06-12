@ECHO OFF
REM Copyright (C) 2023 Rémy Cases
REM See LICENSE file for extended copyright information.
REM This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

SET /A DAY = %2
SET /A PART = %3
IF %1 == nim GOTO NimLaunch
IF %1 == cobol GOTO CobolLaunch
IF %1 == rust GOTO RustLaunch
IF %1 == C99 GOTO C99Launch
GOTO End

:NimLaunch
    .\build\2022\bin\mainNim -d %DAY% -p %PART%
GOTO End

:CobolLaunch
    .\build\2022\bin\mainCob %DAY%%PART%
GOTO End

:RustLaunch
    .\build\2022\release\main_rust --day %DAY% --part %PART%
GOTO End

:C99Launch
    .\build\2022\bin\mainC99 %DAY% %PART%
GOTO End

:End