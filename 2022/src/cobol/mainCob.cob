       >>SOURCE FORMAT FREE
*> Copyright (C) 2023 Rémy Cases
*> See LICENSE file for extended copyright information.
*> This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

IDENTIFICATION DIVISION.
PROGRAM-ID. MAINCOB.
AUTHOR. RémyCases

ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-OPTION.
       02 WS-DAY PIC 99.
       02 WS-PART PIC A.
01 WS-WORDING-OPTION PIC A(80).
01 TMP PIC A(80).
01 WS-PARSED-DAY PIC X VALUE 'N'.
       88 CORRECT-DAY VALUE 'Y'.
       88 INCORRECT-DAY VALUE 'N'.

PROCEDURE DIVISION.
Main.
       ACCEPT WS-OPTION FROM COMMAND-LINE
       
       IF WS-DAY EQUAL TO 1 THEN
           CALL 'DAYONE' USING WS-PART
           SET CORRECT-DAY TO TRUE
       END-IF

       IF WS-DAY EQUAL TO 2 THEN
           CALL 'DAYTWO' USING WS-PART
           SET CORRECT-DAY TO TRUE
       END-IF

       IF WS-DAY EQUAL TO 3 THEN
           CALL 'DAYTHREE' USING WS-PART
           SET CORRECT-DAY TO TRUE
       END-IF

       IF WS-DAY EQUAL TO 4 THEN
           CALL 'DAYFOUR' USING WS-PART
           SET CORRECT-DAY TO TRUE
       END-IF
       
       IF WS-DAY EQUAL TO 5 THEN
       *> I'm cheating here, since I'm not reading the input file
       *> If you have a different input file, you need to change TMP 
       *> to fit your input data
       *> Rule: first digit is the number of stacks, then each block of 
       *> char is the data to put in each stack.
           MOVE "9GTRW GCHPMSVW CLTSGM JHDMWRF PQLHSWFJ PJDNFMS ZBDFGCSJ RTB HNWLC" TO TMP
           STRING WS-PART TMP
           INTO WS-WORDING-OPTION
           CALL 'build/2022/lib/DAYFIVE' USING WS-WORDING-OPTION
           SET CORRECT-DAY TO TRUE
       END-IF

       IF INCORRECT-DAY
           DISPLAY "Incorrect combination of day and part. Day " FUNCTION TRIM(WS-DAY, LEADING) " and part " FUNCTION TRIM(WS-PART, LEADING) " does not exist (yet)." 
       END-IF 
       STOP RUN.
