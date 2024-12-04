       >>SOURCE FORMAT FREE
*> Copyright (C) 2023 Rémy Cases
*> See LICENSE file for extended copyright information.
*> This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

IDENTIFICATION DIVISION.
PROGRAM-ID. DAYFOUR.
AUTHOR. RémyCases

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT DataFile ASSIGN TO "2022/data/input_day_four" 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD DataFile.
01 F-Data PIC X(50) VALUE ZEROES.
    
WORKING-STORAGE SECTION.
01 WS-EOF PIC A.
01 WS-Data PIC X(50).
01 WS-Overlap PIC 9(8).
01 WS-OverlapDisplay PIC Z(8).
01 WS-IncrementOverlap PIC 9(8).
01 WS-FirstStartRange PIC 9(3).
01 WS-FirstEndRange PIC 9(3).
01 WS-SecondStartRange PIC 9(3).
01 WS-SecondEndRange PIC 9(3).
                    
LINKAGE SECTION.
01 L-Part PIC 9 VALUE 1.

PROCEDURE DIVISION USING L-Part.
Main.
       OPEN INPUT DataFile.
           PERFORM UNTIL WS-EOF='Y'
               READ DataFile INTO F-Data
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM BranchingCompute
               END-READ
                   
           END-PERFORM
       CLOSE DataFile

       MOVE WS-Overlap TO WS-OverlapDisplay
       DISPLAY "OVERLAP COUNT: " FUNCTION TRIM(WS-OverlapDisplay, LEADING)
EXIT PROGRAM.

BranchingCompute.
       MOVE F-Data TO WS-Data
       UNSTRING WS-Data DELIMITED BY '-' OR ','
       INTO    WS-FirstStartRange
               WS-FirstEndRange
               WS-SecondStartRange
               WS-SecondEndRange
       
       IF L-Part EQUALS TO 1
           MOVE ZEROES TO WS-IncrementOverlap
           *> case A B B A
           IF WS-FirstStartRange <= WS-SecondStartRange AND WS-SecondEndRange <= WS-FirstEndRange
               MOVE 1 TO WS-IncrementOverlap
           END-IF

           *> case B A A B
           IF WS-SecondStartRange <= WS-FirstStartRange AND WS-FirstEndRange <= WS-SecondEndRange
               MOVE 1 TO WS-IncrementOverlap
           END-IF
       END-IF

       IF L-Part EQUALS TO 2
           MOVE 1 TO WS-IncrementOverlap
           *> case A A B B
           IF WS-FirstEndRange < WS-SecondStartRange
               MOVE ZEROES TO WS-IncrementOverlap
           END-IF

           *> case B B A A
           IF WS-SecondEndRange < WS-FirstStartRange
               MOVE ZEROES TO WS-IncrementOverlap
           END-IF
       END-IF
       
       
       COMPUTE WS-Overlap = WS-Overlap + WS-IncrementOverlap.
