       >>SOURCE FORMAT FREE
*> Copyright (C) 2023 Rémy Cases
*> See LICENSE file for extended copyright information.
*> This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

IDENTIFICATION DIVISION.
PROGRAM-ID. DAYTWO.
AUTHOR. RémyCases

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT DataFile ASSIGN TO "2022/data/input_day_two" 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD DataFile.
01 FStrat.
       02 FOpponentStrat PIC A.
       02 FSpace PIC A.
       02 FSelfStrat PIC A.
    
WORKING-STORAGE SECTION.
01 WSPointFromResult.
       02 WSPoint OCCURS 9 TIMES INDEXED BY I PIC 9.
01 WSOpponentStrat PIC A VALUE 'A'.
01 WSSelfStrat PIC A VALUE 'X'.
01 WSOpponentIndex PIC 9 VALUE 1.
01 WSSelfIndex PIC 9 VALUE 1.
01 WSIndex PIC 9 VALUE 1.
01 WSEOF PIC A.
01 WSScore PIC 9(8) VALUE ZEROS.
01 WSScoreDisplay PIC Z(8).

LINKAGE SECTION.
01 LPart PIC 9 VALUE 1.

PROCEDURE DIVISION USING LPart.
Main.
       IF LPart EQUAL TO 1 THEN
           MOVE 360036603 TO WSPointFromResult
       END-IF
       IF LPart EQUAL TO 2 THEN
           MOVE 312123231 TO WSPointFromResult
       END-IF

       OPEN INPUT DataFile.
              PERFORM UNTIL WSEOF='Y'
                  READ DataFile INTO FStrat
                      AT END MOVE 'Y' TO WSEOF
                      NOT AT END PERFORM BranchingCompute
                  END-READ
              END-PERFORM
       CLOSE DataFile

       MOVE WSScore TO WSScoreDisplay
       DISPLAY "SCORE: " FUNCTION TRIM(WSScoreDisplay, LEADING)
EXIT PROGRAM.

BranchingCompute.
       MOVE FOpponentStrat TO WSOpponentStrat
       MOVE FSelfStrat TO WSSelfStrat
       
       if WSSelfStrat EQUAL TO 'X'
           MOVE 1 TO WSSelfIndex
       END-IF

       if WSSelfStrat EQUAL TO 'Y'
           MOVE 2 TO WSSelfIndex
       END-IF

       if WSSelfStrat EQUAL TO 'Z'
           MOVE 3 TO WSSelfIndex
       END-IF

       if WSOpponentStrat EQUAL TO 'A'
           MOVE 1 TO WSOpponentIndex
       END-IF

       if WSOpponentStrat EQUAL TO 'B'
           MOVE 2 TO WSOpponentIndex
       END-IF

       if WSOpponentStrat EQUAL TO 'C'
           MOVE 3 TO WSOpponentIndex
       END-IF
       
       COMPUTE WSIndex = WSSelfIndex + 3 * (WSOpponentIndex - 1)

       IF LPart EQUAL TO 1 THEN
           COMPUTE WSScore = WSScore + WSSelfIndex + WSPoint(WSIndex)
       END-IF

       IF LPart EQUAL TO 2 THEN
           COMPUTE WSScore = WSScore + 3 * (WSSelfIndex - 1) + WSPoint(WSIndex)
       END-IF.
