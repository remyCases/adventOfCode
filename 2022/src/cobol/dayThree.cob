       >>SOURCE FORMAT FREE
*> Copyright (C) 2023 Rémy Cases
*> See LICENSE file for extended copyright information.
*> This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

IDENTIFICATION DIVISION.
PROGRAM-ID. DAYTHREE.
AUTHOR. RémyCases

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT DataFile ASSIGN TO "2022/data/input_day_three" 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD DataFile.
01 F-Data PIC X(50) VALUE ZEROES.
    
WORKING-STORAGE SECTION.
01 WS-EOF PIC A.
01 WS-Data.
       05 WS-CharData OCCURS 50 TIMES INDEXED BY I PIC 9.
01 WS-CharOrd PIC 9(3).
01 WS-CharInd PIC 9(3).
01 WS-Len PIC 9(4) VALUE ZEROES.
01 WS-Operation PIC 9 VALUE ZEROES.
01 WS-DataLen PIC 9(4) VALUE ZEROES.
01 WS-CNT PIC 9(4) VALUE ZEROES.
01 WS-LineCnt PIC 9 VALUE ZEROES.
01 WS-Priority PIC 9(8) VALUE ZEROS.
01 WS-PriorityDisplay PIC Z(9).
01 WS-Appearance.
       05 WS-CharAppearance OCCURS 52 TIMES INDEXED BY I PIC 9.
01 WS-IntersectAppearance.
       05 WS-CharIntersectAppearance OCCURS 52 TIMES INDEXED BY I PIC 9.

LINKAGE SECTION.
01 L-Part PIC 9 VALUE 1.

PROCEDURE DIVISION USING L-Part.
Main.
       MOVE ZEROES TO WS-LineCnt
       OPEN INPUT DataFile.
           PERFORM UNTIL WS-EOF='Y'
               READ DataFile INTO F-Data
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM BranchingCompute
               END-READ
                   
           END-PERFORM
       CLOSE DataFile

       MOVE WS-Priority TO WS-PriorityDisplay
       DISPLAY "PRIORITIES: " FUNCTION TRIM(WS-PriorityDisplay, LEADING)
EXIT PROGRAM.

BranchingCompute.
       COMPUTE WS-LineCnt = WS-LineCnt + 1
       IF WS-LineCnt EQUALS TO 3
           MOVE ZEROES TO WS-LineCnt
       END-IF

       MOVE F-Data TO WS-Data
       MOVE ZEROES TO WS-DataLen
       INSPECT WS-Data TALLYING WS-DataLen FOR CHARACTERS BEFORE ' '
       
       MOVE ZEROES TO WS-CNT

       IF L-Part EQUALS TO 1
           MOVE ZEROES TO WS-Operation
           MOVE ZEROES TO WS-Appearance
           COMPUTE WS-Len = WS-DataLen / 2
           
           PERFORM ComputeAppearance
           PERFORM ComputePriority
       END-IF

       IF L-Part EQUALS TO 2
           MOVE WS-DataLen TO WS-Len
           IF WS-LineCnt EQUALS TO 1
               MOVE ZEROES TO WS-Operation
               PERFORM ComputeAppearance
           END-IF

           IF WS-LineCnt EQUALS TO 2
               MOVE 1 TO WS-Operation
               PERFORM ComputeAppearance
           END-IF

           IF WS-LineCnt EQUALS TO 0
               PERFORM ComputePriority
               MOVE ZEROES TO WS-Appearance
           END-IF
       END-IF.

ComputeAppearance.
       PERFORM UNTIL WS-CNT EQUAL TO WS-Len
           COMPUTE WS-CNT = WS-CNT + 1
           COMPUTE WS-CharOrd = FUNCTION ORD(WS-CharData(WS-CNT))
           *> lower cases stored from 1 to 26
           IF WS-CharOrd > 97
               COMPUTE WS-CharInd = WS-CharOrd - 97
           END-IF
           *> upper cases stored from 27 to 52
           IF WS-CharOrd < 97
               COMPUTE WS-CharInd = WS-CharOrd - 39
           END-IF

           IF WS-CharAppearance(WS-CharInd) EQUALS TO WS-Operation
               COMPUTE WS-CharIntersectAppearance(WS-CharInd) = 1
           END-IF

       END-PERFORM
       
       MOVE WS-IntersectAppearance TO WS-Appearance
       MOVE ZEROES TO WS-IntersectAppearance.

ComputePriority.
       PERFORM UNTIL WS-CNT EQUAL TO WS-DataLen
           COMPUTE WS-CNT = WS-CNT + 1
           COMPUTE WS-CharOrd = FUNCTION ORD(WS-CharData(WS-CNT))
           *> lower cases stored from 1 to 26
           IF WS-CharOrd > 97
               COMPUTE WS-CharInd = WS-CharOrd - 97
           END-IF
           *> upper cases stored from 27 to 52
           IF WS-CharOrd < 97
               COMPUTE WS-CharInd = WS-CharOrd - 39
           END-IF

           IF WS-CharAppearance(WS-CharInd) EQUALS TO 1 
                   COMPUTE WS-CharAppearance(WS-CharInd) = 0
                   COMPUTE WS-Priority = WS-Priority + WS-CharInd
           END-IF
       END-PERFORM .
