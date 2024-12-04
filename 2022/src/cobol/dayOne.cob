       >>SOURCE FORMAT FREE
*> Copyright (C) 2023 Rémy Cases
*> See LICENSE file for extended copyright information.
*> This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

IDENTIFICATION DIVISION.
PROGRAM-ID. DAYONE.
AUTHOR. RémyCases

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT DataFile ASSIGN TO "2022/data/input_day_one" 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD DataFile.
01 CaloriesContained PIC Z(8).
    
WORKING-STORAGE SECTION.
01 WSCaloriesContained PIC 9(8).
01 WSCaloriesSum PIC 9(8).
01 WSCaloriesMax.
       02 WSCaloriesFirstMax PIC 9(8).
       02 WSCaloriesSecondMax PIC 9(8).
       02 WSCaloriesThirdMax PIC 9(8).
01 WSCaloriesSumMax PIC Z(8).
01 WSEOF PIC A(1).

LINKAGE SECTION.
01 LPart PIC 9 VALUE 1.

PROCEDURE DIVISION USING LPart.
Main.
       OPEN INPUT DataFile.
              PERFORM UNTIL WSEOF='Y'
                  READ DataFile INTO WSCaloriesContained
                      AT END MOVE 'Y' TO WSEOF
                      NOT AT END PERFORM BranchingCompute
                  END-READ
              END-PERFORM
       CLOSE DataFile
       
       IF LPart EQUAL TO 1 THEN
           COMPUTE WSCaloriesSumMax = WSCaloriesFirstMax
       END-IF

       IF LPart EQUAL TO 2 THEN
           COMPUTE WSCaloriesSumMax = WSCaloriesFirstMax +
           WSCaloriesSecondMax + WSCaloriesThirdMax
       END-IF

       DISPLAY "MAX CALORIES: " FUNCTION TRIM(WSCaloriesSumMax)
EXIT PROGRAM.

BranchingCompute.
       *> Empty Line is the separator between chunk of data
       *> An empty line is here read as ZERO
       IF WSCaloriesContained NOT EQUAL TO ZERO
           PERFORM ComputeSum
       END-IF

       IF WSCaloriesContained EQUAL TO ZERO
           MOVE ZERO TO WSCaloriesSum
       END-IF
       PERFORM ComputeMax.

ComputeSum.
       COMPUTE WSCaloriesSum = WSCaloriesSum + WSCaloriesContained.

ComputeMax.
       IF WSCaloriesSum > WSCaloriesFirstMax
           MOVE WSCaloriesSecondMax TO WSCaloriesThirdMax
           MOVE WSCaloriesFirstMax TO WSCaloriesSecondMax
           MOVE WSCaloriesSum TO WSCaloriesFirstMax
       END-IF.

       IF WSCaloriesSum < WSCaloriesFirstMax AND
       WSCaloriesSum > WSCaloriesSecondMax
           MOVE WSCaloriesSecondMax TO WSCaloriesThirdMax
           MOVE WSCaloriesSum TO WSCaloriesSecondMax
       END-IF.

       IF WSCaloriesSum < WSCaloriesSecondMax AND
       WSCaloriesSum > WSCaloriesThirdMax
           MOVE WSCaloriesSum TO WSCaloriesThirdMax
       END-IF.
