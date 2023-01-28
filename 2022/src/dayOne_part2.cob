       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. DAYONE.
AUTHOR. RémyCases

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT DataFile ASSIGN TO "input_day_one.dat" 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD DataFile.
01 CaloriesContained PIC 9(8).
    
WORKING-STORAGE SECTION.
01 WSCaloriesContained PIC 9(8).
01 WSCaloriesSum PIC 9(8).
01 WSCaloriesMax.
       02 WSCaloriesFirstMax PIC 9(8).
       02 WSCaloriesSecondMax PIC 9(8).
       02 WSCaloriesThirdMax PIC 9(8).
01 WSCaloriesSumMax PIC 9(8).
01 WSEOF PIC A(1).

PROCEDURE DIVISION.
Main.
       OPEN INPUT DataFile.
              PERFORM UNTIL WSEOF='Y'
                  READ DataFile INTO WSCaloriesContained
                      AT END MOVE 'Y' TO WSEOF
                      NOT AT END PERFORM BranchingCompute
                  END-READ
              END-PERFORM
       CLOSE DataFile
       
       COMPUTE WSCaloriesSumMax = WSCaloriesFirstMax +
       WSCaloriesSecondMax + WSCaloriesThirdMax
       DISPLAY "Max Calories: " WSCaloriesSumMax
       STOP RUN.

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
