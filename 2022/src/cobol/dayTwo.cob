       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. DAYTWO.
AUTHOR. RémyCases

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT DataFile ASSIGN TO "2022/data/input_day_two.txt" 
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
01 WSOpponentStrat PIC A VALUE 'A'.
01 WSSelfStrat PIC A VALUE 'X'.
01 WSEOF PIC A.
01 WSScore PIC 9(8) VALUE ZEROS.

PROCEDURE DIVISION.
Main.
       OPEN INPUT DataFile.
              PERFORM UNTIL WSEOF='Y'
                  READ DataFile INTO FStrat
                      AT END MOVE 'Y' TO WSEOF
                      NOT AT END PERFORM BranchingCompute
                  END-READ
              END-PERFORM
       CLOSE DataFile
       
       DISPLAY "Score: " WSScore
EXIT PROGRAM.

BranchingCompute.
       MOVE FOpponentStrat TO WSOpponentStrat
       MOVE FSelfStrat TO WSSelfStrat

       if WSSelfStrat EQUAL TO 'X'
           COMPUTE WSScore = WSScore + 1

           if WSOpponentStrat EQUAL TO 'A'
           *> tie
               COMPUTE WSScore = WSScore + 3
           END-IF

           if WSOpponentStrat EQUAL TO 'B'
           *> loose
               COMPUTE WSScore = WSScore + 0
           END-IF

           if WSOpponentStrat EQUAL TO 'C'
           *> win
               COMPUTE WSScore = WSScore + 6
           END-IF
       END-IF

       if WSSelfStrat EQUAL TO 'Y'
           COMPUTE WSScore = WSScore + 2

           if WSOpponentStrat EQUAL TO 'A'
           *> win
               COMPUTE WSScore = WSScore + 6
           END-IF

           if WSOpponentStrat EQUAL TO 'B'
           *> tie
               COMPUTE WSScore = WSScore + 3
           END-IF

           if WSOpponentStrat EQUAL TO 'C'
           *> loose
               COMPUTE WSScore = WSScore + 0
           END-IF
       END-IF

       if WSSelfStrat EQUAL TO 'Z'
           COMPUTE WSScore = WSScore + 3

           if WSOpponentStrat EQUAL TO 'A'
           *> loose
               COMPUTE WSScore = WSScore + 0
           END-IF

           if WSOpponentStrat EQUAL TO 'B'
           *> win
               COMPUTE WSScore = WSScore + 6
           END-IF

           if WSOpponentStrat EQUAL TO 'C'
           *> tie
               COMPUTE WSScore = WSScore + 3
           END-IF
       END-IF.
