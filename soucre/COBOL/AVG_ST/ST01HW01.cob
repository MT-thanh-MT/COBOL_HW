       IDENTIFICATION DIVISION. 
       PROGRAM-ID. ST01HW01.

       ENVIRONMENT DIVISION.
          INPUT-OUTPUT SECTION.
          FILE-CONTROL.
             SELECT STUDENT ASSIGN TO 'RESOURCES/FILEA.txt'
             STATUS IS FILE-A-STATUS
             ORGANIZATION IS LINE SEQUENTIAL.

             SELECT SCORE ASSIGN TO 'RESOURCES/FILEB.txt'
             STATUS IS FILE-B-STATUS
             ORGANIZATION IS LINE SEQUENTIAL. 

             SELECT AVG ASSIGN TO 'RESOURCES/FILEC.txt'
             STATUS IS FILE-C-STATUS
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
          FILE SECTION.

          FD STUDENT.
          01 STUDENT-FILE.
             05 STUDENT-ID PIC X(7).
             05 NAME PIC A(25).

          FD SCORE.
          01 STUDENT-SCORE.
             05 SCORE-STUDENT-ID PIC X(7).
             05 MATH-SCORE PIC 9(2)V99.
             05 LITERATURE-SCORE PIC 9(2)V99.

          FD AVG.
          01 STUDENT-AVG-SCORE.
             05 AVG-NAME PIC A(25).
             05 AVG-SCORE PIC 9(2)V99.
       
          WORKING-STORAGE SECTION.
          01 WS-STUDENT.
             05 WS-STUDENT-ID PIC X(7).
             05 WS-NAME PIC A(25).

          01 WS-SCORE.
             05 WS-SCORE-STUDENT-ID PIC X(7).
             05 WS-MATH-SCORE PIC 9(2)V99.
             05 WS-LITERATURE-SCORE PIC 9(2)V99.

          01 WS-AVG.
             05 WS-AVG-NAME PIC A(25).
             05 WS-AVG-SCORE PIC 9(2)V99.

          01 FILE-A-STATUS   PIC 99.
          01 FILE-B-STATUS   PIC 99.
          01 FILE-C-STATUS   PIC 99.
          01 WS-EOF-A        PIC A(1).
          01 WS-EOF-B        PIC A(1).
          01 FC-A            PIC 99 VALUE 0.
          01 FC-B            PIC 99 VALUE 0.
          01 FILE-NAME       PIC X(25).
          01 IS-ERR          PIC A VALUE 'N'.
          01 STUDENT_EXISTS  PIC A VALUE 'N'.
          
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-INIT.
           PERFORM 101-READ-FA.
           PERFORM 102-END-READ.
           STOP RUN.

       100-INIT.
           OPEN INPUT STUDENT.
           OPEN OUTPUT AVG.
           PERFORM 104-CHECK-FILEA-STATUS.
           PERFORM 106-CHECK-FILEC-STATUS.

       101-READ-FA.
           PERFORM UNTIL WS-EOF-A='Y'
              READ STUDENT INTO WS-STUDENT
                 AT END MOVE 'Y' TO WS-EOF-A
                 NOT AT END 
                    ADD 1 TO FC-A
                    DISPLAY "ID: "WS-STUDENT-ID
                    DISPLAY "NAME: "WS-NAME
                    PERFORM 103-READ-FB
              END-READ
              PERFORM 104-CHECK-FILEA-STATUS
              IF FC-A = 0 THEN
                 DISPLAY "FILE A IS EMPTY!"
                 PERFORM 102-END-READ
                 STOP RUN
              END-IF
           END-PERFORM.

       102-END-READ.
           CLOSE STUDENT.
           CLOSE AVG.
           CLOSE SCORE.

       103-READ-FB.
           MOVE 'N' TO WS-EOF-B
           OPEN INPUT SCORE
           PERFORM 105-CHECK-FILEB-STATUS
           PERFORM UNTIL WS-EOF-B='Y'
               READ SCORE INTO WS-SCORE
                   AT END 
                       MOVE 'Y' TO WS-EOF-B
                   NOT AT END
                       PERFORM 105-CHECK-FILEB-STATUS
                       ADD 1 TO FC-B
                       IF WS-STUDENT-ID = WS-SCORE-STUDENT-ID
                           MOVE WS-NAME TO AVG-NAME
                           MOVE 'Y' TO STUDENT_EXISTS
                           COMPUTE AVG-SCORE = 
                              (WS-MATH-SCORE + WS-LITERATURE-SCORE)/2
                           DISPLAY "AVG: "
                              AVG-NAME, AVG-SCORE
                           WRITE STUDENT-AVG-SCORE
                           END-WRITE
                           PERFORM 106-CHECK-FILEC-STATUS
                           EXIT PERFORM 
                        END-IF 
               END-READ
           END-PERFORM
           IF FC-B = 0 THEN
              DISPLAY "FILE B IS EMPTY!", 
              PERFORM 102-END-READ
              STOP RUN
           END-IF
           IF STUDENT_EXISTS EQUAL 'N' THEN
              DISPLAY "The student with this " WS-STUDENT-ID,
                 " does not have a grade yet"
              MOVE WS-NAME TO AVG-NAME
              MOVE 0 TO AVG-SCORE
              WRITE STUDENT-AVG-SCORE
              END-WRITE
              PERFORM 106-CHECK-FILEC-STATUS
           END-IF
           
           CLOSE SCORE.

       104-CHECK-FILEA-STATUS.
           MOVE 'FLIEA.txt' TO FILE-NAME.
           CALL 'CHECK-FILE-STATUS' USING 
              FILE-A-STATUS, FILE-NAME, IS-ERR.
           PERFORM 107-CHECK-ERR.

       105-CHECK-FILEB-STATUS.
           MOVE 'FLIEB.txt' TO FILE-NAME.
           CALL 'CHECK-FILE-STATUS' USING 
              FILE-B-STATUS, FILE-NAME, IS-ERR.
           PERFORM 107-CHECK-ERR.

       106-CHECK-FILEC-STATUS.
           MOVE 'FLIEC.txt' TO FILE-NAME.
           CALL 'CHECK-FILE-STATUS' USING 
              FILE-C-STATUS, FILE-NAME, IS-ERR.
           PERFORM 107-CHECK-ERR.

       107-CHECK-ERR.
           IF IS-ERR EQUAL 'Y' THEN
              PERFORM 102-END-READ
              STOP RUN
           END-IF.

       END PROGRAM ST01HW01.
