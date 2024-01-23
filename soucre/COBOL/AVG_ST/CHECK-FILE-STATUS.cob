
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-FILE-STATUS.
      
       DATA DIVISION.
          LINKAGE SECTION.
          01 FILE-STATUS PIC 99.
          01 FILE-NAME   PIC X(25).
          01 IS-ERR      PIC A VALUE 'N'.

       PROCEDURE DIVISION USING FILE-STATUS, FILE-NAME, IS-ERR.
      *    DISPLAY FILE-STATUS.
           EVALUATE FILE-STATUS
              WHEN 00 MOVE 'N' TO IS-ERR
              WHEN 37 DISPLAY "CAN`T OPEN FILE ", 
                         FUNCTION TRIM(FILE-NAME)
                      MOVE 'Y' TO IS-ERR
              WHEN 47 DISPLAY "CAN`T OPEN FILE ", 
                         FUNCTION TRIM(FILE-NAME)
                      MOVE 'Y' TO IS-ERR
              WHEN 48 DISPLAY "CAN`T OPEN FILE ", 
                         FUNCTION TRIM(FILE-NAME)
                      MOVE 'Y' TO IS-ERR
           END-EVALUATE.
           EXIT PROGRAM.
