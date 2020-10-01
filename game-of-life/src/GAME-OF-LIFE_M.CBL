       IDENTIFICATION DIVISION.
       PROGRAM-ID.  GOL.       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CONSTS.
         05 Y-DIM PIC 99 VALUE 50.
         05 X-DIM PIC 99 VALUE 80.
       01 FIELD.
         05 X-AXIS OCCURS 50 TIMES.
           06 PARTICLE OCCURS 80 TIMES PIC X VALUE " ".
               88 DEAD     VALUE " ".
               88 ALIVE     VALUE "2".
       01 PREV-FIELD.
         05 PREV-X-AXIS OCCURS 50 TIMES.
           06 PREV-PARTICLE OCCURS 80 TIMES PIC X VALUE " ".
               88 PREV-DEAD     VALUE " ".
               88 PREV-ALIVE     VALUE "2".
       01 NEW-STATES.             
         05 SOURROUNDING-ALIVE PIC 9.
           88 DONOTHING VALUES 2,3.
           88 DIE VALUES 0,1,4,5,6,7,8.
         05 SOURROUNDING-DEAD PIC 9.
           88 DONOTHING VALUES 0,1,2,4,5,6,7,8.
           88 RESURRECT VALUES 3.
       01 INDICES.
         05 COL-INDEX PIC 99.
         05 ROW-INDEX PIC 99.
         05 TEMP-INDIZES.
           06 TEMP-COL-INCREMENT PIC 9.
           06 TEMP-ROW-INCREMENT PIC 9.
           06 INNER-COL-INDEX PIC 99.
           06 INNER-ROW-INDEX PIC 99.
       01 Random-Calculation.
           05 Pseudo-Random-Number USAGE COMP-2.
           05 Rounded-Random-Number PIC 9999.
       01 INPUT-VALUE PIC 9.
       PROCEDURE DIVISION.
           PERFORM UNTIL INPUT-VALUE > 0
               DISPLAY "Choose a scenario:"
               DISPLAY "1 - Random start"
               DISPLAY "2 - Gleiter"
               DISPLAY "3 - F-Pentomino"
               DISPLAY "4 - Nice 54 Round figure"
               ACCEPT INPUT-VALUE
               EVALUATE INPUT-VALUE
               WHEN 1
                   PERFORM InitRandom
               WHEN 2
                   PERFORM InitGleiter
               WHEN 3
                   PERFORM InitFPentomino
               WHEN 4
                   PERFORM InitSpecial
               WHEN OTHER
                   DISPLAY "PLEASE SELECT A VALUE BETWEEN 1 and 4!"
                   MOVE 0 TO INPUT-VALUE
               END-EVALUATE
           END-PERFORM
           PERFORM UNTIL 1=2
             PERFORM Print-Field
             PERFORM Alter-State
             PERFORM Sleep
           END-PERFORM
           GOBACK
           .

       InitGleiter SECTION.
           SET ALIVE(20,40) TO TRUE
           SET ALIVE(21,40) TO TRUE
           SET ALIVE(22,40) TO TRUE
           SET ALIVE(20,39) TO TRUE
           SET ALIVE(21,38) TO TRUE
           .
       InitSpecial SECTION.
           SET ALIVE(20,40) TO TRUE
           SET ALIVE(20,41) TO TRUE
           SET ALIVE(20,42) TO TRUE
           SET ALIVE(21,40) TO TRUE
           SET ALIVE(21,42) TO TRUE
           SET ALIVE(22,40) TO TRUE
           SET ALIVE(22,42) TO TRUE
           SET ALIVE(24,40) TO TRUE
           SET ALIVE(24,42) TO TRUE
           SET ALIVE(25,40) TO TRUE
           SET ALIVE(25,42) TO TRUE
           SET ALIVE(26,40) TO TRUE
           SET ALIVE(26,41) TO TRUE
           SET ALIVE(26,42) TO TRUE
           .
       InitFPentomino SECTION.
           SET ALIVE(20,40) TO TRUE
           SET ALIVE(21,40) TO TRUE
           SET ALIVE(22,40) TO TRUE
           SET ALIVE(21,39) TO TRUE
           SET ALIVE(20,41) TO TRUE
           .
       InitRandom SECTION.
           MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE(1:16))
               TO Pseudo-Random-Number
           PERFORM 1000 TIMES
             MOVE FUNCTION RANDOM
               TO Pseudo-Random-Number
             COMPUTE Rounded-Random-Number=
               Pseudo-Random-Number*X-DIM*Y-DIM
             MOVE "2" TO FIELD(Rounded-Random-Number:1)
           END-PERFORM
           .
       Sleep SECTION.
           call "CBL_GC_NANOSLEEP" using "10000000" end-call
           .
       Alter-State SECTION.
           MOVE FIELD TO PREV-FIELD
           PERFORM VARYING COL-INDEX FROM 1 by 1
             UNTIL COL-INDEX > X-DIM
             PERFORM VARYING ROW-INDEX FROM 1 by 1
               UNTIL ROW-INDEX > Y-DIM
               PERFORM CalculateSourroundingAlive
               EVALUATE TRUE
               WHEN PREV-ALIVE(ROW-INDEX,COL-INDEX) AND DIE
                   SET DEAD(ROW-INDEX,COL-INDEX) TO TRUE
               WHEN PREV-DEAD(ROW-INDEX,COL-INDEX) AND RESURRECT
                   SET ALIVE(ROW-INDEX,COL-INDEX) TO TRUE
               END-EVALUATE
             END-PERFORM
           END-PERFORM
       EXIT.

       CalculateSourroundingAlive SECTION.
           MOVE ZERO TO SOURROUNDING-ALIVE
                        SOURROUNDING-DEAD
           MOVE ZERO TO TEMP-INDIZES
           PERFORM VARYING TEMP-COL-INCREMENT FROM 1 by 1
             UNTIL TEMP-COL-INCREMENT > 3
             PERFORM VARYING TEMP-ROW-INCREMENT FROM 1 by 1
               UNTIL TEMP-ROW-INCREMENT > 3
                  IF NOT (TEMP-ROW-INCREMENT = 2 AND
                     TEMP-COL-INCREMENT = 2) THEN
                   COMPUTE INNER-ROW-INDEX = 
                     ROW-INDEX + TEMP-ROW-INCREMENT - 2
                   COMPUTE INNER-COL-INDEX = 
                     COL-INDEX + TEMP-COL-INCREMENT - 2
                   IF INNER-ROW-INDEX = 0 THEN
                       MOVE Y-DIM TO INNER-ROW-INDEX
                   END-IF
                   IF INNER-COL-INDEX = 0 THEN
                       MOVE X-DIM TO INNER-COL-INDEX
                   END-IF
                   IF INNER-ROW-INDEX > Y-DIM THEN
                       MOVE 1 TO INNER-ROW-INDEX
                   END-IF
                   IF INNER-COL-INDEX > X-DIM THEN
                       MOVE 1 TO INNER-COL-INDEX
                   END-IF
                   IF PREV-ALIVE(INNER-ROW-INDEX,INNER-COL-INDEX) THEN
                     COMPUTE SOURROUNDING-DEAD=SOURROUNDING-DEAD + 1
                     COMPUTE SOURROUNDING-ALIVE=SOURROUNDING-ALIVE + 1
                   END-IF
                  END-IF
              END-PERFORM
           END-PERFORM
           .
       Print-Field SECTION.
           PERFORM VARYING COL-INDEX FROM 1 by 1
               UNTIL COL-INDEX > X-DIM
             PERFORM VARYING ROW-INDEX FROM 1 by 1
               UNTIL ROW-INDEX > Y-DIM
               EVALUATE TRUE
               WHEN DEAD(ROW-INDEX,COL-INDEX) AND 
                   PREV-ALIVE(ROW-INDEX,COL-INDEX)
                DISPLAY " "
                BACKGROUND-COLOR 0
                LINE ROW-INDEX COL COL-INDEX
               WHEN ALIVE(ROW-INDEX,COL-INDEX) AND
                   PREV-ALIVE(ROW-INDEX,COL-INDEX)
                DISPLAY " "
                BACKGROUND-COLOR 2
                LINE ROW-INDEX COL COL-INDEX
               WHEN ALIVE(ROW-INDEX,COL-INDEX) AND
                   PREV-DEAD(ROW-INDEX,COL-INDEX)
                DISPLAY " "
                BACKGROUND-COLOR 3
                LINE ROW-INDEX COL COL-INDEX
              END-EVALUATE
            END-PERFORM
           END-PERFORM
       EXIT.
       
       END PROGRAM GOL.
