       IDENTIFICATION DIVISION.
       PROGRAM-ID.  GOL.       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CONSTS.
         05 X-DIM PIC 99 VALUE 80.
         05 Y-DIM PIC 99 VALUE 50.
       01 FIELD.
         05 X-AXIS OCCURS 50 TIMES.
           06 PARTICLE OCCURS 80 TIMES PIC X VALUE " ".
               88 DEAD     VALUE " ".
               88 ALIVE     VALUE "X".
       01 PREV-FIELD.
         05 PREV-X-AXIS OCCURS 50 TIMES.
           06 PREV-PARTICLE OCCURS 80 TIMES PIC X VALUE " ".
               88 PREV-DEAD     VALUE " ".
               88 PREV-ALIVE     VALUE "X".
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
       PROCEDURE DIVISION.
           PERFORM InitSpecial
           PERFORM UNTIL 1=2
             PERFORM Print-Field
             PERFORM Alter-State
             PERFORM Sleep
           END-PERFORM
           GOBACK
           .

       InitGleiter SECTION.
           SET ALIVE(10,20) TO TRUE
           SET ALIVE(11,20) TO TRUE
           SET ALIVE(12,20) TO TRUE
           SET ALIVE(10,19) TO TRUE
           SET ALIVE(11,18) TO TRUE
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
           SET ALIVE(10,20) TO TRUE
           SET ALIVE(11,20) TO TRUE
           SET ALIVE(12,20) TO TRUE
           SET ALIVE(11,19) TO TRUE
           SET ALIVE(10,21) TO TRUE
           .
       Sleep SECTION.
           call "CBL_GC_NANOSLEEP" using "100000000" end-call
           .
       Alter-State SECTION.
           MOVE FIELD TO PREV-FIELD
           PERFORM VARYING COL-INDEX FROM 2 by 1
             UNTIL COL-INDEX > X-DIM
             PERFORM VARYING ROW-INDEX FROM 2 by 1
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
                   IF INNER-ROW-INDEX <= Y-DIM AND INNER-ROW-INDEX > 0
                    AND INNER-COL-INDEX <= X-DIM AND INNER-COL-INDEX > 0
                    THEN
                     IF PREV-ALIVE(INNER-ROW-INDEX,INNER-COL-INDEX) THEN
                       COMPUTE SOURROUNDING-DEAD=SOURROUNDING-DEAD + 1
                       COMPUTE SOURROUNDING-ALIVE=SOURROUNDING-ALIVE + 1
                     END-IF
                   END-IF
                  END-IF
              END-PERFORM
           END-PERFORM
           .
       Print-Field SECTION.
             PERFORM VARYING ROW-INDEX FROM 1 by 1
               UNTIL ROW-INDEX > X-DIM
               DISPLAY X-AXIS(ROW-INDEX)
      *          BACKGROUND-COLOR PARTICLE(ROW-INDEX,COL-INDEX)
                 LINE ROW-INDEX COL 1
      *      END-PERFORM
           END-PERFORM
       EXIT.
       
       END PROGRAM GOL.
