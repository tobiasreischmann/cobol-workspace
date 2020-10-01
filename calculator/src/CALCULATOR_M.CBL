       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BASEM.
      *****************************************************************
      * Implementation des TIC TACT TOE Spiels.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INPUT-STRING PIC X(50).
       01 RESULT USAGE COMP-1.
       01 TEMP.
           05 ITERATOR PIC 99.
           05 CURRCHAR PIC X.
           05 CURRNUMBER-ITERATOR PIC 999.
           05 CURRNUMBER PIC 999.
           05 CURRNUMBERBUILDER PIC 999.
       PROCEDURE DIVISION.
           MOVE "1\n 23 " TO INPUT-STRING

           DISPLAY INPUT-STRING
           STOP RUN.
       
       Calculate SECTION.
           INITIALIZE RESULT
                      TEMP
           Add 1 TO ITERATOR
           PERFORM UNTIL INPUT-STRING(ITERATOR:1) = " "
               IF INPUT-STRING(ITERATOR:1) IS NUMERIC THEN
                   Add 1 TO CURRNUMBER-ITERATOR
                   MOVE INPUT-STRING(ITERATOR:1) TO 
                       CURRNUMBERBUILDER(CURRNUMBER-ITERATOR:1)
               ELSE
                   PERFORM AddBuildNumber
                   INITIALIZE CURRNUMBERBUILDER
                              CURRNUMBER-ITERATOR
               END-IF
               Add 1 TO ITERATOR
           END-PERFORM
           PERFORM AddBuildNumber
           .

       AddBuildNumber Section.
           MOVE CURRNUMBERBUILDER(1:CURRNUMBER-ITERATOR)
                    TO CURRNUMBER
                   ADD CURRNUMBER TO RESULT
           .
                   
