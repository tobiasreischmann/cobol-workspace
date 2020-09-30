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
            COPY ZUTZCWS     .
       01 HOEHE PIC 99 VALUE 50.
       01 BAUM.
         05 SPITZE PIC X(100).
         05 ZWEIGE PIC X(100).
         05 STAMM PIC X(100).
       01 ZWEIGEBENE PIC 99.
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'Beispiel Testsuite'
           DISPLAY SPACE


           MOVE 'Baum der Höhe 0 ist X'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 0 TO HOEHE
               PERFORM Baue-Baum-Geruest
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE SPITZE TO UT-ACTUAL
           MOVE "X"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ZWEIGE TO UT-ACTUAL
           MOVE ""
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE STAMM TO UT-ACTUAL
           MOVE ""
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL


           PERFORM UT-AFTER
           MOVE "Baum der Höhe 1 ist X mit Stamm"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 1 TO HOEHE
               PERFORM Baue-Baum-Geruest
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE SPITZE TO UT-ACTUAL
           MOVE " X "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ZWEIGE TO UT-ACTUAL
           MOVE ""
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE STAMM TO UT-ACTUAL
           MOVE "| |"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL


           PERFORM UT-AFTER
           MOVE "Baum der Höhe 2 ist X mit Zweigen und Stamm"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 2 TO HOEHE
               PERFORM Baue-Baum-Geruest
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE SPITZE TO UT-ACTUAL
           MOVE " X "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ZWEIGE TO UT-ACTUAL
           MOVE " A "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE STAMM TO UT-ACTUAL
           MOVE "| |"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE "Berechne Zweigebene 1 bei Baumhöhe 3"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 3 TO HOEHE
               MOVE 1 TO ZWEIGEBENE
               PERFORM Baue-Zweige
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ZWEIGE TO UT-ACTUAL
           MOVE " / \ "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE "Berechne Zweigebene 1 bei Baumhöhe 4"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 4 TO HOEHE
               MOVE 1 TO ZWEIGEBENE
               PERFORM Baue-Zweige
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ZWEIGE TO UT-ACTUAL
           MOVE "  / \  "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE "Berechne Zweigebene 2 bei Baumhöhe 4"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 4 TO HOEHE
               MOVE 2 TO ZWEIGEBENE
               PERFORM Baue-Zweige
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ZWEIGE TO UT-ACTUAL
           MOVE " /   \ "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE "Bastel Boden in Zweigebene 2 bei Baumhöhe 4"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 4 TO HOEHE
               MOVE 2 TO ZWEIGEBENE
               PERFORM Bastel-Boden
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ZWEIGE TO UT-ACTUAL
           MOVE "  ___  "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE "Ein Integrationstest von Bastel-Boden und Baue-Zweige"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 4 TO HOEHE
               MOVE 2 TO ZWEIGEBENE
               PERFORM Baue-Zweige
               PERFORM Bastel-Boden
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ZWEIGE TO UT-ACTUAL
           MOVE " /___\ "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE "Does not segfault for Höhe = 1"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 1 TO HOEHE
               PERFORM Main
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE 1 TO UT-ACTUAL
           MOVE 1
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.
               INITIALIZE BAUM

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACE TO UT-FILE-INFORMATION

           .
       UT-END.

           DISPLAY 'IT looks a lot like christmas XXX'
           ACCEPT HOEHE
           PERFORM Main
           STOP RUN

           .
       Main SECTION.
           PERFORM Baue-Baum-Geruest
           DISPLAY SPITZE
           PERFORM Zeige-Zweige
           DISPLAY STAMM


           .
       Bastel-Boden SECTION.
           MOVE ALL "_" TO ZWEIGE(3:HOEHE * 2 - 5)

           .
       Zeige-Zweige SECTION.
           IF HOEHE NOT = 1 THEN
               DISPLAY ZWEIGE
           END-IF
           PERFORM VARYING ZWEIGEBENE FROM 1 BY 1
               UNTIL ZWEIGEBENE > HOEHE - 3
               PERFORM Baue-Zweige
               DISPLAY ZWEIGE
           END-PERFORM
           If HOEHE > 2 THEN
               PERFORM Baue-Zweige
               PERFORM Bastel-Boden
               DISPLAY ZWEIGE
           END-IF


           .
       Baue-Zweige SECTION.
           INITIALIZE ZWEIGE
           MOVE "/" TO ZWEIGE(HOEHE - ZWEIGEBENE:1)
           MOVE "\" TO ZWEIGE(HOEHE + ZWEIGEBENE:1)


           .
       Baue-Baum-Geruest SECTION.
           INITIALIZE BAUM
           EVALUATE HOEHE
           WHEN 0
               MOVE "X" TO SPITZE
           WHEN 1
               MOVE " X " TO SPITZE
               MOVE "| |" TO STAMM
           WHEN OTHER
               MOVE "X" TO SPITZE(HOEHE:)
               MOVE "A" TO ZWEIGE(HOEHE:)
               MOVE "| |" TO STAMM(HOEHE - 1:3)
           END-EVALUATE


           .

