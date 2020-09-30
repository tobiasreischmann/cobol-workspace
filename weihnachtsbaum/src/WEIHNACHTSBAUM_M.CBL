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
       01 HOEHE PIC 99 VALUE 50.
       01 BAUM.
         05 SPITZE PIC X(100).
         05 ZWEIGE PIC X(100).
         05 STAMM PIC X(100).
       01 ZWEIGEBENE PIC 99.
       PROCEDURE DIVISION.
           DISPLAY 'IT looks a lot like christmas XXX'
           ACCEPT HOEHE
           PERFORM Main
           STOP RUN.

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

