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
       01 001-LINE PIC X(80).
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD'
           STOP RUN.

       ReadLine SECTION.
           MOVE "4711112345678900005000000Testüberweisung" TO  001-LINE
       EXIT.
