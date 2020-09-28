       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TICTACTOE.
      *****************************************************************
      * Implementation des TIC TACT TOE Spiels.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 SPIELFELD.
             05 Y-ACHSE OCCURS 3.
               07 ZELLE PIC X OCCURS 3.
           01 player-input-x PIC 99.
           01 player-input-y PIC 99.
           01 CURR-PLAYER PIC X VALUE "X".
               88 X VALUE "X".
               88 O VALUE "O".
         01  RESPONSEZ.
           05  RESPONSE-IN-WS  PIC X        VALUE "C".
         01  DATA-FROM-SCREEN.
           05  ID-IN-WS        PIC XXXX     VALUE SPACES.
           05  NAME-IN-WS      PIC X(20)    VALUE SPACES.

       SCREEN SECTION.
        01  DATA-ENTRY-SCREEN.
          05  VALUE "+_______________+"                  Line 2 COL 4.
          05  VALUE "DATA ENTRY SCREEN" BLANK SCREEN     LINE 1 COL 35.
          05  VALUE "ID #"                               LINE 3 COL 10.
          05  ID-INPUT                                  LINE 3 COL 25
                          PIC  X(4)     TO ID-IN-WS.
          05  VALUE "NAME"                               LINE 5 COL 10.
          05  NAME-INPUT                                 LINE 5 COL 25
                         PIC X(20)     TO NAME-IN-WS.
          05  VALUE "C - TO CONTINUE"                    LINE 11 COL 30.
          05  VALUE "Q - TO QUIT"                        LINE 12 COL 30.
          05  VALUE "ENTER RESPONSE"                     LINE 14 COL 30.
          05  RESPONSE-INPUT                             LINE 14 COL 45
                            PIC X         TO RESPONSE-IN-WS.

       PROCEDURE DIVISION.
           MOVE "X       X" to SPIELFELD
           PERFORM UNTIL RESPONSE-IN-WS = "Q"
               DISPLAY DATA-ENTRY-SCREEN
               ACCEPT DATA-ENTRY-SCREEN
               DISPLAY DATA-ENTRY-SCREEN
               ACCEPT RESPONSE-INPUT
           END-PERFORM
      *    DISPLAY SPIELFELD
      *    PERFORM Player-Move
      *    DISPLAY SPIELFELD

           GOBACK.

       Player-Move SECTION.
           ACCEPT player-input-x
           ACCEPT player-input-y
           MOVE CURR-PLAYER TO ZELLE(player-input-x,player-input-y)
       EXIT.    

       Print-Game-State SECTION.
           DISPLAY ZELLE(1,1) WITH NO ADVANCING
           DISPLAY "|"  WITH NO ADVANCING
           DISPLAY ZELLE(2,1) WITH NO ADVANCING
           DISPLAY "|"  WITH NO ADVANCING
           DISPLAY ZELLE(3,1)
           DISPLAY "_____"
           DISPLAY ZELLE(1,2) WITH NO ADVANCING
           DISPLAY "|"  WITH NO ADVANCING
           DISPLAY ZELLE(2,2) WITH NO ADVANCING
           DISPLAY "|"  WITH NO ADVANCING
           DISPLAY ZELLE(3,2)
           DISPLAY "_____"
           DISPLAY ZELLE(1,3) WITH NO ADVANCING
           DISPLAY "|"  WITH NO ADVANCING
           DISPLAY ZELLE(2,3) WITH NO ADVANCING
           DISPLAY "|"  WITH NO ADVANCING
           DISPLAY ZELLE(3,3) UNDERLINE
           .
       EXIT.
           
       END PROGRAM TICTACTOE.
           
