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
            COPY ZUTZCWS     .
       01 SPIELFELD.
         05 Y-ACHSE OCCURS 3.
           07 ZELLE PIC X OCCURS 3 VALUE " ".
               88 LEER VALUE " ".
               88 X    VALUE "X".
               88 O    VALUE "O".
       01 TEMP-SPIELFELD.
         05 TEMP-Y-ACHSE OCCURS 3.
           07 TEMP-ZELLE PIC X OCCURS 3 VALUE " ".
               88 LEER VALUE " ".
               88 X    VALUE "X".
               88 O    VALUE "O".
       01 player-input-x PIC 99.
       01 player-input-y PIC 99.
       01 CURR-PLAYER PIC X VALUE "X".
           88 PLAYER-X VALUE "X".
           88 PLAYER-O VALUE "O".
       01  RESPONSEZ.
           05  RESPONSE-IN-WS  PIC X         VALUE "C".
       01 ERROR-STATES.
           05 INPUT-VALIDATION PIC X         VALUE " ".
               88 INPUT-CORRECT              VALUE " ".
               88 MORE-THAN-ONE-FIELD-FILLED VALUE "M".
               88 NO-FIELD-FILLED            VALUE "N".
               88 WRONG-VALUE-FILLED         VALUE "W".
               88 EXISTING-VALUE-ALTERED     VALUE "A".
           05 ERROR-MESSAGE PIC X(30).
       01 GAME-STATES.
           05 BOARD-STATE PIC X.
               88 GAME-NOT-FULL              VALUE " ".
               88 GAME-FULL                  VALUE "F".
           05 WINNING-STATE PIC X.
               88 NO-WINNER                  VALUE " ".
               88 PLAYER-WON                 VALUE "W".
           05 LAST-COL PIC 9.
           05 LAST-ROW PIC 9.
       01 INDICES.
           05 COUNTER-COL PIC 9.
           05 COUNTER-ROW PIC 9.
           05 NUM-ALTERED-FIELDS PIC 9.
               88 NO-FIELD-ALTERED           VALUE 0.
               88 ONE-FIELD-ALTERED          VALUE 1.

       SCREEN SECTION.
        01  DATA-ENTRY-SCREEN.
          05  VALUE "TIC TAC TOE" BLANK SCREEN          LINE 1 COL 35.
          05  VALUE "- CURRENT PLAYER ="                LINE 1 COL 47.
          05  PLAYER                                    LINE 1 COL 66
               PIC X USING CURR-PLAYER.
          05  VALUE "   |   |   "                       LINE 3 COL 30.
          05  VALUE "-----------"                       LINE 4 COL 30.
          05  VALUE "   |   |   "                       LINE 5 COL 30.
          05  VALUE "-----------"                       LINE 6 COL 30.
          05  VALUE "   |   |   "                       LINE 7 COL 30.
          05 Zells.
           06  Z11                                       LINE 3 COL 31
                   PIC X USING ZELLE(1,1).
           06  Z12                                       LINE 3 COL 35
                   PIC X USING ZELLE(1,2).
           06  Z13                                       LINE 3 COL 39
                   PIC X USING ZELLE(1,3).
           06  Z21                                       LINE 5 COL 31
                   PIC X USING ZELLE(2,1).
           06  Z22                                       LINE 5 COL 35
                   PIC X USING ZELLE(2,2).
           06  Z23                                       LINE 5 COL 39
                   PIC X USING ZELLE(2,3).
           06  Z31                                       LINE 7 COL 31
                   PIC X USING ZELLE(3,1).
           06  Z32                                       LINE 7 COL 35
                   PIC X USING ZELLE(3,2).
           06  Z33                                       LINE 7 COL 39
                   PIC X USING ZELLE(3,3).
          05  ERROR-MESSAGE-FIELD                        LINE 9 COL 30
                           PIC X(30)     FROM ERROR-MESSAGE.
          05  VALUE "N - NEW GAME"                       LINE 11 COL 30.
          05  VALUE "Q - TO QUIT"                        LINE 12 COL 30.
          05  VALUE "ENTER RESPONSE"                     LINE 14 COL 30.
          05  RESPONSE-INPUT                             LINE 14 COL 45
                            PIC X         TO RESPONSE-IN-WS.

       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'TIC TACT TOE TEST'
           DISPLAY SPACE

           MOVE 'Test, switch-player'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET PLAYER-X TO TRUE
               PERFORM Switch-Current-Player
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF PLAYER-O
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Test, switch-player'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET PLAYER-O TO TRUE
               PERFORM Switch-Current-Player
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF PLAYER-X
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Test, switch-player'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET PLAYER-X TO TRUE
               MOVE 1 TO LAST-COL
               MOVE 1 TO LAST-ROW
               MOVE "X  X  X  " TO SPIELFELD
               PERFORM Check-Player-Won
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF PLAYER-WON
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Test, switch-player'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET PLAYER-X TO TRUE
               MOVE 1 TO LAST-COL
               MOVE 1 TO LAST-ROW
               MOVE "X   X   X" TO SPIELFELD
               PERFORM Check-Player-Won
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF PLAYER-WON
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Test, switch-player'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET PLAYER-X TO TRUE
               MOVE 3 TO LAST-COL
               MOVE 1 TO LAST-ROW
               MOVE "  X X X  " TO SPIELFELD
               PERFORM Check-Player-Won
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF PLAYER-WON
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACE TO UT-FILE-INFORMATION

           .
       UT-END.

           MOVE SPIELFELD to TEMP-SPIELFELD
           PERFORM UNTIL RESPONSE-IN-WS = "Q"
               MOVE SPIELFELD to TEMP-SPIELFELD
               DISPLAY DATA-ENTRY-SCREEN
               IF NOT GAME-FULL AND NOT PLAYER-WON THEN
                   ACCEPT Zells
                   PERFORM Check-Input
                   IF INPUT-CORRECT THEN
                       MOVE SPIELFELD to TEMP-SPIELFELD
                       PERFORM Check-Winning-State
                       PERFORM Switch-Current-Player
                   ELSE
      *                Reset game state
                       MOVE TEMP-SPIELFELD TO SPIELFELD
                       EVALUATE TRUE
                       WHEN MORE-THAN-ONE-FIELD-FILLED
                           MOVE "Only alter one field!"
                               TO ERROR-MESSAGE
                       WHEN NO-FIELD-FILLED
                           MOVE "Enter a value in a cell!"
                               TO ERROR-MESSAGE
                       WHEN WRONG-VALUE-FILLED
                           MOVE "Only enter you own symbol!"
                               TO ERROR-MESSAGE
                       WHEN EXISTING-VALUE-ALTERED
                           MOVE "Don't alter existing values!"
                               TO ERROR-MESSAGE
                       WHEN OTHER
                           MOVE INPUT-VALIDATION TO ERROR-MESSAGE
                       END-EVALUATE
                   END-IF
               ELSE
                   MOVE " " TO RESPONSE-IN-WS
                   PERFORM UNTIL RESPONSE-IN-WS = "Q" OR
                     RESPONSE-IN-WS = "N"
                       ACCEPT RESPONSE-INPUT
                       EVALUATE RESPONSE-IN-WS
                       WHEN = "N"
                           INITIALIZE SPIELFELD
                                      TEMP-SPIELFELD
                                      ERROR-STATES
                                      GAME-STATES
                       WHEN = "Q"
                           GOBACK
                   END-PERFORM
               END-IF
           END-PERFORM

           GOBACK

           .
       Check-Winning-State SECTION.
           PERFORM Check-Player-Won
           IF PLAYER-WON THEN
               MOVE "YOU WON!!!!!" TO ERROR-MESSAGE
           END-IF
           PERFORM Check-Game-Finished
           IF GAME-FULL THEN
               MOVE "GAME OVER!" TO ERROR-MESSAGE
           END-IF
           .
       EXIT.

           .
       Check-Player-Won SECTION.
           SET PLAYER-WON TO TRUE
           PERFORM VARYING COUNTER-COL FROM 1 BY 1
             UNTIL COUNTER-COL > 3
               IF ZELLE(LAST-ROW,COUNTER-COL) NOT = CURR-PLAYER THEN
                   SET NO-WINNER TO TRUE
               END-IF
           END-PERFORM
           IF NO-WINNER THEN
               SET PLAYER-WON TO TRUE
               PERFORM VARYING COUNTER-ROW FROM 1 BY 1
                 UNTIL COUNTER-ROW > 3
                   IF ZELLE(COUNTER-ROW,LAST-COL) NOT = CURR-PLAYER THEN
                       SET NO-WINNER TO TRUE
                   END-IF
               END-PERFORM
           END-IF
           IF NO-WINNER THEN
               SET PLAYER-WON TO TRUE
               PERFORM VARYING COUNTER-ROW FROM 1 BY 1
                 UNTIL COUNTER-ROW > 3
                   IF ZELLE(COUNTER-ROW,COUNTER-ROW) NOT = CURR-PLAYER
                     THEN
                       SET NO-WINNER TO TRUE
                   END-IF
               END-PERFORM
           END-IF
           IF NO-WINNER THEN
              IF ZELLE(3,1) = CURR-PLAYER AND
                 ZELLE(2,2) = CURR-PLAYER AND
                 ZELLE(1,3) = CURR-PLAYER THEN
                 SET PLAYER-WON TO TRUE
              END-IF
           END-IF
           .
       EXIT.

           .
       Check-Game-Finished SECTION.
           SET GAME-FULL TO TRUE
           PERFORM VARYING COUNTER-COL FROM 1 BY 1
               UNTIL COUNTER-COL > 3
               PERFORM VARYING COUNTER-ROW FROM 1 BY 1
                   UNTIL COUNTER-ROW > 3
                   IF TEMP-ZELLE(COUNTER-ROW,COUNTER-COL) = " "
                     THEN
                       SET GAME-NOT-FULL TO TRUE
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       EXIT.

           .
       Check-Input SECTION.
           INITIALIZE INDICES
                      ERROR-STATES
           PERFORM VARYING COUNTER-COL FROM 1 BY 1
               UNTIL COUNTER-COL > 3
               PERFORM VARYING COUNTER-ROW FROM 1 BY 1
                   UNTIL COUNTER-ROW > 3
                   IF ZELLE(COUNTER-ROW,COUNTER-COL) NOT =
                       TEMP-ZELLE(COUNTER-ROW,COUNTER-COL) THEN
                       ADD 1 TO NUM-ALTERED-FIELDS
                       MOVE COUNTER-COL TO LAST-COL
                       MOVE COUNTER-ROW TO LAST-ROW
                       IF ZELLE(COUNTER-ROW,COUNTER-COL) NOT =
                           CURR-PLAYER THEN
                           SET WRONG-VALUE-FILLED TO TRUE
                       END-IF
                       IF TEMP-ZELLE(COUNTER-ROW,COUNTER-COL) NOT = " "
                           THEN
                           SET EXISTING-VALUE-ALTERED TO TRUE
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           EVALUATE TRUE
           WHEN NO-FIELD-ALTERED
               SET NO-FIELD-FILLED TO TRUE
           WHEN NOT ONE-FIELD-ALTERED
               SET MORE-THAN-ONE-FIELD-FILLED TO TRUE
           .
       EXIT.

           .
       Switch-Current-Player SECTION.
           EVALUATE TRUE
           WHEN PLAYER-X
               SET PLAYER-O TO TRUE
           WHEN PLAYER-O
               SET PLAYER-X TO TRUE
           END-EVALUATE
           .
       EXIT.

           .
       END PROGRAM TICTACTOE.

           .

