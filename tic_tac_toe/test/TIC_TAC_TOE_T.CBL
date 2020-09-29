           TESTSUITE 'TIC TACT TOE TEST'

           TESTCASE 'Test, switch-player'
               SET PLAYER-X TO TRUE
               PERFORM Switch-Current-Player
           EXPECT PLAYER-O TO BE TRUE

           TESTCASE 'Test, switch-player'
               SET PLAYER-O TO TRUE
               PERFORM Switch-Current-Player
           EXPECT PLAYER-X TO BE TRUE

           TESTCASE 'Test, switch-player'
               SET PLAYER-X TO TRUE
               MOVE 1 TO LAST-COL
               MOVE 1 TO LAST-ROW
               MOVE "X  X  X  " TO SPIELFELD
               PERFORM Check-Player-Won
           EXPECT PLAYER-WON TO BE TRUE

           TESTCASE 'Test, switch-player'
               SET PLAYER-X TO TRUE
               MOVE 1 TO LAST-COL
               MOVE 1 TO LAST-ROW
               MOVE "X   X   X" TO SPIELFELD
               PERFORM Check-Player-Won
           EXPECT PLAYER-WON TO BE TRUE

           TESTCASE 'Test, switch-player'
               SET PLAYER-X TO TRUE
               MOVE 3 TO LAST-COL
               MOVE 1 TO LAST-ROW
               MOVE "  X X X  " TO SPIELFELD
               PERFORM Check-Player-Won
           EXPECT PLAYER-WON TO BE TRUE
