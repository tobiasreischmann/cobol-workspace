           TESTSUITE 'Beispiel Testsuite'

           BEFORE-EACH
               INITIALIZE FIELD
               INITIALIZE PREV-FIELD
           END-BEFORE

           TESTCASE 'Calculate sourrounding'
               SET PREV-ALIVE(20,10) TO TRUE
               SET PREV-ALIVE(21,10) TO TRUE
               SET PREV-ALIVE(21,11) TO TRUE
               SET PREV-ALIVE(20,11) TO TRUE
               MOVE 20 to ROW-INDEX
               MOVE 10 to COL-INDEX
               PERFORM CalculateSourroundingAlive
           EXPECT SOURROUNDING-ALIVE TO BE 3

           TESTCASE 'Calculate sourrounding with existing dead'
               SET PREV-ALIVE(21,9) TO TRUE
               SET PREV-ALIVE(21,10) TO TRUE
               SET PREV-ALIVE(21,11) TO TRUE
               MOVE 20 to ROW-INDEX
               MOVE 10 to COL-INDEX
               PERFORM CalculateSourroundingAlive
           EXPECT SOURROUNDING-ALIVE TO BE 3

           TESTCASE 'Calculate sourrounding 8'
               SET PREV-ALIVE(21,9) TO TRUE
               SET PREV-ALIVE(21,10) TO TRUE
               SET PREV-ALIVE(21,11) TO TRUE
               SET PREV-ALIVE(20,9) TO TRUE
               SET PREV-ALIVE(20,11) TO TRUE
               SET PREV-ALIVE(19,9) TO TRUE
               SET PREV-ALIVE(19,10) TO TRUE
               SET PREV-ALIVE(19,11) TO TRUE
               MOVE 20 to ROW-INDEX
               MOVE 10 to COL-INDEX
               PERFORM CalculateSourroundingAlive
           EXPECT SOURROUNDING-DEAD TO BE 8

           TESTCASE 'Lonely die'
               SET ALIVE(20,10) TO TRUE
               SET ALIVE(21,10) TO TRUE
               PERFORM Alter-State
           EXPECT DEAD(20,10) TO BE TRUE

           TESTCASE 'Crowded die'
               SET ALIVE(20,10) TO TRUE
               SET ALIVE(21,9) TO TRUE
               SET ALIVE(21,10) TO TRUE
               SET ALIVE(21,11) TO TRUE
               SET ALIVE(20,9) TO TRUE
               SET ALIVE(20,11) TO TRUE
               PERFORM Alter-State
           EXPECT DEAD(20,10) TO BE TRUE

           TESTCASE 'Survive'
               SET ALIVE(20,10) TO TRUE
               SET ALIVE(21,11) TO TRUE
               SET ALIVE(20,9) TO TRUE
               SET ALIVE(20,11) TO TRUE
               PERFORM Alter-State
           EXPECT ALIVE(20,10) TO BE TRUE

           TESTCASE 'Survive2'
               SET ALIVE(20,14) TO TRUE
               SET ALIVE(20,15) TO TRUE
               SET ALIVE(20,16) TO TRUE
               PERFORM Alter-State
           EXPECT ALIVE(20,15) TO BE TRUE

           TESTCASE 'Resurrect'
               SET ALIVE(21,11) TO TRUE
               SET ALIVE(20,9) TO TRUE
               SET ALIVE(20,11) TO TRUE
               PERFORM Alter-State
           EXPECT ALIVE(20,10) TO BE TRUE

           TESTCASE 'Stay dead'
               SET ALIVE(20,9) TO TRUE
               SET ALIVE(20,11) TO TRUE
               PERFORM Alter-State
           EXPECT DEAD(20,10) TO BE TRUE