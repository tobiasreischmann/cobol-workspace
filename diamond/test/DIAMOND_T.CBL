           TESTSUITE 'Testfälle für unseren Diamanten'

           TESTCASE 'Testfall 1'
               MOVE 'E' to Char-Value
               MOVE 'B' to Curr-Char-Value
               PERFORM Calculate-Spaces-Before
           EXPECT NumberOfSpaces TO BE 03

           TESTCASE 'Testfall 2'
               MOVE 'F' to Char-Value
               MOVE 'A' to Curr-Char-Value
               PERFORM Calculate-Spaces-Before
           EXPECT NumberOfSpaces TO BE 05

           TESTCASE 'Testfall 3'
               MOVE 'F' to Char-Value
               MOVE 'B' to Curr-Char-Value
               PERFORM Calculate-Diamond-Size
               PERFORM Calculate-Spaces-Within
           EXPECT NumberOfSpaces TO BE 01

           TESTCASE 'Testfall 4'
               MOVE 'C' to Char-Value
               INITIALIZE Curr-Char-Value
               PERFORM Calculate-Diamond-Size
               MOVE 'C' to Curr-Char-Value
               PERFORM Calculate-Spaces-Within
           EXPECT NumberOfSpaces TO BE 03

           TESTCASE 'Testfall 5'
               MOVE 'C' to Char-Value
               MOVE 'B' to Curr-Char-Value
               PERFORM Calculate-Diamond-Size
               PERFORM Calculate-Spaces-Within
           EXPECT NumberOfSpaces TO BE 01
