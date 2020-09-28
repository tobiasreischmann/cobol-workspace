       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIAMOND.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Char-Value.
           05 Numeric-Value USAGE BINARY-CHAR.
       01  Curr-Char-Value VALUE 'A'.
           05 Curr-Numeric-Value USAGE BINARY-CHAR.
       01  Counter PIC 99.
       01  DiamondSize PIC 99.
       01  TempSize PIC 99.

       PROCEDURE DIVISION.
           ACCEPT Char-Value
           COMPUTE DiamondSize = Numeric-Value - Curr-Numeric-Value.
           
           PERFORM PRINT-HEAD-COLUMN
           PERFORM DiamondSize TIMES
               Add 1 to Curr-Numeric-Value
               PERFORM PRINT-MIDDLE-COLUMN
           END-PERFORM 
           
           COMPUTE TempSize = DiamondSize - 1
           PERFORM TempSize TIMES
               Add -1 to Curr-Numeric-Value
               PERFORM PRINT-MIDDLE-COLUMN
           END-PERFORM 

           Add -1 to Curr-Numeric-Value
           PERFORM PRINT-HEAD-COLUMN
           GOBACK.

       PRINT-HEAD-COLUMN Section.
           PERFORM VARYING Counter FROM 0 by 1
               UNTIL Counter = Numeric-Value - Curr-Numeric-Value
               DISPLAY " " WITH NO ADVANCING
           END-PERFORM
               DISPLAY "A" WITH NO ADVANCING
           PERFORM VARYING Counter FROM 0 by 1
               UNTIL Counter = Numeric-Value - Curr-Numeric-Value
               DISPLAY " " WITH NO ADVANCING
           END-PERFORM
           DISPLAY " "
       Exit.

       PRINT-MIDDLE-COLUMN Section.
           PERFORM VARYING Counter FROM 0 by 1
               UNTIL Counter = Numeric-Value - Curr-Numeric-Value
               DISPLAY " " WITH NO ADVANCING
           END-PERFORM
               DISPLAY Curr-Char-Value WITH NO ADVANCING
           PERFORM VARYING Counter FROM 0 by 1
               UNTIL Counter = (2 * (DiamondSize - 
                   (Numeric-Value - Curr-Numeric-Value + 1))) + 1
               DISPLAY " " WITH NO ADVANCING
           END-PERFORM               
               DISPLAY Curr-Char-Value WITH NO ADVANCING
           PERFORM VARYING Counter FROM 0 by 1
               UNTIL Counter = Numeric-Value - Curr-Numeric-Value
               DISPLAY " " WITH NO ADVANCING
           END-PERFORM
           DISPLAY " "
       Exit.

       END PROGRAM DIAMOND.
