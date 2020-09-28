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
       01  NumberOfSpaces PIC 99.

       PROCEDURE DIVISION.
           DISPLAY "Enter capital letter:"
           ACCEPT Char-Value
           PERFORM Calculate-Diamond-Size
           COMPUTE Counter = DiamondSize - 1

      * Print the first 'A' Row
           PERFORM PRINT-HEAD-COLUMN

      * Print all rows until the middle one    
           PERFORM Counter TIMES
               Add 1 to Curr-Numeric-Value
               PERFORM PRINT-MIDDLE-COLUMN
           END-PERFORM 

      * Print the middle row
           Add 1 to Curr-Numeric-Value
           PERFORM PRINT-MIDDLE-COLUMN
           
      * Print all rows until the final row
           PERFORM Counter TIMES
               Add -1 to Curr-Numeric-Value
               PERFORM PRINT-MIDDLE-COLUMN
           END-PERFORM 
           
      * Print the final 'A' row
           Add -1 to Curr-Numeric-Value
           PERFORM PRINT-HEAD-COLUMN
           GOBACK.

       PRINT-HEAD-COLUMN Section.
           PERFORM Calculate-Spaces-Before

           Perform Print-Spaces

           DISPLAY "A" WITH NO ADVANCING

           Perform Print-Spaces
           DISPLAY " "
       Exit.

       PRINT-MIDDLE-COLUMN Section.
           PERFORM Calculate-Spaces-Before
           Perform Print-Spaces

           DISPLAY Curr-Char-Value WITH NO ADVANCING

           PERFORM Calculate-Spaces-Within
           Perform Print-Spaces

           DISPLAY Curr-Char-Value WITH NO ADVANCING

           PERFORM Calculate-Spaces-Before
           Perform Print-Spaces
           DISPLAY " "
       Exit.

       Calculate-Diamond-Size SECTION.
      * Calculating the Dimanond Size (A=65).
           COMPUTE DiamondSize = Numeric-Value - 65.
       EXIT.

       Calculate-Spaces-Within SECTION.
           PERFORM Calculate-Spaces-Before
           COMPUTE NumberOfSpaces = 
             (2 * (DiamondSize - (NumberOfSpaces + 1))) + 1
           .
       EXIT.

       Calculate-Spaces-Before SECTION.
           COMPUTE NumberOfSpaces = Numeric-Value - Curr-Numeric-Value
           .
       EXIT.

       Print-Spaces SECTION.
           PERFORM NumberOfSpaces TIMES
               DISPLAY " " WITH NO ADVANCING
           END-PERFORM
       EXIT.

       END PROGRAM DIAMOND.
