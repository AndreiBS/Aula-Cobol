      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 28/10/2023
      * Purpose: USANDO ADD CORR
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDCORR.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-REG-1.
          03 WS-CODIGO            PIC 9(02).
          03 WS-NOME              PIC X(15).
          03 WS-TEL               PIC X(09).
       01 WS-REG-2.
          03 WS-CODIGO            PIC 9(02).
          03 WS-NOME              PIC X(15).
          03 WS-TEL               PIC X(09).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            MOVE '01CARLOS GOMES   9514-1234'  TO   WS-REG-1
            MOVE 02                            TO WS-CODIGO OF WS-REG-2

            DISPLAY WS-REG-1
            DISPLAY WS-REG-2
            ADD CORR WS-REG-1                 TO WS-REG-2
      *     MOVE CORR WS-REG-1                 TO WS-REG-2

      *     ADD WS-CODIGO OF WS-REG-1        TO  WS-CODIGO OF WS-REG-2
      *     MOVE WS-NOME  OF WS-REG-1        TO  WS-NOME   OF WS-REG-2
      *     MOVE WS-TEL   OF WS-REG-1        TO  WS-TEL    OF WS-REG-2

            DISPLAY WS-REG-1
            DISPLAY WS-REG-2
            .
            STOP RUN.
       END PROGRAM ADDCORR.
