      ******************************************************************
      * Author: ANDREI BATISTA
      * Date:06/10/2023
      * Purpose: AULA INSTRUÇÃO INITIALISE 
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INITIALIZE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-NOME.
          03 WS-PRIMEIRO-NOME      PIC X(10) VALUE 'ANDREI'.
          03 FILLER                PIC X(10) VALUE 'BATISTA'.
          03 FILLLER               PIC 9(03) VALUE 1.
       
       
       PROCEDURE DIVISION.
       
            
            DISPLAY 'SAIDA 1:'
            DISPLAY WS-NOME
            
            DISPLAY 'SAIDA 2:'
            INITIALIZE WS-NOME REPLACING ALPHANUMERIC BY SPACES
                                         NUMERIC      BY 3.
            DISPLAY WS-NOME
            
            DISPLAY 'SAIDA 3:'
            INITIALIZE WS-NOME REPLACING ALPHANUMERIC BY SPACES
                                         NUMERIC      BY ZEROS.
            DISPLAY WS-NOME
            .           
            
            STOP RUN.
       END PROGRAM INITIALIZE.