      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 05/10/2023
      * Porpose: Aula do comando STRING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. string.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-CONTEUDO                  PIC X(30) VALUE SPACES.
       77 WS-TEXTO                     PIC X(40) VALUE SPACES.
       77 WS-PONTEIRO                  PIC 9(02) VALUE ZEROS.
       PROCEDURE DIVISION.

      *********************** FORMA 1 **********************************
            INITIALIZE WS-CONTEUDO
                       WS-TEXTO

            STRING 
               'ANDREI'
               ' '
               'BATISTA'
               DELIMITED BY SIZE INTO WS-CONTEUDO
            END-STRING 

            DISPLAY WS-CONTEUDO

      *********************** FORMA 2 **********************************
            INITIALIZE WS-CONTEUDO
                       WS-TEXTO
                  
            MOVE 'O ANDREI BATISTA ESTA NA AULA DE STRING' TO WS-TEXTO        
            STRING 
               WS-TEXTO(1/8)
               WS-TEXTO(18/21)
               DELIMITED BY SIZE INTO WS-CONTEUDO
            END-STRING

            DISPLAY WS-CONTEUDO


      *********************** FORMA 3 **********************************
            INITIALIZE WS-CONTEUDO
                       WS-TEXTO
                  
            MOVE 'O ANDREI BATISTA ESTA NA AULA DE STRING' TO WS-TEXTO        
            STRING 
               WS-TEXTO(1/7)
               ' '
               WS-TEXTO(18/39)

               DELIMITED BY SIZE INTO WS-CONTEUDO
            END-STRING

            DISPLAY WS-CONTEUDO

      *********************** FORMA 4  **********************************
            INITIALIZE WS-CONTEUDO
                       WS-TEXTO
                  
            MOVE 'O ANDREI BATISTA;ESTA NA AULA DE STRING' TO WS-TEXTO        
            STRING 
               WS-TEXTO
               DELIMITED BY ';' INTO WS-CONTEUDO
            END-STRING

            DISPLAY WS-CONTEUDO


      *********************** FORMA 5 **********************************
            INITIALIZE WS-CONTEUDO
                       WS-TEXTO
                    
            MOVE 'O                ESTA NA AULA DE STRING' TO WS-TEXTO  
            SET WS-PONTEIRO                                TO 3      
            STRING 
               'ANDREI BATISTA'
               DELIMITED BY SIZE INTO WS-TEXTO
               WITH POINTER WS-PONTEIRO
            END-STRING

            DISPLAY WS-CONTEUDO

            STOP RUN.

       END PROGRAM string.

      
