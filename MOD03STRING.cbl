      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 13/10/2023
      * Purpose: Aula modulo 3 string
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOD03STRING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      ******** VARIAVEIS PARA FAZER O REMENCIONAMENTO*******************
       77 WS-TM-1                  PIC 99.
       77 WS-TM-2                  PIC 99.
       COPY 'LAYOUT001'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            MOVE 'ANDREI'           TO WS-PRIMEIRO-NOME
            MOVE 'BATISTA'          TO WS-SEGUNDO-NOME
            MOVE '5511991485110'    TO WS-TELEFONE
            MOVE 'RUA DEZ, 03'      TO WS-RUA
            MOVE 'SILVEIRA'         TO WS-BAIRRO
            MOVE 'BARUERI'          TO WS-CIDADE
            MOVE 'SP'               TO WS-UF
            MOVE '6433260'          TO WS-CEP
            MOVE 'BRASILEIRO'       TO WS-NACIONALIDADE
            MOVE 'AUTONOMO'         TO WS-PROFISSAO

            MOVE ZEROS              TO WS-TM-1
            INSPECT FUNCTION REVERSE(WS-PRIMEIRO-NOME)
                    TALLYING WS-TM-1 FOR LEADING ' '


            DISPLAY '1 - NOME COMPLETO: ' WS-PRIMEIRO-NOME
               (1:(FUNCTION LENGTH(WS-PRIMEIRO-NOME) - WS-TM-1))
                                          ' '
                                          WS-SEGUNDO-NOME
            DISPLAY '2 - TELEFONE.....: ' '+'  WS-PAIS
                                          ' (' WS-DDD
                                          ') ' WS-PREFIXO
                                          '-'  WS-SUFIXO

            MOVE ZEROS              TO WS-TM-1
            INSPECT FUNCTION REVERSE(WS-RUA)
                    TALLYING WS-TM-1 FOR LEADING ' '

            MOVE ZEROS              TO WS-TM-2
            INSPECT FUNCTION REVERSE(WS-CIDADE)
                    TALLYING WS-TM-2 FOR LEADING ' '
            DISPLAY '3 - ENDERECO.....: ' WS-RUA
            (1:(FUNCTION LENGTH(WS-RUA) - WS-TM-1)) ' '
                                          WS-BAIRRO
                                          WS-CIDADE
            (1:(FUNCTION LENGTH(WS-CIDADE) - WS-TM-1)) ' '
                                          WS-UF
                FUNCTION CONCATENATE(' - CEP: '
                                     WS-CEP-1
                                     '-'
                                     WS-CEP-2
                                      )
            DISPLAY '4 - NACIONALIDADE: ' WS-NACIONALIDADE
            DISPLAY '5 - PROFISSAO....: ' WS-PROFISSAO


            GOBACK.
       END PROGRAM MOD03STRING.
