      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 15/10/2023
      * Purpose: CADASTRAR CONTATOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISCONTATOS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
               SELECT CONTATOS ASSIGN TO
                'C:\COBOL\CONTATOS.TXT'
               ORGANISATION IS SEQUENTIAL
               ACCESS  MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD CONTATOS.
          COPY FD_CONTT.

       WORKING-STORAGE SECTION.
       01 WS-REGISTRO                      PIC X(22) VALUE SPACES.
       01 FILLER REDEFINES WS-REGISTRO.
          03 WS-ID-CONTATO                 PIC 9(02).
          03 WS-NM-CONTATO                 PIC X(20).
       77 WS-FS                            PIC 99.
          88 FS-OK                         VALUE 0.
       77 WS-EOF                           PIC X.
          88 EOF-OK                        VALUE 'S' FALSE 'N'.
       77 WS-EXIT                          PIC X.
          88 EXIT-OK                       VALUE 'F' FALSE 'N'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY '*** CADASTRO DE CONTATOS ***'
            SET EXIT-OK           TO FALSE
            PERFORM P300-CADASTRA THRU P300-FIM UNTIL EXIT-OK
            PERFORM P900-FIM
            .
       P300-CADASTRA.
            SET EOF-OK            TO FALSE
            SET FS-OK             TO TRUE

            DISPLAY 'PARA REGISTRAR UM CONTATO, INFORME: '
            DISPLAY 'Um numero para Identificacao e tecle <ENTER>: '
            ACCEPT WS-ID-CONTATO
            DISPLAY 'Um nome para o Contato e tecle <ENTER>: '
            ACCEPT WS-NM-CONTATO

            OPEN EXTEND CONTATOS
            IF WS-FS EQUAL 35 THEN
                OPEN OUTPUT CONTATOS
            END-IF
            IF FS-OK THEN
                MOVE WS-ID-CONTATO         TO ID-CONTATO
                MOVE WS-NM-CONTATO         TO NM-CONTATO

                WRITE REG-CONTATOS
                DISPLAY 'Contato gravado com Sucesso! '
            ELSE
                DISPLAY 'ERRO AO ABRIR O ARQUIVO DE CONTATOS. '
                DISPLAY 'FILE STATUS: ' WS-FS
            END-IF
            CLOSE CONTATOS

            DISPLAY
               'TECLE: '
               '<QUALQUER TECLA> para continuar, ou <F> para finalizar.'
            ACCEPT WS-EXIT
            .
       P300-FIM.
       P900-FIM.
            GOBACK.
       END PROGRAM SISCONTATOS.
