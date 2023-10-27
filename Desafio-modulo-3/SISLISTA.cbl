      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 24/10/2023
      * Purpose: VER ALUNOS CADASTRADOS NO PROG SISCADAS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISLISTA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
               SELECT ALUNOS ASSIGN TO
               'C:\COBOL\Desafio-modulo-3\REGISTRO.DAT'
               ORGANISATION IS INDEXED
               ACCESS  MODE IS SEQUENTIAL
               RECORD  KEY  IS ID-ALUNO
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD ALUNOS.
          COPY FD_REGIS.

       WORKING-STORAGE SECTION.
       01 WS-REGISTRO                      PIC X(50) VALUE SPACES.
       01 FILLER REDEFINES WS-REGISTRO.
          03 WS-ID-ALUNO                   PIC 9(03).
          03 WS-NM-ALUNO                   PIC X(20).
          03 WS-TL-ALUNO                   PIC X(15).
       77 WS-FS                            PIC 99.
          88 FS-OK                         VALUE 0.
       77 WS-EOF                           PIC X.
          88 EOF-OK                        VALUE 'S' FALSE 'N'.
       77 WS-EXIT                          PIC X.
          88 EXIT-OK                       VALUE 'F''f' FALSE 'N'.
       77 WS-CONT                          PIC 9(003) VALUE ZEROS.

       LINKAGE SECTION.
       01 LK-COM-AREA.
          03 LK-MENSAGEM                   PIC X(50).

       PROCEDURE DIVISION USING LK-COM-AREA.
       MAIN-PROCEDURE.
            DISPLAY '*** LISTAGEM DE ALUNOS ***'
            SET EXIT-OK           TO FALSE
            PERFORM P300-LISTAR THRU P300-FIM
            PERFORM P900-FIM
            .
       P300-LISTAR.
            SET EOF-OK            TO FALSE
            SET FS-OK             TO TRUE
            SET WS-CONT           TO 0.

            OPEN INPUT ALUNOS
            IF FS-OK THEN
               PERFORM UNTIL EOF-OK

                   READ ALUNOS   INTO WS-REGISTRO
                       AT END
                           SET EOF-OK TO TRUE
                       NOT AT END
                           ADD 1      TO WS-CONT
                           DISPLAY 'REGISTRO '
                                   WS-CONT
                                   ': '
                                   WS-ID-ALUNO
                                   ' - '
                                   WS-NM-ALUNO
                                   '  '
                                   WS-TL-ALUNO
                   END-READ
               END-PERFORM
            ELSE
                   DISPLAY 'ERRO AO ABRIR O ARQUIVO DE ALUNOS.'
                   DISPLAY 'LIFE STATUS: ' WS-FS
            END-IF


            CLOSE ALUNOS
            .
       P300-FIM.
       P900-FIM.
            GOBACK.
       END PROGRAM SISLISTA.
