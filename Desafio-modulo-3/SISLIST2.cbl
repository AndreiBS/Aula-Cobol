      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 25/10/2023
      * Purpose: MOSTRAR LISTRA DE MATERIAS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISLIST2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
               SELECT MATERIAS ASSIGN TO
               'C:\COBOL\Desafio-modulo-3\MATERIAS.DAT'
               ORGANISATION IS INDEXED
               ACCESS  MODE IS SEQUENTIAL
               RECORD  KEY  IS ID-MATERIA
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD MATERIAS.
          COPY FD_MATER.

       WORKING-STORAGE SECTION.
       01 WS-REGISTRO                      PIC X(50) VALUE SPACES.
       01 FILLER REDEFINES WS-REGISTRO.
          03 WS-ID-MATERIA                 PIC 9(03).
          03 WS-NM-MATERIA                 PIC X(20).
          03 WS-NT-APROVACAO               PIC 9(02)V99.
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
            DISPLAY '*** LISTAGEM DE MATERIAS ***'
            SET EXIT-OK           TO FALSE
            PERFORM P300-LISTAR THRU P300-FIM
            PERFORM P900-FIM
            .
       P300-LISTAR.
            SET EOF-OK            TO FALSE
            SET FS-OK             TO TRUE
            SET WS-CONT           TO 0.

            OPEN INPUT MATERIAS
            IF FS-OK THEN
               PERFORM UNTIL EOF-OK

                   READ MATERIAS   INTO WS-REGISTRO
                       AT END
                           SET EOF-OK TO TRUE
                       NOT AT END
                           ADD 1      TO WS-CONT
                           DISPLAY 'REGISTRO '
                                   WS-CONT
                                   ': '
                                   WS-ID-MATERIA
                                   ' - '
                                   WS-NM-MATERIA
                                   '  '
                                   WS-NT-APROVACAO
                   END-READ
               END-PERFORM
            ELSE
                   DISPLAY 'ERRO AO ABRIR O ARQUIVO DE MATERIAS.'
                   DISPLAY 'LIFE STATUS: ' WS-FS
            END-IF


            CLOSE MATERIAS
            .
       P300-FIM.
       P900-FIM.
            GOBACK.
       END PROGRAM SISLIST2.
