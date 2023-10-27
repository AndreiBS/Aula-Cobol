      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 25/10/2023
      * Purpose: CADASTRO DE MATERIAS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISCADA2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
               SELECT MATERIAS ASSIGN TO
                'C:\COBOL\Desafio-modulo-3\MATERIAS.DAT'
               ORGANISATION IS INDEXED
               ACCESS  MODE IS RANDOM
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
          88 EXIT-OK                       VALUE 'F' 'f' FALSE 'N'.

       LINKAGE SECTION.
       01 LK-COM-AREA.
          03 LK-MENSAGEM                   PIC X(50).

       PROCEDURE DIVISION USING LK-COM-AREA.
       MAIN-PROCEDURE.
            DISPLAY '*** CADASTRO DE MATERIAS ***'
            SET EXIT-OK           TO FALSE
            PERFORM P300-CADASTRA THRU P300-FIM UNTIL EXIT-OK
            PERFORM P900-FIM
            .
       P300-CADASTRA.
            SET EOF-OK            TO FALSE
            SET FS-OK             TO TRUE
            DISPLAY 'PARA REGISTRAR UMA MATERIA, INFORME: '
            DISPLAY 'Um numero para Identificacao e tecle <ENTER>: '
            ACCEPT WS-ID-MATERIA
            DISPLAY 'Um nome para a Materia e tecle <ENTER>: '
            ACCEPT WS-NM-MATERIA
            DISPLAY 'Uma nota de Aprovacao e tecle <ENTER>: '
            ACCEPT WS-NT-APROVACAO

            OPEN I-O MATERIAS
            IF WS-FS EQUAL 35 THEN
                OPEN OUTPUT MATERIAS
            END-IF
            IF FS-OK THEN
                MOVE WS-ID-MATERIA         TO ID-MATERIA
                MOVE WS-NM-MATERIA         TO NM-MATERIA
                MOVE WS-NT-APROVACAO       TO NT-APROVACAO

                WRITE REG-MATERIA
                       INVALID KEY
                           DISPLAY 'MATERIA JA CADASTRADO! '
                       NOT INVALID KEY
                           DISPLAY 'Materia gravada com Sucesso! '
                END-WRITE
            ELSE
                DISPLAY 'ERRO AO ABRIR O ARQUIVO DE MATERIAS. '
                DISPLAY 'FILE STATUS: ' WS-FS
            END-IF
            CLOSE MATERIAS

            DISPLAY
               'TECLE: '
               '<QUALQUER TECLA> para continuar, ou <F> para finalizar.'
            ACCEPT WS-EXIT
            .
       P300-FIM.
       P900-FIM.
            GOBACK.
       END PROGRAM SISCADA2.
