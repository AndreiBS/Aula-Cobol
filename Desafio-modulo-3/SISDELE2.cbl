      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 25/10/2023
      * Purpose: DELETAR MATERIAS CADASTRADAS NO PROG SISCADAS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISDELE2.
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
          88 EOF-OK                        VALUE 'S' 's' FALSE 'N'.
       77 WS-EXIT                          PIC X.
          88 EXIT-OK                       VALUE 'F' 'f' FALSE 'N'.
       77 WS-CONFIRM                       PIC X VALUE SPACES.

       LINKAGE SECTION.
       01 LK-COM-AREA.
          03 LK-MENSAGEM                   PIC X(50).
       PROCEDURE DIVISION USING LK-COM-AREA.

       MAIN-PROCEDURE.
            DISPLAY '*** EXCLUI MATERIAS ***'
            SET EXIT-OK           TO FALSE
            PERFORM P300-EXCLUIR THRU P300-FIM UNTIL EXIT-OK
            PERFORM P900-FIM
            .
       P300-EXCLUIR.
            SET EOF-OK            TO FALSE
            SET FS-OK             TO TRUE
            MOVE SPACES           TO WS-CONFIRM
            OPEN I-O MATERIAS

            IF FS-OK THEN
                DISPLAY 'Informe o numero de identificacao da materia: '
                ACCEPT ID-MATERIA

                READ MATERIAS INTO WS-REGISTRO
                   KEY IS ID-MATERIA
                   INVALID KEY
                       DISPLAY 'MATERIA NAO EXISTE! '
                   NOT INVALID KEY
                       DISPLAY 'MATERIA SELECIONADA: ' WS-NM-MATERIA
                       DISPLAY 'TECLE: '
                               '<S> para excluir ou <QUALQUER TECLA>'
                               ' para abortar.'
                       ACCEPT WS-CONFIRM
                       IF WS-CONFIRM EQUAL 'S' OR 's' THEN
                           DELETE MATERIAS RECORD
                           DISPLAY 'Materia excluida com sucesso!'
                       ELSE
                           DISPLAY 'Alteracao nao realizada.'
                       END-IF
                END-READ
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
       END PROGRAM SISDELE2.
