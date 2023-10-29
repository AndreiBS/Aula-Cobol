      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                             DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------
       DATA DIVISION.
      *----------------------------------------------------------------
       FILE SECTION.


      *----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------

      * VARIAVEIS DO EXTENSO

       01  TABELA.
           05  FILLER        PIC X(016)   VALUE "HUM           04".
           05  FILLER        PIC X(016)   VALUE "DOIS          05".
           05  FILLER        PIC X(016)   VALUE "TRES          05".
           05  FILLER        PIC X(016)   VALUE "QUATRO        07".
           05  FILLER        PIC X(016)   VALUE "CINCO         06".
           05  FILLER        PIC X(016)   VALUE "SEIS          05".
           05  FILLER        PIC X(016)   VALUE "SETE          05".
           05  FILLER        PIC X(016)   VALUE "OITO          05".
           05  FILLER        PIC X(016)   VALUE "NOVE          05".
           05  FILLER        PIC X(016)   VALUE "DEZ           04".
           05  FILLER        PIC X(016)   VALUE "ONZE          05".
           05  FILLER        PIC X(016)   VALUE "DOZE          05".
           05  FILLER        PIC X(016)   VALUE "TREZE         06".
           05  FILLER        PIC X(016)   VALUE "QUATORZE      09".
           05  FILLER        PIC X(016)   VALUE "QUINZE        07".
           05  FILLER        PIC X(016)   VALUE "DEZESSEIS     10".
           05  FILLER        PIC X(016)   VALUE "DEZESSETE     10".
           05  FILLER        PIC X(016)   VALUE "DEZOITO       08".
           05  FILLER        PIC X(016)   VALUE "DEZENOVE      09".
           05  FILLER        PIC X(016)   VALUE "VINTE         06".
           05  FILLER        PIC X(016)   VALUE "TRINTA        07".
           05  FILLER        PIC X(016)   VALUE "QUARENTA      09".
           05  FILLER        PIC X(016)   VALUE "CINQUENTA     10".
           05  FILLER        PIC X(016)   VALUE "SESSENTA      09".
           05  FILLER        PIC X(016)   VALUE "SETENTA       08".
           05  FILLER        PIC X(016)   VALUE "OITENTA       08".
           05  FILLER        PIC X(016)   VALUE "NOVENTA       08".
           05  FILLER        PIC X(016)   VALUE "CENTO         06".
           05  FILLER        PIC X(016)   VALUE "DUZENTOS      09".
           05  FILLER        PIC X(016)   VALUE "TREZENTOS     10".
           05  FILLER        PIC X(016)   VALUE "QUATROCENTOS  13".
           05  FILLER        PIC X(016)   VALUE "QUINHENTOS    11".
           05  FILLER        PIC X(016)   VALUE "SEISCENTOS    11".
           05  FILLER        PIC X(016)   VALUE "SETECENTOS    11".
           05  FILLER        PIC X(016)   VALUE "OITOCENTOS    11".
           05  FILLER        PIC X(016)   VALUE "NOVECENTOS    11".
           05  FILLER        PIC X(016)   VALUE "CEM           04".
           05  FILLER        PIC X(016)   VALUE "MIL           04".
           05  FILLER        PIC X(016)   VALUE "MILHAO        07".
           05  FILLER        PIC X(016)   VALUE "MILHOES       08".
           05  FILLER        PIC X(016)   VALUE "REAL          05".
           05  FILLER        PIC X(016)   VALUE "REAIS         06".
           05  FILLER        PIC X(016)   VALUE "CENTAVO       08".
           05  FILLER        PIC X(016)   VALUE "CENTAVOS      09".
       01  TB                REDEFINES    TABELA.
           05  NOMES         OCCURS 44    TIMES.
               10  NOME      PIC X(014).
               10  QTDL      PIC 9(002).

       01  CONTADORES.
           05  INDI          PIC 9(002)   VALUE ZEROES.
           05  FLG           PIC 9(001)   VALUE ZEROES.
           05  AUX           PIC 9(003)   VALUE ZEROES.
           05  I             PIC 9(001)   VALUE ZEROES.
           05  J             PIC 9(002)   VALUE ZEROES.
           05  K             PIC 9(003)   VALUE ZEROES.
           05  X             PIC 9(002)   VALUE ZEROES.
           05  WS-CONTRO1    PIC 9(002)   VALUE ZEROES.
           05  WS-CONTRO2    PIC 9(002)   VALUE ZEROES.

       01  NUMERO.
           05  NUM           PIC 9(12)V99 VALUE ZEROES.

       01  NUM1              REDEFINES    NUMERO.
           05  N             PIC 9(003)   OCCURS 3 TIMES.
           05  CENT          PIC 9(002).

       01  CAMPO.
           05  CMP1          PIC 9(001)   VALUE ZEROES.
           05  CMP2          PIC 9(002)   VALUE ZEROES.
           05  CMPOS         REDEFINES    CMP2.
               10  CMP3      PIC 9(001).
               10  CMP4      PIC 9(001).

       01  PALAVRA.
           05  PAL           PIC X(015)   VALUE SPACES.

       01  LETRAS            REDEFINES    PALAVRA.
           05  LET           PIC X(001)   OCCURS 15 TIMES.

       01  EXTENSO.
           03  T-NUM         PIC 9(12)V99 VALUE ZEROES.
           03  ASTERIX.
               05 FILLER     PIC X(001)   VALUE SPACES.
               05 ASTERES    PIC X(199)   VALUE SPACES.
           03  EXT           REDEFINES    ASTERIX
                             PIC X(001)   OCCURS 200
                                          INDEXED BY CONTA.

      *----------------------------------------------------------------
       LINKAGE SECTION.
      *----------------------------------------------------------------

       01  WL-PARAMETROS-EXT.
           03  FILLER        PIC S9(04) COMP.
           03  WS-VALOR-EXT  PIC 9(16)V99.
           03  WS-EXTENSO    PIC X(200).

      *----------------------------------------------------------------
       PROCEDURE DIVISION USING WL-PARAMETROS-EXT.
      *----------------------------------------------------------------

           PERFORM P0000-INICIAL
           PERFORM P1000-PRINCIPAL
           PERFORM P9000-FINAL.

      *----------------------------------------------------------------
       P0000-INICIAL.
      *----------------------------------------------------------------

           MOVE WS-VALOR-EXT TO T-NUM
           MOVE SPACES TO WS-EXTENSO.

      *----------------------------------------------------------------
       P1000-PRINCIPAL.
      *----------------------------------------------------------------

           MOVE T-NUM   TO NUM.
           COMPUTE NUM = NUM * 1000
           MOVE " "     TO ASTERIX
           MOVE ASTERIX TO ASTERES
           MOVE ZEROES  TO CONTADORES

           PERFORM 3 TIMES
                   ADD 1 TO J
                   IF  N (J) NOT EQUAL ZEROES
                       ADD J TO FLG
                       MOVE N (J) TO AUX
                   END-IF
           END-PERFORM

           PERFORM P1100-EXTENSO THRU P1100-FIM UNTIL I EQUAL 4

           PERFORM VARYING CONTA FROM 1 BY 1
                   UNTIL CONTA GREATER THAN 124
                   IF EXT (CONTA)     EQUAL SPACES AND
                      EXT (CONTA + 1) EQUAL SPACES
                      MOVE "*" TO EXT (CONTA)
                   END-IF
           END-PERFORM.

      *----------------------------------------------------------------
       P1100-EXTENSO.
      *----------------------------------------------------------------

           MOVE ZEROES TO WS-CONTRO1

           ADD 1 TO I
           IF  I EQUAL 4
               PERFORM P1500-CENTAVO
               MOVE 1 TO WS-CONTRO1
           END-IF.

           IF WS-CONTRO1 EQUAL ZEROES
              IF  N (I) GREATER THAN 0
                  MOVE N (I) TO CAMPO
                  PERFORM P1200-ESCREVE THRU P1200-FIM
              ELSE
                  MOVE 1 TO WS-CONTRO1
              END-IF
           END-IF.

           IF WS-CONTRO1 EQUAL ZEROES
              IF  I EQUAL 1
                  PERFORM P1300-MILHAO
              END-IF
           END-IF

           IF WS-CONTRO1 EQUAL ZEROES
              IF  I EQUAL 2
                  PERFORM P1400-MILHAR
              END-IF
           END-IF.

       P1100-FIM.
           EXIT.

      *----------------------------------------------------------------
       P1200-ESCREVE.
      *----------------------------------------------------------------

           MOVE ZEROES TO WS-CONTRO2

           IF  CAMPO EQUAL 100
               MOVE 37 TO INDI
               PERFORM P2200-MONTA
               MOVE 1 TO WS-CONTRO2
           END-IF.

           IF  WS-CONTRO2 EQUAL ZEROES
               IF  CMP1 NOT EQUAL ZEROES
                   ADD 27 CMP1 GIVING INDI
                   PERFORM P2200-MONTA
                   PERFORM P1800-TEST2
               END-IF
           END-IF

           IF  WS-CONTRO2 EQUAL ZEROES
               IF  CMP2 EQUAL ZEROES
                   MOVE 1 TO WS-CONTRO2
               END-IF
           END-IF

           IF  WS-CONTRO2 EQUAL ZEROES
               IF  CMP2 LESS THAN 21
                   MOVE CMP2 TO INDI
                   PERFORM P2200-MONTA
                   MOVE 1 TO WS-CONTRO2
               ELSE
                   ADD 18 CMP3 GIVING INDI
                   PERFORM P2200-MONTA
                   PERFORM P1900-TEST3
               END-IF
           END-IF

           IF  WS-CONTRO2 EQUAL ZEROES
               IF  CMP4 NOT EQUAL ZEROES
                   MOVE CMP4 TO INDI
                   PERFORM P2200-MONTA
               END-IF
           END-IF.

       P1200-FIM.
           EXIT.

      *----------------------------------------------------------------
       P1300-MILHAO.
      *----------------------------------------------------------------

           IF  N (1) EQUAL 1
               MOVE 39 TO INDI
           ELSE
               MOVE 40 TO INDI
           END-IF

           PERFORM P2200-MONTA.

           EVALUATE FLG
               WHEN 1
                    MOVE "DE" TO PALAVRA
                    PERFORM P2300-WRITER 3 TIMES
               WHEN 3
                    PERFORM P2100-MOVE-E
               WHEN 4
                    PERFORM P2000-TEST4
               WHEN 6
                    SUBTRACT 1 FROM K
                    MOVE "," TO PALAVRA
                    PERFORM P2300-WRITER 2 TIMES
           END-EVALUATE.

      *----------------------------------------------------------------
       P1400-MILHAR.
      *----------------------------------------------------------------

           MOVE 38 TO INDI
           PERFORM P2200-MONTA

           IF  FLG GREATER THAN 4
               PERFORM P2000-TEST4
           END-IF.

      *----------------------------------------------------------------
       P1500-CENTAVO.
      *----------------------------------------------------------------

           IF  FLG NOT EQUAL ZEROES
               PERFORM P1600-CRUZEIRO
           END-IF

           IF CENT NOT EQUAL ZEROES
               MOVE 0 TO CMP1
               MOVE CENT TO CMP2
               PERFORM P1200-ESCREVE THRU P1200-FIM
               MOVE 43 TO INDI
               PERFORM P2200-MONTA
           END-IF

           IF  CENT GREATER THAN 1
               PERFORM P1700-PLURAL
           END-IF.

      *----------------------------------------------------------------
       P1600-CRUZEIRO.
      *----------------------------------------------------------------

           IF  AUX GREATER THAN 1
               MOVE 42 TO INDI
           ELSE
               MOVE 41 TO INDI
           END-IF

           PERFORM P2200-MONTA.
           IF  CENT NOT EQUAL ZEROES
               PERFORM P2100-MOVE-E
           END-IF.

      *----------------------------------------------------------------
       P1700-PLURAL.
      *----------------------------------------------------------------

           MOVE "S" TO EXT (K)
           ADD 1    TO K
           MOVE " " TO EXT (K).

      *----------------------------------------------------------------
       P1800-TEST2.
      *----------------------------------------------------------------

           IF  CMP2 NOT EQUAL ZEROES
               PERFORM P2100-MOVE-E
           END-IF.

      *----------------------------------------------------------------
       P1900-TEST3.
      *----------------------------------------------------------------

           IF  CMP4 NOT EQUAL ZEROES
               PERFORM P2100-MOVE-E
           END-IF.

      *----------------------------------------------------------------
       P2000-TEST4.
      *----------------------------------------------------------------

           IF  CENT NOT EQUAL ZEROES
               SUBTRACT 1 FROM K
               MOVE "," TO PALAVRA
               PERFORM P2300-WRITER 2 TIMES
           ELSE
               PERFORM P2100-MOVE-E
           END-IF.

      *----------------------------------------------------------------
       P2100-MOVE-E.
      *----------------------------------------------------------------

           MOVE "E" TO PALAVRA
           PERFORM P2300-WRITER 2 TIMES.

      *----------------------------------------------------------------
       P2200-MONTA.
      *----------------------------------------------------------------

           MOVE 0           TO J
           MOVE NOME (INDI) TO PALAVRA
           MOVE QTDL (INDI) TO X
           PERFORM P2300-WRITER X TIMES
           MOVE 0           TO J.

      *----------------------------------------------------------------
       P2300-WRITER.
      *----------------------------------------------------------------

           ADD 1        TO J
           ADD 1        TO K
           MOVE LET (J) TO EXT (K).

      *----------------------------------------------------------------
       P9000-FINAL.
      *----------------------------------------------------------------

           MOVE FUNCTION LOWER-CASE (ASTERES) TO ASTERES
           MOVE ASTERIX TO WS-EXTENSO.
           GOBACK.
