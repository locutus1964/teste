****  RESUMO - IMPRIME RESUMO DO REGISTRO DE SAIDAS ****
****  09/02/93  ****

MENS = 'Impressora preparada para imprimir o Resumo ?'
DO PEGRESP WITH [SN]
  IF MC = 'N'
    RETURN
  ENDIF
    
   MData = space(5)
   IMPRIME = 1
   MPAG  = 1
   ? MENSAG('Informe Per¡odo do Resumo de Registro de Saidas a ser Impresso')
   ? janela(16,45,20,77)
   ? borda(16,45,20,77)
   @ 18,48 say 'Informe o mes e ano ' get MDATA pict '99/99'
   read
   
   IF LASTKEY() = 27
      CLOSE DATA
      RETURN
   ENDIF

   ? MENSAG('Preparando/imprimindo o Resumo... Aguarde')

   DO CASE
     CASE SUBS(MDATA,1,2) = '01'
         NOME = 'JANEIRO'
     CASE SUBS(MDATA,1,2) = '02'
         NOME = 'FEVEREIRO'
     CASE SUBS(MDATA,1,2) = '03'
         NOME = 'MARCO'
     CASE SUBS(MDATA,1,2) = '04'
         NOME = 'ABRIL'
     CASE SUBS(MDATA,1,2) = '05'
         NOME = 'MAIO'
     CASE SUBS(MDATA,1,2) = '06'
         NOME = 'JUNHO'
     CASE SUBS(MDATA,1,2) = '07'
         NOME = 'JULHO'
     CASE SUBS(MDATA,1,2) = '08'
         NOME = 'AGOSTO'
     CASE SUBS(MDATA,1,2) = '09'
         NOME = 'SETEMBRO'
     CASE SUBS(MDATA,1,2) = '10'
         NOME = 'OUTUBRO'
     CASE SUBS(MDATA,1,2) = '11'
         NOME = 'NOVEMBRO'
     CASE SUBS(MDATA,1,2) = '12'
         NOME = 'DEZEMBRO'
   ENDCASE

   USE CADSAIDA
   ZAP
   APPEND FROM CADNF FOR VAL(SUBS(DEMI,7,2)+SUBS(DEMI,4,2)) = ;
          VAL(SUBS(MDATA,4,2) + SUBS(MDATA,1,2))
   USE CADSAIDA
   INDEX ON STR(NNF,6) TO CADSAIDA.IDX
   USE CADSAIDA INDEX CADSAIDA.IDX
   TOTAL ON NNF TO CADSAID2 FIELDS IPI,ICM,VAL

   CLOSE DATA

   SELE B
   USE TPNF INDEX TPNF.IDX
   SELE C
   USE CLIEN ORDER P_CGC
   SELE D
   USE ESTADOS INDEX ESTADOS.IDX
   SELE A
   USE CADSAID2

   STOR 0 TO VALT, ICMDEB, ICMTRIB, TOTICM, IPIDEB, IPITRIB, TOTIPI 
   STOR 0 TO MTIP, MCGC, MNOPE, MEST, MALIQ 
   MICMT  = SPACE(01)
   MIPIT  = SPACE(01)

   DO WHILE !EOF()
      MTIP = TIP
      MCGC = CGC

      SELE B
      SEEK MTIP
      IF !EOF() .OR. !BOF()
         MICMT = ICMT
         MIPIT = IPIT
      ENDIF

      SELE C
      SEEK MCGC
      IF !EOF() .OR. !BOF()
         MEST = EST
      ENDIF

      SELE D
      SEEK MEST
      IF !EOF() .OR. !BOF()
         MALIQ = ICM
      ENDIF

      SELE A
      REPL ALIQ WITH MALIQ, ICMT WITH MICMT, IPIT WITH MIPIT
      SKIP
   ENDDO

   CLOSE DATA

   ? MENSAG('Imprimindo o Relatorio de Resumo do Registro de Saidas... Aguarde')

   USE CAD
   ZAP
   INDEX ON STR(NNF,6) TO CAD.IDX
   CLOSE DATA

   SELE B
   USE CAD INDEX CAD.IDX
   SELE A
   USE CADSAID2

   DO WHILE !EOF()
      STOR 0 TO MICM1, MICM2, MICM3, MIPI1, MIPI2, MICM4, MIPI4, MIPI3, ;
                MVAL, MNNF
      MDEMI = SPACE(8)

      IF SUBS(NOPE,1,1) = SPACE(1)
         SKIP
         LOOP
      ENDIF

      IF SUBS(CANCELNF,1,2) <> SPACE(2)
         SKIP
         LOOP
      ENDIF

      MVAL = VAL + FRE - DESCONTO - ABATIMENTO
      IF ICMT = '1' .OR. ICMT = '3'
         IF BASEICM <> 0
            MICM1 = BASEICM
         ELSE
            MICM1 = VAL
         ENDIF
      ENDIF
      IF ICMT = '1'
         MICM2 = MVAL - MICM1
      ENDIF
      IF ICMT = '3'
         MICM3 = MVAL - MICM1
      ENDIF
      IF ICMT = '2'
         MICM2 = MVAL
      ENDIF
      IF ICMT = '4'
         MICM3 = MVAL
      ENDIF
      IF IPIT = '1'
         MIPI1 = VAL
         MIPI2 = MVAL - MIPI1
      ENDIF
      IF IPIT = '2'
         MIPI2 = MVAL
      ENDIF
      IF IPIT = '3'
         MIPI1 = VAL
         MIPI3 = MVAL - MIPI1
      ENDIF
      IF IPIT = '4'
         MIPI3 = MVAL
      ENDIF

      MDEMI = DEMI
      MNNF = NNF
      MNOPE = NOPE
      MICM4 =ICM
      MIPI4 = IPI

      SELE B
      APPEND BLANK
      REPL VALCONT WITH MVAL, VALICM WITH MICM1, ICMISENTO WITH MICM2, OUTRAICM WITH MICM3
      REPL VALIPI WITH MIPI1, IPISENTO WITH MIPI2, OUTRAIPI WITH MIPI3, DEMI WITH MDEMI
      REPL NNF WITH MNNF, NOPE WITH MNOPE, ICM WITH MICM4, IPI WITH MIPI4

      SELE A
      SKIP
   ENDDO

   CLOSE DATA

   USE CAD
   COPY TO CAD1Q FOR VAL(SUBS(DEMI,1,2)) < 16 .AND. SUBS(CANCELNF,1,2) = SPACE(2)
   USE CAD
   COPY TO CAD2Q FOR VAL(SUBS(DEMI,1,2)) > 15 .AND. SUBS(CANCELNF,1,2) = SPACE(2)

   USE CAD2Q
   INDEX ON NOPE TO CAD2Q.IDX
   USE CAD2Q INDEX CAD2Q.IDX
   TOTAL ON NOPE TO CAD2 FIELDS VALCONT,IPI,ICM,VALICM,VALIPI,IPISENTO, ICMISENTO, OUTRAICM, OUTRAIPI

   USE CAD1Q
   INDEX ON NOPE TO CAD1Q.IDX
   USE CAD1Q INDEX CAD1Q.IDX
   TOTAL ON NOPE TO CAD1 FIELDS VALCONT,IPI,ICM,VALICM, VALIPI, IPISENTO, ICMISENTO, OUTRAICM, OUTRAIPI
   USE CAD1

   SET DEVICE TO PRINT
   SET PRINT ON

   DO WHILE .NOT. EOF()
      LIN = 13
      @ 01,01 SAY REPL('-',219)
      @ 02,01 SAY '|'
      @ 02,219 say '|'
      @ 03,01 SAY '|'
      @ 03,64 SAY  'R  E  S  U  M  O    D  O    R  E  G  I  S  T  R  O    D  E    S  A  I  D  A  S'
      @ 03,150 SAY NOME + '/' + SUBS(MDATA,4,2)
      @ 03,219 SAY '|'
      @ 04,01 SAY '|'
      @ 04,219 SAY '|'
      @ 05,01 SAY REPL('-',219)
      @  06,1   say '|    DOCUMENTOS FISCAIS    |                | CODIFICACAO |                 V A L O R E S   F I S C A I S  -  I C M                 |               V A L O R E S   F I S C A I S  -  I P I             |                 |'
      @  07,1   say '|--------------------------|      VALOR     |-------------|-------------------------------------------------------------------------|-------------------------------------------------------------------|                 |'
      @  08,1   say '|        NATUREZA          |                |      |      |    Operacoes com Debito do Imposto   |  Operacoes sem Debito do Imposto | Operacoes com Debito do Imposto|  Operacoes sem Debito do Imposto |   OBSERVACOES   |'
      @  09,1   say '|                          |                |CONTAB|FISCAL|--------------------------------------|----------------------------------|--------------------------------|----------------------------------|                 |'
      @  10,1   say '|        OPERACAO          |     CONTABIL   |      |      |     BASE DE    |ALIQ |    IMPOSTO    |    ISENTAS OU   |     OUTRAS     |     BASE DE    |    IMPOSTO    |    ISENTAS OU   |     OUTRAS     |                 |'
      @  11,1   say '|                          |                |      |      |     CALCULO    |     |    DEBITADO   |  NAO TRIBUTADAS |                |     CALCULO    |    DEBITADO   |  NAO TRIBUTADAS |                |                 |'
      @  12,1   say '|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|'

      DO WHILE LIN < 61
         IF VAL(SUBS(DEMI,1,2)) < 16 .AND. IMPRIME = 1
            IMPRIME = 2
            @ LIN,01 SAY '|'
            @ LIN,03 SAY 'PRIMEIRA QUINZENA:'
            @ LIN,219 SAY '|'
            LIN = LIN + 1
         ENDIF

         IF VAL(SUBS(DEMI,1,2)) > 15 .AND. IMPRIME = 2
            IMPRIME = 3
            @ LIN,01 SAY '|'
            @ LIN,03 SAY 'SEGUNDA QUINZENA:'
            @ LIN,219 SAY '|'
            LIN = LIN + 1
         ENDIF

         IF IMPRIME = 4
            IMPRIME = 5
            @ LIN,01 SAY '|'
            @ LIN,03 SAY 'GERAL:'
            @ LIN,219 SAY '|'
            LIN = LIN + 1
         ENDIF

         @ LIN,01 SAY '|'
         @ LIN,16 SAY NOPE
         @ LIN,28 SAY VALCONT PICT '99,999,999,999.99'
         @ LIN,45 SAY '|'
         @ LIN,52 SAY '|'
         @ LIN,59 SAY '|'
         @ LIN,76 SAY '|'
         @ LIN,82 SAY '|'
         @ LIN,84 SAY ICM PICT '999,999,999.99'
         @ LIN,98 SAY '|'
         @ LIN,99 SAY ICMISENTO PICT '99,999,999,999.99'
         @ LIN,116 SAY '|'
         @ LIN,117 SAY OUTRAICM PICT '9,999,999,999.99'
         @ LIN,133 SAY '|'
         @ LIN,150 SAY '|'
         @ LIN,152 SAY IPI PICT '999,999,999.99'
         @ LIN,166 SAY '|'
         @ LIN,167 SAY IPISENTO PICT '99,999,999,999.99'
         @ LIN,184 SAY '|'
         @ LIN,185 SAY OUTRAIPI PICT '9,999,999,999.99'
         @ LIN,201 SAY '|'
         @ LIN,219 SAY '|'
         LIN = LIN + 1

         VALT = VALT + VALCONT
         ICMDEB = ICMDEB + ICM
         ICMTRIB = ICMTRIB + ICMISENTO
         TOTICM = TOTICM + OUTRAICM
         IPIDEB = IPIDEB + IPI
         IPITRIB = IPITRIB + IPISENTO
         TOTIPI = TOTIPI + OUTRAIPI
         SKIP

         IF EOF()
            IF IMPRIME = 2
               LIN = LIN + 1
               @ LIN,01 SAY '|'
               @ LIN,03 SAY 'TOTAL PRIMEIRA QUINZENA: '
               USE CAD2
            ENDIF

            IF IMPRIME = 3
               LIN = LIN + 1
               @ LIN,01 SAY '|'
               @ LIN,03 SAY 'TOTAL SEGUNDA QUINZENA: '
               USE CAD
               INDEX ON NOPE TO CAD.IDX
               USE CAD INDEX CAD.IDX
               TOTAL ON NOPE TO CADSAT FIELDS VALCONT,IPI,ICM,VALICM, VALIPI, IPISENTO, ICMISENTO, OUTRAICM, OUTRAIPI
               USE CADSAT
               IMPRIME = 4
            ENDIF

            IF IMPRIME = 5
               LIN = LIN + 1
               @ LIN,01 SAY '|'
               @ LIN,03 SAY 'TOTAL GERAL: '
            ENDIF

            @ LIN,28 SAY VALT PICT '99,999,999,999.99'
            @ LIN,45 SAY '|'
            @ LIN,52 SAY '|'
            @ LIN,59 SAY '|'
            @ LIN,76 SAY '|'
            @ LIN,82 SAY '|'
            @ LIN,84 SAY ICMDEB PICT '999,999,999.99'
            @ LIN,98 SAY '|'
            @ LIN,99 SAY ICMTRIB PICT '99,999,999,999.99'
            @ LIN,116 SAY '|'
            @ LIN,117 SAY TOTICM PICT '9,999,999,999.99'
            @ LIN,133 SAY '|'
            @ LIN,150 SAY '|'
            @ LIN,152 SAY IPIDEB PICT '999,999,999.99'
            @ LIN,166 SAY '|'
            @ LIN,167 SAY IPITRIB PICT '99,999,999,999.99'
            @ LIN,184 SAY '|'
            @ LIN,185 SAY TOTIPI PICT '9,999,999,999.99'
            @ LIN,201 SAY '|'
            @ LIN,219 SAY '|'
            LIN = LIN + 2
            STOR 0 TO VALT, ICMDEB, ICMTRIB, TOTICM, IPIDEB, IPITRIB, TOTIPI 

            IF IMPRIME = 5
               LIN = LIN - 1
               @ LIN,01 SAY REPL('-',219)
               EXIT
            ENDIF
         ENDIF
         LOOP
      ENDDO
      EJECT
   ENDDO

   USE CAD
   ZAP
   USE CADSAIDA
   ZAP

   CLOSE DATA
   SET PRINT OFF
   SET DEVICE TO SCREEN
   ERASE CADSAID2.DBF
   ERASE CADSAT.DBF
   ERASE CAD1.DBF
   ERASE CAD2.DBF
   ERASE CAD1Q.DBF
   ERASE CAD2Q.DBF

   ERASE CADSAID2.NTX
   ERASE CADSAT.NTX
   ERASE CAD1.NTX
   ERASE CAD2.NTX
   ERASE CAD1Q.NTX
   ERASE CAD2Q.NTX
   RETURN
