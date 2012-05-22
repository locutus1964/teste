****  RELDEF - IMPRIME REGISTRO DE SAIDAS DEFINITIVO  ****
****  26/01/93  ****

   ? JANELA(16,45,20,77)
   ? BORDA(16,45,20,77)
   
   MDATA = space(5)
   MPAG = 1

   ? MENSAG('Informe o periodo do Registro de Saidas a ser Impresso')
   @ 18,48 say 'Mes e Ano para emiss„o 'get MDATA pict '99/99'
   read
   
   IF LASTKEY() = 27
      CLOSE DATA
      RETURN
   ENDIF
   
   ? MENSAG('Coloque papel de 132 colunas na impressora e tecle ENTER')
     =INKEY(0)

   ? MENSAG('Selecionando/Classificando os dados... Aguarde')

   USE PARAM
   MFOLHA = UFOLHA

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

   STOR 0 TO MTIP, MCGC, MNOPE, MEST, MALIQ, IMPRIME
   STOR 0 TO VAL1, ICM1, ICMDEB1, ICMTRIB1, TOTICM1, IPI1
   STOR 0 TO IPIDEB1, IPITRIB1, TOTIPI1
   STOR 0 TO VAL2, ICM2, ICMDEB2, ICMTRIB2, TOTICM2, IPI2 
   STOR 0 TO IPIDEB2, IPITRIB2, TOTIPI2 
   STOR 0 TO VALT, ICMTOT, ICMDEB, ICMTRIB, TOTICM, IPIT
   STOR 0 TO IPIDEB, IPITRIB, TOTIPI 
   STOR SPACE(01) TO MICMT, MIPIT

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
   ? MENSAG('Imprimindo o Relat¢rio de Registro de Saidas... Aguarde')

   DO COMPIMPR
   
   SET DEVICE TO PRINT
   SET PRINT ON

   USE CADSAID2
   INDEX ON STR(NNF,6) TO CADSAID2.IDX
   USE CADSAID2 INDEX CADSAID2.IDX

   DO WHILE .NOT. EOF()
      LIN = 17
      MFOLHA = MFOLHA + 1
      @ 01,01 SAY REPL('-',219)
      @ 02,01 SAY '|'
      @ 02,219 say '|'
      @ 03,01 SAY '|'
      @ 03,81 SAY  'R  E  G  I  S  T  R  O     D  E     S  A  I  D  A  S'
      @ 03,219 SAY '|'
      @ 04,01 SAY '|'
      @ 04,219 SAY '|'
      @ 05,01 SAY '|  EMPRESA - '
      @ 05,153 SAY 'C.G.C. -  '
      @ 05,219 SAY '|'
      @ 06,01 SAY '|  INSCR.ESTADUAL - '
      @ 06,153 SAY 'MES / ANO : '+ MDATA
      @ 06,219 SAY '|'
      @ 07,01 SAY '|  FOLHA : '
      @ 07,13 SAY PADL(MFOLHA,4,'0')
      @ 07,219 SAY '|'
      @ 08,01 SAY '|'
      @ 08,219 SAY '|'
      @ 09,01 SAY REPL ('-',219)

      @  10,1   say '|    DOCUMENTOS FISCAIS    |                | CODIFICACAO |                 V A L O R E S   F I S C A I S  -  I C M                 |               V A L O R E S   F I S C A I S  -  I P I             |                 |'
      @  11,1   say '|--------------------------|      VALOR     |-------------|-------------------------------------------------------------------------|-------------------------------------------------------------------|                 |'
      @  12,1   say '|     |SERIE|        |     |                |      |      |    Operacoes com Debito do Imposto   |  Operacoes sem Debito do Imposto | Operacoes com Debito do Imposto|  Operacoes sem Debito do Imposto |   OBSERVACOES   |'
      @  13,1   say '|ESPE-|     | NUMERO | DIA |                |CONTAB|FISCAL|--------------------------------------|----------------------------------|--------------------------------|----------------------------------|                 |'
      @  14,1   say '| CIE |SUB- |        |     |     CONTABIL   |      |      |     BASE DE    |ALIQ |    IMPOSTO    |    ISENTAS OU   |     OUTRAS     |     BASE DE    |    IMPOSTO    |    ISENTAS OU   |     OUTRAS     |                 |'
      @  15,1   say '|     |SERIE|        |     |                |      |      |     CALCULO    |     |    DEBITADO   |  NAO TRIBUTADAS |                |     CALCULO    |    DEBITADO   |  NAO TRIBUTADAS |                |                 |'
      @  16,1   say '|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|'

      IF MPAG <> 1
         @ LIN,01 SAY '|'
         @ LIN,03 SAY 'DE TRANSPORTE '
         @ LIN,28 SAY VALT PICT '99,999,999,999.99'
         @ LIN,45 SAY '|'
         @ LIN,52 SAY '|'
         @ LIN,59 SAY '|'
         @ LIN,60 SAY ICMTOT PICT '9,999,999,999.99'
         @ LIN,76 SAY '|'
         @ LIN,82 SAY '|'
         @ LIN,84 SAY ICMDEB PICT '999,999,999.99'
         @ LIN,98 SAY '|'
         @ LIN,99 SAY ICMTRIB PICT '99,999,999,999.99'
         @ LIN,116 SAY '|'
         @ LIN,117 SAY TOTICM  PICT '9,999,999,999.99'
         @ LIN,133 SAY '|'
         @ LIN,134 SAY IPIT PICT '9,999,999,999.99'
         @ LIN,150 SAY '|'
         @ LIN,152 SAY IPIDEB PICT '999,999,999.99'
         @ LIN,166 SAY '|'
         @ LIN,167 SAY IPITRIB PICT '99,999,999,999.99'
         @ LIN,184 SAY '|'
         @ LIN,185 SAY TOTIPI PICT '9,999,999,999.99'
         @ LIN,201 SAY '|'
         @ LIN,219 SAY '|'
         LIN = LIN + 2
      ENDIF

      MPAG = MPAG + 1

      DO WHILE LIN <= 59
         STOR 0 TO VALICM, ICMISENTO, OUTRAICM, VALIPI, IPISENTO, OUTRAIPI, VALCONT 
         MCANCEL = SPACE(9)

         IF SUBS(NOPE,1,1) = SPACE(1)
            MCANCEL = 'PREST. SERVICOS'
         ELSE
            IF SUBS(CANCELNF,1,2) = SPACE(2)
               VALCONT = VAL + FRE - DESCONTO - ABATIMENTO
               IF ICMT = '1' .OR. ICMT = '3'
                  IF BASEICM <> 0
                     VALICM = BASEICM
                  ELSE
                     VALICM = VAL
                  ENDIF
               ENDIF
               IF ICMT = '1'
                  ICMISENTO = VALCONT - VALICM
               ENDIF
               IF ICMT = '3'
                  OUTRAICM = VALCONT - VALICM
               ENDIF
               IF ICMT = '2'
                  ICMISENTO = VALCONT
               ENDIF
               IF ICMT = '4'
                  OUTRAICM = VALCONT
               ENDIF
               IF IPIT = '1'
                  VALIPI = VAL
                  IPISENTO = VALCONT - VALIPI
               ENDIF
               IF IPIT = '2'
                  IPISENTO = VALCONT
               ENDIF
               IF IPIT = '3'
                  VALIPI   = VAL
                  OUTRAIPI = VALCONT - VALIPI
               ENDIF
               IF IPIT = '4'
                  OUTRAIPI = VALCONT
               ENDIF
            ELSE
               MCANCEL = '   CANCELADA   '
            ENDIF
         ENDIF
        @ LIN,01 SAY '|'
        @ LIN,03 SAY 'NFF'
        @ LIN,07 SAY '|'
        @ LIN,10 SAY 'U'
        @ LIN,13 SAY '|'
        @ LIN,15 SAY NNF
        @ LIN,22 SAY '|'
        @ LIN,24 SAY SUBS(DEMI,1,2)
        @ LIN,28 SAY '|'
        @ LIN,29 SAY VALCONT PICT '9,999,999,999.99'
        @ LIN,45 SAY '|'
        @ LIN,52 SAY '|'
        @ LIN,54 SAY NOPE
        @ LIN,59 SAY '|'
        @ LIN,60 SAY VALICM PICT '9,999,999,999.99'
        @ LIN,76 SAY '|'
        IF ICM > 0
           @ LIN,77 SAY ALIQ
        ELSE
           @ LIN,78 SAY '0.00'
        ENDIF
        @ LIN,82 SAY '|'
        @ LIN,84 SAY ICM PICT '999,999,999.99'
        @ LIN,98 SAY '|'
        @ LIN,99 SAY ICMISENTO PICT '99,999,999,999.99'
        @ LIN,116 SAY '|'
        @ LIN,117 SAY OUTRAICM PICT '9,999,999,999.99'
        @ LIN,133 SAY '|'
        @ LIN,134 SAY VALIPI PICT '9,999,999,999.99'
        @ LIN,150 SAY '|'
        @ LIN,152 SAY IPI PICT '999,999,999.99'
        @ LIN,166 SAY '|'
        @ LIN,167 SAY IPISENTO PICT '99,999,999,999.99'
        @ LIN,184 SAY '|'
        @ LIN,185 SAY OUTRAIPI PICT '9,999,999,999.99'
        @ LIN,201 SAY '|'
        @ LIN,203 SAY MCANCEL
        @ LIN,219 SAY '|'
        LIN = LIN + 1

        IF VAL(SUBS(DEMI,1,2)) < 16
           VAL1 = VAL1 + VALCONT
           ICM1 = ICM1 + VALICM
           ICMDEB1 = ICMDEB1 + ICM
           ICMTRIB1 = ICMTRIB1 + ICMISENTO
           TOTICM1 = TOTICM1 + OUTRAICM
           IPI1 = IPI1 + VALIPI
           IPIDEB1 = IPIDEB1 + IPI
           IPITRIB1 = IPITRIB1 + IPISENTO
           TOTIPI1 = TOTIPI1 + OUTRAIPI
           SKIP

           IF VAL(SUBS(DEMI,1,2)) >=16
              LIN = LIN + 1
              @ LIN,01 SAY '|'
              @ LIN,03 SAY 'TOTAL PRIMEIRA QUINZENA:'
              @ LIN,28 SAY VAL1 PICT '99,999,999,999.99'
              @ LIN,45 SAY '|'
              @ LIN,52 SAY '|'
              @ LIN,59 SAY '|'
              @ LIN,60 SAY ICM1 PICT '9,999,999,999.99'
              @ LIN,76 SAY '|'
              @ LIN,82 SAY '|'
              @ LIN,84 SAY ICMDEB1 PICT '999,999,999.99'
              @ LIN,98 SAY '|'
              @ LIN,99 SAY ICMTRIB1 PICT '99,999,999,999.99'
              @ LIN,116 SAY '|'
              @ LIN,117 SAY TOTICM1 PICT '9,999,999,999.99'
              @ LIN,133 SAY '|'
              @ LIN,134 SAY IPI1 PICT '9,999,999,999.99'
              @ LIN,150 SAY '|'
              @ LIN,152 SAY IPIDEB1 PICT '999,999,999.99'
              @ LIN,166 SAY '|'
              @ LIN,167 SAY IPITRIB1 PICT '99,999,999,999.99'
              @ LIN,184 SAY '|'
              @ LIN,185 SAY TOTIPI1 PICT '9,999,999,999.99'
              @ LIN,201 SAY '|'
              @ LIN,219 SAY '|'
              LIN = LIN + 2
              LOOP
           ENDIF
        ELSE
           VAL2 = VAL2 + VALCONT
           ICM2 = ICM2 + VALICM
           ICMDEB2 = ICMDEB2 + ICM
           ICMTRIB2 = ICMTRIB2 + ICMISENTO
           TOTICM2 = TOTICM2 + OUTRAICM
           IPI2 = IPI2 + VALIPI
           IPIDEB2 = IPIDEB2 + IPI
           IPITRIB2 = IPITRIB2 + IPISENTO
           TOTIPI2 = TOTIPI2 + OUTRAIPI
           SKIP
        ENDIF

        VALT = VAL1 + VAL2
        ICMTOT = ICM1 + ICM2
        ICMDEB = ICMDEB1 + ICMDEB2
        ICMTRIB = ICMTRIB1 + ICMTRIB2
        TOTICM = TOTICM1 + TOTICM2
        IPIT = IPI1 + IPI2
        IPIDEB = IPIDEB1 + IPIDEB2
        IPITRIB = IPITRIB1 + IPITRIB2
        TOTIPI = TOTIPI1 + TOTIPI2

        IF EOF()
           LIN = LIN + 1
           @ LIN,01 SAY '|'
           @ LIN,03 SAY 'TOTAL SEGUNDA QUINZENA:'
           @ LIN,28 SAY VAL2 PICT '99,999,999,999.99'
           @ LIN,45 SAY '|'
           @ LIN,52 SAY '|'
           @ LIN,59 SAY '|'
           @ LIN,60 SAY ICM2 PICT '9,999,999,999.99'
           @ LIN,76 SAY '|'
           @ LIN,82 SAY '|'
           @ LIN,84 SAY ICMDEB2 PICT '999,999,999.99'
           @ LIN,98 SAY '|'
           @ LIN,99 SAY ICMTRIB2 PICT '99,999,999,999.99'
           @ LIN,116 SAY '|'
           @ LIN,117 SAY TOTICM2 PICT '9,999,999,999.99'
           @ LIN,133 SAY '|'
           @ LIN,134 SAY IPI2 PICT '9,999,999,999.99'
           @ LIN,150 SAY '|'
           @ LIN,152 SAY IPIDEB2 PICT '999,999,999.99'
           @ LIN,166 SAY '|'
           @ LIN,167 SAY IPITRIB2 PICT '99,999,999,999.99'
           @ LIN,184 SAY '|'
           @ LIN,185 SAY TOTIPI2 PICT '9,999,999,999.99'
           @ LIN,201 SAY '|'
           @ LIN,219 SAY '|'
           LIN = LIN + 2

           @ LIN,01 SAY '|'
           @ LIN,03 SAY 'TOTAL GERAL:'
           @ LIN,28 SAY VALT PICT '99,999,999,999.99'
           @ LIN,45 SAY '|'
           @ LIN,52 SAY '|'
           @ LIN,59 SAY '|'
           @ LIN,60 SAY ICMTOT PICT '9,999,999,999.99'
           @ LIN,76 SAY '|'
           @ LIN,82 SAY '|'
           @ LIN,84 SAY ICMDEB PICT '999,999,999.99'
           @ LIN,98 SAY '|'
           @ LIN,99 SAY ICMTRIB PICT '99,999,999,999.99'
           @ LIN,116 SAY '|'
           @ LIN,117 SAY TOTICM PICT '9,999,999,999.99'
           @ LIN,133 SAY '|'
           @ LIN,134 SAY IPIT PICT '9,999,999,999.99'
           @ LIN,150 SAY '|'
           @ LIN,152 SAY IPIDEB PICT '999,999,999.99'
           @ LIN,166 SAY '|'
           @ LIN,167 SAY IPITRIB PICT '99,999,999,999.99'
           @ LIN,184 SAY '|'
           @ LIN,185 SAY TOTIPI PICT '9,999,999,999.99'
           @ LIN,201 SAY '|'
           @ LIN,219 SAY '|'
           LIN = LIN + 1
           @ LIN,01 SAY REPL('-',219)

           EJECT
           SET PRINT OFF
           SET DEVICE TO SCREEN
           CLOSE DATA

           mf = ' '
           MENS = 'Atualiza o N£mero de Folhas Impressas (S/N) '
           ? MENSAG(MENS)
           @ 23,POS say mens get mf pict '!' valid(mf$'SN')
           read

           IF mf = 'S'
              USE PARAM
              REPL  UFOLHA WITH MFOLHA
           ENDIF
*****
           USE CADSAIDA
           ZAP
*****
           CLOSE DATA
           ERASE CADSAID2.DBF
           ERASE CADSAID2.NTX
           RETURN
        ENDIF
      ENDDO

      LIN = LIN + 1
      @ LIN,01 SAY '|'
      @ LIN,03 SAY 'A TRANSPORTAR: '
      @ LIN,28 SAY VALT PICT '99,999,999,999.99'
      @ LIN,45 SAY '|'
      @ LIN,52 SAY '|'
      @ LIN,59 SAY '|'
      @ LIN,60 SAY ICMTOT PICT '9,999,999,999.99'
      @ LIN,76 SAY '|'
      @ LIN,82 SAY '|'
      @ LIN,84 SAY ICMDEB PICT '999,999,999.99'
      @ LIN,98 SAY '|'
      @ LIN,99 SAY ICMTRIB PICT '99,999,999,999.99'
      @ LIN,116 SAY '|'
      @ LIN,117 SAY TOTICM PICT '9,999,999,999.99'
      @ LIN,133 SAY '|'
      @ LIN,134 SAY IPIT PICT '9,999,999,999.99'
      @ LIN,150 SAY '|'
      @ LIN,152 SAY IPIDEB PICT '999,999,999.99'
      @ LIN,166 SAY '|'
      @ LIN,167 SAY IPITRIB PICT '99,999,999,999.99'
      @ LIN,184 SAY '|'
      @ LIN,185 SAY TOTIPI PICT '9,999,999,999.99'
      @ LIN,201 SAY '|'
      @ LIN,219 SAY '|'
      LIN = LIN + 1
      @ LIN,01 SAY REPL ('-',219)
      EJECT
   ENDDO


