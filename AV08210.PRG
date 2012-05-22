* AV08210 - CALCULA A POSICAO FINANCEIRA DE CLIENTES
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 02/03/87  -  11/10/93
*
 DO TESTIMP2
 IF CP = 'N'
    RETURN
 ENDIF

 ? CABTELA(0,0,24,79)
 MENS = 'Posi‡„o Financeira de Clientes'
 ? MENSAGR(MENS)
 @ 01,POS SAY MENS
 @ 10,20 SAY "Data Inicial :"
 @ 12,20 SAY "Data Final :"
 DTINI = {}
 DTFIM = {}

 @ 10,40 GET DTINI 
 @ 12,40 GET DTFIM 
 READ

 MENS = DADCORR
 DO PEGRESP WITH [SN]
   IF MC = 'N'
     RETURN
   ENDIF
     
 * PREPARACAO DO ARQUIVO COM DUPLICATAS E VENCIMENTOS

 =MENSAG('Classificando duplicatas para emissao do relatorio...Aguarde')
 USE DUPL
 COPY TO CADWORK FOR (VENC >= DTINI .AND. VENC <= DTFIM) .AND.;
                     EMPTY(PAG) .AND. EMPTY(DTCANCEL)

 USE CADWORK
 INDEX ON DTOS(VENC)+CGC TO POSFIN.IDX
 CLOSE DATABASES

 KEY2   = {}
 VALDIA = 0.00
 TVAL   = 0
 VDESCTO = 0.00
 GDESCTO = 0.00

 SELE 0
   USE CLIEN ORDER P_CGC
 SELE 0
   USE CADWORK INDEX POSFIN.IDX

 PAGCTR  = 0
 TAMREL  = 2
 
 ? MENSAG('Imprimindo Posi‡„o Financeira de Clientes... Aguarde')

 DO DEFSAIDA
  
 DO WHILE !EOF()
 
    TITULO = 'VALORES A RECEBER DE CLIENTES - DE ' + DTOC(DTINI) + ' ATE ' + DTOC(DTFIM)
    ? CABEC('POSFIN',TITULO,133)

    @ 3,1 SAY 'VENCIMEN'
    @ 3,10 SAY 'NOME DE GUERRA'
    @ 3,28 SAY 'UF'
    @ 3,31 SAY 'REGI'
    @ 3,39 SAY 'VEND'
    @ 3,47 SAY ' DUPLIC'
    @ 3,54 SAY ' EMISSAO'
    @ 3,67 SAY 'BCO'
    @ 3,71 SAY 'VALOR DUPLIC  ACUMULADO TOTAL'
    @ 3,104 SAY ' ACUMULADO DIA'
    @ 3,121 SAY ' ACUM C/DESCTO'
    @ 4,001 SAY REPLICATE('-',131)
    LIN = 5

    DO WHILE !EOF() .AND. LIN < 64

      @ LIN,001 SAY VENC
       MCGC = CGC

       SELE CLIEN
       SEEK MCGC
       IF !(EOF() .OR. BOF())
          @ LIN,12 SAY GUE
          @ LIN,28 SAY EST
          @ LIN,31 SAY FmtRegiao(REGIAO, IDREGIAO)
          @ LIN,39 SAY FmtRepr(VEN, IDREPR)
       ELSE
          @ LIN,12 SAY 'NAO CADASTRADO'
       ENDIF

       SELE Cadwork
         KEY2 = VENC
         @ LIN,47 SAY NDUP PICT '999999'
         @ LIN,PCOL() SAY '/'
         @ LIN,PCOL() SAY PDUP PICT '9'
         @ LIN,56 SAY EMIS
         MVDP = (VDP - DESC)
         @ LIN,67 SAY NBANCO PICT '999'
         @ LIN,71 SAY MVDP PICT '9,999,999.99'
         IF ! EMPTY(DTDESCTO) 
           @ LIN,121 SAY 'DESCONTADA'
           VDESCTO = (VDESCTO + MVDP)
           GDESCTO = (GDESCTO + MVDP)
         ELSE  
           TVAL = TVAL + MVDP - VALDEV
           VALDIA = VALDIA + MVDP - VALDEV
         ENDIF  

       IF VALDEV > 0
          LIN = LIN + 1
          @ LIN,42 SAY 'VALOR DEVOLUCAO'
          @ LIN,66 SAY VALDEV PICT '9,999,999.99'
       ENDIF

       IF DESC > 0
          LIN = LIN + 1
          @ LIN,42 SAY 'VALOR DO DESCONTO'
          @ LIN,66 SAY DESC PICT '9,999,999.99'
       ENDIF

       @ LIN,84 SAY TVAL PICT '9,999,999.99'
       SKIP
       IF VENC <> KEY2
          @ LIN,103 SAY VALDIA PICT '9,999,999.99'
          IF VDESCTO > 0
            @ LIN,121 SAY (VALDIA + VDESCTO) PICT '9,999,999.99'
            VDSCTO = 0.00
          ENDIF  
          STOR 0.00 TO VALDIA
          KEY2 = VENC
          LIN = LIN + 1
       ENDIF
       LIN = LIN + 1
       LOOP
    ENDDO
    
    IF EOF()
       @ LIN,80 SAY 'TOTAL DAS DUPL DESCONTADAS'
       @ LIN,117 SAY GDESCTO PICT '9,999,999.99'
       LIN = LIN + 2
       @ LIN,80 SAY 'TOTAL DO MOVIMENTO'
       @ LIN,117 SAY (TVAL + GDESCTO) PICT '9,999,999.99'
       LIN = LIN + 1
       @ LIN,01 SAY REPL('-',132)
       LIN = LIN + 1
       @ LIN,57 SAY 'FINAL DESTE RELATORIO'
    ENDIF
 ENDDO

 DO IMPSAIDA

 ERASE POSFIN.DBF
 ERASE POSFIN.NTX
 RETURN

