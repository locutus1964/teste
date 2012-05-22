* AV08100 - CALCULA A POSICAO FINANCEIRA DE CLIENTES
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
 DTINI = SPACE(08)
 DTFIM = SPACE(08)

 @ 10,40 GET DTINI PICTURE "99/99/99"
 @ 12,40 GET DTFIM PICTURE "99/99/99"
 READ

 MENS = DADCORR
 DO PEGRESP WITH [SN]
   IF MC = 'N'
     RETURN
   ENDIF
     
 * PREPARACAO DO ARQUIVO COM DUPLICATAS E VENCIMENTOS
 ? MENSAG('Classificando duplicatas para emissao do relatorio...Aguarde')

 USE DUPL
 COPY TO CADWORK FOR CTOD(VENC) >= CTOD(DTINI) .AND.;
                     CTOD(VENC) <= CTOD(DTFIM) .AND.;
                     EMPTY(PAG) .AND.;
                     EMPTY(DTCANCEL)

 USE CADWORK
 INDEX ON SUBSTR(VENC,7,2)+'/'+SUBSTR(VENC,4,2)+'/'+SUBSTR(VENC,1,2)+CGC TO POSFIN
 CLOSE DATABASES

 KEY2   = SPACE(08)
 VALDIA = 0.00
 TVAL   = 0
 VDESCTO = 0.00
 GDESCTO = 0.00

 SELE clien
   USE CLIEN ORDER P_CGC
 SELE cadwork
   USE CADWORK INDEX POSFIN

 PAGCTR  = 0
 TAMREL  = 2
 
 ? MENSAG('Imprimindo Posi‡„o Financeira de Clientes... Aguarde')

 DO DEFSAIDA
  
 DO WHILE !EOF()
 
    TITULO = 'VALORES A RECEBER DE CLIENTES - DE '+DTINI+' ATE '+DTFIM
    ? CABEC('POSFIN',TITULO,131)

    @ 3,1 SAY 'VENCIMEN'
    @ 3,10 SAY 'NOME DE GUERRA'
    @ 3,26 SAY 'UF'
    @ 3,29 SAY 'REGI'
    @ 3,34 SAY 'VEND'
    @ 3,40 SAY ' DUPLIC'
    @ 3,49 SAY ' EMISSAO'
    @ 3,58 SAY 'BCO'
    @ 3,67 SAY 'VALOR DUPLIC  ACUMULADO TOTAL'
    @ 3,100 SAY '  ACUMULADO DIA'
    @ 3,117 SAY '  ACUM C/DESCTO'
    @ 4,001 SAY REPLICATE('-',131)
    LIN = 5

    DO WHILE !EOF() .AND. LIN < 64

      @ LIN,001 SAY VENC
       MCGC = CGC

       SELE B
       SEEK MCGC
       IF !(EOF() .OR. BOF())
          @ LIN,10 SAY GUE
          @ LIN,26 SAY EST
          @ LIN,29 SAY FmtRegiao(REGIAO,IDREGIAO)
          @ LIN,34 SAY VEN
       ELSE
          @ LIN,11 SAY 'NAO CADASTRADO'
       ENDIF

       SELE A
         KEY2 = VENC
         @ LIN,39 SAY NDUP PICT '999999'
         @ LIN,PCOL() SAY '/'
         @ LIN,PCOL() SAY PDUP PICT '9'
         @ LIN,49 SAY EMIS
         MVDP = (VDP - DESC)
         @ LIN,58 SAY NBANCO PICT '999'
         @ LIN,62 SAY MVDP PICT '99,999,999,999.99'
         IF LEFT(DTDESCTO,2) <> SPACE(02) .AND. LEFT(BXDESCTO,2) = SPACE(02)
           @ LIN,117 SAY 'DESCONTADA'
           VDESCTO = (VDESCTO + MVDP)
           GDESCTO = (GDESCTO + MVDP)
         ELSE  
           TVAL = TVAL + MVDP - VALDEV
           VALDIA = VALDIA + MVDP - VALDEV
         ENDIF  

       IF VALDEV > 0
          LIN = LIN + 1
          @ LIN,40 SAY 'VALOR DEVOLUCAO'
          @ LIN,62 SAY VALDEV PICT '99,999,999,999.99'
       ENDIF

       IF DESC > 0
          LIN = LIN + 1
          @ LIN,40 SAY 'VALOR DO DESCONTO'
          @ LIN,62 SAY DESC PICT '99,999,999,999.99'
       ENDIF

       @ LIN,80 SAY TVAL PICT '9,999,999,999.99'
       SKIP
       IF VENC <> KEY2
          @ LIN,099 SAY VALDIA PICT '9,999,999,999.99'
          IF VDESCTO > 0
            @ LIN,117 SAY (VALDIA + VDESCTO) PICT '9,999,999,999.99'
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
       @ LIN,117 SAY GDESCTO PICT '9,999,999,999.99'
       LIN = LIN + 2
       @ LIN,80 SAY 'TOTAL DO MOVIMENTO'
       @ LIN,117 SAY (TVAL + GDESCTO) PICT '9,999,999,999.99'
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