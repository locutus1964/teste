* AV08251 - CALCULO E IMPRESSAO DOS VALORES RECEBIDOS NO MES POR CLIENTE
* SISTEMA ADMINISTRACAO DE EMPRESAS
* ADAO ELI PEREIRA - 02-12-92
*
MODULO=PROGRAM()
DO TESTIMP2
IF CP = 'N'
  RETURN
ENDIF

MMES   = SPACE(5)
MMOEDA = 0
? CABTELA(0,0,24,79)
MENS = 'Valores recebidos por Cliente'
? MENSAGR(MENS)
@ 01,POS SAY MENS

 @ 10,20 SAY "Data Inicial :"
 @ 12,20 SAY "Data Final :"
 DTINI = {}
 DTFIM = {}

 @ 10,40 GET DTINI 
 @ 12,40 GET DTFIM 
 READ

 IF LASTKEY() = 27
   CLOSE DATA
   RETURN
 ENDIF
      
 MENS = DADCORR
 DO PEGRESP WITH [SN]
 IF MC = 'N'
   RETURN
 ENDIF
     
 * PREPARACAO DO ARQUIVO COM DUPLICATAS E VENCIMENTOS
 ? MENSAG('Classificando duplicatas para emissao do relatorio...Aguarde')

 dInicio = DTINI
 dFim    = DTFIM

 SELECT;
   DUPL.*;
 FROM;
   DUPL, CABECNF NF;
 WHERE;
   (DUPL.NDUP = NF.NNF AND NF.CANCELADA = .F.) AND;
   ((PAG >= dInicio AND PAG <= dFim) OR;
    (TIPOCART = 'DE' AND DTDESCTO >= dInicio AND DTDESCTO <= dFim)); 
 INTO TABLE;
   CADWORK1.DBF  

  CLOSE DATA 
  MENS= 'Classificando valores recebidos por Cliente... Aguarde'
  ? MENSAGR(MENS)
  @ 14,POS SAY MENS
*
  USE CADWORK1
  * Preenche as data de pagamento com a data de descto para classificacao 
  REPLACE ALL PAG WITH DTDESCTO FOR TIPOCART = 'DE' 
  INDEX ON dtos(PAG) + CGC + STR(NDUP,6) TO CADWORK1.IDX
  
  CLOS DATA
*
  ? MENSAG('Imprimindo Relatorio... Aguarde')
*
  PAGCTR = 0
  TAMREL = 2
*
SELE C
USE CADMOEDA INDEX CADMOEDA.IDX
SELE B
USE CLIEN ORDER P_CGC
SELE A
USE CADWORK1 INDEX CADWORK1.IDX
*
TVDESC  = 0.00
GVDESC  = 0.00
TVDP    = 0.00
GVDP    = 0.00
TJUROS  = 0.00
GJUROS  = 0.00
TDESC   = 0.00
GDESC   = 0.00
TVALDEV = 0.00
GVALDEV = 0.00
TITULO = 'VALORES RECEBIDOS POR CLIENTE DE - ' + DTOC(DTINI) + ' ATE ' + DTOC(DTFIM)
MCGC   = SPACE(16)
*
SELE A

DO DEFSAIDA

DO WHILE .NOT. EOF()
  ? CABEC(MODULO,TITULO,132)
  @ 03,001 SAY 'NOME DE GUERRA  UF REGIAO  VEND'
  @ 03,034 SAY 'DUP/PARC'
  @ 03,046 SAY ' VENCTO'
  @ 03,056 SAY 'DT PAGTO'
  @ 03,066 SAY 'ATR'
  @ 03,071 SAY 'VALOR DUPLICAT'
  @ 03,088 SAY 'VALOR JUROS'
  @ 03,103 SAY 'VALOR DESCTO'
  @ 03,119 SAY 'LIQ.RECEBIDO'
  @ 04,001 SAY REPLICATE('-',132)

  LIN = 5
*
  DO WHILE .NOT. EOF() .AND. LIN < 64
    SELE A

    IF CGC <> MCGC
      CCGC = CGC
      SELE B
      SEEK CCGC
      IF !(EOF() .OR. BOF())
        @ LIN,01 SAY GUE
        @ LIN,17 SAY EST
        @ LIN,20 SAY FmtRegiao(REGIAO, IDREGIAO)
        @ LIN,28 SAY FmtRepr(VEN, IDREPR)
      ELSE
        @ LIN,01 SAY 'CLIENTE NAO CADASTRADO'
      ENDIF
      MCGC = CGC
    ENDIF
 
    SELE A
    
    DuplDescto = TIPOCART = 'DE'
    DuplGarant = TIPOCART = 'GA'
    DuplNormal = TIPOCART = 'CS'
    
    @ LIN,36 SAY NDUP PICT '999999'
    @ LIN,42 SAY '/'
    @ LIN,43 SAY PDUP PICT '9'
    @ LIN,45 SAY VENC
    IF DuplDescto
      IF EMPTY(BXDESCTO) 
        dData = {}
      ELSE  
        dData = BXDESCTO
      ENDIF  
    ELSE
      dData = PAG
    ENDIF    
    @ LIN,56 SAY dData
    @ LIN,66 SAY PAG - VENC PICT '999'
    MVDP = VDP
    MPAG = PAG
    @ LIN,77 SAY MVDP PICT '9,999,999.99'
    IF DuplDescto
      TVDESC = TVDESC + MVDP
      GVDESC = GVDESC + MVDP
    ELSE
      TVDP = (TVDP + MVDP)
      GVDP = (GVDP + MVDP)
    ENDIF  
    @ LIN,86 SAY JUROS PICT '9,999,999.99'
    TJUROS = (TJUROS + JUROS)
    GJUROS = (GJUROS + JUROS)
    @ LIN,101 SAY DESC PICT '9,999,999.99'
    TDESC = (TDESC + DESC)
    GDESC = (GDESC + DESC)
    @ LIN,117 SAY (MVDP + JUROS - DESC) PICT '9,999,999.99'
    * Imprime o tipo de Carteira 
    IF .NOT. DuplNormal
      @ LIN,133 SAY TIPOCART
    ENDIF  

    LIN = LIN + 1

    SKIP

    IF PAG <> MPAG .OR. EOF()
      TOTDUPL = TVDP + TJUROS - TDESC
      @ LIN, 10 SAY 'TOTAIS DO DIA: ' + DTOC(MPAG)
      @ LIN, 69 SAY TVDP    PICT '9,999,999,999.99'
      @ LIN, 86 SAY TJUROS  PICT   '999,999,999.99'
      @ LIN,101 SAY TDESC   PICT   '999,999,999.99'
      @ LIN,116 SAY TOTDUPL PICT '99,999,999,999.99'
      LIN = LIN + 1
      @ LIN, 104 SAY 'DESCONTADAS: ' + TRANSFORM( TVDESC, '9,999,999,999.99' )
      LIN = LIN + 1
      @ LIN, 104 SAY 'TOTAL......: ' + TRANSFORM( TOTDUPL + TVDESC, '9,999,999,999.99' )
      TVDESC = 0.00
      TVDP   = 0.00
      TJUROS = 0.00
      TDESC  = 0.00
      LIN = LIN + 1
      @ LIN,001 SAY REPLICATE('-',132)
      LIN = LIN + 1
    ENDIF

    IF EOF()
      TOTDUPL = GVDP + GJUROS - GDESC
      @ LIN, 10 SAY 'TOTAIS DOS RECEBIMENTOS'
      @ LIN, 69 SAY GVDP     PICT '9,999,999,999.99'
      @ LIN, 86 SAY GJUROS   PICT   '999,999,999.99'
      @ LIN,101 SAY GDESC    PICT   '999,999,999.99'
      @ LIN,116 SAY TOTDUPL  PICT '99,999,999,999.99'
      LIN = LIN + 1
      @ LIN, 104 SAY 'DESCONTADAS: ' + TRANSFORM( GVDESC, '9,999,999,999.99' ) 
      LIN = LIN + 1
      @ LIN, 104 SAY 'TOTAL......: ' + TRANSFORM( TOTDUPL + GVDESC, '9,999,999,999.99' )
    ENDIF
  ENDDO
ENDDO

DO IMPSAIDA
RETURN
