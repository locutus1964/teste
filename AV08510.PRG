**********************************************************
* MODULO...: RELATORIO DE FATURAMENTO POR PERIODO
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 18/07/97
**********************************************************

PRIVATE Tela, Maxlin, lin, TamLinha, nCol, TotDup, TamPag

TamPag = _PLENGTH - 1
MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio de Faturamento por Per¡odo'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )

dInicio = ObDataInicio()
dFim    = ObDataFim()
nMenos  = 0
@ 08, 19 SAY 'Per¡odo de Faturamento:'
@ 10, 19 SAY ' DATA INICIAL...........:' GET dInicio
@ 12, 19 SAY ' DATA FINAL.............:' GET dFim 
@ 14, 19 SAY ' DIAS A MENOS (Feriados):' GET nMenos PICT [99]
READ

IF LASTKEY() = 27
  =Finaliza()
  RETURN
ENDIF   

*----------- Seleciona as Notas Fiscais
=SelecNfs()

lin = 0
PAGCTR = 0
tamrel = 2
tamlinha = 80
nCols = 133

*--- Seleciona Dispositivo de Saida
DO TESTIMP2

IF cp = 'N'
  =Finaliza()
  RETURN
ENDIF   

=MENSAG( 'Preparando Relat¢rio...')

*--- Arquivo de Trabalho
=MENSAG('Indexando Duplicatas...')
SELE 0
USE CADWORK
INDEX ON NNF TO CADWORK.IDX
SELE 0
USE DUPL 
INDEX ON NDUP TO TMPDUP.IDX
SELE 0
USE CLIEN ORDER P_CGC
SELE 0
USE TPNF INDEX TPNF.IDX

nTotValor  = 0  
nTot5XX    = 0  &&--- Vendas no Estado
nTot6XX    = 0  &&--- Vendas Fora do Estado
nTotIpi    = 0  &&--- Total dos Valor de I.P.I.
nTotIcm    = 0  &&--- Total dos Valor de I.C.M.
nTotDupl   = 0  &&--- Total do Valor das Duplicatas  
nNotFound  = 0

=MENSAG('Gerando Relatorio...')
DO DEFSAIDA
SELE CADWORK
SET RELATION TO NNF   INTO DUPL,;
                CGC   INTO CLIEN,;
                TIPNF INTO TPNF
 
GO TOP
=ImprCabec()
nTQtdNFVnd = 0
DO WHILE ! EOF()
  MDIA = DAY(DEMISSAO)
  nValorDia  = 0
  nIpiDia    = 0
  nIcmDia    = 0
  nNotasDia  = 0
  QtdNotaVnd = 0
  nDuplDia   = 0
  DO WHILE !EOF() .AND. MDIA = DAY(DEMISSAO)
    MNNF = NNF
    nNotasDia = nNotasDia + 1
    nValorDia = nValorDia + VALPROD
    nIpiDia   = nIpiDia + VALIPI
    =ImprLinha()
    IF CANCELADA 
      @ Lin, 108 SAY '** C A N C E L A D A **'
    ELSE  
      IF TPNF->VENDA = 'S' 
        QtdNotaVnd = QtdNotaVnd + 1
        =ImprDupl()
        nIcmDia = nIcmDia + VALICM
        * nTotIcm = nTotIcm + VALICM
      ENDIF  
    ENDIF  
    =AddLin()
    SKIP
    DO WHILE (NNF - MNNF) > 1
      MNNF = MNNF + 1
      @ Lin, 05 SAY '** N.F. Nao Encontrada **'
      @ Lin, 31 SAY MNNF PICT [999999]
      =AddLin()
      nNotFound = nNotFound + 1
    ENDDO
  ENDDO 
  nTotValor = nTotValor + nValorDia
  nTotIpi   = nTotIpi + nIpiDia
  nTotIcm   = nTotIcm + nIcmDia
  nTQtdNFVnd = nTQtdNFVnd + QtdNotaVnd
  =ImprTotDia()  
ENDDO
=ImprTotRel()  
DO IMPSAIDA

=FINALIZA()

*-------------------------------------------------------------------------

*=================
FUNCTION IMPRLINHA
*=================
  IF ! EOF( 'CLIEN' )
    @ Lin, 00 SAY CLIEN->GUE
    @ Lin, 16 SAY CLIEN->EST
  ELSE
    @ Lin, 00 SAY '<< CGC Invalido >>'  
  ENDIF  
  @ Lin, 19 SAY FmtRegiao(REGIAO,IDREGIAO)
  @ Lin, 27 SAY FmtRepr(VEN,IDREPR) 
  @ Lin, 35 SAY NNF PICT [999999]
  @ Lin, 42 SAY DEMISSAO
  IF VALPROD > 0 
    @ Lin, 53 SAY VALPROD PICT [99,999.99]
  ENDIF
  IF VALIPI > 0  
    @ Lin, 64 SAY VALIPI PICT [99,999.99]
  ENDIF
  IF VALICM > 0  
    @ Lin, 73 SAY VALICM PICT [9,999.99]
  ENDIF  
  @ Lin, 82 SAY TIPNF  
  @ Lin, 86 SAY SUBSTR(TPNF->DESN,1,20)

RETURN 

*================
FUNCTION IMPRDUPL
*================
  PRIVATE nTotal, DuplAnt, DesdAnt, Repetiu 
  
  SELE DUPL
  MNDUP = NDUP
  nTotal = 0 
  TotDup = ContarDupl()
  ContRepet = 1
  DO WHILE ! EOF() .AND. MNDUP = NDUP
    DuplAnt = NDUP
    DesdAnt = PDUP
    IF ContRepet > 1 
      @ Lin, 133 SAY ContRepet PICT "(9)"
    ELSE  
      IF PDUP > 1
        =AddLin()
      ENDIF  
      @ Lin, 106 SAY STR(NDUP,6)
      IF TotDup > 1
        @ Lin, 112 SAY '/' + STR(PDUP,1)
      ENDIF  
      @ Lin, 115 SAY VDP  PICT [99,999.99]
      @ Lin, 125 SAY VENC 
      nTotal   = nTotal   + VDP
      nDuplDia = nDuplDia + VDP
    ENDIF  
    SKIP  
    IF !EOF() .AND. NDUP = DuplAnt .AND. PDUP = DesdAnt
      ContRepet = ContRepet + 1
    ENDIF  
  ENDDO 
  nTotDupl = nTotDupl + nTotal
  IF SUBSTR(TPNF->NOPE,1,1) = '5'   &&--- Venda no Estado
    nTot5XX = nTot5XX + nTotal
  ELSE  
*    IF SUBSTR(TPNF->NOPE,1,1) = '6' &&--- Venda Fora do Estado
    nTot6XX = nTot6XX + nTotal
*    ENDIF
  ENDIF    

  SELE CADWORK
RETURN

*==================
FUNCTION IMPRTOTDIA
*==================
  PRIVATE TXT, Media
  
  IF Lin + 3 >= TamPag
    =ImprCabec()
  ENDIF  
  @ Lin+0,  00 SAY REPLICATE('-', nCols )
  @ Lin+1,  00 SAY 'TOTAIS DO DIA:'
  @ Lin+1,  15 SAY 'Qtd.Notas: ' + STR( nNotasDia, 4 )
  @ Lin+1,  51 SAY nValorDia PICT [99,999.99]
  @ Lin+1,  61 SAY nIpiDia   PICT [99,999.99]
  @ Lin+1,  70 SAY nIcmDia   PICT [99,999.99]
  @ Lin+1,  89 SAY 'Qtd.Notas (Vendas): ' + STR( QtdNotaVnd, 3 )
  @ Lin+1, 114 SAY nDuplDia  PICT [999,999.99]
  @ Lin+2,  00 SAY REPLICATE('=', nCols )
  @ Lin+3,  00
  Lin = PROW()
  
RETURN

*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT, Media, NumDias
  
  IF Lin + 7 >= TamPag
    =ImprCabec()
  ENDIF  
  TXT = 'F I N A L   D O   R E L A T O R I O'
  NumDias = NDiasUteis( dInicio, dFim ) - nMenos
  IF NumDias = 0
    Media = 0
  ELSE   
    Media = nTotDupl / NumDias
  ENDIF   
  @ Lin+1,  00 SAY REPLICATE('*', nCols )
  @ Lin+2,  00 SAY 'TOTAL GERAL:' 
  @ Lin+2,  15 SAY 'Qtd.Notas: ' + TRANSFORM( RECCOUNT(), [9,999] )
  @ Lin+2,  48 SAY nTotValor PICT [999,999.99]
  @ Lin+2,  59 SAY nTotIpi   PICT [999,999.99]
  @ Lin+2,  70 SAY nTotIcm   PICT [99,999.99]
  @ Lin+2,  89 SAY 'Qtd.Notas (Vendas): ' + STR( nTQtdNFVnd, 4 )
  @ Lin+2, 114 SAY nTotDupl  PICT [999,999.99]
  @ Lin+3,  86 SAY 'VENDAS NO ESTADO 5.XX.....: ' + TRANSFORM( nTot5XX, [999,999.99] ) +;
                                                    TRANSFORM( nTot5XX / nTotDupl * 100, [ 999.9 %] ) 
  @ Lin+4,  00 SAY 'NUM.DIAS: ' + STR(NumDias,3)
  @ Lin+4,  15 SAY 'MEDIA DE FATURAMENTO: ' + TRANSFORM( Media, [999,999.99])
  @ Lin+4,  86 SAY 'VENDAS FORA DO ESTADO 6.XX: ' + TRANSFORM( nTot6XX, [999,999.99] ) +;
                                                    TRANSFORM( nTot6XX / nTotDupl * 100, [ 999.9 %] ) 
  @ Lin+5,  00 SAY REPLICATE('*', nCols )
  @ Lin+6,  00+CENTRATXT(TXT,nCols) SAY TXT
  IF nNotFound > 0 
    IF nNotFound = 1  
       cTexto = 'Nota Fiscal nao encontrada ***'
    ELSE   
       cTexto = 'Notas Fiscais nao encontradas ***'
    ENDIF   
    @ Lin+8, 00 SAY '*** Aviso: ' + TRANSFORM( nNotFound, [9,999 ] ) + cTexto
  ENDIF                   

  
RETURN

*================
FUNCTION FINALIZA
*================
  RESTORE SCREEN FROM Tela
  CLOSE DATABASES
*  ERASE CADWORK.DBF
*  ERASE PEDTMP1.DBF
RETURN   

*=================
FUNCTION IMPRCABEC
*=================
 =CABEC132( MODULO, 'Relatorio de Faturamento - DE: ' +;
                     DTOC(dInicio) + ' A ' + DTOC( dFim ) )
 @ 05, 00 SAY 'Nome de Guerra  UF Regiao  Vend      N.F.  Emissao   Valor NF    Ipi        Icm Tipo      Descricao        Duplic.    Valor Vencim.' 
 @ 06, 00 SAY REPLICATE('=', nCols)
 @ 07, 00 
 LIN = PROW()
 
RETURN  

*==============
FUNCTION ADDLIN
*==============

  Lin = Lin + 1 
  IF Lin >= TamPag
    =ImprCabec()
  ENDIF   
  
RETURN

*====================
FUNCTION OBDATAINICIO
*====================
  PRIVATE dInicio, cMes, cAno
  
  cAno = STR(YEAR(HOJE),4,0)
  cMes = STR(MONTH(HOJE),2,0)
  dInicio = CTOD('01/' + cMes + '/' + cAno)

RETURN dInicio

*=================
FUNCTION OBDATAFIM
*=================
  PRIVATE dFim
  
  dFim = HOJE

RETURN dFim

*================
FUNCTION SELECNFS
*================

  *------- Checagem das Notas Fiscais Emitidas

  =MENSAG( 'Pesquisando Notas Fiscais...' )
 
  SELECT;
    NF.CGC, NF.DEMISSAO, NNF, VALPROD, VALIPI, VALICM, TIPNF,;
    NF.VEN, NF.IDREPR, NF.REGIAO, NF.IDREGIAO, PE2, PE3, PE4, CANCELADA; 
  FROM;
    CABECNF NF;
  WHERE;
    (NF.DEMISSAO >= dInicio) AND (NF.DEMISSAO <= dFim);
  INTO TABLE CADWORK
  CLOSE DATABASES

RETURN

*==================
FUNCTION CONTARDUPL
*==================
  PRIVATE nTotal
  
  nTotal = 1
  IF CADWORK->PE2 <> 0 
     nTotal = nTotal + 1
  ENDIF 
  IF CADWORK->PE3 <> 0 
    nTotal = nTotal + 1
  ENDIF
  IF CADWORK->PE4 <> 0 
    nTotal = nTotal + 1
  ENDIF
  
RETURN nTotal  