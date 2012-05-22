**********************************************************
* MODULO...: RELATORIO DE FATURAMENTO POR REGIAO/VALOR
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 15/10/97
**********************************************************

PRIVATE Tela, Maxlin, lin, TamLinha, nCol, TotDup, TamPag

TamPag = _PLENGTH - 1
MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat.de Faturamento por Per°odo - Ordem de Regi∆o/Valor'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )

SELE 0
USE REGIAO order X_REGIAO
SELE 0
USE VEND ORDER X_VEND

dInicio = {}
dFim    = {}
MREGIAO = EmptyRegiao
nMenos  = 0
@ 06, 19 SAY 'Per°odo de Faturamento:'
@ 08, 19 SAY ' DATA INICIAL...........:' GET dInicio
@ 10, 19 SAY ' DATA FINAL.............:' GET dFim 
@ 12, 19 SAY ' DIAS A MENOS (Feriados):' GET nMenos PICT [99]
@ 14, 19 SAY ' REGI«O.................:' GET MREGIAO PICT [9999.99];
                                         VALID MRegiao = EmptyRegiao .OR. ChkRegiao(MREGIAO,.T.)
READ

IF LASTKEY() = 27
  =Finaliza()
  RETURN
ENDIF   

*----------- Determina a ordem de classificaá∆o
MENS = 'Ordem de classificaá∆o (R)egi∆o/(V)alor: '
MC = 'R'
DO PEGRESP WITH [RV]

*----------- Seleciona as Notas Fiscais
nTotVenda = 0
=SelecNfs()

lin = 0
PAGCTR = 0
tamrel = 2
tamlinha = 80
nCols = 133

=MENSAG( 'Preparando Relat¢rio...')

*--- Arquivo de Trabalho
=MENSAG('Indexando Duplicatas...')
SELE 0
USE CADWORK
SELE 0
USE DUPL 
INDEX ON NDUP TO TMPDUP.IDX
SELE 0
USE CLIEN ORDER P_CGC

*--- Seleciona Dispositivo de Saida
DO TESTIMP2

IF cp = 'N'
  =Finaliza()
  RETURN
ENDIF   

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
                CGC   INTO CLIEN

GO TOP
=ImprCabec()
nTQtdNFVnd = 0
DO WHILE ! EOF()
  MREGIAO = FmtRegiao(regiao, IDREGIAO)
  nValorRegiao = 0
  nIpiRegiao   = 0
  nIcmRegiao   = 0
  nNfRegiao    = 0
  QtdNotaVnd   = 0
  nDuplRegiao  = 0
  DO WHILE !EOF() .AND. MREGIAO = FmtRegiao(regiao, IDREGIAO)
    nNfRegiao    = nNfRegiao + 1
    nValorRegiao = nValorRegiao + VALPROD
    nIpiRegiao   = nIpiRegiao + VALIPI
    =ImprLinha()
    QtdNotaVnd = QtdNotaVnd + 1
    =ImprDupl()
    nIcmRegiao = nIcmRegiao + VALICM
    *nTotIcm    = nTotIcm + VALICM
    =AddLin()
    SKIP
  ENDDO 
  nTotValor = nTotValor + nValorRegiao
  nTotIpi   = nTotIpi + nIpiRegiao
  nTotIcm   = nTotIcm + nIcmRegiao
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
*    @ Lin, 00 SAY '<< CGC Invalido >>'  
  ENDIF  
  @ Lin, 19 SAY FmtRegiao(regiao, IDREGIAO)
  @ Lin, 27 SAY FmtRepr(VEN, IDREPR)
  @ Lin, 35 SAY NNF PICT [999999]
  @ Lin, 42 SAY DEMISSAO
  IF VALPROD > 0 
    @ Lin, 53 SAY VALPROD PICT [999,999.99]
  ENDIF
  IF VALIPI > 0  
    @ Lin, 64 SAY VALIPI PICT [99,999.99]
  ENDIF
  IF VALICM > 0  
    @ Lin, 74 SAY VALICM PICT [99,999.99]
  ENDIF  
  @ Lin, 84 SAY TIPO
  @ Lin, 88 SAY substr(DESN,1,20)

RETURN 

*================
FUNCTION IMPRDUPL
*================
  PRIVATE nTotal 
  
  SELE DUPL
  MNDUP = NDUP
  nTotal = 0 
  TotDup = ContarDupl()
  DO WHILE ! EOF() .AND. MNDUP = NDUP
    IF PDUP > 1
      =AddLin()
    ENDIF  
    @ Lin, 108 SAY STR(NDUP,6)
    IF TotDup > 1
      @ Lin, 114 SAY '/' + STR(PDUP,1)
    ENDIF  
    @ Lin, 117 SAY VDP  PICT [99,999.99]
    @ Lin, 127 SAY VENC 
    nTotal      = nTotal + VDP
    nDuplRegiao = nDuplRegiao + VDP
    SKIP    
  ENDDO 
  nTotDupl = nTotDupl + nTotal
  IF SUBSTR(CADWORK->NOPE,1,1) = '5'   &&--- Venda no Estado
    nTot5XX = nTot5XX + nTotal
  ELSE  
    IF SUBSTR(CADWORK->NOPE,1,1) = '6' &&--- Venda Fora do Estado
      nTot6XX = nTot6XX + nTotal
    ENDIF
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
  @ Lin+1,  00 SAY 'TOTAIS DA REGIAO:'
  @ Lin+1,  18 SAY 'Qtd.Notas: ' + STR( nNfRegiao, 4 )
  @ Lin+1,  47 SAY nValorRegiao PICT [999,999.99]
  @ Lin+1,  58 SAY nIpiRegiao   PICT [99,999.99]
  @ Lin+1,  69 SAY nIcmRegiao   PICT [99,999.99]
  @ Lin+1,  89 SAY 'Qtd.Notas (Vendas): ' + STR( QtdNotaVnd, 3 )
  @ Lin+1, 114 SAY nDuplRegiao  PICT [999,999.99]
  @ Lin+1, 125 SAY nDuplRegiao / nTotVenda * 100 PICT [999.9 %]
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
  @ Lin+2,  47 SAY nTotValor PICT [999,999.99]
  @ Lin+2,  57 SAY nTotIpi   PICT [999,999.99]
  @ Lin+2,  68 SAY nTotIcm   PICT [999,999.99]
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
                     DTOC(dInicio) + ' A ' + DTOC( dFim ) +;
                     '  - POR REGI«O ')
 @ 05, 00 SAY 'Nome de Guerra  UF Regiao   Vend   N.F.   Emissao   Valor NF    Ipi        Icm     Tipo      Descricao     Duplic.    Valor Vencimento' 
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

*================
FUNCTION SELECNFS
*================
  PRIVATE cRegiao, cOrdem

  IF MREGIAO = EmptyRegiao
    cRegiao = ''
  ELSE  
    cRegiao = 'AND NF.REGIAO = "' + SUBSTR(MREGIAO,1,4) + '" AND IDREGIAO=' + SUBSTR(MREGIAO,6,2)
  ENDIF  
  *------- Checagem das Notas Fiscais Emitidas

  =MENSAG( 'Pesquisando Notas Fiscais...' )

  *----------- Seleciona as Notas Fiscais do Per°odo
  SELECT;
    NF.CGC, NF.DEMISSAO, NNF, VALPROD, VALIPI, VALICM,;
    TP.TIPO, TP.DESN, TP.NOPE,;
    NF.VEN, NF.IDREPR, NF.REGIAO, NF.IDREGIAO, PE2, PE3, PE4, (VALPROD+VALIPI) AS VALNF;
  FROM;
    CABECNF NF, TPNF TP;
  WHERE;
    (NF.DEMISSAO >= dInicio) AND (NF.DEMISSAO <= dFim) AND;
    (TP.TIPO = NF.TIPNF) AND (NF.CANCELADA = .F.) AND (TP.VENDA = 'S') &cRegiao;
  ORDER BY;
    NF.REGIAO, NF.IDREGIAO, NF.VEN, NF.IDREPR, NF.NNF;
  INTO TABLE CADWORK
  
  CLOSE DATABASES
  
  IF MC = 'V'  
    *------------ Agrupa pelo valor de Venda das Regioes
    SELECT;
      REGIAO, IDREGIAO, SUM(VALPROD + VALIPI) AS TOTREGIAO;
    FROM CADWORK;
    GROUP BY REGIAO, IDREGIAO;
    ORDER BY TOTREGIAO DESCENDING;
    INTO TABLE REGWORK
    
    SELECT R.TOTREGIAO, C.VALNF, C.*;
      FROM; 
        CADWORK C, REGWORK R;
      WHERE; 
        R.REGIAO = C.REGIAO AND R.IDREGIAO = C.IDREGIAO;
      ORDER BY;
        R.TOTREGIAO DESC, C.VALNF DESC;  
      INTO TABLE TMP
      
    CLOSE DATABASES
    
    ERASE REGWORK.DBF
    ERASE CADWORK.DBF
    RENAME TMP.DBF TO CADWORK.DBF
      
  ENDIF  
  
  *----- Calcula o Total das Vendas para o calculo de porcentagem
  SELECT SUM(VALIPI + VALPROD) AS TOTAL;
  FROM CADWORK;
  INTO TABLE TOTAL
  nTotVenda = TOTAL->TOTAL   
  CLOSE DATABASES
  
  ERASE TOTAL.DBF

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