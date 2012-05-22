**********************************************************
* MODULO...: RELATORIO DE FATURAMENTO POR MES RESUMIDO POR REGIAO
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 20/10/97
**********************************************************

PRIVATE Tela, Maxlin, lin, TamLinha, nCol, TamPag, nMeses, ColOffSet
PRIVATE nMes1, nMes2, nAno1, nAno2, QtdMeses

ColOffSet = 3
nMes1 = 0
nMes2 = 0
nAno1 = 0
nAno2 = 0
QtdMeses = 0
TamPag = _PLENGTH - 1
DIMENSION aValMes(12), aTotMes(12), aIndice(12)
MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio de Faturamento Resumido por Mes/Repres.'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )

nMeses = 0
cMesAno1 = '01/' + STR(YEAR(HOJE),4)
cMesAno2 = '12/' + STR(YEAR(HOJE),4)
@ 06, 19 SAY 'Per°odo de Faturamento:'
@ 08, 19 SAY ' MES/ANO Inicial....:' GET cMesAno1 PICT [99/9999] 
@ 10, 19 SAY ' MES/ANO Final......:' GET cMesAno2 PICT [99/9999] VALID CHKPERIODO()
READ

IF LASTKEY() = 27
  =Finaliza()
  RETURN
ENDIF   

MENS = DADCORR
DO PEGRESP WITH [SN]

IF MC = 'N'
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

=MENSAG( 'Preparando Relat¢rio...')

*--- Arquivo de Trabalho
=MENSAG('Indexando Duplicatas...')
SELE 0
USE CADWORK
SELE 0
USE VEND INDEX VEND.IDX
*--- Seleciona Dispositivo de Saida
DO TESTIMP2

IF cp = 'N'
  =Finaliza()
  RETURN
ENDIF   

=MENSAG('Gerando Relatorio...')
DO DEFSAIDA

SELE CADWORK
SET RELATION TO VEN INTO VEND

FOR i = 1 TO 12
  aTotMes[i] = 0
  aIndice[i] = 0
NEXT i

nColIni = 38  
GO TOP
=ImprCabec()  
DO WHILE ! EOF()
  MVEN = VEN
  =ImprRepres()
  DO WHILE ! EOF() .AND. MVEN = VEN
    n = ((ANO - nAno1) * 12) + MES - (nMes1 - 1)
    aValMes[n] = SOMA
    aTotMes[n] = aTotMes[n] + SOMA
    SKIP
  ENDDO  
  =ImprLinha()
  =AddLin()
ENDDO
=ImprTotRel()  
DO IMPSAIDA

=FINALIZA()

*-------------------------------------------------------------------------
*==================
FUNCTION IMPRREPRES
*==================

  FOR i = 1 TO 12
    aValMes[i] = 0
  NEXT i
  IF ! FOUND('VEND')
    cRepres = VEN + ' << Repres. nao cadastrado >>'
  ELSE
    cRepres = VEN + ' ' + VEND->VENGUE
  ENDIF    
  @ Lin, ColOffSet+00 SAY cRepres
  
RETURN 

*Repres.                               JAN/97 FEV/97 MAR/97 ABR/97 MAI/97 JUN/97 JUL/97 AGO/97 SET/97 OUT/97 NOV/97 DEZ/97  TOTAL
*9999 - XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 99.999 99.999 99.999 99.999 99.999 99.999 99.999 99.999 99.999 99.999 99.999 99.999 999.999
*0                                     38     45     52     59     66     73     80     87     94     101    105    115    122

*=================
FUNCTION IMPRLINHA
*=================
  PRIVATE nTotal, i
  
  nTotal = 0
  FOR i = 1 TO nMeses 
    IF aValMes[i] > 0
      @ Lin, ColOffSet+CalcCol(i) SAY aValMes[i] PICT [99,999]
    ENDIF  
    nTotal = nTotal + aValMes[i]
  NEXT i 
  IF nTotal > 0 
    @ Lin, ColOffSet+CalcCol(13)+1 SAY nTotal PICT [999,999]
  ENDIF  

RETURN 

*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT, nTotal, i
  
  TXT = 'F I N A L   D O   R E L A T O R I O'
  @ Lin+1,  ColOffSet+00 SAY REPLICATE('=', nCols-ColOffSet)
  @ Lin+3,  ColOffSet+00 SAY 'TOTAL GERAL:' 
  nTotal = 0
  FOR i = 1 TO nMeses 
    IF aTotMes[i] > 0
      @ Lin+3, ColOffSet+CalcCol(i) SAY aTotMes[i] PICT [99,999]
    ENDIF  
    nTotal = nTotal + aTotMes[i]
  NEXT i 
  IF nTotal > 0 
    @ Lin+3, ColOffSet+CalcCol(13)+1 SAY nTotal PICT [999,999]
  ENDIF 
  @ Lin+5, ColOffSet+00 SAY 'MEDIA MENSAL:'  
  @ Lin+5, ColOffSet+CalcCol(13)+1 SAY (nTotal / QtdMeses) PICT [999,999]  
  @ Lin+7, ColOffSet+00 SAY REPLICATE('*', nCols-ColOffSet )
  @ Lin+8, ColOffSet+00+CENTRATXT(TXT,nCols) SAY TXT
  
RETURN

*================
FUNCTION FINALIZA
*================
  RESTORE SCREEN FROM Tela
  CLOSE DATABASES
* ERASE CADWORK.DBF
* ERASE PEDTMP1.DBF
RETURN   

*===============
FUNCTION CALCCOL
*===============
  PARAMETER nCol

  nPosicao = nColIni + (nCol - 1) * 7
  
RETURN nPosicao

*=================
FUNCTION IMPRCABEC
*=================
  PRIVATE nAno, nMes, txtAno

  =CABEC132( MODULO, 'Relatorio de Faturamento Resumido por Repres. - PERIODO DE: ' + cMesAno1 + ' A ' + cMesAno2 )
  @ 05, ColOffSet+00 SAY 'Repres.'

  txtAno = SUBSTR(cMesAno1,6,2)
  *--- Incluir as Colunas dos Meses aqui
  nMes = VAL(SUBSTR(cMesAno1,1,2))
  nAno = VAL(txtAno)
  FOR i = 1 TO nMeses 
    cNome = SUBSTR(GetNomeMes(nMes),1,3)
    @ 05, ColOffSet+CalcCol(i) SAY cNome + '/' + txtAno
    nMes = nMes + 1
    IF nMes > 12
      nMes = 1
      nAno = nAno + 1
    ENDIF  
  NEXT i
  
  @ 05, ColOffSet+123 SAY 'TOTAL'
  @ 06, ColOffSet+00 SAY REPLICATE('=', nCols-ColOffSet)
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
  PRIVATE dInicio, dFim
  
  =MENSAG( 'Pesquisando Notas Fiscais...' )
  
  dInicio = CTOD( '01/' + cMesAno1 )
  n = NDIAMES( CTOD('01/' + cMesAno2 ))
  dFim  = CTOD( STR(n,2) + '/' + cMesAno2 )

  QtdMeses = NumMeses(dInicio, dFim)
  CLOSE DATA
  *------- Checagem das Notas Fiscais Emitidas
  SELECT; 
    NF.VEN, MONTH(NF.DEMISSAO) AS MES, YEAR(DEMISSAO) AS ANO, SUM(VALPROD) AS SOMA;
  FROM; 
    CABECNF NF, TPNF TP;
  GROUP BY;
    NF.VEN, MES;
  WHERE; 
    (TP.TIPO = NF.TIPNF) AND (NF.CANCELADA = .F.) AND;
    (TP.VENDA = 'S') AND (DEMISSAO >= dInicio AND DEMISSAO <= dFim);
  ORDER BY; 
    NF.VEN, ANO, MES;
  INTO TABLE CADWORK
  
  CLOSE DATABASES

RETURN

*==================
FUNCTION CHKPERIODO
*==================

  IF CTOD('01/' + cMesAno1 ) > CTOD('01/' + cMesAno2 )
    =MENSAG( 'O primeiro per°odo n∆o pode ser maior que o segundo' )
    =INKEY(0)
    RETURN .F.
  ENDIF  
  
  nMes1 = VAL(SUBSTR(cMesAno1, 1, 2))
  nAno1 = VAL(SUBSTR(cMesAno1, 4, 4))
  nMes2 = VAL(SUBSTR(cMesAno2, 1, 2))
  nAno2 = VAL(SUBSTR(cMesAno2, 4, 4))

  nMeses = ((nAno2 - nAno1) * 12) + nMes2 - nMes1 + 1
    
RETURN .T.