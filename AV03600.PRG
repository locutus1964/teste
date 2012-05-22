**********************************************************
* MODULO...: RELATORIO DE DUPL.QUE VENCEM NO PERIODO
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 11/03/98
**********************************************************

PRIVATE Tela, Maxlin, lin, TamLinha, nCol, TotDup, TamPag

TamPag = _PLENGTH - 1
MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio de Duplicatas que Vencem no Per¡odo'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )

dInicio = {}
dFim    = {}
=InicDatas()

@ 08, 19 SAY ' Per¡odo de Vencimento:'
@ 10, 19 SAY ' DATA INICIAL...........:' GET dInicio
@ 12, 19 SAY ' DATA FINAL.............:' GET dFim 
READ

IF LASTKEY() = 27
  =Finaliza()
  RETURN
ENDIF   

MENS = DADCORR
MC = 'S' 
DO PEGRESP WITH [SN]

IF MC <> 'S'
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

=MENSAG('Gerando Relatorio...')
DO DEFSAIDA

USE CADWORK
 
GO TOP

VRepr   = 0
Total   = 0

TotDesc    = 0
TotDevol   = 0
TotQtdDupl = 0
TotQtdNf   = 0
Liquido    = 0
EmAberto   = 0
TotLiq     = 0
TotAberto  = 0

=ImprCabec()
DO WHILE ! EOF()
  MREGIAO = REGIAO
  DO WHILE MREGIAO = REGIAO .AND. !EOF() 
    MVEN = VEN
    VRepr = 0
    Desconto  = 0
    Devolucao = 0
    Liquido   = 0
    EmAberto = 0
    QtdDupl = 0
    QtdNF = 0
    NFAnt = 0
    GueAnt = ''
    DO WHILE MVEN = VEN .AND. MREGIAO = REGIAO .AND. !EOF()
      =ImprLinha()
      =AddLin()
      SKIP
    ENDDO
    =ImprTotVen()
    Total      = Total + VRepr
    TotQtdDupl = TotQtdDupl + QtdDupl
    TotQtdNf   = TotQtdNf + QtdNF
    TotDesc    = TotDesc + Desconto
    TotDevol   = TotDevol + Devolucao
    TotLiq     = TotLiq   + Liquido
    TotAberto  = TotAberto + EmAberto
  ENDDO 
ENDDO
=ImprTotRel()  
DO IMPSAIDA

=FINALIZA()

*-------------------------------------------------------------------------

*=================
FUNCTION IMPRLINHA
*=================
  PRIVATE ValLiq

  ValLiq = 0  
  EmAberto = 0
  IF GueAnt <> GUE 
    GueAnt = GUE
    @ Lin, 00 SAY GUE
    @ Lin, 16 SAY EST
    @ Lin, 19 SAY REGIAO PICT [9999]
    @ Lin, 26 SAY VEN    PICT [9999]
  ENDIF  
  @ Lin,  31 SAY NDUP   PICT [999999]
  @ Lin,  37 SAY '/'+ STR(PDUP,1)
  @ Lin,  43 SAY EMIS
  @ Lin,  53 SAY VENC
  @ Lin,  62 SAY PAG
  @ Lin,  71 SAY VDP    PICT [999,999.99]
  @ Lin,  84 SAY DESC   PICT [999,999.99]
  @ Lin,  97 SAY VALDEV PICT [999,999.99]
  IF ! EMPTY(CTOD(PAG)) 
    ValLiq = VDP - DESC - VALDEV
    Liquido   = Liquido + ValLiq  
    @ Lin, 110 SAY ValLiq PICT [999,999.99]  
  ELSE
    EmAberto = EmAberto + VDP
    @ Lin, 123 SAY VDP PICT [999,999.99]  
  ENDIF
  
  VRepr = VRepr + VDP
  Desconto  = Desconto + DESC
  Devolucao = Devolucao + VALDEV
  QtdDupl = QtdDupl + 1
  
  IF NfAnt <> NDUP 
    NFAnt = NDUP 
    QtdNF = QtdNf + 1
  ENDIF
    
RETURN 

*==================
FUNCTION IMPRTOTVEN
*==================
  PRIVATE TXT, Total
  
  IF Lin + 3 >= TamPag
    =ImprCabec()
  ENDIF  
  Total = Liquido + EmAberto
  @ Lin+0,  00 SAY REPLICATE('-', nCols )
  @ Lin+1,  00 SAY '   TOTAL DO REPR.:'
  @ Lin+1,  25 SAY 'QTD.NF.: ' + TRANSFORM(QtdNF, [99,999])
  @ Lin+1,  43 SAY 'QTD.DUPL.: ' + TRANSFORM(QtdDupl,[99,999])
  @ Lin+1,  69 SAY VRepr     PICT [9,999,999.99]
  @ Lin+1,  82 SAY Desconto  PICT [9,999,999.99]
  @ Lin+1,  95 SAY Devolucao PICT [9,999,999.99]
  @ Lin+1, 108 SAY Liquido   PICT [9,999,999.99]
  @ Lin+1, 121 SAY EmAberto  PICT [9,999,999.99]
  @ Lin+2, 107 SAY 'TOTAL (A + B):'
  @ Lin+2, 121 SAY Total     PICT [9,999,999.99]
  @ Lin+3,  00 SAY REPLICATE('=', nCols )
  @ Lin+4,  00
  Lin = PROW()
  
RETURN

*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT, Total
  
  IF Lin + 7 >= TamPag
    =ImprCabec()
  ENDIF  
  Total = TotLiq + TotAberto
  TXT = 'F I N A L   D O   R E L A T O R I O'
  @ Lin+1,  00 SAY REPLICATE('*', nCols )
  @ Lin+2,  00 SAY '   TOTAL GERAL...:'
  @ Lin+2,  25 SAY 'QTD.NF.: ' + TRANSFORM(TotQtdNF, [99,999])
  @ Lin+2,  43 SAY 'QTD.DUPL.: ' + TRANSFORM(TotQtdDupl,[99,999])
  @ Lin+2,  69 SAY Total     PICT [9,999,999.99]
  @ Lin+2,  82 SAY TotDesc   PICT [9,999,999.99]
  @ Lin+2,  95 SAY TotDevol  PICT [9,999,999.99]
  @ Lin+2, 108 SAY TotLiq    PICT [9,999,999.99]
  @ Lin+2, 121 SAY TotAberto PICT [9,999,999.99]
  @ Lin+3, 107 SAY 'TOTAL (A + B):'
  @ Lin+3, 121 SAY Total     PICT [9,999,999.99]
  @ Lin+4,  00 SAY REPLICATE('*', nCols )
  @ Lin+5,  00+CENTRATXT(TXT,nCols) SAY TXT
  
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
 =CABEC132( MODULO, 'Relatorio de Duplicatas Vencendo - DE: ' +;
                     DTOC(dInicio) + ' A ' + DTOC( dFim ) )
 @ 05, 00 SAY 'Nome de Guerra  UF Regiao Vend Dupl/Parc.  Emissao    Venc.    Pagto.       Valor     Desconto    Devolucao   A-Recebido  B-Em Aberto'
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

*=================
FUNCTION INICDATAS
*=================
  PRIVATE Mes, Ano, UltDia, cData
 
  Mes = MONTH(DATE()) - 1
  Ano = YEAR(DATE())
  IF Mes < 1 
    Mes = 12
    Ano = Ano - 1
  ENDIF  
  cData = '01/' + STR(Mes,2) + '/' + STR( ANO, 4 )
  dINICIO = CTOD(cData)
  UltDia  = NDIAMES(dInicio)
  cData   = STR(UltDia, 2) + '/' + STR(MES,2) + '/' + STR( ANO, 5 )
  dFIM    = CTOD(cData)

RETURN .T.

*================
FUNCTION SELECNFS
*================

  *------- Checagem das Notas Fiscais Emitidas

  =MENSAG( 'Pesquisando Notas Fiscais...' )
 
  SELECT;
    CLI.GUE, CLI.EST, NF.REGIAO, NF.VEN,;
    DUP.NDUP, DUP.PDUP, DUP.VENC, DUP.EMIS, DUP.VDP, DUP.PAG,;
    DUP.DESC, DUP.VALDEV;
  FROM;
    CABECNF NF, DUPL DUP, CLIEN CLI, TPNF TP;
  WHERE;
    (NF.CANCELADA = .F. AND TP.VENDA = 'S' AND NF.TIPNF = TP.TIPO AND;
     NF.NNF = DUP.NDUP AND NF.CGC = CLI.CGC) AND; 
    ((CTOD(PAG) >= dInicio AND CTOD(PAG) <= dFim) OR;
     (CTOD(VENC) >= dInicio AND CTOD(VENC) <= dFim AND EMPTY(CTOD(PAG))));
  ORDER BY;
    NF.REGIAO, NF.VEN, DUP.NDUP, DUP.PDUP;
  INTO TABLE;
    CADWORK

  CLOSE DATABASES

RETURN

