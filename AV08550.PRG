**********************************************************
* MODULO...: RELATORIO DE ATIVO REALIZAVEL
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 31/05/99
**********************************************************

PRIVATE Tela, Maxlin, lin, TamLinha, nCol, TotDup, TamPag

TamPag = _PLENGTH - 1
MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio de Faturamento por Per¡odo (Ativo Realiz vel)'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )

dInicio = ObDataInicio()
dFim    = ObDataFim()
nMenos  = 0
@ 08, 19 SAY 'Per¡odo de Faturamento:'
@ 10, 19 SAY ' DATA INICIAL...........:' GET dInicio
@ 12, 19 SAY ' DATA FINAL.............:' GET dFim 
READ

IF LASTKEY() = 27
  =Finaliza()
  RETURN
ENDIF   

mens = DADCORR
MC   = 'S'
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
SELE 0
USE CLIEN ORDER P_CGC

nTotValor  = 0  
nTotIpi    = 0
nTotDupls  = 0
nQtdDupls  = 0

=MENSAG('Gerando Relatorio...')
DO DEFSAIDA
SELE CADWORK
 
GO TOP
=ImprCabec()
MCGC = space(20)
DO WHILE ! EOF()
  IF MCGC <> CGC 
    =ImprLinha()
  ENDIF
  MCGC = CGC          
  MNNF = NNF
  DO WHILE ! EOF() .AND. MNNF = NNF
    nTotValor = nTotValor + VALPROD
    nTotIpi   = nTotIpi   + VALIPI
    nTotDupls = nTotDupls + VDP    
    nQtdDupls = nQtdDupls + 1
    =ImprDupl()
    =AddLin()    
    SKIP
  ENDDO
ENDDO
=ImprTotRel()  
DO IMPSAIDA

=FINALIZA()

RETURN 
*-------------------------------------------------------------------------

*=================
FUNCTION IMPRLINHA
*=================
  IF SEEK(CGC, 'CLIEN')
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
    @ Lin, 51 SAY VALPROD PICT [99,999.99]
  ENDIF
  IF VALIPI > 0  
    @ Lin, 62 SAY VALIPI PICT [99,999.99]
  ENDIF
  @ Lin, 80 SAY TIPNF  
  @ Lin, 84 SAY DESN
RETURN 

*================
FUNCTION IMPRDUPL
*================
 
  @ Lin, 106 SAY STR(NNF,6)
  @ Lin, 112 SAY '/' + STR(PDUP,1)
  @ Lin, 115 SAY VDP  PICT [99,999.99]
  @ Lin, 125 SAY VENC PICT [99/99/99]

RETURN

*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT, Media, NumDias
  
  IF Lin + 7 >= TamPag
    =ImprCabec()
  ENDIF  
  TXT = 'F I N A L   D O   R E L A T O R I O'
  @ Lin+1,  00 SAY REPLICATE('*', nCols )
  @ Lin+2,  00 SAY 'TOTAL GERAL:' 
  @ Lin+2,  15 SAY 'Qtd.Dupls.: ' + TRANSFORM(nQtdDupls, [9,999])
  @ Lin+2,  50 SAY nTotValor PICT [999,999.99]
  @ Lin+2,  61 SAY nTotIpi   PICT [999,999.99]  
  @ Lin+2, 114 SAY nTotDupls PICT [999,999.99]  
  @ Lin+5,  00 SAY REPLICATE('*', nCols )
  @ Lin+6,  00+CENTRATXT(TXT,nCols) SAY TXT
 
RETURN

*================
FUNCTION FINALIZA
*================
  RESTORE SCREEN FROM Tela
  CLOSE DATABASES
*  ERASE CADWORK.DBF
RETURN   

*=================
FUNCTION IMPRCABEC
*=================
 =CABEC132( MODULO, 'Relatorio de Faturamento (Ativo Realizavel) - DE: ' +;
                     DTOC(dInicio) + ' A ' + DTOC( dFim ) )
 @ 05, 00 SAY 'Nome de Guerra  UF Regiao  Vend      N.F.  Emissao   Valor NF    Ipi            Tipo      Descricao        Duplic.    Valor Vencim.' 
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
  
  cAno = STR(YEAR(DATE()),4,0)
  cMes = STR(MONTH(DATE()),2,0)
  dInicio = CTOD('01/' + cMes + '/' + cAno)

RETURN dInicio

*=================
FUNCTION OBDATAFIM
*=================
  PRIVATE dFim
  
  dFim = DATE()

RETURN dFim

*================
FUNCTION SELECNFS
*================

  *------- Checagem das Notas Fiscais Emitidas

  =MENSAG( 'Pesquisando Notas Fiscais...' )
 
  SELECT;
    NF.CGC, NF.DEMISSAO, NNF, DUP.PDUP, NF.VEN, NF.IDREPR, NF.REGIAO, NF.IDREGIAO, ; 
    NF.VALPROD, NF.VALIPI, NF.VALICM, NF.TIPNF, TP.DESN, ;
    DUP.VENC, DUP.VDP ;
  FROM;
    CABECNF NF, DUPL.DBF DUP, TPNF.DBF TP ;
  WHERE;
    (NF.DEMISSAO BETWEEN dInicio AND dFim) ;
    AND (CANCELADA = .F. AND EMPTY(DUP.DTCANCEL)) ;
    AND (NF.NNF = DUP.NDUP) ;
    AND (TIPNF = TP.TIPO AND TP.VENDA = 'S') ;
    AND (EMPTY(DUP.PAG) OR (DUP.PAG BETWEEN dInicio AND dFim)) ;
    AND (DUP.TIPOCART <> 'DE') ;
  ORDER BY;
    NF.NNF, DUP.PDUP;  
  INTO TABLE; 
    CADWORK
  
  CLOSE DATABASES

RETURN

