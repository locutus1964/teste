**********************************************************
* MODULO...: RELATORIO DE PRODUTOS VENDIDOS POR REGIAO
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 20/09/97
* AV05999
**********************************************************

PRIVATE Tela, Maxlin, lin, TamLinha, nCol, TotDup, TamPag

TamPag = _PLENGTH - 1
MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio de Produtos Vendidos por Regi∆o'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )
SELE 0
USE ESTADOS INDEX ESTADOS.IDX
SELE 0
USE REGIAO INDEX REGIAO.IDX
SELE 0
USE VEND INDEX VEND.IDX

dInicio = {}
dFim    = {}
MCEST   = SPACE(02)
MREGIAO = SPACE(04)
MCVR = SPACE(07)

DO WHILE .T.
  MCVR = EmptyRepr
  @ 03, 19 SAY 'Per°odo das Vendas:'
  @ 05, 19 SAY ' DATA INICIAL...........:' GET dInicio
  @ 07, 19 SAY ' DATA FINAL.............:' GET dFim 
  @ 10, 19 SAY ' UF.....................:' GET MCEST 
  @ 12, 19 SAY ' REGIAO.................:' GET MREGIAO PICT [9999]
  @ 14, 19 SAY ' REPRESENTANTE..........:' GET MCVR    PICT [9999.99];
                                           VALID MCVR = EmptyRepr .OR. CHKREPR(MCVR, .T.)
  READ
  
  IF LASTKEY() = 27
    =Finaliza()
    RETURN
  ENDIF  

  MC = 'S'
  MENS = DADCORR
  DO PEGRESP WITH [SN]
  IF MC = 'S'
    EXIT
  ENDIF  

ENDDO  

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

SELE 0
USE CADWORK

SELE CADWORK

nTotValor  = 0  
nTot5XX    = 0  &&--- Vendas no Estado
nTot6XX    = 0  &&--- Vendas Fora do Estado
nTotIpi    = 0  &&--- Total dos Valor de I.P.I.
nTotIcm    = 0  &&--- Total dos Valor de I.C.M.

=MENSAG('Gerando Relatorio...')
DO DEFSAIDA
SELE CADWORK
 
GO TOP
=ImprCabec()
DO WHILE ! EOF()
  cRegiao = REGIAO
  DO WHILE cRegiao = REGIAO .AND. ! EOF()
    cVen = VEN
    DO WHILE cVen = VEN .AND. cRegiao = REGIAO .AND. ! EOF()
      =ImprLinha()
      =AddLin()
      SKIP
    ENDDO  
  ENDDO  
  =AddLin()
ENDDO
=ImprTotRel()  
DO IMPSAIDA

=FINALIZA()

*-------------------------------------------------------------------------

*=================
FUNCTION IMPRLINHA
*=================
   @ LIN, 00 SAY EST
   @ LIN, 03 SAY REGIAO
   @ LIN, 08 SAY VEN
   @ LIN, 13 SAY REF
   @ LIN, 18 SAY DES
   @ LIN, 72 SAY SUM_QTD PICT [99,999]
RETURN 

*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT
  
  IF Lin + 7 >= TamPag
    =ImprCabec()
  ENDIF  
  TXT = 'F I N A L   D O   R E L A T O R I O'
  
  @ Lin+1,  00 SAY REPLICATE('*', nCols )
  @ Lin+5,  00 SAY REPLICATE('*', nCols )
  @ Lin+6,  00+CENTRATXT(TXT,nCols) SAY TXT
    
RETURN

*================
FUNCTION FINALIZA
*================
  RESTORE SCREEN FROM Tela
  CLOSE DATABASES
  * ERASE CADWORK.DBF
  * ERASE CADWORK.IDX
RETURN   

*=================
FUNCTION IMPRCABEC
*=================
 =CABEC132( MODULO, 'Relatorio de Produtos Vendidos por Regi∆o - DE: ' +;
                     DTOC(dInicio) + ' A ' + DTOC( dFim ) )
 @ 05, 00 SAY 'TESTE'
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
  PRIVATE cRegiao, cVend, cEstado, cGRUPO, cEmbal, cLinProd

  CLOSE DATABASES
  *cLinProd = "P.LPR='02'"
  *cGrupo   = "P.GRUPO='10'"
  *cEmbal  = "IT.EMBALAGEM='CTE'"
  
  IF EMPTY(MCEST)
    cEstado = ".T."
  ELSE
    cEstado = "CLI.EST = '" + MCEST + "'"
  ENDIF   
  
  IF EMPTY(MREGIAO)
    cRegiao = ".T."  
  ELSE 
    cRegiao = "NF.REGIAO='" + MREGIAO + "'"
  ENDIF

  IF MCVR = EmptyRepr
    cVend = ".T."  
  ELSE 
    cVend = "V.IDREPR='" + SUBSTR(MCVR,6,2) + "'"
  ENDIF

  cLinProd = ".T."
  cGrupo   = ".T."
  cEmbal   = ".T."
  
  *------- Checagem das Notas Fiscais Emitidas
  SELE 0
  USE TPNF INDEX TPNF.IDX

  SELE 0
  USE CABECNF ORDER P_NNF

  SELE CABECNF
  SET RELATION TO TIPNF INTO TPNF

  =MENSAG( 'Filtrando Notas Fiscais de Vendas...' )    

  COPY TO TMP;
       FOR (DEMISSAO >= dInicio .AND. DEMISSAO <= dFim) .AND.;
           .NOT. CANCELADA .AND.;
           TPNF->VENDA = 'S'       
   
  =MENSAG( 'Totalizando Itens...' )  
  
  CLOSE DATA
  
  SELECT;
    CLI.EST,;
    NF.REGIAO, NF.VEN,;
    P.LPR, L.DLPR, P.GRUPO,; 
    IT.EMBALAGEM, IT.REF, P.DES, SUM(IT.QTD); 
  FROM;
    TMP NF, CADNF IT, PROD P, CLIEN CLI, GRUPOS G, LIMPROD L, VEND V;
  GROUP BY;
    NF.REGIAO, NF.VEN, IT.REF;  
  HAVING;
    &cRegiao AND &cVend; 
  WHERE;
    &cEstado AND &cLinProd AND &cGrupo AND &cEmbal AND;
    (CLI.CGC = NF.CGC) AND;
    (NF.NNF = IT.NNF)  AND;
    (NF.IDREPR = V.IDREPR)   AND;
    (IT.REF = P.REF)   AND;
    (P.LPR = L.LPR)    AND;
    (P.GRUPO = G.GRUPO);  
  ORDER BY; 
    CLI.EST, NF.REGIAO, 10 DESCENDING, NF.VEN;
  INTO TABLE CADWORK
  
  CLOSE DATABASES
  ERASE TMP.DBF
  
&&  CLI.EST, NF.REGIAO, NF.VEN, L.LPR, P.GRUPO, IT.EMBALAGEM;
RETURN
