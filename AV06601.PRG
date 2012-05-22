**********************************************************
* MODULO...: RELATORIO DE EMBARQUE DE NOTAS FISCAIS
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 22/06/97
***********************************************************

PRIVATE cTela, Maxlin, ContDupl, lin, tamlinha, TotCli, NCols, TamPag,;
        dInicio, dFim, dIniAnt, dFimAnt, TotPed, TotNota, TotGeral, MREGIAO, MCVR

TamPag = _PLENGTH - 1
NCOLS  = 134
LPONT  = REPLICATE('-',NCOLS)
LPONTD = REPLICATE('=',NCOLS)
MODULO=PROGRAM()
SAVE SCREEN TO cTela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio de Embarque de Notas Fiscais'
@ 01, Centratxt(MSG,80) SAY MSG

dInicio = {}
dFim    = {}
dIniAnt = {}
dFimAnt = {}
=InicDatas()
MIDREGIAO = 0
MRegiao   = EmptyRegiao
MIDREPR   = 0
RESUMIDO  = 'N'
DO WHILE .T.
  =MENSAG('<ESC> para sair')
  =AbreDbfs()
  mcvr  = EmptyRepr
  @ 04, 25 SAY 'Per°odo de Emiss∆o dos Pedidos (Atendidos):'
  @ 06, 25 SAY ' DATA INICIAL:' GET dInicio VALID ChkInicio()
  @ 07, 25 SAY ' DATA FINAL..:' GET dFim
  @ 09, 25 SAY 'Per°odo de Emiss∆o dos Pedidos (N∆o Atendidos):'
  @ 11, 25 SAY ' DATA INICIAL:' GET dIniAnt
  @ 12, 25 SAY ' DATA FINAL..:' GET dFimAnt
  @ 14, 25 SAY 'REPRES.......:' GET MCVR PICT [9999.99] ;
                                VALID MCVR = EmptyRepr .OR. ChkRepr(MCVR, .T.)
  @ 15, 25 SAY '              * Deixe em Branco para selecionar todos'
  @ 16, 25 SAY 'RESUMIDO S/N?' GET RESUMIDO PICT [@!] VALID RESUMIDO $ [SN]
  READ

  IF LASTKEY() = 27
    =Finaliza()
    EXIT
  ENDIF   
  
  MC = ''
  MENS = DADCORR
  DO pegresp WITH [SN]
  IF MC = 'N'
    CLOSE DATA
    LOOP
  ENDIF  
  
  *--- Seleciona os Pedidos
  =SelecPedidos()

  lin = 0
  PAGCTR = 0
  tamrel = 2
  tamlinha = 80

  *--- Seleciona Dispositivo de Saida

  DO TESTIMP2
  IF cp = 'N' 
    =Finaliza()
    EXIT
  ENDIF   

  =MENSAG( 'Preparando Relat¢rio...')
  SET CENTURY OFF
  *--- Arquivo de Trabalho
  SELE 0 
  USE CLIEN ORDER P_CGC
  SELE 0
  USE REGIAO ORDER X_REGIAO
  SELE 0
  USE VEND  ORDER X_VEND
  SELE 0
  USE TRANS ORDER P_TRA
                               
  ContCli     = 0  && Contador de Clientes
  CliRep      = 0  && Clientes do Representante
  TotGeral    = 0  && Total Geral
  TotPed      = 0  && Total dos Pedidos
  TotNota     = 0  && Total em Notas Fiscais
  TotPedRep   = 0  && Total de Pedidos do Repr.
  TotNotaRep  = 0  && Total em Notas Fiscais Repr.
  TotPedReg   = 0  && Total em NF por Regiao
  TotNotaReg  = 0  && Total em Pedidos por Regiao
 
  DO DEFSAIDA

  IF RESUMIDO = 'N'
    *--- Relatorio Analitico
    =ImprRelAna()
  ELSE  
    *--- Relatorio Sintetico
    =ImprRelSin()
  ENDIF  

  DO IMPSAIDA
  SET CENTURY ON
    
ENDDO

=FINALIZA()

RETURN 

**=======================================================================
*                  R E L A T O R I O   A N A L I T I C O
**=======================================================================

*==================
FUNCTION IMPRRELANA
*==================

  ContCli     = 0  
  CliRep      = 0  
  TotGeral    = 0  
  TotPed      = 0  
  TotNota     = 0  
  SELE 0
  USE WORK  
  INDEX ON FmtRepr(VEN, IDREPR) + FmtRegiao(REGIAO, IDREGIAO) TO WORK.IDX
  SET RELATION TO TRA INTO TRANS,;
                  CGC INTO CLIEN

  SELE WORK
  GO TOP
  DO WHILE ! EOF()
    MVEN       = FmtRepr(VEN, IDREPR)
    MREGIAO    = FmtRegiao(REGIAO, IDREGIAO)
    =ImprCabec()    
    CliRep     = 0  
    TotPedRep  = 0
    TotNotaRep = 0
    n = 0
    lFirst = .T.
    DO WHILE ! EOF() .AND. MVEN = FMTREPR(VEN,IDREPR)
      MREGIAO = FmtRegiao(REGIAO, IDREGIAO)
      IF ! lFirst
        =ImpCabRep()
      ENDIF  
      lFirst     = .F.
      TotPedReg  = 0  
      TotNotaReg = 0  
      DO WHILE ! EOF() .AND. MVEN = FMTREPR(VEN,IDREPR) .AND. MREGIAO = FmtRegiao(REGIAO, IDREGIAO)
        =ImprLinha()
        SKIP
      ENDDO
      TotNotaRep = TotNotaRep + TotNotaReg
      TotPedRep  = TotPedRep + TotPedReg
      *=ImpRepAna()
    ENDDO
    TotNota = TotNota + TotNotaRep
    TotPed  = TotPed + TotPedRep 
    =ImprResVen( TotPedRep, TotNotaRep, CliRep, '=' )
  ENDDO  
  *-------- Final do Relatorio
  *Lin = 00  &&-- Resumo de Relatorio em uma nova pagina
  *=ImprResVen( TotPed, TotNota, ContCli, '*' )
  =ImprRodape()
  
RETURN

*=================
FUNCTION IMPRLINHA
*=================
  PRIVATE VALNF, VALPED, cPed
  
  IF NNF <> 0
    VALPED = 0
    VALNF = WORK->TOTNF
  ELSE  
    VALPED = TOTPED
    VALNF = 0
  ENDIF 

  IF Lin + 2 >= TamPag
    =ImprCabec()
  ENDIF  

  &&---- Primeira Linha
  @ Lin+0, 000 SAY CLIEN->GUE
  @ Lin+0, 016 SAY CLIEN->EST
  @ Lin+0, 019 SAY '||' 
  @ Lin+0, 022 SAY PINT
  @ Lin+0, 032 SAY '|' 
  IF ! EMPTY( DTPEDREP) 
    @ Lin+0, 034 SAY DTPEDREP
  ENDIF  
  @ Lin+0, 043 SAY '|' 
  @ Lin+0, 045 SAY den 
  @ Lin+0, 054 SAY '|' 
  IF VALPED > 0
    @ Lin+0, 056 SAY VALPED PICT [999,999.99]
  ENDIF
  @ Lin+0, 067 SAY '||' 
  IF VALNF > 0  
    @ Lin+0, 071 SAY NNF PICT [999999]
    @ Lin+0, 079 SAY '|' 
    @ Lin+0, 081 SAY DEMISSAO
    @ Lin+0, 090 SAY '|' 
    @ Lin+0, 092 SAY VALNF PICT [999,999.99]
    @ Lin+0, 103 SAY '||' 
    @ Lin+0, 106 SAY TRANS->GUETRA
    @ Lin+0, 123 SAY '|' 
    @ Lin+0, 125 SAY DTENTREG
    @ Lin+0, 133 SAY '|' 
  ELSE  
    @ Lin+0, 079 SAY '|' 
    @ Lin+0, 090 SAY '|' 
    @ Lin+0, 103 SAY '||' 
    @ Lin+0, 123 SAY '|' 
    @ Lin+0, 133 SAY '|' 
  ENDIF 

  &&---- Segunda Linha 
  cPed = PADR( ALLTRIM(STR(PED,6)), 9, ' ')
  @ Lin+1, 019 SAY '||' 
  @ Lin+1, 022 SAY cPed
  @ Lin+1, 032 SAY '|' 
  IF ! EMPTY( DEM )
     @ Lin+1, 034 SAY DEM PICT [99/99/99]
  ENDIF   

  @ Lin+1, 043 SAY '|' 
  @ Lin+1, 054 SAY '|' 
  @ Lin+1, 067 SAY '||' 
  @ Lin+1, 079 SAY '|'   
  @ Lin+1, 090 SAY '|'   
  @ Lin+1, 103 SAY '||' 
  @ Lin+1, 106 SAY NCOLETA
  @ Lin+1, 123 SAY '|' 
  @ Lin+1, 133 SAY '|' 
  TotPedReg  = TotPedReg + VALPED
  TotNotaReg = TotNotaReg + VALNF
  ContCli = ContCli + 1
  CliRep  = CliRep + 1  
  Lin = Lin + 2      
  
RETURN 

*=================  
FUNCTION IMPREPANA
*=================

  IF Lin + 2 >= TamPag
    =ImprCabec()
  ENDIF   
  *@ Lin+0, 00 SAY LPONT
  @ Lin+0, 32 SAY 'Tot.Pedidos: ' + TRANSFORM( TotPedReg, [999,999.99] )
  @ Lin+0, 69 SAY 'Tot.Notas: ' + TRANSFORM( TotNotaReg, [999,999.99] )
  @ Lin+1, 00 SAY LPONT
  @ Lin+2, 00
  Lin = PROW()
  
RETURN

*=================
FUNCTION IMPRCABEC
*=================
  PRIVATE cTitulo
  
  =CHKREPR(MVEN, .F.)
  =CHKREGIAO(MREGIAO, .F.)
  cTitulo = 'Relatorio de Situacao de Pedidos - Analitico'
  cTitulo = cTitulo + ' - Periodo: ' + DTOC( dInicio ) + ' A ' + DTOC( dFim )
  =CABEC132( MODULO,  cTitulo )
  @ 05, 00 SAY ' C L I E N T E     ||                P E D I D O S                 || N O T A S  F I S C A I S         ||      E M B A R Q U E       |' 
  @ 06, 00 SAY 'Nome de Guerra  UF ||   No Rep. | Data Rep.|Dt.Entrega|            ||          |          |            ||  Transportadora  |         |' 
  @ 07, 00 SAY '                   ||   No CF   | Data CF  |c/Cliente |   Valor    || No Nota  | Emissao  |   Valor    ||   No Coleta      |  Data   |'
  @ 08, 00 SAY LPONT
  @ 09, 00 SAY 'REPR: '   + FMTREPR(WORK->VEN, WORK->IDREPR) + ' - ' + VEND->RAZ 
  @ 09, 62 SAY 'REGIAO: ' + FmtRegiao(WORK->REGIAO, WORK->IDREGIAO) + ' - ' + REGIAO->DREGIAO
  @ 10, 00 SAY LPONTD
  @ 11, 00
  LIN = PROW()
RETURN

*=================
FUNCTION IMPCABREP
*=================
  PRIVATE cTitulo

  =CHKREPR(MVEN, .F.)
  =CHKREGIAO(MREGIAO, .F.)
  @ Lin+01, 00 SAY LPONT
  @ Lin+02, 00 SAY 'REPR: ' + FMTREPR(WORK->VEN,WORK->IDREPR) + ' - ' + VEND->RAZ 
  @ Lin+02, 62 SAY 'REGIAO: ' + FmtRegiao(WORK->REGIAO, WORK->IDREGIAO) + ' - ' + REGIAO->DREGIAO
  @ Lin+03, 00 SAY LPONTD
  @ Lin+04, 00
  Lin = Lin + 5
  
RETURN

*==============
FUNCTION ADDLIN
*==============

  Lin = Lin + 1 
  IF Lin >= TamPag
    =ImprCabec()
  ENDIF   

RETURN

*=======================================================================
*                  R E L A T O R I O    S I N T E T I C O
*=======================================================================

*==================
FUNCTION IMPRRELSIN
*==================

  ContCli     = 0  
  CliRep      = 0  
  TotGeral    = 0  
  nTotPed     = 0  
  nTotNota    = 0  
  TotPedRep   = 0  
  TotNotaRep  = 0  
  TotPedReg   = 0  
  TotNotaReg  = 0  
  SELE 0
  USE TOTALNF
  SELE 0
  USE WORK  
  INDEX ON FmtRepr(VEN,IDREPR)+FmtRegiao(REGIAO,IDREGIAO) TO WORK.IDX
  SET RELATION TO TRA      INTO TRANS,;
                  CGC      INTO CLIEN
  SELE WORK                  
  GO TOP
  ContCli = 0
  DO WHILE ! EOF()
    MVEN = FmtRepr(VEN,IDREPR)
    =ImprCabSin()
    CliRep = 0
    TotPedRep  = 0
    TotNotaRep = 0
    DO WHILE ! EOF() .AND. MVEN = FmtRepr(VEN, IDREPR)
      MREGIAO  = FmtRegiao(REGIAO, IDREGIAO)
      MDREGIAO = REGIAO->DREGIAO
      MVENGUE  = VEND->VENGUE
      TotPedReg  = 0
      TotNotaReg = 0
      DO WHILE ! EOF() .AND. MVEN = FmtRepr(VEN, IDREPR) .AND. FmtRegiao(REGIAO, IDREGIAO)
        *-- Acumula a Regiao 
        IF NNF <> 0
          TotNotaReg = TotNotaReg + TOTNF   &&-- Pedido Atendido
        ELSE  
          TotPedReg = TotPedReg + TOTPED      &&-- Pedido nao atendido
        ENDIF 
        ContCli = ContCli + 1
        CliRep  = CliRep + 1  
        SKIP
      ENDDO
      *--- Acumula o Representante
      TotPedRep  = TotPedRep + TotPedReg
      TotNotaRep = TotNotaRep + TotNotaReg
      IF Lin + 6 >= TamPag
        =ImprCabSin()
      ENDIF  
      =ImpLinSin()
    ENDDO
    *--- Acumula Geral
    nTotPed = nTotPed + TotPedRep
    nTotNota = nTotNota + TotNotaRep
    =ImpRepSin()
  ENDDO  
  *------ Final do Relatorio
  =ImprResVen( nTotPed, nTotNota, ContCli, '*' )
  =ImprRodape()
RETURN 
  
*==================
FUNCTION IMPRCABSIN
*==================
  PRIVATE cTitulo
  =CHKREPR(MVEN, .F.)
  =CHKREGIAO(MREGIAO, .F.)
  cTitulo = 'Relatorio de Situacao de Pedidos - Sintetico'
  cTitulo = cTitulo + ' - Periodo: ' + DTOC( dInicio ) + ' A ' + DTOC( dFim )
  =CABEC132( MODULO,  cTitulo )
  @ 05, 00 SAY 'Repr Nome de Guerra  Regiao      Nome da Regiao            || Valor Pedidos   ||      Valor Notas'
  @ 06, 00 SAY LPONTD
  @ 07, 00
  LIN = PROW()
RETURN

*=================
FUNCTION IMPLINSIN
*=================

  @ Lin+0, 000 SAY FMTREPR(MVEN,MIDREPR)
  @ Lin+0, 005 SAY MVENGUE
  @ Lin+0, 021 SAY MREGIAO
  @ Lin+0, 029 SAY MDREGIAO  
  @ Lin+0, 059 SAY '||' 
  @ Lin+0, 064 SAY TotPedReg  PICT [999,999.99]
  @ Lin+0, 078 SAY '||' 
  @ Lin+0, 088 SAY TotNotaReg PICT [999,999.99]
  @ Lin+1, 00
  Lin = PROW()
  IF Lin >= TamPag
    =ImprCabSin()
  ENDIF  
  
RETURN 

*=================
FUNCTION IMPREPSIN
*=================
  PRIVATE nTotal, nPorcPed, nPorcNF
  
  IF Lin + 5 >= TamPag
    =ImprCabSin()
  ENDIF   
  
  nTotal = TotPedRep + TotNotaRep
  @ Lin+0,  00 SAY LPONT
  @ Lin+1,  51 SAY 'Tot.Pedidos: ' + TRANSFORM( TotPedRep, [999,999.99] )
  @ Lin+1,  77 SAY 'Tot.Notas: ' + TRANSFORM( TotNotaRep, [999,999.99] )
  @ Lin+1, 104 SAY 'Tot.Pedidos+Notas: ' + TRANSFORM( nTotal, [999,999.99] )
  @ Lin+2,  00 SAY TotNotaRep / TOTALNF->TOTALNF * 100 PICT [999.9 % do Total das Vendas]
  
  IF nTotal > 0
    nPorcPed = TotPedRep  / nTotal * 100
    nPorcNF  = TotNotaRep / nTotal * 100
  ELSE
    nPorcPed = 0   
    nPorcNF  = 0
  ENDIF   
  
  @ Lin+2,  68 SAY nPorcPed         PICT [999.9 %]
  @ Lin+2,  92 SAY nPorcNF          PICT [999.9 %]
  @ Lin+2, 127 SAY nPorcPed+nPorcNF PICT [999.9 %]
  @ Lin+3,  00 SAY LPONTD
  @ Lin+4,  00
  Lin = PROW()
  
RETURN

*-----------------------------------------------------------------------
**----- Funcoes Auxiliares

*==================
FUNCTION IMPRRESVEN
*==================
PARAMETER nValPed, nValNota, nCli, Caract
  PRIVATE Total
  
  Total = nValPed + nValNota
  IF (Lin + 4) >= TamPag
    =ImprCabec()
  ENDIF    
  
  @ Lin+0,  00 SAY REPLICATE( Caract, NCOLS )
  @ Lin+1,  00 SAY 'Qtd.Clientes: ' + STR( nCli, 3 )
  @ Lin+1,  19 SAY '||         Em Carteira Acumulado ==> ' + TRANSFORM( nValPed, [999,999.99] )
  @ Lin+1,  67 SAY '|| Faturamento Acum. ==> ' + TRANSFORM( nValNota, [999,999.99] ) 
  @ Lin+1, 103 SAY '||Carteira+Fatura==> ' + TRANSFORM( Total, [999,999.99] )
  @ Lin+2,  19 SAY '||'
  @ Lin+2,  60 SAY ROUND(nValNota / Total * 100, 1) PICT [999.9%]
  @ Lin+2,  67 SAY '||'
  @ Lin+2,  96 SAY ROUND(nValPed / Total * 100, 1) PICT [999.9%]
  @ Lin+2, 103 SAY '||'
  @ Lin+2, 128 SAY ROUND((nValPed+nValNota) / Total * 100, 1) PICT [999.9%] 
  @ Lin+3,  00 SAY REPLICATE( Caract, NCOLS )
  @ Lin+5,  00
  Lin = PROW()  
  
RETURN

*=================
FUNCTION INICDATAS
*=================
  PRIVATE Mes, Ano, UltDia, cData, cHoje
 
  cHoje   = DTOC(HOJE)
  dInicio = CTOD('01/' + SUBSTR(cHoje,4,2) + '/' + SUBSTR(cHoje,7,4 ))
  dFim    = CTOD(cHoje)
  Mes     = MONTH(HOJE) - 1
  Ano     = YEAR(HOJE)
  IF Mes < 1 
    Mes = 12
    Ano = Ano - 1
  ENDIF  
  cData   = '01/' + STR(Mes,2) + '/' + STR( ANO, 4 )
  dIniAnt = CTOD(cData)
  UltDia  = NDIAMES(dInicio)
  cData   = STR(UltDia, 2) + '/' + STR(MES,2) + '/' + STR( ANO, 5 )
  dFimAnt = CTOD(cData)

RETURN 

*====================
FUNCTION SELECPEDIDOS
*====================
  PRIVATE COND 

  *------- Inicializa o Filtro
  COND = '.T.'
  IF MCVR <> EmptyRepr
    COND = 'VEN= "' + SUBSTR(MCVR, 1, 4) + '" AND IDREPR=' + SUBSTR(MCVR,6,2)
  ENDIF    
  
  *------- Preparando as Tabelas 
  =MENSAG('Pesquisando Pedidos do Per°odo...')
  USE PEDIC
  COPY FIELDS CGC, PED, IDREPR, VEN, IDREGIAO, REGIAO, DEM, DEN, PINT, DTPEDREP, VALPED, VALIPI; 
     TO PEDTMP1;
     FOR (DEM >= dIniAnt .AND. DEM <= dFim) AND (&COND = .T.)
  
  =MENSAG('Pesquisando Pedidos Atendidos...')
  CLOSE DATA
  SELECT;
    PED.CGC, PED.DEM, PED.DEN, PED.PED, (PED.VALPED + PED.VALIPI) AS TOTPED,;
    PED.IDREPR, PED.VEN, PED.IDREGIAO, PED.REGIAO, PED.PINT, PED.DTPEDREP,;
    NF.NNF, (NF.VALPROD + NF.VALIPI) AS TOTNF, DEMISSAO, DTENTREG, NCOLETA, TRA;
  FROM;
    PEDTMP1 PED, CABECNF NF;
  WHERE;
    (NF.DEMISSAO BETWEEN dInicio AND dFim) AND (PED.PED = NF.PED) AND (NF.CANCELADA = .F.);
  ORDER BY;
    PED.PED;
  INTO TABLE WORK
   
  CLOSE DATA
  
  =MENSAG( 'Pesquisando Pedidos N∆o Atendidos...' ) 
  
  &&--- Seleciona Pedidos do Periodo
  SELECT 0
  USE PEDTMP1
  SELECT 0
  USE WORK
  SELECT 0
  USE CABECNF ORDER F_PED
  
  && Relaciona os Pedidos do Periodo com as Notas Fiscais
  SELECT PEDTMP1
  SET RELATION TO PED INTO CABECNF
  GO TOP
  DO WHILE ! EOF()
    IF ! FOUND('CABECNF') .AND. (DEM >= dIniAnt .AND. DEM <= dFimAnt)
      SELECT WORK
      APPEND BLANK
      REPLACE CGC      WITH PEDTMP1->CGC
      REPLACE PED      WITH PEDTMP1->PED
      REPLACE IDREPR   WITH PEDTMP1->IDREPR
      REPLACE VEN      WITH PEDTMP1->VEN 
      REPLACE IDREGIAO WITH PEDTMP1->IDREGIAO
      REPLACE REGIAO   WITH PEDTMP1->REGIAO
      REPLACE DEM      WITH PEDTMP1->DEM
      REPLACE PINT     WITH PEDTMP1->PINT
      REPLACE DTPEDREP WITH PEDTMP1->DTPEDREP   
      REPLACE TOTPED   WITH PEDTMP1->VALPED + PEDTMP1->VALIPI 
    ENDIF
    SELECT PEDTMP1
    SKIP
  ENDDO

  CLOSE DATA
  SELECT SUM(TOTNF) AS TOTALNF FROM WORK INTO TABLE TOTALNF
  CLOSE DATA
          
RETURN

*================
FUNCTION ABREDBFS
*================

  SELE 0
  USE REGIAO ORDER P_REGIAO
  SELE 0
  USE VEND ORDER X_VEND
  
RETURN 

*================
FUNCTION FINALIZA
*================
  RESTORE SCREEN FROM cTela
  CLOSE DATABASES
* ERASE WORK.DBF
RETURN   

*==================
FUNCTION IMPRRODAPE
*==================
  PRIVATE TXT

  TXT = 'F I N A L   D O   R E L A T O R I O'
  @ Lin+1, 00 SAY REPLICATE( '=', NCOLS )
  @ Lin+2, 00+CENTRATXT(TXT,NCOLS) SAY TXT
  
RETURN

* CABEC132 - IMPRIME LINHA BASICA DO CABECALHO DOS RELATORIOS (132 COLUNAS)
*
*----------------
FUNCTION CABEC132
PARAMETERS Modulo,Titulo
*----------------
  PAGCTR = PAGCTR + 1
  @ 00, 00 SAY CHR(15)
  @ 01, 00 SAY REPLICATE('=',NCOLS)
  @ 02, 00 SAY MREL
  @ 02, 26 SAY '|'
  @ 02, 26+CentraTxt(Titulo,113-26+1) SAY Titulo
  @ 02,113 SAY '|'
  @ 02,116 SAY TIME()
  @ 02,125 SAY HOJE
  @ 03, 00 SAY Modulo
  @ 03, 26 SAY '|'
  @ 03,113 SAY '|'
  @ 03,124 SAY 'PAG: ' + STR(PAGCTR,3)
  @ 04, 00 SAY REPLICATE('-',NCOLS)
 
RETURN

******************
FUNCTION CHKINICIO
******************
  IF dInicio < dIniAnt
    dIniAnt = dInicio
  ENDIF  
RETURN .T.


* C L I E N T E     ||                P E D I D O S                 || N O T A S  F I S C A I S         ||      E M B A R Q U E        | 
*Nome de Guerra  UF ||   No Rep. | Data Rep.|Dt.Entrega|            ||          |          |            ||  Transportadora  |          | 
*                   ||   No CF   | Data CF  |c/Cliente |   Valor    || No Nota  | Emissao  |   Valor    ||   No Coleta      |  Data    |
*XXXXXXXXXXXXXXX XX || XXXXXXXXX | xx/xx/xx | xx/xx/xx | 999,999.99 ||  999999  | XX/XX/XX | 999,999.99 || XXXXXXXXXXXXXXXX | XX/XX/XX | 
*00              16 19 22        3234       4345       5456         67  71      7981       9092         103106            123 125      134

*Repr Nome de Guerra  Regiao      Nome da Regiao            || Valor Pedidos   ||      Valor Notas
*9999 XXXXXXXXXXXXXXX 9999   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ||   999,999.99    ||        999,999.99
*0    5               21     28                             59   64            78        88                
