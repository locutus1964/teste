**********************************************************
* MODULO...: RELATORIO DE EMBARQUE DE NOTAS FISCAIS
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 22/06/97
***********************************************************

PRIVATE Tela, Maxlin, ContDupl, lin, tamlinha, TotCli, NCols
PRIVATE dInicio, dFim, TotPed, TotNota, TotGeral, MREGIAO, MCVR, TamPag

TamPag = _PLENGTH - 1
NCOLS = 132
LPONT  = REPLICATE('-',NCOLS)
LPONTD = REPLICATE('=',NCOLS)
MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio de Embarque de Notas Fiscais'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )
dInicio = {}
dFim    = {}
=InicDatas()
=AbreDbfs()
mRegiao = SPACE(4)
mcvr = SPACE(7)

@ 05, 25 SAY 'Per¡odo de EmissÆo dos Pedidos:'
@ 08, 25 SAY 'DATA INICIAL:' GET dInicio
@ 10, 25 SAY 'DATA FINAL..:' GET dFim
@ 12, 25 SAY 'REGIÇO......:' GET MRegiao PICT [9999];
                              VALID MREGIAO = SPACE(4) .OR. ChkRegiao(MREGIAO, .T.)
@ 13, 25 SAY '              * Deixe em Branco para selecionar todas'                              
@ 14, 25 SAY 'REPRES......:' GET MCVR PICT [9999.99];
                              VALID MCVR = EmptyRepr .OR. ChkRepr(MCVR, .T.)
@ 15, 25 SAY '              * Deixe em Branco para selecionar todos'
READ
IF LASTKEY() = 27
  =Finaliza()
  RETURN
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
  RETURN
ENDIF   

=MENSAG( 'Preparando Relat¢rio...')

*--- Arquivo de Trabalho
SELE 0
USE TRANS INDEX TRANS

SELE 0
USE CABECNF 
INDEX ON PED TO TMP

SELE 0
USE WORK  
INDEX ON VEN+REGIAO+DTOS(CTOD(DEM)) TO WORK

SELE CABECNF
SET RELATION TO TRA INTO TRANS

SELE WORK
SET RELATION TO PED INTO CABECNF
                                
GO TOP

ContCli     = 0  && Contador de Clientes
CliRep      = 0  && Clientes do Representante
TotGeral    = 0  && Total Geral
TotPed      = 0  && Total dos Pedidos
TotNota     = 0  && Total em Notas Fiscais
TotPedRep   = 0  && Total de Pedidos do Repr.
TotNotaRep  = 0  && Total em Notas Fiscais Repr

DO DEFSAIDA
DO WHILE ! EOF()
  =ImprCabec()
  CliRep     = 0  
  TotPedRep  = 0  
  TotNotaRep = 0  
  MVEN = VEN
  DO WHILE ! EOF() .AND. MVEN = VEN
    =ImprLinha()
    SKIP
  ENDDO
  =ImprTotRep()
ENDDO  
*-------- Final do Relatorio
=ImprTotRel()
DO IMPSAIDA

=FINALIZA()

*-------------------------------------------------------------------------
*================
FUNCTION FINALIZA
*================
  RESTORE SCREEN FROM Tela
  CLOSE DATABASES
  *ERASE WORK.DBF
  *ERASE WORK1.DBF
  *ERASE TMP.IDX
RETURN   

*=================
FUNCTION IMPRCABEC
*=================

  =CABEC132( MODULO, 'Relatorio de Situa‡Æo de Pedidos' )
  @ 05, 00 SAY ' C L I E N T E     ||       P  E  D  I  D  O  S         ||   N O T A S  F I S C A I S      ||      E M B A R Q U E        |' 
  @ 06, 00 SAY '                   || No Rep.   | Data Rep.|            ||         |          |            ||  Transportadora  |          |' 
  @ 07, 00 SAY 'Nome de Guerra  UF || No CF     | Data CF  |   Valor    || No Nota | Emissao  |   Valor    ||   No Coleta      |  Data    |'
  @ 08, 00 SAY LPONT
  @ 09, 00 SAY 'REPR: ' + WORK->VEN + ' - ' + WORK->RAZ 
  @ 09, 59 SAY 'REGIAO: ' + WORK->REGIAO + ' - ' + WORK->DREGIAO
  @ 10, 00 SAY REPLICATE('=',NCOLS)
  @ 11, 00
  LIN = PROW()
 
RETURN  

* C L I E N T E     ||       P  E  D  I  D  O  S         ||   N O T A S  F I S C A I S      ||      E M B A R Q U E        |' 
*                   || No Rep.   | Data Rep.|            ||         |          |            ||  Transportadora  |          |' 
*Nome de Guerra  UF || No CF     | Data CF  |   Valor    || No Nota | Emissao  |   Valor    ||   No Coleta      |  Data    |'
*XXXXXXXXXXXXXX  XX || XXXXXXXXX | xx/xx/xx | 999,999.99 || 999999  | XX/XX/XX | 999,999.99 || XXXXXXXXXXXXXXXX | XX/XX/XX |' 
*00              16 19 22        3234       4345         56 59      6769       7880         91 94             111 113      |' 

*=================
FUNCTION IMPRLINHA
*=================
  PRIVATE VALNF, VALPED, cPed
  
  IF FOUND( 'CABECNF' ) .AND. .NOT. CABECNF->CANCELADA
    VALPED = 0
    VALNF = CABECNF->VALPROD + CABECNF->VALIPI 
  ELSE  
    VALPED = EXP_7
    VALNF = 0
  ENDIF 
  IF Lin + 2 >= TamPag
    =ImprCabec()
  ENDIF  
  &&---- Primeira Linha
  @ Lin+1, 000 SAY GUE
  @ Lin+1, 016 SAY EST
  @ Lin+1, 019 SAY '||' 
  @ Lin+1, 022 SAY PINT
  @ Lin+1, 032 SAY '|' 
  @ Lin+1, 034 SAY DTPEDREP
  @ Lin+1, 043 SAY '|' 
  @ Lin+1, 045 SAY VALPED PICT [999,999.99]
  @ Lin+1, 056 SAY '||' 
  IF VALNF > 0  
    @ Lin+1, 059 SAY CABECNF->NNF PICT [999999]
    @ Lin+1, 067 SAY '|' 
    @ Lin+1, 069 SAY CABECNF->DEMISSAO
    @ Lin+1, 078 SAY '|' 
    @ Lin+1, 080 SAY VALNF PICT [999,999.99]
    @ Lin+1, 091 SAY '||' 
    @ Lin+1, 094 SAY TRANS->GUETRA
    @ Lin+1, 111 SAY '|' 
    @ Lin+1, 113 SAY CABECNF->DTENTREG
    @ Lin+1, 122 SAY '|' 
  ELSE  
    @ Lin+1, 067 SAY '|' 
    @ Lin+1, 078 SAY '|' 
    @ Lin+1, 091 SAY '||' 
    @ Lin+1, 111 SAY '|' 
    @ Lin+1, 122 SAY '|' 
  ENDIF 
  &&---- Segunda Linha 
  cPed = PADR( ALLTRIM(STR(PED,6)), 9, ' ')
  @ Lin+2, 019 SAY '||' 
  @ Lin+2, 022 SAY cPed
  @ Lin+2, 032 SAY '|' 
  @ Lin+2, 034 SAY DEM PICT [99/99/99]
  @ Lin+2, 043 SAY '|' 
  @ Lin+2, 056 SAY '||' 
  @ Lin+2, 067 SAY '|'   
  @ Lin+2, 078 SAY '|'   
  @ Lin+2, 091 SAY '||' 
  @ Lin+2, 094 SAY CABECNF->NCOLETA
  @ Lin+2, 111 SAY '|' 
  @ Lin+2, 122 SAY '|' 
      
  TotNota    = TotNota + VALNF
  TotNotaRep = TotNotaRep + VALNF
  TotPed     = TotPed + VALPED
  TotPedRep  = TotPedRep + VALPED
  ContCli = ContCli + 1
  CliRep  = CliRep + 1  
  Lin = Lin + 2
RETURN 

*================
FUNCTION CABECCLI
*================
  
  * Se o Cabecalho nao couber nesta pagina, imprime na proxima pagina
  IF Lin + 6 >= TamPag
    =ImprCabec()
  ENDIF  

RETURN  

*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT, Total
*Tot.Clientes:   2                    |      ||                                     |       ||                                 |
*Em Carteira Acumulado ==>       0,00 |  0,0%|| Faturamento Acumulado ==>   1.156,56| 100,0%|| Carteira + Fatura ==>   1.156,56|100,0%
*                                     37     44                                     83      91                                 126  
  TXT = 'F I N A L   D O   R E L A T O R I O'
  Total = TotPed + TotNota
  IF Lin + 5 >= TamPag
    =ImprCabec()
  ENDIF    
  @ Lin+0, 00 SAY REPLICATE( '*', NCOLS )
  @ Lin+1, 00 SAY 'Tot.Clientes: ' + STR( ContCli, 3 )
  @ Lin+1, 37 SAY '|'
  @ Lin+1, 44 SAY '||'
  @ Lin+1, 83 SAY '|'
  @ Lin+1, 91 SAY '||'
  @ Lin+2, 00 SAY 'Em Carteira Acumulado ==> ' + TRANSFORM( TotPed, [999,999.99] )
  @ Lin+2, 37 SAY '|' + TRANSFORM( ROUND(TotPed / Total * 100, 1), [999.9%] )
  @ Lin+2, 44 SAY '|| Faturamento Acumulado ==> ' + TRANSFORM( TotNota, [999,999.99] ) 
  @ Lin+2, 83 SAY '|' + TRANSFORM( ROUND(TotNota / Total * 100, 1), [999.9%] )
  @ Lin+2, 91 SAY '|| Carteira + Fatura ==> ' +;
                  TRANSFORM( Total, [999,999.99] ) + '| 100,0%'
  @ Lin+3, 00 SAY REPLICATE( '*', NCOLS )
  @ Lin+4, 02+CENTRATXT(TXT,NCOLS) SAY TXT

  
RETURN

*==================
FUNCTION IMPRTOTREP
*==================

  IF Lin + 3 >= TamPag
    =ImprCabec()
  ENDIF   
  @ Lin+1, 00 SAY LPONT
  @ Lin+2, 32 SAY 'Tot.Pedidos: ' + TRANSFORM( TotPedRep, [999,999.99] )
  @ Lin+2, 69 SAY 'Tot.Notas: ' + TRANSFORM( TotNotaRep, [999,999.99] )
  @ Lin+3, 00 SAY LPONT
  Lin = Lin + 4
  @ Lin, 00

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

RETURN 

*====================
FUNCTION SELECPEDIDOS
*====================
  PRIVATE COND
  
  *------- Preparando as Tabelas 
  =MENSAG( 'Pesquisando Pedidos...' )
  
  COND = '.T.'
  IF MREGIAO <> SPACE(4) 
    COND = 'PED.REGIAO = MREGIAO'
  ENDIF  
  IF ! EMPTY(COND)
    IF MCVR <> SPACE(4)
      COND = COND + '.AND. PED.VEN=MCVR'
    ENDIF   
  ELSE
    IF MCVR <> SPACE(4)
      COND = 'PED.VEN=MCVR'
    ENDIF
  ENDIF  
    
  SELECT;
    CLI.GUE, CLI.EST, PED, PINT, DEM, DtPedRep, (ValPed + ValIpi),;
    PED.REGIAO, PED.VEN, VEN.RAZ, REGIAO.DREGIAO;
  FROM; 
    CLIEN CLI, PEDIC PED, VEND VEN, REGIAO;
  WHERE; 
    (CTOD(DEM) >= dInicio AND CTOD(DEM) <= dFim ) AND;
    (PED.CGC = CLI.CGC) AND (PED.VEN = VEN.CVR) AND (PED.REGIAO = REGIAO.REGIAO) AND;
    (&COND = .T.);
  INTO TABLE WORK  
  
  CLOSE DATA
  
          
RETURN

*================
FUNCTION ABREDBFS
*================

  SELE 0
  USE REGIAO INDEX REGIAO
  SELE 0
  USE VEND INDEX VEND
  
RETURN 
