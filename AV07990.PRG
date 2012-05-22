**********************************************************
* MODULO...: RELATORIO DE EMBARQUE DE NOTAS FISCAIS
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 22/06/97
***********************************************************

PRIVATE Tela, Maxlin, ContDupl, lin, tamlinha, TotCli, NCols
PRIVATE dInicio, dFim, TotPed, TotNota, TotGeral, MREGIAO, MCVR

NCOLS = 132
LPONT = REPLICATE('-',NCOLS)
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

@ 08, 19 SAY 'Per¡odo de Vencimento:'
@ 10, 19 SAY ' DATA INICIAL:' GET dInicio
@ 12, 19 SAY ' DATA FINAL..:' GET dFim
@ 14, 19 SAY ' REGIÇO......:' GET MRegiao PICT [9999];
                              VALID MREGIAO = SPACE(4) .OR. ChkRegiao(MREGIAO, .T.)
@ 14, 39 SAY '* Deixe em Branco para selecionar todas'                              
@ 16, 19 SAY ' REPRES......:' GET MCVR PICT [9999.99];
                              VALID MCVR = EmptyRepr .OR. ChkRepr(MCVR, .T.)
@ 16, 39 SAY '* Deixe em Branco para selecionar todos'
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
USE REGIAO INDEX REGIAO
SELE 0 
USE VEND INDEX VEND
SELE 0
USE WORK1  
INDEX ON VEN+REGIAO+GUE+DTOS(CTOD(DEM)) TO WORK1
SELE WORK1
SET RELATION TO VEN INTO VEND,;
                REGIAO INTO REGIAO
COND = ''
IF MREGIAO <> SPACE(4) 
  COND = 'REGIAO = MREGIAO'
ENDIF  
IF ! EMPTY(COND)
  IF MCVR <> SPACE(4)
     COND = COND + '.AND. VEN=MCVR'
  ENDIF   
ELSE
  IF MCVR <> SPACE(4)
    COND = 'VEN=MCVR'
  ENDIF
ENDIF  
IF ! EMPTY(COND) 
  SET FILTER TO &COND
ENDIF  
GO TOP

ContCli     = 0  && Contador de Clientes
CliRep      = 0  && Clientes do Representante
TotGeral    = 0  && Total Geral
TotPed      = 0  && Total dos Pedidos
TotNota     = 0  && Total em Notas Fiscais
TotPedRep   = 0  && Total de Pedidos do Repr.
TotNotaRep  = 0  && Total em Notas Fiscais Repr

DO DEFSAIDA
=ImprCabec()
DO WHILE ! EOF()
  CliRep     = 0  
  TotPedRep  = 0  
  TotNotaRep = 0  
  MVEN = VEN
  OLDREGIAO = SPACE(4)
  DO WHILE ! EOF() .AND. MVEN = VEN
    =ImprCabRepr()
    OLDREGIAO = REGIAO
    DO WHILE ! EOF() .AND. OLDREGIAO = REGIAO
      =ImprLinha()
      SKIP
    ENDDO
    =AddLin()
    =AddLin()
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
  ERASE WORK.DBF
  *ERASE WORK1.DBF
RETURN   

*=================
FUNCTION IMPRCABEC
*=================
 
 =CABEC132( MODULO, 'Relacao de Situa‡Æo de Pedidos ' )
 @ 05, 00 SAY 'C L I E N T E      P  E  D  I  D  O  S                           N O T A S  F I S C A I S    E M B A R Q U E'
 @ 06, 00 SAY 'Nome de Guerra  UF No Rep    Emissao  No CF  Emissao    Valor    No Nota Emissao    Valor    Data     Transportadora  No Coleta'
 @ 07, 00 SAY REPLICATE('=',NCOLS)
 @ 08, 00
 LIN = 08
 
RETURN  

*===================
FUNCTION IMPRCABREPR
*===================
  IF (LIN + 2) > 66 
    =ImprCabec()
  ENDIF
  IF EMPTY( OLDREGIAO )
    @ LIN+0, 00 SAY 'REPRES: ' + VEN + '-' + VEND->RAZ
  ENDIF  
  @ LIN+0, 60 SAY 'REGIAO: ' + REGIAO + '-' + REGIAO->DREGIAO
  LIN = LIN + 1
  @ LIN, 00 

  
RETURN  

*C L I E N T E      P  E  D  I  D  O  S                           N O T A S  F I S C A I S    E M B A R Q U E
*Nome de Guerra  UF No Rep    Emissao  No CF  Emissao    Valor    No Nota Emissao    Valor    Data     Transportadora  No Coleta
*xxxxxxxxxxxxxxx xx xxxxxxxxx xx/xx/xx xxxxxx xx/xx/xx 999,999.99 999999  xx/xx/xx 999,999.99 xx/xx/xx XXXXXXXXXXXXXXX xxxxxxxxxx 
*0               16 19        29       38     45       54         65      73       82         93       102             118 
*=================
FUNCTION IMPRLINHA
*=================
  =AddLin()
  @ Lin,  00 SAY GUE
  @ Lin,  16 SAY EST
  @ Lin,  19 SAY PINT 
  @ Lin,  29 SAY DTPEDREP
  @ Lin,  38 SAY PED    PICT [999999]
  @ Lin,  45 SAY DEM
  @ Lin,  54 SAY EXP_7  PICT [999,999.99]
  @ Lin,  65 SAY NNF    PICT [999999]
  @ Lin,  73 SAY DEMISSAO
  @ Lin,  82 SAY EXP_10 PICT [999,999.99]
  @ Lin,  93 SAY DTENTREG
  @ Lin, 102 SAY GUETRA
  @ Lin, 118 SAY NCOLETA
  ContCli = ContCli + 1
  TotPed = TotPed + EXP_7
  TotNota = TotNota + EXP_10
  CliRep     = CliRep + 1  
  TotPedRep  = TotPedRep + EXP_7  
  TotNotaRep = TotNotaRep + EXP_10  

RETURN 

*================
FUNCTION CABECCLI
*================
  
  * Se o Cabecalho nao couber nesta pagina, imprime na proxima pagina
  IF Lin + 6 >= 66
    =ImprCabec()
  ENDIF  

RETURN  

*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT
  
  TXT = 'F I N A L   D O   R E L A T O R I O'
  =AddLin()
  @ Lin+0,  00 SAY LPONT
  @ Lin+1,  00 SAY 'Tot.Clientes: ' + STR( ContCli, 3 )
  @ Lin+1,  41 SAY 'Tot.Pedidos: ' + TRANSFORM( TotPed, [999,999.99] )
  @ Lin+1,  71 SAY 'Tot.Notas: ' + TRANSFORM( TotNota, [999,999.99] )
  @ Lin+2,  02+CENTRATXT(TXT,NCOLS) SAY TXT
  @ Lin+3,  00 SAY LPONT
  
RETURN

*==================
FUNCTION IMPRTOTREP
*==================
  IF Lin + 2 > 66
    =ImprCabec()
  ENDIF   
  @ Lin+0,  00 SAY 'Tot.Clientes: ' + STR( CliRep, 3 )
  @ Lin+0,  41 SAY 'Tot.Pedidos: ' + TRANSFORM( TotPedRep, [999,999.99] )
  @ Lin+0,  71 SAY 'Tot.Notas: ' + TRANSFORM( TotNotaRep, [999,999.99] )
  @ Lin+1,  00 SAY LPONT
  Lin = Lin + 2
  @ Lin, 00

RETURN

*==============
FUNCTION ADDLIN
*==============

  Lin = Lin + 1 
  IF Lin > 66
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

  *------- Preparando as Tabelas 
  =MENSAG( 'Abrindo Pedidos...' )
  
  SELE 0
  USE PEDIC 
  COPY FIELDS CGC, PED, PINT, DEM, DtPedRep, ValPed, ValIpi;
       TO WORK;
       FOR CTOD(DEM) >= dInicio .AND. CTOD(DEM) <= dFim 
  CLOSE DATA
  
  =MENSAG( 'Gerando Consulta SQL...' )
  SELECT CLI.GUE, CLI.EST, W.PINT, W.PED, W.DTPEDREP, W.DEM,;
         W.VALPED + W.VALIPI, NF.NNF, NF.DEMISSAO, NF.VALPROD + NF.VALIPI,;
         T.GUETRA, NF.DTENTREG, NF.NCOLETA, NF.VEN, NF.REGIAO;
         FROM CLIEN CLI, WORK W, CABECNF NF, TRANS T;
         WHERE (W.PED = NF.PED) AND;
               (W.CGC = CLI.CGC) AND;
               (NF.TRA = T.TRA);
         INTO TABLE WORK1      
         
  CLOSE DATA

RETURN

*===============
FUNCTION CHKREPR
*===============
  PRIVATE lAchou
  
  lAchou = SEEK( MCVR, 'VEND' ) 
  
  IF ! lAchou
    =MENSAG( 'Representante nÆo cadastrado, tente novamente' )
  ENDIF  
  
RETURN lAchou

*================
FUNCTION ABREDBFS
*================

  SELE 0
  USE REGIAO INDEX REGIAO
  SELE 0
  USE VEND INDEX VEND
  
RETURN 
