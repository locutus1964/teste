**********************************************************
* MODULO...: RELATORIO DE EMBARQUE DE NOTAS FISCAIS
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 22/06/97
***********************************************************

PRIVATE Tela, Maxlin, ContDupl, lin, tamlinha, TotCli, NCols, TamPag
PRIVATE dInicio, dFim, TotPed, TotNota, TotGeral, MREGIAO, MCVR

TamPag = _PLENGTH - 1
_PEJECT="NONE"
NCOLS = 132
LPONT  = REPLICATE('-',NCOLS)
LPONTD = REPLICATE('=',NCOLS)
MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio de Embarque de Notas Fiscais'
@ 01, Centratxt(MSG,80) SAY MSG

dInicio = {}
dFim    = {}
=InicDatas()
mRegiao = SPACE(4)
mcvr    = SPACE(7)
RESUMIDO = 'N'
DO WHILE .T.
  =MENSAG( '<ESC> para sair' )
  =AbreDbfs()
  @ 05, 25 SAY 'Per°odo de Emiss∆o dos Pedidos:'
  @ 08, 25 SAY 'DATA INICIAL:' GET dInicio
  @ 10, 25 SAY 'DATA FINAL..:' GET dFim
  @ 12, 25 SAY 'REPRES......:' GET MCVR PICT [9999.99];
                              VALID MCVR = EmptyRepr .OR. ChkRepr(MCVR, .T.)
  @ 13, 25 SAY '              * Deixe em Branco para selecionar todos'
  @ 14, 25 SAY 'RESUMIDO S/N?' GET RESUMIDO PICT [@!] VALID RESUMIDO $ [SN]
  READ

  IF LASTKEY() = 27
    =Finaliza()
    EXIT
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
    LOOP
  ENDIF   

  =MENSAG( 'Preparando Relat¢rio...')

  *--- Arquivo de Trabalho
  SELE 0 
  USE CLIEN ORDER P_CGC
  SELE 0
  USE REGIAO INDEX REGIAO
  SELE 0
  USE VEND INDEX VEND
  SELE 0
  USE TRANS INDEX TRANS

  SELE 0
  USE WORK  
  INDEX ON VEN+REGIAO TO WORK
  SET RELATION TO TRA    INTO TRANS,;
                  VEN    INTO VEND,;
                  REGIAO INTO REGIAO,;
                  CGC    INTO CLIEN
                               
  ContCli     = 0  && Contador de Clientes
  CliRep      = 0  && Clientes do Representante
  TotGeral    = 0  && Total Geral
  TotPed      = 0  && Total dos Pedidos
  TotNota     = 0  && Total em Notas Fiscais
  TotPedRep   = 0  && Total de Pedidos do Repr.
  TotNotaRep  = 0  && Total em Notas Fiscais Repr
 
  SELE WORK
  GO TOP

  DO DEFSAIDA

  IF RESUMIDO = 'N'
    *--- Relatorio Analitico
    =ImprRelAna()
  ELSE  
    *---- Relatorio Sintetico
    =ImprRelSin()
  ENDIF  
  
  DO IMPSAIDA
  =FINALIZA()
  
ENDDO

*-------------------------------------------------------------------------
*==================
FUNCTION IMPRRELANA
*==================

  ContCli     = 0  
  CliRep      = 0  
  TotGeral    = 0  
  TotPed      = 0  
  TotNota     = 0  
  TotPedRep   = 0  
  TotNotaRep  = 0  

  GO TOP
  DO WHILE ! EOF()
    MVEN = VEN
    CliRep     = 0  
    DO WHILE ! EOF() .AND. MVEN = VEN 
      =ImprCabec()
      TotPedRep  = 0  
      TotNotaRep = 0  
      MREGIAO = REGIAO
      DO WHILE ! EOF() .AND. MVEN = VEN .AND. MREGIAO = REGIAO
        =ImprLinha()
        SKIP
      ENDDO
      =ImpRepAna()
    ENDDO  
  ENDDO  
  *-------- Final do Relatorio
  =ImprTotRel()

RETURN

*==================
FUNCTION IMPRRELSIN
*==================
  PRIVATE VALNF, VALPED, cPed

  ContCli     = 0  
  TotGeral    = 0  
  TotPed      = 0  
  TotNota     = 0  
  
  GO TOP
  =ImprCabSin()
  DO WHILE ! EOF()
    MVEN   = VEN
    CliRep      = 0
    TotPedRep   = 0  
    TotNotaRep  = 0  
    DO WHILE ! EOF() .AND. MVEN = VEN 
      =ImpLinSin()
      MREGIAO = REGIAO
      DO WHILE ! EOF() .AND. MVEN = VEN .AND. MREGIAO = REGIAO
        IF NNF <> 0
          VALPED = 0
          VALNF = WORK->EXP_10
        ELSE  
          VALPED = EXP_4
          VALNF = 0
        ENDIF 
        TotNota    = TotNota + VALNF
        TotNotaRep = TotNotaRep + VALNF
        TotPed     = TotPed + VALPED
        TotPedRep  = TotPedRep + VALPED
        ContCli = ContCli + 1
        CliRep  = CliRep + 1  
        SKIP
      ENDDO
    ENDDO  
    =ImpRepSin()
  ENDDO  
  *------ Final do Relatorio
  =ImprTotRel()
RETURN 
  
*================
FUNCTION FINALIZA
*================
  RESTORE SCREEN FROM Tela
  CLOSE DATABASES
  *ERASE WORK.DBF
RETURN   

*=================
FUNCTION IMPRCABEC
*=================

* C L I E N T E     ||       P  E  D  I  D  O  S         ||   N O T A S  F I S C A I S      ||      E M B A R Q U E        |' 
*                   || No Rep.   | Data Rep.|            ||         |          |            ||  Transportadora  |          |' 
*Nome de Guerra  UF || No CF     | Data CF  |   Valor    || No Nota | Emissao  |   Valor    ||   No Coleta      |  Data    |'
*XXXXXXXXXXXXXX  XX || XXXXXXXXX | xx/xx/xx | 999,999.99 || 999999  | XX/XX/XX | 999,999.99 || XXXXXXXXXXXXXXXX | XX/XX/XX |' 
*00              16 19 22        3234       4345         56 59      6769       7880         91 94             111 113      |' 
  =CABEC132( MODULO, 'Relatorio de Situacao de Pedidos - Analitico' )
  @ 05, 00 SAY ' C L I E N T E     ||       P  E  D  I  D  O  S         ||   N O T A S  F I S C A I S      ||      E M B A R Q U E        |' 
  @ 06, 00 SAY '                   || No Rep.   | Data Rep.|            ||         |          |            ||  Transportadora  |          |' 
  @ 07, 00 SAY 'Nome de Guerra  UF || No CF     | Data CF  |   Valor    || No Nota | Emissao  |   Valor    ||   No Coleta      |  Data    |'
  @ 08, 00 SAY LPONT
  @ 09, 00 SAY 'REPR: ' + WORK->VEN + ' - ' + VEND->RAZ 
  @ 09, 59 SAY 'REGIAO: ' + WORK->REGIAO + ' - ' + REGIAO->DREGIAO
  @ 10, 00 SAY LPONTD
  @ 11, 00
  LIN = PROW()
RETURN

*==================
FUNCTION IMPRCABSIN
*==================
*Repr Nome de Guerra  Regiao      Nome da Regiao            || Valor Pedidos   ||      Valor Notas
*9999 XXXXXXXXXXXXXXX 9999   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ||   999,999.99    ||        999,999.99
*0    5               21     28                             59   64            78        88                
  =CABEC132( MODULO, 'Relatorio de Situaá∆o de Pedidos - Sintetico' )
  @ 05, 00 SAY 'Repr Nome de Guerra  Regiao      Nome da Regiao            || Valor Pedidos   ||      Valor Notas'
  @ 06, 00 SAY LPONTD
  @ 07, 00
  LIN = PROW()
RETURN


*=================
FUNCTION IMPRLINHA
*=================
  PRIVATE VALNF, VALPED, cPed
  
  IF NNF <> 0
    VALPED = 0
    VALNF = WORK->EXP_10
  ELSE  
    VALPED = EXP_4
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
  @ Lin+0, 034 SAY DTPEDREP
  @ Lin+0, 043 SAY '|' 
  IF VALPED > 0
    @ Lin+0, 045 SAY VALPED PICT [999,999.99]
  ENDIF  
  @ Lin+0, 056 SAY '||' 
  IF VALNF > 0  
    @ Lin+0, 059 SAY NNF PICT [999999]
    @ Lin+0, 067 SAY '|' 
    @ Lin+0, 069 SAY DEMISSAO
    @ Lin+0, 078 SAY '|' 
    @ Lin+0, 080 SAY VALNF PICT [999,999.99]
    @ Lin+0, 091 SAY '||' 
    @ Lin+0, 094 SAY TRANS->GUETRA
    @ Lin+0, 111 SAY '|' 
    @ Lin+0, 113 SAY DTENTREG
    @ Lin+0, 122 SAY '|' 
  ELSE  
    @ Lin+0, 067 SAY '|' 
    @ Lin+0, 078 SAY '|' 
    @ Lin+0, 091 SAY '||' 
    @ Lin+0, 111 SAY '|' 
    @ Lin+0, 122 SAY '|' 
  ENDIF 

  &&---- Segunda Linha 
  cPed = PADR( ALLTRIM(STR(PED,6)), 9, ' ')
  @ Lin+1, 019 SAY '||' 
  @ Lin+1, 022 SAY cPed
  @ Lin+1, 032 SAY '|' 
  @ Lin+1, 034 SAY DEM PICT [99/99/99]
  @ Lin+1, 043 SAY '|' 
  @ Lin+1, 056 SAY '||' 
  @ Lin+1, 067 SAY '|'   
  @ Lin+1, 078 SAY '|'   
  @ Lin+1, 091 SAY '||' 
  @ Lin+1, 094 SAY NCOLETA
  @ Lin+1, 111 SAY '|' 
  @ Lin+1, 122 SAY '|' 
  Lin = Lin + 2    

  TotNota    = TotNota + VALNF
  TotNotaRep = TotNotaRep + VALNF
  TotPed     = TotPed + VALPED
  TotPedRep  = TotPedRep + VALPED
  ContCli = ContCli + 1
  CliRep  = CliRep + 1  
  
RETURN 

*=================
FUNCTION IMPLINSIN
*=================
  PRIVATE VALNF, VALPED, cPed
  
  IF NNF <> 0
    VALPED = 0
    VALNF = EXP_10
  ELSE  
    VALPED = EXP_4
    VALNF = 0
  ENDIF 

  @ Lin+0, 000 SAY WORK->VEN
  @ Lin+0, 005 SAY VEND->VENGUE
  @ Lin+0, 021 SAY WORK->REGIAO
  @ Lin+0, 028 SAY REGIAO->DREGIAO  
  @ Lin+0, 059 SAY '||' 
  @ Lin+0, 064 SAY VALPED PICT [999,999.99]
  @ Lin+0, 078 SAY '||' 
  @ Lin+0, 088 SAY VALNF PICT [999,999.99]
  Lin = Lin + 1
  IF Lin >= TamPag
    =ImprCabSin()
  ENDIF  
  
RETURN 

*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT, Total

  TXT = 'F I N A L   D O   R E L A T O R I O'
  Total = TotPed + TotNota
  IF Lin + 5 >= TamPag
    =ImprCabec()
  ENDIF    
  @ Lin+1, 00 SAY REPLICATE( '*', NCOLS )
  @ Lin+2, 00 SAY 'Tot.Clientes: ' + STR( ContCli, 3 )
  @ Lin+2, 37 SAY '|'
  @ Lin+2, 44 SAY '||'
  @ Lin+2, 83 SAY '|'
  @ Lin+2, 91 SAY '||'
  @ Lin+3, 00 SAY 'Em Carteira Acumulado ==> ' + TRANSFORM( TotPed, [999,999.99] )
  @ Lin+3, 37 SAY '|' + TRANSFORM( ROUND(TotPed / Total * 100, 1), [999.9%] )
  @ Lin+3, 44 SAY '|| Faturamento Acumulado ==> ' + TRANSFORM( TotNota, [999,999.99] ) 
  @ Lin+3, 83 SAY '|' + TRANSFORM( ROUND(TotNota / Total * 100, 1), [999.9%] )
  @ Lin+3, 91 SAY '|| Carteira + Fatura ==> ' +;
                  TRANSFORM( Total, [999,999.99] ) + '| 100,0%'
  @ Lin+4, 00 SAY REPLICATE( '*', NCOLS )
  @ Lin+5, 02+CENTRATXT(TXT,NCOLS) SAY TXT

  
RETURN

*=================
FUNCTION IMPREPANA
*=================

  IF Lin + 4 >= TamPag
    =ImprCabec()
  ENDIF   
  @ Lin+1, 00 SAY LPONT
  @ Lin+2, 32 SAY 'Tot.Pedidos: ' + TRANSFORM( TotPedRep, [999,999.99] )
  @ Lin+2, 69 SAY 'Tot.Notas: ' + TRANSFORM( TotNotaRep, [999,999.99] )
  @ Lin+3, 00 SAY LPONT
  @ Lin+4, 00
  Lin = PROW()
  
RETURN

*=================
FUNCTION IMPREPSIN
*=================

  IF Lin + 4 >= TamPag
    =ImprCabSin()
  ENDIF   
  @ Lin+0, 00 SAY LPONT
  @ Lin+1, 51 SAY 'Tot.Pedidos: ' + TRANSFORM( TotPedRep, [999,999.99] )
  @ Lin+1, 77 SAY 'Tot.Notas: ' + TRANSFORM( TotNotaRep, [999,999.99] )
  @ Lin+2, 00 SAY LPONT
  @ Lin+3, 00
  Lin = PROW()
  
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

  *------- Inicializa o Filtro
  COND = '.T.'
  IF ! EMPTY(MCVR)
    COND = 'VEN=MCVR'
  ENDIF    
  
  *------- Preparando as Tabelas 
  =MENSAG( 'Pesquisando Pedidos do Per°odo...' )
  USE PEDIC
  COPY FIELDS CGC, PED, VEN, REGIAO, DEM, PINT, DTPEDREP, VALPED, VALIPI; 
       TO PEDTMP1;              
       FOR (CTOD(DEM) >= dInicio AND CTOD(DEM) <= dFim) .AND.;
           (&COND = .T.);     
  
  =MENSAG( 'Pesquisando Pedidos Atendidos...' )
  SELECT;
     PED.CGC, PED.DEM, PED.PED, (PED.VALPED + PED.VALIPI), PED.VEN, PED.REGIAO,;
     PED.PINT, PED.DTPEDREP,;
     NF.NNF, (NF.VALPROD + NF.VALIPI), DEMISSAO, DTENTREG, NCOLETA, TRA;
  FROM;
     PEDTMP1 PED, CABECNF NF;
  WHERE;
     (PED.PED = NF.PED) AND (NF.CANCELADA = .F.);
  ORDER BY PED.PED;
  INTO TABLE WORK
   
  CLOSE DATA
  
  =MENSAG( 'Pesquisando Pedidos N∆o Atendidos...' ) 
  &&--- Seleciona Pedidos do Periodo
  SELECT 0
  USE PEDTMP1
  SELECT 0
  USE WORK
  SELECT 0
  USE CABECNF 
  INDEX ON PED TO NFTMP1
  && Relaciona os Pedidos do Periodo com as Notas Fiscais
  SELECT PEDTMP1
  SET RELATION TO PED INTO CABECNF
  GO TOP
  DO WHILE ! EOF()
    IF ! FOUND( 'CABECNF' )
      SELECT WORK
      APPEND BLANK
      REPLACE CGC      WITH PEDTMP1->CGC
      REPLACE PED      WITH PEDTMP1->PED
      REPLACE VEN      WITH PEDTMP1->VEN 
      REPLACE REGIAO   WITH PEDTMP1->REGIAO
      REPLACE DEM      WITH PEDTMP1->DEM
      REPLACE PINT     WITH PEDTMP1->PINT
      REPLACE DTPEDREP WITH PEDTMP1->DTPEDREP   
      REPLACE EXP_4    WITH PEDTMP1->VALPED + PEDTMP1->VALIPI 
    ENDIF
    SELECT PEDTMP1
    SKIP
  ENDDO
       
*  SELECT;
*    CLI.GUE, CLI.EST, PED, PINT, DEM, DtPedRep, (ValPed + ValIpi),;
*    PED.REGIAO, PED.VEN, VEN.RAZ, REGIAO.DREGIAO, PED.NNF;
*  FROM; 
*    CLIEN CLI, PEDTMP2 PED, VEND VEN, REGIAO;
*  WHERE; 
*    (PED.CGC = CLI.CGC) AND;
*    (PED.VEN = VEN.CVR) AND;
*    (PED.REGIAO = REGIAO.REGIAO);
*  INTO TABLE WORK  
  
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
