********************************************************************
* MODULO...: RELATORIO DOS MAIORES CLIENTES POR FATURAMENTO (REGIAO)
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 23/06/97
********************************************************************

PRIVATE Tela, Maxlin, ContDupl, lin, tamlinha, TotCli, nCols,;
        nDias, dInicio, dFim, PorcAcum, nLimPorc

nCols = 132
nDias = 60
dInicio = {}
dFim = {}
MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio dos Maiores Clientes por Faturamento (Regi∆o)'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )
mRegiao = SPACE(7)
mcvr = SPACE(7)

*--- Tabela das Regioes
SELE 0
USE REGIAO INDEX REGIAO.IDX
SELE 0 
USE VEND INDEX VEND.IDX

@ 10, 19 SAY ' REGI«O......:' GET MRegiao PICT [9999];
                              VALID MREGIAO = SPACE(4) .OR. ChkRegiao(mregiao, .T.)
@ 12, 19 SAY ' REPRES......:' GET MCVR PICT [9999.99];
                              VALID MCVR = cEmptyRepr .OR. ChkRepr(MCVR, .T.)
@ 14, 19 SAY '* Deixe em Branco para Selecionar Tudo'
READ
IF LASTKEY() = 27
  =Finaliza()
  RETURN
ENDIF   

nLimPorc = 100

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

*---- Seleciona as Notas Fiscais
=SelecNfs()

*---- Calcula o Total Geral
=MENSAG( 'Calculando Total Geral...' )
nTotal=CalcTotal()

*--- Arquivos de Trabalho
=MENSAG( 'Preparando Relat¢rio...')
SELE 0
USE WORKCLI
INDEX ON REGIAO TO TMPWORK.IDX
GO TOP

SELE 0
USE WORKTOT
SELE WORKTOT
GO TOP

ContCliGeral   = 0   && Contador de Clientes
ContCli        = 0   && Contador por Regiao
TotGeral       = 0   && Total Geral
TotRegiao      = 0   && Total da Regiao 
PorcAcum       = 0.0 && Porcentagem Acumulada
PorcAcumRegiao = 0.0 && Porcentagem Acumulada da Regiao
DO DEFSAIDA
=ImprCabec()
DO WHILE ! EOF()
  *---- Procura a proxima regiao
  SELE WORKCLI
  SEEK( WORKTOT->REGIAO )
  MREGIAO = REGIAO
  TotRegiao = 0
  ContCli   = 0
  PorcAcumRegiao  = 0.0
  *------- Enquanto esta na mesma regiao
  DO WHILE !EOF('WORKTOT') .AND. MREGIAO=REGIAO
    ContCli = ContCli + 1
    =ImprCli()
    TotRegiao = TotRegiao + SUM_VAL
    SKIP
  ENDDO  
  *-------- Fim da Regiao
  =ImprTotRegiao() 
  TotGeral = TotGeral + TotRegiao
  ContCliGeral = ContCliGeral + ContCli
  SELECT WORKTOT
  SKIP
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

RETURN   

*=================
FUNCTION IMPRCABEC
*=================
 
 =CABEC132( MODULO, 'Relacao dos Maiores Clientes por Regiao - DE: ' +;
                    DTOC(dInicio) + ' ATE ' + DTOC(dFim) )
 LIN = 5
 @ LIN, 00 SAY 'Nome de Guerra   UF Regiao Vend         Tot.Faturado  Porc. Porc.Acum.'
 =AddLin()
 @ LIN, 00 SAY REPLICATE('=',nCols)
 =AddLin()
 
RETURN  

*================
FUNCTION CABECCLI
*================
  * Se o Cabecalho nao couber nesta pagina, imprime na proxima pagina
  IF Lin + 6 >= 66
    =ImprCabec()
  ENDIF  
  
RETURN  


*===============
FUNCTION IMPRCLI
*===============
  PRIVATE Porc
  
  Porc = SUM_VAL / WORKTOT->SUM_SUM_VA
  PorcAcumRegiao = PorcAcumRegiao + Porc
  =AddLin()
  @ LIN, 00 SAY GUE
  @ LIN, 16 SAY EST
  @ LIN, 20 SAY REGIAO
  @ LIN, 26 SAY VEN
  @ Lin, 41 SAY SUM_VAL PICT [9,999,999.99]
  @ Lin, 54 SAY Porc * 100           PICT [999.99 %]
  @ Lin, 63 SAY PorcAcumRegiao * 100 PICT [999.99 %]  
RETURN

*=====================
FUNCTION IMPRTOTREGIAO
*=====================
  PRIVATE Porc
  
  Porc = TotRegiao / nTotal
  PorcAcum = PorcAcum + Porc
  =AddLin()  
  @ Lin, 02 SAY 'Tot.Cli: ' + TRANSFORM( ContCli, [9,999] )
  @ Lin, 24 SAY 'Total da Regiao:'
  @ Lin, 41 SAY TotRegiao PICTURE '9,999,999.99' 
  @ Lin, 54 SAY Porc * 100     PICT [999.99 %]
  @ Lin, 63 SAY PorcAcum * 100 PICT [999.99 %]
  =AddLin()  
  @ Lin, 00 SAY REPLICATE('-',nCols)
RETURN

*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT
  
  TXT = 'F I N A L   D O   R E L A T O R I O'
  =AddLin()
  @ Lin+0, 00 SAY REPLICATE('-', nCols )
  @ Lin+1, 00 SAY 'Tot.Clientes: ' + TRANSFORM( ContCliGeral, [9,999] )
  @ Lin+1, 28 SAY 'Total Geral:' 
  @ Lin+1, 41 SAY TotGeral PICT [9,999,999.99]
  @ Lin+2, 00 SAY REPLICATE('-', nCols )
  @ Lin+3, 02+CENTRATXT(TXT,nCols) SAY TXT

  
RETURN

*==============
FUNCTION ADDLIN
*==============

  Lin = Lin + 1 
  IF Lin > 66
    =ImprCabec()
  ENDIF   

RETURN

*================
FUNCTION SELECNFS
*================

  *------- Preparando as Tabelas 
  SELE 0
  USE CADWORK1
  GO BOTTOM
  dFim = CTOD(DEMI)
  GO TOP
  dInicio = CTOD(DEMI)
  COND = '.T.'
  IF MREGIAO <> SPACE(4) 
    COND = 'CLI.REGIAO = MREGIAO'
  ENDIF  
  IF ! EMPTY(COND)
    IF MCVR <> SPACE(4)
      COND = COND + '.AND. CLI.VEN=MCVR'
    ENDIF   
  ELSE
    IF MCVR <> SPACE(4)
      COND = 'CLI.VEN=MCVR'
    ENDIF
  ENDIF  
  
  *-------- Gera Linhas para os Clientes com maior faturamento
  =MENSAG( 'Gerando Totais por Clientes..' )
  SELECT SUM(NF.VAL), CLI.GUE, CLI.REGIAO, CLI.VEN, CLI.EST;
         FROM CLIEN CLI, CADWORK1 NF;
         GROUP BY NF.CGC;
         HAVING &COND;
         WHERE CLI.CGC=NF.CGC; 
         ORDER BY CLI.REGIAO, 1 DESCENDING;
         INTO TABLE WORKCLI

  *----- Gera as Linhas das Regioes que mais vendem
  =MENSAG( 'Gerando Totais por Regiao...' )
  SELECT SUM(SUM_VAL), REGIAO, VEN;
         FROM WORKCLI;
         GROUP BY REGIAO;
         ORDER BY 1 DESCENDING;
         INTO TABLE WORKTOT
  
  CLOSE DATA    

RETURN

*=================
FUNCTION CALCTOTAL
*=================
  PRIVATE nTot
  
  USE WORKTOT
  nTot = 0
  DO WHILE !EOF()
    nTot = nTot + SUM_SUM_VA
    SKIP
  ENDDO

  CLOSE DATA
RETURN nTot

