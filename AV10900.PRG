**********************************************************
* MODULO...: RELATORIO DOS MAIORES CLIENTES POR FATURAMENTO 
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 23/06/97
***********************************************************

PRIVATE Tela, Maxlin, ContDupl, lin, tamlinha, TotCli, nCols,;
        nDias, dInicio, dFim, PorcAcum, nLimPorc, Tab

nCols = 133
nDias = 60 
dInicio = {}
dFim = {}
Tab = 10

MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio dos Maiores Clientes por Faturamento (Geral)'
@ 01, Centratxt(MSG,80) SAY MSG
nLimPorc = 100
@ 09, 20 SAY 'Entre com o limite (%) de Faturamento'
@ 10, 20 SAY 'para exibi‡Æo no Relat¢rio'
@ 12, 20 SAY '  Porcentagem:' GET nLimPorc PICT [999];
                                        VALID nLimPorc <= 100
READ

IF LASTKEY() = 27
  =FINALIZA()
  RETURN
ENDIF  

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

*----- Calcula Totais
nTotal = CalcTotal()

*--- Arquivos de Trabalho
=MENSAG( 'Preparando Relat¢rio...')
SELE 0
USE WORKCLI
SELE WORKCLI
GO TOP

ContCliGeral  = 0   && Contador de Clientes
TotGeral      = 0   && Total Geral
PorcAcum      = 0.0 && Porcentagem Acumulada
DO DEFSAIDA
=ImprCabec()
DO WHILE ! EOF() 
  =ImprCli()
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
 
 =CABEC132( MODULO, 'Relacao dos Maiores Clientes Geral - DE: ' +;
                    DTOC(dInicio) + ' ATE ' + DTOC(dFim) )
 LIN = 5
 @ LIN, Tab+00 SAY 'Nome de Guerra   UF Regiao Vend          Tot.Faturado Porc. Porc.Acum.'
 =AddLin()
 @ LIN, Tab+00 SAY REPLICATE('=',nCols-Tab)
 =AddLin()
 
RETURN  

* CABEC132 - IMPRIME LINHA BASICA DO CABECALHO DOS RELATORIOS (132 COLUNAS)
*
*--------------------
FUNCTION CABEC132
PARAMETERS Modulo,Titulo
*--------------------
  PAGCTR = PAGCTR + 1
  @ 00, Tab+00 SAY CHR(15)
  @ 01, Tab+00 SAY REPLICATE('=',nCols-Tab)
  @ 02, Tab+00 SAY MREL
  @ 02,116 SAY TIME()
  @ 02,125 SAY HOJE
  @ 03, Tab+00 SAY Modulo
  @ 03, CentraTxt(Titulo,132) SAY Titulo
  @ 03,124 SAY 'PAG: ' + STR(PAGCTR,3)
  @ 04, Tab+00 SAY REPLICATE('-',nCols-Tab)
 
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
  
  Porc = SUM_VAL / nTotal * 100
  PorcAcum = PorcAcum + Porc

  IF PorcAcum <= nLimPorc
    =AddLin()
    @ LIN, Tab+00 SAY GUE
    @ LIN, Tab+16 SAY EST
    @ LIN, Tab+20 SAY FmtRegiao(REGIAO,IDREGIAO)
    @ LIN, Tab+26 SAY VEN
    @ Lin, Tab+41 SAY SUM_VAL PICT [9,999,999.99]
    @ Lin, Tab+54 SAY Porc     PICT [999.99 %]
    @ Lin, Tab+63 SAY PorcAcum PICT [999.99%]
    ContCliGeral = ContCliGeral + 1
    TotGeral     = TotGeral + SUM_VAL
  ENDIF  
  
RETURN


*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT
  
  TXT = 'F I N A L   D O   R E L A T O R I O'
  =AddLin()
  @ Lin+0, Tab+00 SAY REPLICATE('-', nCols-Tab )
  @ Lin+1, Tab+00 SAY 'Tot.Clientes: ' + TRANSFORM( ContCliGeral, [9,999] )
  @ Lin+1, Tab+28 SAY 'Total Geral:' 
  @ Lin+1, Tab+41 SAY TotGeral PICT [9,999,999.99]
  @ Lin+2, Tab+00 SAY REPLICATE('-', nCols-Tab )
  @ Lin+3, Tab+02+CENTRATXT(TXT,nCols) SAY TXT

  
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
  =MENSAG( 'Selecionando Notas Fiscais...' )
  SELE 0
  USE CADWORK1
  GO BOTTOM
  dFim = CTOD(DEMI)
  GO TOP
  dInicio = CTOD(DEMI)
  *-------- Gera Linhas para os Clientes com maior faturamento
  =MENSAG( 'Gerando Totais por Clientes..' )
  SELECT SUM(NF.VAL), CLI.GUE, CLI.REGIAO, CLI.IDREGIAO, CLI.VEN, CLI.EST;
         FROM CLIEN CLI, CADWORK1 NF;
         GROUP BY NF.CGC;
         WHERE CLI.CGC=NF.CGC; 
         ORDER BY 1 DESCENDING;
         INTO TABLE WORKCLI

  CLOSE DATA    

RETURN

*=================
FUNCTION CALCTOTAL
*=================
  PRIVATE nTot
  
  USE WORKCLI
  nTot = 0
  DO WHILE !EOF()
    nTot = nTot + SUM_VAL
    SKIP
  ENDDO

  CLOSE DATA
RETURN nTot

