**********************************************************
* MODULO...: RELATORIO DE DUPLICATAS EM ABERTO POR PERIODO
* AUTOR....: JAIR GONCALVES 
* CRIADO EM: 12/06/97
***********************************************************

PRIVATE Tela, Maxlin, ContDupl, lin, tamlinha, TotCli

MODULO=PROGRAM()
SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Relat¢rio de Duplicatas em Aberto por Per¡odo'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )
dInicio = DATE() - 30
dFim    = ObDataFim()
mRegiao   = EmptyRegiao
midregiao = 0
mcvr      = EmptyRepr
midrepr   = 0
*--- Tabela das Regioes
SELE 0
USE REGIAO ORDER X_REGIAO
SELE 0 
USE VEND INDEX VEND.IDX

@ 08, 15 SAY 'Per¡odo de Vencimento:'
@ 10, 15 SAY ' DATA INICIAL:' GET dInicio
@ 12, 15 SAY ' DATA FINAL..:' GET dFim
@ 14, 15 SAY ' REGIÇO......:' GET MRegiao PICT [9999.99];
                              VALID MREGIAO = EmptyRegiao .OR. ChkRegiao(MREGIAO, .T.)
@ 14, 39 SAY '* Deixe em Branco para selecionar todas'                              
@ 16, 15 SAY ' REPRES......:' GET MCVR PICT [9999.99];
                              VALID MCVR = EmptyRepr .OR. ChkRepr(MCVR, .T. )
@ 16, 39 SAY '* Deixe em Branco para selecionar todos'
READ
IF LASTKEY() = 27
  =Finaliza()
  RETURN
ENDIF   

MENS = DADCORR
MC = 'N'
DO PEGRESP WITH [SN]

IF MC = 'N'
  =FINALIZA()
  RETURN
ENDIF
  
*--- Seleciona as Duplicatas
=SelecDupls()

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
USE WORK2  
SELE WORK2
INDEX ON REGIAO+STR(IDREGIAO,2)+VEN+STR(IDREPR)+CGC TO WORK2.IDX

COND = ''
IF MREGIAO <> EmptyRegiao
  COND=AdicFiltro(COND, 'REGIAO = "' + SUBSTR(MREGIAO,1,4) + '" AND MIDREGIAO = IDREGIAO') 
ENDIF  
IF MCVR <> SPACE(4)
  COND=AdicFiltro(COND, 'VEN= "' + SUBSTR(MCVR,1,4) + '" AND IDREPR = MIDREPR')
ENDIF  
IF ! EMPTY(COND) 
  SET FILTER TO &COND
ENDIF  
GO TOP

ContCli   = 0  && Contador de Clientes
ContDupl  = 0  && Contador de Duplicatas
TotGeral  = 0  && Total Geral
DebCli    = 0  && Debito do Cliente
TotAtraso = 0  && Total de Dias em Atraso
TotRegiao = 0  && Total do Debito da Regiao
GDE       = 0 && Total Geral Desconto 
GGA       = 0 && Total Geral Garantia
GCA       = 0 && Total Geral Carteira
GCS       = 0 && Total Geral Carteira Simples
GPL       = 0 && Total Geral Perdas e Lucros
GJD       = 0 && Total Geral Juridico

DO DEFSAIDA
=ImprCabec()
DO WHILE ! EOF()
  MREGIAO = FmtRegiao(REGIAO, IDREGIAO)
  TotRegiao = 0
  DO WHILE !EOF() .AND. MREGIAO = FmtRegiao(REGIAO, IDREGIAO)
    *------- Enquanto e o mesmo cliente
    MCGC = CGC
    DebCli = 0
    lNovoCli = .T.
    ContCli = ContCli + 1
    *------- Enquanto esta na mesma regiao
    DO WHILE !EOF() .AND. MREGIAO = FmtRegiao(REGIAO, IDREGIAO) .AND. MCGC = CGC
      CartSimples = TIPOCART = 'CS' 
      ContDupl  = ContDupl + 1
      Atraso    = DATE() - VENC
      TotAtraso = TotAtraso + Atraso
      DebCli    = DebCli + VDp
      =AcumTipoCart()
      =ImprDupl()
      SKIP
      lNovoCli = .F.
    ENDDO  
    TotGeral  = TotGeral + DebCli
    TotRegiao = TotRegiao + DebCli
    *-------- Fim do Cliente
    =ImprTotCli()
  ENDDO  
  *-------- Fim da Regiao
  =ImprTotRegiao() 
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
 
 =CABEC132( MODULO, 'Relacao de Duplicatas em Aberto - DE: ' +;
                    DTOC(dInicio) + ' A ' + DTOC( dFim ) )
 @ 05, 00 SAY 'Nome de Guerra  UF  Regiao  Vend    DDD   Fone 1    Dupl/Parc Emissao  Vencimento      Valor  Atraso   Banco Agencia  Total Debito'
 @ 06, 00 SAY REPLICATE('=',132)
 LIN = 7
   
RETURN  

*================
FUNCTION IMPRDUPL
*================
  =AddLin()
  IF lNovoCli
    @ Lin, 00 SAY GUE
    @ Lin, 16 SAY EST
    @ Lin, 20 SAY FmtRegiao(REGIAO, IDREGIAO)
    @ Lin, 28 SAY FmtRepr(VEN,IDREPR)
    @ Lin, 36 SAY '('+ DDD1 + ')' + FON1
  ENDIF  
  @ Lin,  52 SAY STR(NDUP,5) + '-' + STR(PDUP,1)
  @ Lin,  60 SAY EMIS
  @ Lin,  71 SAY VENC
  @ Lin,  82 SAY VDP PICT [9,999,999.99]
  IF .NOT. CartSimples
    @ Lin, 95 SAY TIPOCART
  ENDIF  
  @ Lin,  98 SAY STR( Atraso, 3 )
  @ Lin, 107 SAY NBANCO
  @ Lin, 114 SAY AGENCIA

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
FUNCTION IMPRTOTCLI
*==================
  @ Lin, 118 SAY '-->' + TRANSFORM( DebCli, '9,999,999.99' )
RETURN

*=====================
FUNCTION IMPRTOTREGIAO
*=====================
  IF Lin + 2 > 66 
    =ImprCabec()
  ENDIF  
  =AddLin()
  @ Lin+0, 51 SAY 'Total da Regiao:' + TRANSFORM( TotRegiao, '9,999,999.99' )
  @ Lin+1, 00 SAY REPLICATE('-',132)
  =AddLin()
RETURN

*==================
FUNCTION IMPRTOTREL
*==================
  PRIVATE TXT, Media
  
  TXT = 'F I N A L   D O   R E L A T O R I O'
  Media = 0
  IF ContDupl > 0
    Media = TotAtraso / ContDupl
  ENDIF  
  PGDE = GDE / TotGeral * 100
  PGGA = GGA / TotGeral * 100  
  PGCA = GCA / TotGeral * 100  
  PGCS = GCS / TotGeral * 100
  PGPL = GPL / TotGeral * 100
  PGJD = GJD / TotGeral * 100
  PGERAL = PGDE + PGGA + PGCA + PGCS + PGPL + PGJD
  =AddLin()
  @ Lin+ 0,  00 SAY REPLICATE('-', 130 )
  @ Lin+ 1,  94 SAY 'DESCONTO........: ' + TRANSFORM( GDE, [9,999,999.99] ) +;
                                           TRANSFORM( PGDE, [ 999.9 %] )
  @ Lin+ 2,  94 SAY 'GARANTIA........: ' + TRANSFORM( GGA, [9,999,999.99] ) +;
                                           TRANSFORM( PGGA, [ 999.9 %] )
  @ Lin+ 3,  94 SAY 'CARTEIRA........: ' + TRANSFORM( GCA, [9,999,999.99] ) +;
                                           TRANSFORM( PGCA, [ 999.9 %] )  
  @ Lin+ 4,  94 SAY 'PERDAS E LUCROS.: ' + TRANSFORM( GPL, [9,999,999.99] ) +;
                                           TRANSFORM( PGPL, [ 999.9 %] )  
  @ Lin+ 5,  94 SAY 'JURIDICO........: ' + TRANSFORM( GJD, [9,999,999.99] ) +;         
                                           TRANSFORM( PGJD, [ 999.9 %] )   
  @ Lin+ 6,  94 SAY 'CARTEIRA SIMPLES: ' + TRANSFORM( GCS, [9,999,999.99] ) +;  
                                           TRANSFORM( PGCS, [ 999.9 %] )  
  @ Lin+ 8,  31 SAY 'TOT.DUPLS: ' + STR( ContDupl, 3 )
  @ Lin+ 8,  55 SAY 'MEDIA ATRASO: ' + STR( Media, 3, 1 )
  @ Lin+ 8,  94 SAY 'DEBITO GERAL....: '+ TRANSFORM( TotGeral, [9,999,999.99] ) +;
                                          TRANSFORM( PGERAL, [ 999.9 %] )  
  @ Lin+ 9,  02+CENTRATXT(TXT,132) SAY TXT
  @ Lin+10,  00 SAY REPLICATE('-', 130 )
  
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
FUNCTION OBDATAFIM
*=================
  PRIVATE dFim
  
  *--- Se a Data de Inicial ‚ Segunda-Feira, a Data Final e Sexta-Feira 
  dFim = DATE()
  IF DOW(dFim) = 2     
     dFim = Date() - 3  
  ENDIF

RETURN dFim

*==================
FUNCTION SELECDUPLS
*==================

  *------- Preparando as Tabelas 
  
  =MENSAG( 'Abrindo Duplicatas...' )
  SELE 0
  USE DUPL 

  *------- Checagem das Duplicatas em Aberto

  =MENSAG( 'Indexando Duplicatas...' )

  SELECT;
    Cgc, Ndup, PDup, Emis, Venc, VDp, Agencia, NBanco, TipoCart;
  FROM;
    DUPL, CABECNF NF;
  WHERE;
    (DUPL.NDUP = NF.NNF AND NF.CANCELADA = .F.) AND;
    (VENC >= dInicio AND VENC <= dFim AND EMPTY(PAG));
  INTO TABLE;
    WORK1

  =MENSAG( 'Preparando Consulta...' )

  SELECT;
    CLI.Cgc,  CLI.GUE,  CLI.Ddd1,;
    CLI.Fon1, CLI.Regiao, CLI.IdRegiao, CLI.Ven, CLI.IdRepr, CLI.Est,;
    DUP.NDup, DUP.PDup, DUP.Emis, DUP.Venc, DUP.VDp, DUP.Agencia, DUP.NBanco, DUP.TipoCart;
  FROM;
    WORK1 DUP, CLIEN CLI;
  WHERE;
    DUP.CGC = CLI.CGC;
  INTO TABLE;
    WORK2;
  ORDER BY;
    CLI.REGIAO, CLI.GUE, DUP.NDUP, DUP.PDUP

  CLOSE DATABASES

RETURN

*=====================
PROCEDURE ACUMTIPOCART
*=====================
  DO CASE
    CASE TIPOCART = 'DE'
      GDE = GDE + VDP
    CASE TIPOCART = 'GA'
      GGA = GGA + VDP
    CASE TIPOCART = 'CA'
      GCA = GCA + VDP
    CASE TIPOCART = 'CS'
      GCS = GCS + VDP
    CASE TIPOCART = 'PL'
      GPL = GPL + VDP
    CASE TIPOCART = 'JD'
      GJD = GJD + VDP
  ENDCASE    
RETURN 
