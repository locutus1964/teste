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
mRegiao = SPACE(4)
mcvr = SPACE(7)
*--- Tabela das Regioes
SELE 0
USE REGIAO INDEX REGIAO
SELE 0 
USE VEND INDEX VEND

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
ContDupl    = 0  && Contador de Duplicatas
TotGeral    = 0  && Total Geral
DebCli      = 0  && Debito do Cliente
TotAtraso   = 0  && Total de Dias em Atraso
TotRegiao   = 0  && Total do Debito da Regiao
DO DEFSAIDA
=ImprCabec()
DO WHILE ! EOF()
  MREGIAO = REGIAO
  TotRegiao = 0
  DO WHILE !EOF() .AND. MREGIAO=REGIAO
    *------- Enquanto e o mesmo cliente
    MCGC = CGC
    DebCli = 0
    lNovoCli = .T.
    ContCli = ContCli + 1
    *------- Enquanto esta na mesma regiao
    DO WHILE !EOF() .AND. MREGIAO = REGIAO .AND. MCGC = CGC
      ContDupl  = ContDupl + 1
      Atraso    = DATE() - CTOD(Venc)
      TotAtraso = TotAtraso + Atraso
      DebCli    = DebCli + VDp
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
 LIN = 5
 @ LIN, 00 SAY 'Nome de Guerra  UF  Regiao Vend  DDD   Fone 1    Dupl/Parc Emissao  Vencimento      Valor  Atraso   Banco Agencia  Total Debito'
 =AddLin()
 @ LIN, 00 SAY REPLICATE('=',132)
 =AddLin()
   
RETURN  

*================
FUNCTION IMPRDUPL
*================
  =AddLin()
  IF lNovoCli
    @ Lin, 00 SAY GUE
    @ Lin, 16 SAY EST
    @ Lin, 20 SAY REGIAO
    @ Lin, 26 SAY VEN
    @ Lin, 31 SAY '(' + DDD1 + ') ' + FON1
  ENDIF  
  @ Lin,  49 SAY STR(NDUP,5) + '-' + STR(PDUP,1)
  @ Lin,  59 SAY EMIS
  @ Lin,  68 SAY VENC
  @ Lin,  77 SAY VDP PICT [9,999,999.99]
  @ Lin,  91 SAY STR( Atraso, 3 )
  @ Lin, 100 SAY NBANCO
  @ Lin, 107 SAY AGENCIA
  
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
  @ Lin, 112 SAY '--->' + TRANSFORM( DebCli, '9,999,999.99' )
  IF WORK2->TIPOCART <> 'N'
    @ Lin, 129 SAY TIPOCART
  ENDIF  
  =AddLin()
RETURN

*=====================
FUNCTION IMPRTOTREGIAO
*=====================
  IF Lin + 2 > 66 
    =ImprCabec()
  ENDIF  
  =AddLin()
  @ Lin,  48 SAY 'Total da Regiao:'
  @ Lin, 116 SAY  TotRegiao PICTURE '9,999,999.99' 
  =AddLin()  
  @ Lin, 00 SAY REPLICATE('-',132)
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
  =AddLin()
  @ Lin+0,  00 SAY REPLICATE('-', 130 )
  @ Lin+1,  31 SAY 'TOT.DUPLS: ' + STR( ContDupl, 3 )
  @ Lin+1,  55 SAY 'MEDIA ATRASO: ' + STR( Media, 3, 1 )
  @ Lin+1, 102 SAY 'DEBITO GERAL: '+ TRANSFORM( TotGeral, '9,999,999.99' )
  @ Lin+2,  02+CENTRATXT(TXT,132) SAY TXT
  @ Lin+3,  00 SAY REPLICATE('-', 130 )
  
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
    (CTOD(Venc) >= dInicio AND CTOD(Venc) <= dFim AND EMPTY(CTOD(Pag)));
  INTO TABLE;
    WORK1

*  SELE DUPL
*  COPY FIELDS Cgc, Ndup, PDup, Emis, Venc, VDp, Agencia, NBanco, TipoCart;
*     TO WORK1;
*     FOR EMPTY(DTCANCEL) AND ; 
*         CTOD(Venc) >= dInicio AND;
*         CTOD(Venc) <= dFim AND;
*         EMPTY(CTOD(Pag))

  =MENSAG( 'Preparando Consulta...' )

  SELECT;
    CLI.Cgc,  CLI.GUE,  CLI.Ddd1,;
    CLI.Fon1, CLI.Regiao, CLI.Ven, CLI.Est,;
    DUP.NDup, DUP.PDup, DUP.Emis, DUP.Venc, DUP.VDp, DUP.Agencia, DUP.NBanco, DUP.TipoCart;
  FROM;
    WORK1 DUP, CLIEN CLI;
  WHERE;
    DUP.CGC=CLI.CGC;
  INTO TABLE;
    WORK2;
  ORDER BY;
    CLI.REGIAO, CLI.GUE,DUP.NDUP,DUP.PDUP

  CLOSE DATABASES

RETURN


