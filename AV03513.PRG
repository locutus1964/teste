* AV03513 - CALCULO E IMPRESSAO DAS COMISSOES DE VENDEDORES
* SISTEMA ADMINISTRACAO DE VENDAS - 27/02/98
* JAIR GONCALVES
*

PRIVATE APePag, ReprSelec, dInicio, dFim, Tecla, qtddup,;
        NCOLS, AliqIR, TEMAVSP, DIRAVSP, MAXLIN 

MAXLIN 	     = 64
NaoPaga      = 1
PagaSFat     = 2
NaoPagarMais = 3

DIMENSION APePag(4)
*===========================================================================
USE PARCOMIS

* Mudanca de criterio no pagamento de comissoes
* do sistema de pagamento por grupo para pagamento por Total de Venda
* e inclusao do comissionamento sobre a venda direta e comissao sobre faturamento

dMudanca    = CTOD('')              && Data da mudanca de criterio para pagamento de comissao
AliqIR      = parcomis->ALIQIR        && Aliquota do Imposto de Renda  
IncluirIPI  = parcomis->COMIPI        && Define se deve ser calculada a comissao sobre o IPI
PgComGrupo  = parcomis->PGCOMGRUPO    && Calcular sobre o Grupo ou sobre o total de venda
ComisVenda  = parcomis->COMISVENDA    && Comissao sobre Venda de Representante
ComisDireta = parcomis->COMISDIRET    && Comissao sobre Venda Direta
ComisFatur  = parcomis->COMISFAT      && Comissao sobre faturamento 
FATCOMIS    = parcomis->FATMINIMO     && Valor limite de faturamento para pagamento de comissao
VALMINIR    = PARCOMIS->VALMINIR      && Valor limite para dedu��o do I.R.

*===========================================================================
DIRAVSP = GETENV('AVSP')
TEMAVSP = ! EMPTY(DIRAVSP)
  
MODULO = PROGRAM()
CLOSE DATA
NCOLS = 133
do TESTIMP2
if CP = 'N'
  return
endif
dInicio = {}
dFim = {}
QtdDup = 0
=janela(15,36,20,78)
=borda(15,36,20,78)
=InicDatas()
@17,38 say 'Data para sele��o - inicial:' GET dINICIO
@18,38 say '                    final..:' GET dFIM
read
MENS = DADCORR
do PEGRESP with [SN]
if MC = 'N' .or. empty(mc)
  return
endif

=SelecNfs()
  
IF ! AbreDbf()
  =Finaliza()
  Return
ENDIF   

MDIA = ALLTRIM(STR(DAY(DATE())))
MMES = MONTH(DATE())
MANO = ALLTRIM(STR(YEAR(DATE())))
MMESREF  = UPPER(GETNOMEMES[MONTH(dInicio)]) + '/' + ALLTRIM(STR(YEAR(dInicio)))
MDATAREF = MDIA + ' de ' + UPPER(GETNOMEMES[MMES]) + ' de ' + MANO

FazMalaDir = .F.
MC = 'N'
MENS = 'Prepara Mala Direta dos Representantes'
DO PEGRESP with [SN]
IF MC = 'S'
  MDATADEP = {}
  @23, 02 CLEAR TO 23, 78 
  @23, 02 SAY 'Data do Dep�sito da Comiss�o: ' GET MDATADEP
  READ
  FazMalaDir = MC = 'S'
ENDIF    

ReprSelec = .F.  && Todos os Representantes
Continua  = .T.

MENS = 'Deseja Selecionar Todos os Representantes'
do PEGRESP with [SN]
if MC = 'N'
  DO WHILE SelecRepr()
    ReprSelec = .T.  
    IF ! PgComGrupo
      =CalcComisVnd()
    ELSE
      =CalcComisGrp()
    ENDIF    
    =AbreDbf()
  ENDDO  
else
  IF ! PgComGrupo
    =CalcComisVnd()
  ELSE
    =CalcComisGrp()
  ENDIF    
endif

=FINALIZA()

*-------------------- Sub-Rotinas --------------------------
*====================
FUNCTION CALCCOMISVND
*====================
  sele regiao
  INDEX ON FMTREGIAO(REGIAO,IDREGIAO) TO XTMP 
  sele CADWORK
  go top
  =mensag('Calculando comissoes...')
  DO WHILE ! EOF()
    MVEND     = FmtRepr(VEN, IDREPR)
    MREGIAO   = FmtRegiao(REGIAO, IDREGIAO)
    IF ListaRepr()
      MPGCOMIS  = .F.
      IF SEEK(MREGIAO, 'REGIAO') 
        MPGCOMIS = REGIAO->PGCOMIS
      ENDIF  
      VALCOMI   = 0.00000 
      VALCOMII  = 0.00000 
      VALCOMFAT = 0.00000  
      VALIR     = 0.00000
      MCOMISSAO = ObComissao(DEMISSAO)
      * Subtrai o Desconto Concedido na Duplicata
      MVALDUP = VDP - DESC - VALDEV
      * Subtrai o IPI da 1a duplicata
      IF PDUP = 1 
        MVALDUP  = MVALDUP - VALIPI
      ENDIF   
      VALCOMI = MVALDUP * (MCOMISSAO / 100)       &&-- Comissao sobre Valor
      *--- Se paga comissao sobre faturamento 
      IF DUPL->PGCOMFAT = PagaSFat .AND. SUBSTR(DUPL->EMIS,4,7) = '03/1999'
        VALCOMFAT = VALCOMFAT + MVALDUP
      ENDIF
      VALIR = VALCOMI * ALIQIR / 100
      SELECT COMISSAO
      APPEND BLANK
      repl CGC      with CADWORK->CGC
      repl DUPL     with CADWORK->NNF
      repl PDUP     with CADWORK->PDUP
      repl EMISSAO  with CADWORK->DEMISSAO
      repl VENC     with CADWORK->VENC
      repl RECEB    with CADWORK->PAG
      repl VALDUP   with MVALDUP
      repl VALPROD  with CADWORK->VALPROD
      repl JUROS    with CADWORK->JUROS
      repl VEND     with MVEND
      repl REGIAO   with MREGIAO
      repl VALIPI   with CADWORK->VALIPI
      repl COMISSAO with VALCOMI
      repl COMISFAT with VALCOMFAT
      repl PED      with CADWORK->PED
      repl PINT     with CADWORK->OBS + ' ' + CADWORK->PINT
      repl VALDEV   with cadwork->VALDEV
      repl DESC     with cadwork->DESC
      repl IR       with VALIR
      repl PAI      with CADWORK->PAI
    ENDIF
    select cadwork 
    SKIP
  ENDDO
  
  close data

  =IMPRELAT()
  
  IF FazMalaDir
    =CRIAMALADIR()
  ENDIF
  =GeraTabExcel()
  
RETURN 

*====================
FUNCTION CALCCOMISGRP
*====================
RETURN

  
*================
FUNCTION FINALIZA
*================
  close data 
* if file('comtemp.dbf')
*   erase comtemp.dbf
* endif

return

*=================
FUNCTION SELECREPR
*=================
  PRIVATE xlixo, Tecla, Dbf, mregiao

  * Esta rotina seleciona alguns representantes  
  DBF = SELECT() 
  sele 0
  USE VEND ORDER P_VEND
  sele ComTemp
  ZAP
  @23,01 clear to 23,78
  save screen to xlixo
  @04,00 clear to 21,79
  @18,28 say 'Tecle ESC para cancelar'
  mregiao = EmptyRegiao
  mcvr    = EmptyRepr
  Tecla   = 0
  do while Tecla <> 27 .AND. Tecla <> 13
    @12,30 say 'Vendedor.....:' get mcvr    pict '9999.99';
                                valid MCVR = EmptyRepr .OR. ChkRepr(MCVR, .T.)
    @14,30 say 'Regiao.......:' get mregiao pict '9999.99';
                                valid mregiao = EmptyRegiao .OR. ChkRegiao(MREGIAO, .T.)
    read
    Tecla = lastkey()
    IF Tecla = 13  && ENTER
      MENS = DADCORR
      DO PegResp WITH [SN]
      IF MC = 'N'
         Tecla = 0
         =MENSAG('')
         LOOP
      ENDIF   
      SET ORDER TO TAG PRIMX
      IF ! seek(MCVR + MREGIAO) 
        append blank
        replace repr     with mcvr
        replace regiao   with mregiao
      ELSE
        =MENSAG('Dados j� digitados - ENTER')
        Tecla = 0
      ENDIF
    ENDIF
  enddo
  restore screen from xlixo
  SELECT(DBF)  

RETURN Tecla = 13

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


*===============
FUNCTION ABREDBF
*===============

  sele 0 
  USE CADWORK
  go bott
  QTDDUP = RECNO()
  if bof()
    =MENSAG('N�o houve Duplicatas recebidas no per�odo - ENTER')
    =INKEY(0)
    close data
    return .F.
  endif
  * Cria Arquivo de Comissao
  SELE 0
  CREATE COMISNEW FROM XCOMIS 
  USE
  SELE 0
  USE COMISNEW ALIAS COMISSAO
  sele 0
  USE REGIAO ORDER P_REGIAO
  sele 0
  use ComTemp
  sele 0
  use PEDIC  ORDER P_PED
  SELE 0
  USE DUPL
  INDEX ON STR(NDUP,5)+STR(PDUP,2) TO TMPDUP
    
RETURN .T.  

*=================
function ListaRepr
*=================
  PRIVATE nDbf, mfaz
  
 * Esta fun��o decide se o vendedor + regiao deverao ser listados
 nDbf = SELECT()
 mfaz = .f.
 sele ComTemp
 if reccount() > 0
   SET ORDER TO TAG REPRX
   if seek(MVEND)
     IF ComTemp->REGIAO = EmptyRegiao
       mfaz = .t.
     ELSE  
       SET ORDER TO TAG REGIAOX
       if SEEK(MREGIAO)
         mfaz = .T.
       endif  
     ENDIF  
   endif
 else
   mfaz = .t.  
 endif
 select(nDbf)

return mfaz

*******************
FUNCTION OBCOMISSAO
*******************
  PARAMETER DtEmissao
  
  PRIVATE lPagaDireto 
  
  lPagaDireto  = DtEmissao >= dMudanca
  PgComGrupo   = DtEmissao < dMudanca

  mcomissao  = 0.0
  IF MVEND = '0199.07' 
    IF lPagaDireto 
      mcomissao = ComisDireta
    ENDIF  
  ELSE    
    IF MPGCOMIS
      IF PgComGrupo 
        mgrupo = prod->grupo
        =seek(mgrupo,'grupos')
        mcomissao = grupos->comissao
      ELSE
        mcomissao = ComisVenda        
      ENDIF
    ENDIF  
  ENDIF
  
RETURN mcomissao

******************
PROCEDURE SELECNFS
******************

  CLOSE DATA
  
  =MENSAG('Calculando faturamento...')
  VALFATUR = CalcTotFat()
 
  * Se paga comissao sobre o faturamento 
  CodPag = IIF(VALFATUR > FATCOMIS, PagaSFat, NaoPaga)

  USE DUPL
  REPLACE ALL PGCOMFAT WITH CodPag FOR EMIS >= dInicio .AND. EMIS <= dFim  
  USE
  
  IF TEMAVSP
    USE &DIRAVSP\CABPEDSP
    REPLACE ALL PGCOMFAT WITH CodPag FOR DTENTREG >= dInicio .AND. DTENTREG <= dFim  
    USE
  ENDIF
  
  =MENSAG('Selecionando as Notas Fiscais que pagam comiss�o... Aguarde')
  
  SELECT;
    NF.NNF, NF.DEMISSAO, NF.CGC, NF.REGIAO, NF.IDREGIAO, NF.VEN, NF.IDREPR, NF.VALIPI, NF.VALPROD,;  
    NF.CP1, NF.CP2, NF.CP3, NF.CP4, NF.PE1, NF.PE2, NF.PE3, NF.PE4, NF.PED, PED.PINT,;
    DP.PDUP, DP.VENC, DP.VALDEV, DP.VDP, DP.PAG, DP.JUROS, DP.DESC, DP.PGCOMFAT,;
    ' ' AS OBS, V.PAI;
  FROM;
    "DUPL.DBF" DP, "CABECNF.DBF" NF, "TPNF.DBF" TP, "PEDIC.DBF" PED, "VEND.DBF" V;
  WHERE;
    (PAG BETWEEN dInicio AND dFim);
    AND;
    (NF.NNF = DP.NDUP AND NF.CANCELADA = .F.);
    AND;
    (NF.PED = PED.PED); 
    AND;
    (NF.TIPNF = TP.TIPO AND TP.VENDA = "S");
    AND (NF.VEN = V.CVR AND NF.IDREPR = V.IDREPR);
  ORDER BY;
    NF.REGIAO, NF.IDREGIAO, NF.VEN, NF.IDREPR, NF.NNF;
  INTO TABLE;
    CADWORK  
         
CLOSE DATA

*******************
FUNCTION CALCTOTFAT
*******************
  PRIVATE MTOTAL, MVENDASP
  
  CLOSE DATABASES
  MVENDASP = 0
  MTOTAL = 0
  * Calcula total de faturamento NF
  SELECT;
    SUM(VALPROD) AS TOTAL;
  FROM;
    CABECNF NF, TPNF TP;
  WHERE;
    NF.CANCELADA = .F.; 
    AND (DEMISSAO BETWEEN dInicio AND dFim);
    AND (NF.TIPNF = TP.TIPO AND TP.VENDA = "S");
  INTO TABLE;
    VENDANF
  MTOTAL = VENDANF->TOTAL
  SELECT CABECNF 
  USE
  SELECT TPNF
  USE

  IF TEMAVSP
    * Calcula total de faturamento AVSP
    SELECT;
      SUM(VALPEDIDO) AS VALSP;
      FROM &DIRAVSP\CABPEDSP.DBF;
      WHERE DTENTREG BETWEEN dInicio AND dFim;
      INTO TABLE;
        VENDASP
     MVENDASP = VENDASP->VALSP 
     SELECT VENDASP
     USE     
  ENDIF   
  MTOTAL = MTOTAL + MVENDASP

  CLOSE DATABASES
  
RETURN MTOTAL


*=============================================================================
*  Imprime o Relatorio de comissoes 
*=============================================================================
******************
PROCEDURE IMPRELAT
******************   
  PRIVATE cCliente, CABEC2
  
  CABEC2 = 'REPR.   REGIAO  DUP/PARC N.GUERRA         EMISSAO    VENCTO     RECEBTO    ATR VAL.DUPL.  DESC.DUPL. VAL.IPI COMISSAO  PED.   P.REPR'

  cCliente = SPACE(15)
  sele 0
  use COMISNEW ALIAS COMISSAO
  index ON VEND + REGIAO + str(DUPL,6)  TAG VENDX
  go top
  sele 0
  USE VEND 
  INDEX ON FMTREPR(CVR,IDREPR) TO X1
  sele 0
  use REGIAO 
  INDEX ON FMTREGIAO(REGIAO,IDREGIAO) TO X2
  sele 0
  use CLIEN ORDER P_CGC
  sele 0
  use DUPL ORDER XDUPANO
  store space(3) to MVEN, KEY
  store 0 to PAGCTR, MVDP, TCOM, VCOM, MATRASO, MCOMISFAT, MVALDP
  TAMREL = 2
  =MENSAG('Imprimindo Comiss�o de Vendedores ... Aguarde')
  do DEFSAIDA
  sele COMISSAO
  ImpDetalhe = vopcao = 1

  GCOMISSAO = 0
  GVALIPI   = 0
  GIR       = 0
  GCOMFAT   = 0
  GVALDUP   = 0
  GDESC     = 0
  DO WHILE ! EOF()
    * Imprimir Cabec Vendedor
    if ImpDetalhe
      =ImprCabec()
    endif  
    VCOMISSAO = 0
    VVALIPI   = 0
    VIR       = 0
    VCOMFAT   = 0
    VVALDUP   = 0
    VDESC     = 0
    MVEND = VEND
    lNovoVen = .T.
    DO WHILE ! EOF() .AND. MVEND = VEND
      MREGIAO = REGIAO
      lNovaReg = .T.
      RCOMISSAO = 0
      RVALIPI   = 0
      RIR       = 0
      RCOMFAT   = 0
      RVALDUP   = 0
      RDESC     = 0
      DO WHILE ! EOF() .AND. MVEND = VEND .AND. MREGIAO = REGIAO 
        MCOMISFAT = COMISFAT
        MDUPL     = DUPL
        MPDUP     = PDUP
        MVALOR    = VALPROD
        =GetValLin()
        if ImpDetalhe
          =ImprLinha()
        endif  
        lNovoVen = .F.
        lNovaReg = .F.
        RVALDUP   = RVALDUP   + MVALDP
        RCOMISSAO = RCOMISSAO + COMISSAO  && Acumula a Comissao 
        RCOMFAT   = RCOMFAT   + MCOMISFAT && Acumula a Comissao sobre o faturamento
        RValIpi   = RValIpi   + VALIPI    && Acumula o IPI
        RIR  	  = RIR       + IR        && Acumula Imposto de Renda
        RDESC     = RDESC     + DESC      && Acumula a Desconto s/Duplicata
        SKIP
      ENDDO && Loop Regiao 
      VVALDUP   = VVALDUP   + RVALDUP
      VCOMISSAO = VCOMISSAO + RCOMISSAO
      VCOMFAT   = VCOMFAT   + RCOMFAT      
      VVALIPI   = VVALIPI   + RValIpi
      VIR       = VIR       + RIR       
      VDESC     = VDESC     + RDESC
      * Imprimir Resumo Regiao
      IF ImpDetalhe
        =ImpTotReg()
      ENDIF
    ENDDO  && Loop Representante
    IF VIR < VALMINIR
      VIR = 0
    ENDIF  
    GVALDUP   = GVALDUP   + VVALDUP
    GCOMISSAO = GCOMISSAO + VCOMISSAO    
    GCOMFAT   = GCOMFAT   + VCOMFAT    
    GDESC     = GDESC     + VDESC
    GIR       = GIR       + VIR    
    * Imprimir Resumo do Vendedor
    if ImpDetalhe
      =ImpTotRep()
    endif  
  ENDDO 
  * Imprimir Total de Comissoes
  =ImpTotGer()
  do IMPSAIDA
 
RETURN

*==================
PROCEDURE IMPRLINHA
*==================
  IF lNovoVen 
    @lin, 001 SAY MVEND
  ENDIF   
  IF lNovaReg
    @lin, 009 SAY MREGIAO
  ENDIF  

  IF COMISFAT > 0
    @lin, 017 say '#'
  ENDIF
  @lin,018 say MDUPL       pict '99999'
  @lin,023 say MPDUP       pict '-9'
  @lin,026 say cCliente
  @lin,043 say EMISSAO
  @lin,054 say VENC
  @lin,065 say RECEB
     
  if MATRASO <> 0
    @lin,076 say MATRASO pict '999'
  endif

  @lin,080 say MVALDP             pict '99,999.99'
  @lin,090 say COMISSAO->DESC     pict '99,999.99'
  @lin,100 say VALIPI             pict '9,999.99' 
  @lin,109 say COMISSAO           pict '99,999.99'
  @lin,119 say PED                pict '99,999'
  @lin,126 say PINT 
  if VALDEV > 0
    =AddLin()
    @lin,115 say 'C/DEVOL.: ' + TRANSFORM(VALDEV, '99,999.99')
  endif
  =AddLin()

RETURN

*******************
PROCEDURE IMPTOTREG
*******************

   @lin,1 say replicate('-',NCOLS)
   =AddLin()
   @lin,010 say 'TOTAL DA REGIAO'
   @lin,080 say RVALDUP                 pict '99,999.99'
   @lin,090 say RDESC                   pict '99,999.99'
   @lin,100 say RVALIPI                 pict '9,999.99'
   @lin,110 say RCOMISSAO               pict '9,999.99'
   =AddLin()
   @lin,001 say replicate('=',NCOLS)
   =AddLin()

RETURN   

*******************
PROCEDURE IMPTOTREP
*******************
  PRIVATE Texto

  IF lin + 4 > MAXLIN
    =ImprCabec()
  ENDIF  
  @lin+0,010 say 'TOTAL do REPRESENTANTE'
  @lin+0,080 say VVALDUP                 pict '99,999.99'
  @lin+0,090 say VDESC                   pict '99,999.99'
  @lin+0,100 say VVALIPI                 pict '9,999.99'
  @lin+0,110 say VCOMISSAO               pict '9,999.99'

  IF VIR > 0
    @lin+1,110 say 'I.R.: ' + TRANSFORM( AliqIR, [(99.9 %)] ) + ':'
    @lin+1,126 say TRANSFORM( VIR, '9,999.99')  
  ELSE
    @lin+1,110 say 'I.R.: << ISENTO >>'
  ENDIF    

  @lin+2,115 say 'LIQUIDO:' 
  @lin+2,125 say TRANSFORM((VCOMISSAO - VIR), '99,999.99')
  
  @lin+3,001 say replicate('*',NCOLS)
  lin = lin + 4
 
RETURN  

*******************
PROCEDURE IMPRCABEC
*******************

  PAGCTR = PAGCTR + 1
  @0,001 say MREL
  @0,030 say MODULO
  @0,045 say 'COMISSOES DE REPRESENTANTES DE '+DTOC(dINICIO)+' ATE '+DTOC(dFIM)
  @0,112 say dtoc(date())
  @0,124 say 'PAG '+str(PAGCTR,3)
  =seek(COMISSAO->vend,'vend')
  =seek(COMISSAO->REGIAO,'regiao')
  @1,001 say alltrim(vend->raz)
  @2,001 say replicate('*',NCOLS)
  @3,001 say CABEC2
  lin = 4
  
RETURN

*******************
PROCEDURE IMPCABREG
*******************

  @lin, 001 say MVEND
  @lin, 011 say MREGIAO

RETURN

*******************
PROCEDURE IMPTOTGER
*******************
  PRIVATE MTOTAL, MCOMLIQ, MVALCOMFAT
  
  MCOMLIQ    = GCOMISSAO - GIR
  MVALCOMFAT = GCOMFAT * COMISFATUR / 100
  MTOTAL     = MCOMLIQ + MVALCOMFAT
  PAGCTR     = PAGCTR + 1
  lin = 1
  @lin+0,001 say MREL
  @lin+0,030 say MODULO
  @lin+0,045 say 'COMISSOES DE REPRESENTANTES DE '+DTOC(dINICIO)+' ATE '+DTOC(dFIM)
  @lin+0,114 say dtoc(date())
  @lin+0,124 say 'PAG '+str(PAGCTR,3)
  @lin+1,001 say replicate('*',NCOLS)
  @lin+2,001 say CABEC2
  @lin+3,001 say replicate('*',NCOLS)  
  @lin+4,010 say 'TOTAL DAS COMISSOES'
  @lin+4,077 say GVALDUP                 pict '999,999.99'
  @lin+4,088 say GDESC                   pict '999,999.99'
  @lin+4,100 say GValIpi                 pict '99,999.99'        
  @lin+4,110 say GCOMISSAO               pict '99,999.99'
  @lin+5,117 say 'BRUTO: ' + TRANSFORM( GCOMISSAO, '999,999.99' )
  @lin+6,110 say 'I.R.' + TRANSFORM( AliqIR, [(99.9 %)] ) + ':-'
  @lin+6,126 say GIR pict '9,999.99'
  @lin+7,010 say 'QUANTIDADE DE DUPLICATAS '+str(QTDDUP,4)  
  @lin+7,123 say '==========='
  @lin+8,115 say 'LIQUIDO:'
  @lin+8,125 say TRANSFORM(MCOMLIQ, '99,999.99')
  @lin+9,106 say '# COMIS.S/FATUR.:+' + TRANSFORM(MVALCOMFAT, '999,999.99')  
  @lin+10,123 say '==========='
  @lin+11,111 say 'TOTAL GERAL: ' + TRANSFORM(MTOTAL, '999,999.99')  
  @lin+12,001 say replicate('*',NCOLS)
  @lin+13,010 say 'F I N A L    D E S T E    R E L A T O R I O'
  @0, 0
  
RETURN


*******************
PROCEDURE ACUMTOTAL
*******************

  *---- Acumula a Comissao
  VCOMISSAO = (VCOMISSAO + COMISSAO)
  RCOMISSAO = (RCOMISSAO + COMISSAO)
  GCOMISSAO = (GCOMISSAO + COMISSAO)
  
  *---- Acumula a Comissao sobre o faturamento
  VCOMFAT = (VCOMFAT + MCOMISFAT)
  RCOMFAT = (RCOMFAT + MCOMISFAT)
  GCOMFAT = (GCOMFAT + MCOMISFAT)

  *---- Acumula o Vendedor
  VValIpi   = VVALIpi + VALIPI
  VSIPI     = VSIPI   + MVALOR
  VCIPI     = VCIPI   + MVALDP
     
  *---- Acumula a Regiao
  RValIpi   = RVALIPI + VALIPI      
  RSIPI     = RSIPI   + MVALOR
  RCIPI     = RCIPI   + MVALDP
      
  *---- Acumula Global
  GValIpi   = GVaLIpi + VALIPI
  GSIPI     = GSIPI   + MVALOR
  GCIPI     = GCIPI   + MVALDP
  
  *---- Acumula Imposto de Renda
  RIR  = RIR  + IR
  GIR  = GIR  + IR
  VIR  = VIR  + IR

  *---- Acumula a Desconto s/Duplicata
  VDESC = (VDESC + DESC)
  RDESC = (RDESC + DESC)
  GDESC = (GDESC + DESC)

RETURN


****************
PROCEDURE ADDLIN
****************

  lin = lin + 1
  IF lin > MAXLIN
    =ImprCabec()
  ENDIF
  
RETURN 

*******************
PROCEDURE GETVALLIN
*******************

  MCGC = CGC
  if seek(MCGC,'clien')
    cCliente = CLIEN->GUE
  else
    cCliente = SUBSTR(MCGC,1,15)
  endif
  MATRASO = RECEB - VENC
  MVALDP  = VALDUP 

RETURN  

**********************
PROCEDURE CRIAMALADIR
**********************
  PRIVATE OldDbf

  =MENSAG('Gerando Arquivo de Mala Direta...')
  SELE 0
  USE MALADCOM  
  ZAP
  SELE 0
  USE COMISNEW ALIAS COMISSAO ORDER VENDX
  SELE 0 
  USE VEND 
  INDEX ON FMTREPR(CVR,IDREPR) TO X
  
  SELE COMISSAO
  SET FILTER TO VEND <> '0199.07'
  GO TOP
  DO WHILE ! EOF()
    MVEND = VEND
    =SEEK(MVEND, 'VEND')
    SELE MALADCOM
    APPEND BLANK
    REPLACE DATA    WITH MDATAREF
    REPLACE DATADEP WITH DTOC(MDATADEP)
    REPLACE MESREF  WITH MMESREF
    REPLACE REPR    WITH MVEND
    REPLACE NOME    WITH VEND->RAZ
    REPLACE ENDER   WITH VEND->END
    REPLACE BAIRRO  WITH VEND->BAI
    REPLACE CIDADE  WITH VEND->CID
    REPLACE EST     WITH VEND->EST
    REPLACE CEP     WITH VEND->CEP
    SELECT COMISSAO
    GTOTDUP = 0.0
    GTOTCOM = 0.0
    GTOTIR  = 0.0
    CONT    = 1
    DO WHILE ! EOF() .AND. MVEND = VEND
      MREGIAO = REGIAO
      MTOTDUP = 0.0
      MTOTCOM = 0.0
      MTOTIR  = 0.0
      DO WHILE ! EOF() .AND. MVEND = VEND .AND. MREGIAO = REGIAO
        MTOTDUP = MTOTDUP + VALDUP
        MTOTCOM = MTOTCOM + COMISSAO
        MTOTIR  = MTOTIR  + IR
        SKIP
      ENDDO
      GTOTDUP = GTOTDUP + MTOTDUP
      GTOTCOM = GTOTCOM + MTOTCOM
      GTOTIR  = GTOTIR  + MTOTIR
      SELECT MALADCOM
      CREGIAO   = 'REGIAO'   + STR(CONT,1)
      CVALOR    = 'VALOR'    + STR(CONT,1)
      CCOMISSAO = 'COMISSAO' + STR(CONT,1)
      REPLACE &CREGIAO   WITH MREGIAO
      REPLACE &CVALOR    WITH TRANSFORM(MTOTDUP, '99,999,999.99')
      REPLACE &CCOMISSAO WITH TRANSFORM(MTOTCOM, '99,999,999.99') 
      SELECT COMISSAO
      CONT = CONT + 1
    ENDDO  
    SELE MALADCOM
    IF GTOTIR < VALMINIR
      GTOTIR = 0
      TEXTO = 'ISENTO'
    ELSE
      TEXTO = TRANSFORM(GTOTIR, '999,999.99')    
    ENDIF  
    REPLACE TOTVAL WITH TRANSFORM(GTOTDUP, '99,999,999.99')
    REPLACE TOTCOM WITH TRANSFORM(GTOTCOM, '99,999,999.99')
    REPLACE TOTIR  WITH TEXTO
    REPLACE TOTLIQ WITH TRANSFORM(GTOTCOM - GTOTIR, '99,999,999.99')    
    SELE COMISSAO
  ENDDO  
  =MENSAG('')
RETURN

**********************
PROCEDURE GERATABEXCEL
**********************
  PRIVATE OldDbf

  =MENSAG('Gerando Resumo de Comiss�o...')
  CLOSE DATA
  SELE 0
  USE RESCOM  
  ZAP
  COPY FIELD PAI TO TMP1
  SELE 0 
  USE TMP1 
  INDEX ON PAI TO TMP1
  SELE 0
  USE COMISNEW ALIAS COMISSAO 
  INDEX ON PAI+VEND TO TMP
  SELE 0 
  USE VEND 
  INDEX ON FMTREPR(CVR,IDREPR) TO X
  
  SELE COMISSAO
  GO TOP
  DO WHILE ! EOF()
    MPAI = PAI
    CONT = 0
    GTOTDUP = 0
    GTOTCOM = 0
    GTOTIR  = 0
    SELE COMISSAO    
    DO WHILE !EOF() .AND. MPAI = PAI
      MVEND = VEND
      =SEEK(MVEND, 'VEND')
      MNOME = VEND->RAZ
      SELE RESCOM
      APPEND BLANK
      REPLACE REPR    WITH MVEND + ' - ' + VEND->RAZ
      IF EMPTY(VEND->BANCO) 
        MBANCO = 'SEM BANCO'
      ELSE
        MBANCO =  VEND->BANCO 
      ENDIF  
      REPLACE BANCO   WITH MBANCO
      REPLACE AGCONTA WITH VEND->AGENCIA + ' - ' + VEND->CTACORR
      MTOTDUP = 0
      MTOTCOM = 0
      MTOTIR  = 0
      SELECT COMISSAO
      DO WHILE ! EOF() .AND. MPAI = PAI .AND. MVEND = VEND
        MTOTDUP = MTOTDUP + VALDUP
        MTOTCOM = MTOTCOM + COMISSAO
        MTOTIR  = MTOTIR  + IR
        SELECT COMISSAO
        SKIP
      ENDDO && Fim Loop Vendedor
      IF MTOTIR >= VALMINIR
        SELECT TMP1
        APPEND BLANK
        REPLACE PAI WITH MPAI
      ENDIF  
      SELECT RESCOM
      REPLACE FATLIQ   WITH MTOTDUP
      REPLACE COMTOT   WITH MTOTCOM
      REPLACE IR       WITH MTOTIR    
      REPLACE PAI	   WITH MPAI
      REPLACE COMLIQ   WITH COMTOT - IR
      GTOTDUP = GTOTDUP + MTOTDUP
      GTOTCOM = GTOTCOM + MTOTCOM
      GTOTIR  = GTOTIR  + MTOTIR
      SELE COMISSAO
      IF ! EMPTY(PAI) THEN
        CONT = CONT + 1
      ENDIF  
    ENDDO && Fim Loop Pai
    IF CONT > 1
      IF GTOTIR >= VALMINIR THEN
        SELECT TMP1
        APPEND BLANK
        REPLACE PAI WITH MPAI
      ENDIF  
      SELECT RESCOM
      APPEND BLANK
      REPLACE PAI	   WITH MPAI
      REPLACE EXCLUIR  WITH '*'
      REPLACE REPR     WITH '** TOTAL DE ' + MNOME
      REPLACE FATLIQ   WITH GTOTDUP
      REPLACE COMTOT   WITH GTOTCOM
      REPLACE IR       WITH GTOTIR    
      REPLACE COMLIQ   WITH GTOTCOM - GTOTIR
    ENDIF  
    SELECT COMISSAO
  ENDDO  && Fim Loop Geral

  * Zera o campo de IR para os que ficaram, fora da faixa
  SELECT RESCOM
  GO TOP
  DO WHILE ! EOF()
    IF ! SEEK(PAI,'TMP1')
      REPLACE IR WITH 0
    ENDIF   
    SKIP
  ENDDO  
  
  * Calcula Acumulado por Banco
  CLOSE DATA
  SELECT;
     BANCO, SUM(COMLIQ) AS TOTAL;
    FROM "RESCOM.DBF";
    WHERE NOT EXCLUIR = '*';
    GROUP BY BANCO;
    INTO TABLE TMP.DBF
  CLOSE DATA
  SELE 0
  USE TMP
  SELE 0
  USE RESCOM
  APPEND BLANK
  APPEND BLANK
  REPLACE BANCO WITH 'TOTAL POR BANCO'
  SELE TMP
  GO TOP
  DO WHILE !EOF()
    SELE RESCOM
    APPEND BLANK
    REPLACE BANCO WITH TMP->BANCO
    REPLACE TOTAL WITH TMP->TOTAL
    SELE TMP
    SKIP
  ENDDO  
  SELE RESCOM
  T = ALLTRIM(STR(MONTH(dInicio),2))
  NOME = 'CO_' + REPLICATE('0',2-LEN(T)) + T + SUBSTR(MANO,3,2) + '.DBF'
  COPY FIELD REPR, FATLIQ, COMTOT, IR, RETENCOES, COMLIQ, BANCO, AGCONTA, TOTAL TO &NOME 
  CLOSE DATA
  =MENSAG('')

RETURN
