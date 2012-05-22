* AV03508 - CALCULO E IMPRESSAO DAS COMISSOES DE VENDEDORES
* SISTEMA ADMINISTRACAO DE VENDAS - 27/02/98
* JAIR GONCALVES
*

PRIVATE APePag, ReprSelec, dInicio, dFim, Tecla, qtddup, NCOLS, AliqIR

DIMENSION APePag(4)

* Data da mudanca de criterio para pagamento de comissao
* do sistema de pagamento por grupo para pagamento por Total de Venda
* e inclusao do comissionamento sobre a venda direta
dMudanca = {01/06/98}
* Aliquota do Imposto de Renda  
AliqIR = 1.5      
* Define se deve ser calculada a comissao sobre o IPI
IncluirIPI = .F.  
* Define se o calculo da comissao sera sobre o Grupo do Produto ou 
* sobre o total de venda
ComisGrupo  = .T.  
* Comissao sobre Venda de Representante
ComisVenda = 8.50
* Comissao sobre Venda Direta
ComisDireta = 3.20

MODULO = PROGRAM()
NCOLS = 133
do TESTIMP2
if CP = 'N'
  return
endif
dInicio = {}
dFim = {}
QtdDup = 0
=janela(15,38,20,78)
=borda(15,38,20,78)
=InicDatas()
@17,40 say 'Data para sele‡„o - inicial:' GET dINICIO
@18,40 say '                    final..:' GET dFIM
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

ReprSelec = .F.  && Todos os Representantes
Continua  = .T.

MENS = 'Deseja Selecionar Todos os Representantes'
do PEGRESP with [SN]
if MC = 'N'
  DO WHILE SelecRepr()
    ReprSelec = .T.  
    =CalcComissao()
    =AbreDbf()
  ENDDO  
else
  =CalcComissao()
endif

=FINALIZA()

*-------------------- Sub-Rotinas --------------------------
*====================
FUNCTION CALCCOMISSAO
*====================

  sele regiao
  SET ORDER TO P_REGIAO
  sele CADWORK
  go top
  MVALOR   = 0
  VALCOMI  = 0 
  valcomii = 0 
  MVALIPI  = 0
  =mensag( 'Calculando comissoes...')
  do while !eof()
    if VALDEV >= VDP
      skip
      loop
    endif
    MCGC      = CGC
    MNDUP     = NDUP
    MPDUP     = PDUP
    MVEN      = VEN
    MIDREPR   = IDREPR
    MVEND     = FMTREPR( VEN, IDREPR )
    MREGIAO   = FmtRegiao(REGIAO,IDREGIAO)
    MIDREGIAO = IDREGIAO
    MEMISSAO  = EMIS
    MVENC     = VENC
    MRECEB    = PAG
    MJUROS    = JUROS
    MDESC     = DESC
    MVALDEV   = VALDEV
    MVDP      = VDP
    MOBS      = OBS
    sele CADNF
    if SEEK( STR(MNDUP,5)+STR(MPDUP,2),'DUPL' )
      APEPAG[1] = CADWORK->PE1
      APEPAG[2] = CADWORK->PE2
      APEPAG[3] = CADWORK->PE3
      APEPAG[4] = CADWORK->PE4
      MPED = PED
      if ListaRepr()
        MPGCOMIS  = .F.
        MCOMISSAO = 0.0
        if SEEK( CADWORK->IDREGIAO, 'regiao')
          MPGCOMIS = regiao->PGCOMIS
        endif
        =SEEK( MNDUP ) 
        *---- Loop dos itens da Nota Fiscal
        do while NNF = MNDUP
          *---- Procura a referencia
          MREF  = REF
          =seek(mref,'prod')
          MVALOR  = MVALOR  + VAL
          MVALIPI = MVALIPI + IPI
          mcomissao = ObComissao( CADNF->DEMI )
          VALCOMI  = VALCOMI + ((VAL * MCOMISSAO) / 100)     &&-- Comissao sobre Valor
          IF IncluirIPI 
            VALCOMII = VALCOMII + ((IPI * MCOMISSAO) / 100)  &&-- Comissao sobre o IPI
          ENDIF  
          skip
        enddo 
        *--- Fim do Loop de Itens da Nota Fiscal
         
        * Processar Duplicatas  
        * Obtem a diferenca entre a parte inteira das duplicatas e os centavos
 
        DPAR = 0      
        DPAR = AcumDifDupl( MVALOR, APEPAG[1], APEPAG[2], APEPAG[3], APEPAG[4]) 
 
        DPARIPI = 0
        DPARIPI = AcumDifDupl( MVALIPI, APEPAG[1], APEPAG[2], APEPAG[3], APEPAG[4]) 
 
        MVALDUP = INT(MVALOR * APEPAG[MPDUP] / 100) 
        
        sele PEDIC
        =seek(MPED)
        MPINT   = PINT
         
        if MPDUP = 1
          MVALDUP  = MVALDUP + DPAR
          VALCOMI = VALCOMI + VALCOMII  
        endif
         
        *-- Calcula o valor do Ipi na Duplicata           
        MIPIDUP = CADWORK->VDP - MVALDUP
        MVALDUP = MVALDUP - (MDESC + MVALDEV)
        VALCOMI = ((VALCOMI * APEPAG[MPDUP]) / 100)
         
        sele COMISSAO
        APPEND BLANK
        repl DUPL     with MNDUP
        repl PDUP     with MPDUP
        repl EMISSAO  with CTOD(MEMISSAO)
        repl VENC     with CTOD(MVENC)
        repl RECEB    with CTOD(MRECEB)
        repl VALOR    with MVALDUP
        repl JUROS    with MJUROS
        repl VEND     with MVEN
        repl IDREPR   with MIDREPR
        repl REGIAO   with MREGIAO
        repl IDREGIAO with MIDREGIAO
        repl VALIPI   with MIPIDUP
        repl COMISSAO with VALCOMI
        repl PED      with MPED
        repl PINT     with MOBS+MPINT
        repl VALDEV   with cadwork->VALDEV
        repl DESC     with cadwork->DESC
        stor 0 to MVALIPI, VALCOMI, MVALOR, VALCOMII
      endif
    endif
    sele CADWORK
    skip
  enddo
  close data

  =ImpRelat()
  
RETURN 
  
*------------------ Imprime o Relatorio de comissoes -----------------
******************
PROCEDURE IMPRELAT
******************   
  PRIVATE cCliente

  cCliente = SPACE(15)
    
  sele 0
  use COMISSAO
  index ON VEND + STR(IDREPR,2) + REGIAO + str(DUPL,6)  TO COMISSAO.IDX
  go top
  sele 0
  USE VEND ORDER P_VEND
  sele 0
  use REGIAO ORDER P_REGIAO
  sele 0
  use CLIEN ORDER P_CGC
  sele 0
  use DUPL index DUPL.IDX
  store space(3) to MVEN, KEY
  store 0 to PAGCTR, MVDP, TCOM, VCOM
  TAMREL = 2
  =MENSAG('Imprimindo Comiss„o de Vendedores ... Aguarde')
  stor 0.00 to VCOMISSAO, RCOMISSAO, GCOMISSAO,;
               VSIPI,   RSIPI,   GSIPI,;
               VVALIPI, RVALIPI, GVALIPI,;
               VCIPI,   RCIPI,   GCIPI,; 
               IR,      RIR,     GIR,;
               MVALDP,  MATRASO
      
  do DEFSAIDA
  sele COMISSAO
  ImpDetalhe = vopcao = 1

  DO WHILE ! EOF()
    * Imprimir Cabec Vendedor
    if ImpDetalhe
      =ImprCabec()
    endif  

    STORE 0.00 TO VCOMISSAO, VSIPI, VCIPI, VVALIPI, VIR
    lin = 5
    MVEND = FMTREPR( VEND, IDREPR )  
    lNovoVen = .T.
    DO WHILE ! EOF() .AND. MVEND = FMTREPR(VEND,IDREPR)
      MREGIAO = FmtRegiao(REGIAO,IDREGIAO)
      lNovaReg = .T.
      STORE 0.00 TO RCOMISSAO, RSIPI, RCIPI, RVALIPI, RIR
      DO WHILE ! EOF() .AND. MVEND = FMTREPR(VEND,IDREPR) .AND. MREGIAO = FmtRegiao(REGIAO, IDREGIAO) 
        MDUPL = DUPL
        MPDUP = PDUP
        =GetValLin()
        if ImpDetalhe
          =ImprLinha()
        endif  
        =AcumTotal()
        lNovoVen = .F.
        lNovaReg = .F.
        SKIP
      ENDDO  
      * Imprimir Resumo Regiao
      IF ImpDetalhe
        =ImpTotReg()
      ENDIF
    ENDDO  
    * Imprimir Resumo do Vendedor
    if ImpDetalhe
      =ImpTotRep()
    endif  
  ENDDO
  * Imprimir Total de Comissoes
  =ImpTotGer()
  do IMPSAIDA
  
RETURN

*******************
PROCEDURE GETVALLIN
*******************

  if seek(str(MDUPL,6)+str(MPDUP,1),'dupl')
    MCGC = dupl->CGC
    MVALDP = (dupl->VDP - dupl->VALDEV)
    MVALDEV = dupl->VALDEV
    if seek(MCGC,'clien')
      cCliente = CLIEN->GUE
    else
      cCliente = SUBSTR(MCGC,1,15)
    endif
  endif
  * TVALOR  = (MVALDP + VALIPI)
  TVALOR  = MVALDP - MVALIPI
  MATRASO = RECEB - VENC
  MVALOR  = (VALOR - MVALDEV)  
  IR      = COMISSAO * AliqIR / 100
  
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

  @lin,017 say MDUPL       pict '999999'
  @lin,023 say MPDUP       pict '-9'
  @lin,026 say cCliente
  @lin,043 say EMISSAO
  @lin,053 say VENC
  @lin,062 say RECEB
     
  if MATRASO <> 0
    @lin,071 say MATRASO pict '999'
  endif

  @lin,077 say MVALOR             pict '99,999.99'
  @lin,089 say MVALDP             pict '99,999.99'
  @lin,099 say VALIPI             pict '9,999.99' 

  IF COMISSAO->DESC <> 0
    @lin, 107 say '*'
  ENDIF
        
  @lin, 110 say COMISSAO pict '99,999.99'
      
  @lin,120 say PED  pict '9,999'
  @lin,126 say PINT 
  if MVALDEV > 0
    @lin+1,126 say 'C/DEVOL.'
  endif
  
  lin = lin + 1

RETURN

*******************
PROCEDURE ACUMTOTAL
*******************

  *---- Acumula a Comissao
  VCOMISSAO = (VCOMISSAO + COMISSAO)
  RCOMISSAO = (RCOMISSAO + COMISSAO)
  GCOMISSAO = (GCOMISSAO + COMISSAO)

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
  RIR    = RIR  + IR
  GIR    = GIR  + IR
  VIR    = VIR  + IR

RETURN

*================
FUNCTION FINALIZA
*================
  close data 
*  if file('arqtemp.dbf')
*    erase arqtemp.dbf
*    erase arqtemp.idx
*  endif

return

*=================
FUNCTION SELECREPR
*=================
  PRIVATE xlixo, Tecla, Dbf, mregiao

  * Esta rotina seleciona alguns representantes  
  DBF = SELECT() 
  sele 0
  USE VEND ORDER P_VEND
  sele ArqTemp
  index on FmtRepr(VEN,IDREPR) + FmtRegiao(REGIAO,IDREGIAO) to arqtemp.IDX
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
      IF !seek(str(ExtractRepr(MCVR),5)+mregiao)
        append blank
        replace regiao   with regiao->regiao
        replace idregiao with regiao->idregiao
        replace ven      with vend->cvr
        replace idrepr   with vend->idrepr
      ELSE
        =MENSAG('Dados j  digitados - ENTER')
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
  copy stru fields VEN, IDREPR, REGIAO, IDREGIAO to ArqTemp
  go bott
  QTDDUP = RECNO()
  if bof()
    =MENSAG('N„o houve Duplicatas recebidas no per¡odo - ENTER')
    =INKEY(0)
    close data
    return .F.
  endif
  sele 0
  use COMISSAO
  zap
  sele 0
  USE REGIAO ORDER P_REGIAO
  sele 0
  use PROD   ORDER P_REF
  sele 0
  use GRUPOS inde GRUPOS.IDX
  sele 0
  use CADNF  ORDER F_NNF
  sele 0
  use ArqTemp
  sele 0
  use PEDIC  ORDER P_PED
  SELE 0
  USE DUPL
  INDEX ON STR(NDUP,5)+STR(PDUP,2) TO TMPDUP
    
RETURN .T.  

*******************
PROCEDURE IMPTOTREG
*******************

   @lin,1 say replicate('-',NCOLS)
   lin  = lin + 1
   @lin,010 say 'TOTAL DA REGIAO'
   @lin,076 say RSIPI                   pict '999,999.99'
   @lin,088 say RCIPI                   pict '999,999.99'
   @lin,098 say RVALIpi                 pict '99,999.99'
   @lin,110 say RCOMISSAO               pict '99,999.99'
   lin = lin + 1
   @lin,001 say replicate('=',NCOLS)
   lin = lin + 1

RETURN   

*******************
PROCEDURE IMPTOTREP
*******************

  @lin+0,010 say 'TOTAL do REPRESENTANTE'
  @lin+0,076 say VSIPI                   pict '999,999.99'
  @lin+0,088 say VCIPI                   pict '999,999.99'
  @lin+0,098 say VVALIPI                 pict '99,999.99'
  @lin+0,110 say VCOMISSAO               pict '99,999.99'

  @lin+1,110 say 'I.R.' + TRANSFORM( AliqIR, [(99.9 %)] ) + ':'
  @lin+1,126 say TRANSFORM( VIR, '9,999.99')  

  @lin+2,115 say 'LIQUIDO:' 
  @lin+2,125 say TRANSFORM((RCOMISSAO - VIR), '99,999.99')
  
  @lin+3,001 say replicate('*',NCOLS)
  lin = lin + 4
 
RETURN  

*******************
PROCEDURE IMPRCABEC
*******************

  PAGCTR = PAGCTR + 1
  @1,001 say MREL
  @1,030 say MODULO
  @1,045 say 'COMISSOES DE REPRESENTANTES DE '+DTOC(dINICIO)+' ATE '+DTOC(dFIM)
  @1,114 say dtoc(date())
  @1,124 say 'PAG '+str(PAGCTR,3)
  =seek(COMISSAO->IDREPR,'vend')
  =seek(COMISSAO->IDREGIAO,'regiao')
  @2,001 say alltrim(vend->raz)
  @3,001 say replicate('*',NCOLS)
  @4,001 say 'REPR.   REGIAO  DUP/PARC N.GUERRA         EMISSAO   VENCTO   RECEBTO  ATR VALOR S/IPI VALOR C/IPI      IPI    COMISSAO  PED. PED.REPR'

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

  PAGCTR = PAGCTR + 1
  lin = 1
  @lin+0,001 say MREL
  @lin+0,030 say MODULO
  @lin+0,045 say 'COMISSOES DE REPRESENTANTES DE '+DTOC(dINICIO)+' ATE '+DTOC(dFIM)
  @lin+0,114 say dtoc(date())
  @lin+0,124 say 'PAG '+str(PAGCTR,3)
  @lin+1,001 say replicate('*',NCOLS)
  @lin+3,010 say 'TOTAL DAS COMISSOES'
  @lin+3,076 say GSIPI                   pict '999,999.99'
  @lin+3,088 say GCIPI                   pict '999,999.99'
  @lin+3,098 say GValIpi                 pict '99,999.99'        
  @lin+3,110 say GCOMISSAO               pict '99,999.99'
  
  @lin+4,110 say 'I.R.' + TRANSFORM( AliqIR, [(99.9 %)] ) + ':'
  @lin+4,126 say GIR pict '9,999.99'
  @lin+5,010 say 'QUANTIDADE DE DUPLICATAS '+str(QTDDUP,4)  
  @lin+5,115 say 'LIQUIDO:'
  @lin+5,125 say TRANSFORM((GCOMISSAO - GIR), '99,999.99')
  @lin+7,001 say replicate('*',NCOLS)
  @lin+8,010 say 'F I N A L    D E S T E    R E L A T O R I O'
  @0, 0
  
RETURN

*=================
function ListaRepr
*=================
  PRIVATE nDbf, mfaz
  
 * Esta fun‡„o decide se o vendedor + regiao deverao ser listados
 nDbf = SELECT()
 mfaz = .t.
 sele ArqTemp
 if reccount() # 0
   if ! seek(MVEND+MREGIAO)
     mfaz = .f.
   endif
 endif
 select(nDbf)

return mfaz

*******************
FUNCTION OBCOMISSAO
*******************
  PARAMETER DtEmissao
  
  PRIVATE lPagaDireto 
  
  lPagaDireto = CTOD(DtEmissao) >= dMudanca
  ComisGrupo   = CTOD(dtEmissao) < dMudanca

  mcomissao  = 0.0
  IF VAL > 0 
    IF MVEND = '0199.07' 
      IF lPagaDireto 
        mcomissao = ComisDireta
      ENDIF  
    ELSE    
      IF MPGCOMIS 
        IF ComisGrupo 
          mgrupo = prod->grupo
          =seek(mgrupo,'grupos')
          mcomissao = grupos->comissao
        ELSE
          mcomissao = ComisVenda        
        ENDIF
      ENDIF  
    ENDIF  
  ENDIF
  
RETURN mcomissao

******************
PROCEDURE SELECNFS
******************

  =MENSAG('Selecionando as Notas Fiscais que pagam comiss„o... Aguarde')
  sele 0
  SELECT;
    D.NDUP, D.PDUP, D.EMIS, D.VENC, D.VALDEV, D.VDP, D.PAG, D.JUROS, D.DESC,;
    NF.NNF, NF.CGC, NF.REGIAO, NF.IDREGIAO, NF.VEN, NF.IDREPR,;
    NF.CP1, NF.CP2, NF.CP3, NF.CP4,;
    NF.PE1, NF.PE2, NF.PE3, NF.PE4,;
    ' ' AS OBS;
  FROM;
    DUPL D, CABECNF NF, TPNF T;
  WHERE;
    (D.NDUP = NF.NNF AND NF.CANCELADA = .F.); 
    AND (NF.TIPNF = T.TIPO AND T.VENDA = "S");
    AND (CTOD(PAG) BETWEEN dInicio AND dFim);
  INTO TABLE;
    CADWORK  
  *CLOSE DATA
  *--- Estas Notas nao foram processadas na comissao de Agosto/98
  *IF dInicio <= {01/09/98} 
  *  SELECT;
  *    D.NDUP, D.PDUP, D.EMIS, D.VENC, D.VALDEV, D.VDP, D.PAG, D.JUROS, D.DESC,;
  *    NF.NNF, NF.CGC, NF.REGIAO, NF.IDREGIAO, NF.VEN, NF.IDREPR,;
  *    NF.CP1, NF.CP2, NF.CP3, NF.CP4,;
  *    NF.PE1, NF.PE2, NF.PE3, NF.PE4,;
  *    '#' AS OBS;
  *  FROM;
  *    DUPL D, CABECNF NF, TPNF T;
  *  WHERE;
  *    (D.NDUP = NF.NNF AND NF.CANCELADA = .F.); 
  *    AND (NF.TIPNF = T.TIPO AND T.VENDA = "S");
  *    AND (CTOD(PAG) BETWEEN {01/08/98} AND {31/08/98});
  *    AND (NDUP IN (4807, 4795, 4804, 4810));
  *  INTO TABLE;
  *    MESANT
  
  * SELE CADWORK
  * APPEND FROM MESANT
                       
  *ENDIF
          
CLOSE DATA
