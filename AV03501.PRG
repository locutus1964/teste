* AV03500 - CALCULO E IMPRESSAO DAS COMISSOES DE VENDEDORES
* SISTEMA ADMINISTRACAO DE VENDAS - 03/03/87
* ADAO ELI PEREIRA
*

PRIVATE APePag, ReprSelec, dInicio, dFim, Tecla, qtddup, NCOLS  

DIMENSION APePag(4)
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
=MENSAG('Selecionando as Notas Fiscais que pagam comiss„o... Aguarde')
use DUPL
copy to CADWORK for CTOD(PAG) >= dINICIO .and.;
                    CTOD(PAG) <= dFIM .and.;
                    EMPTY(DTCANCEL)
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


*------------------- Sub-Rotinas
*====================
FUNCTION CALCCOMISSAO
*====================

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
    MNDUP    = NDUP
    MPDUP    = PDUP
    MEMISSAO = EMIS
    MVENC    = VENC
    MRECEB   = PAG
    MJUROS   = JUROS
    MDESC    = DESC
    MVALDEV  = VALDEV
    MVDP     = VDP
    sele CADNF
    if seek(str(MNDUP,6))
      MCGC = CGC
      MPE1  = PE1
      MPE2  = PE2
      MPE3  = PE3
      MPE4  = PE4
      MPED  = PED
      MVEND = VEN
      =seek(mcgc,'clien')
      mregiao = clien->regiao
      if av03500faz()
         MPGCOMIS = .F.
         if seek(MREGIAO,'regiao')
            MPGCOMIS = regiao->PGCOMIS
         endif
         *---- Loop dos itens da Nota Fiscal
         do while NNF = MNDUP
            MREF  = REF
            =seek(mref,'prod')
            mgrupo = prod->grupo
            =seek(mgrupo,'grupos')
            mcomissao = grupos->comissao
            MVAL    = (VAL - (MDESC + MVALDEV))
            MIPI    = IPI
            if MPDUP > 1
               MIPI = 0
            endif
            *---- CORRIGE VALOR QUANDO HOUVER DEVOLUCAO TOTAL
            if MVAL < 0
               MVAL = 0
            endif
            stor 0 to MDESC, MVALDEV
            MVALOR  = MVALOR  + MVAL
            MVALIPI = MVALIPI + MIPI
            if MPGCOMIS .and. MVEND <> '0199' .and. MVAL > 0
               VALCOMI  = VALCOMI  + ((MVAL * MCOMISSAO) / 100)  &&---- So Val
               VALCOMII = VALCOMII + ((MIPI * MCOMISSAO) / 100)  &&---- So IPI
            endif
            skip
         enddo   
         *---- Obtem a diferenca entre a parte inteira das duplicatas e os centavos
         DPAR = 0      
         DPAR = AcumDifDupl( MVALOR, MPE1, MPE2, MPE3, MPE4 ) 
         sele PEDIC
         =seek(str(MPED,6))
         MPINT = PINT
         if MPDUP = 1
            MVALOR = INT((MVALOR * MPE1) / 100) + MVALIPI + DPAR
            if VALCOMI > 0
               VALCOMI = ((VALCOMI * MPE1) / 100) + VALCOMII
            endif
         endif
         if MPE1 <> 100
            if MPDUP = 2
               MVALOR = INT((MVALOR * MPE2) / 100) 
               if VALCOMI > 0
                  VALCOMI = ((VALCOMI * MPE2) / 100)
               endif
               MVALIPI = 0.00
            endif
            if MPDUP = 3
               MVALOR = INT((MVALOR * MPE3) / 100)
               if VALCOMI > 0
                  VALCOMI = ((VALCOMI * MPE3) / 100)
               endif
               MVALIPI = 0.00
            endif
            if MPDUP = 4
               MVALOR = INT((MVALOR * MPE4) / 100)
               if VALCOMI > 0
                  VALCOMI = ((VALCOMI * MPE4) / 100)
               endif
               MVALIPI = 0.00
            endif
         endif
         sele COMISSAO
         APPE BLAN
         repl DUPL     with MNDUP
         repl PDUP     with MPDUP
         repl EMISSAO  with CTOD(MEMISSAO)
         repl VENC     with CTOD(MVENC)
         repl RECEB    with CTOD(MRECEB)
         repl VALOR    with MVALOR
         repl JUROS    with MJUROS
         repl VEND     with MVEND
         repl REGIAO   with MREGIAO
         repl VALIPI   with MVALIPI
         repl COMISSAO with VALCOMI
         repl PED      with MPED
         repl PINT     with MPINT
         repl VALDEV   with cadwork->VALDEV
         repl DESC     with cadwork->DESC
         stor 0 to MVALIPI, VALCOMI, MVALOR, VALCOMII
      endif
    endif
    sele CADWORK
    skip
  enddo
  close data
  sele 0
  use COMISSAO
  index ON VEND + REGIAO + str(DUPL,6)  TO COMISSAO
  go top
  sele 0
  use vend index vend
  sele 0
  use REGIAO inde REGIAO
  sele 0
  use CLIEN index CLIEN
  sele 0
  use DUPL index DUPL
  stor space(3) to MVEN, KEY
  stor 0 to PAGCTR, MVDP, TCOM, VCOM
  TAMREL = 2
  =MENSAG('Imprimindo Comiss„o de Vendedores ... Aguarde')
  stor 0.00 to VCOMISSAO, RCOMISSAO, GCOMISSAO, VSIPI, RSIPI, GSIPI, nIPI
  stor 0.00 to VCIPI, RCIPI, GCIPI, REG_IR, TOT_IR, nTotIpi, nGeralIpi
  do DEFSAIDA
  sele COMISSAO
  
  do while .not. eof()
    if vopcao = 1
      PAGCTR = PAGCTR + 1
      @1,001 say MREL
      @1,030 say MODULO=PROGRAM()
      @1,045 say 'COMISSOES DE REPRESENTANTES DE '+DTOC(dINICIO)+' ATE '+DTOC(dFIM)
      @1,114 say dtoc(date())
      @1,124 say 'PAG '+str(PAGCTR,3)
      =seek(str(comissao->dupl,6),'dupl')
      =seek(dupl->cgc,'clien')
      =seek(clien->ven,'vend')
      =seek(clien->regiao,'regiao')
      @2,001 say alltrim(vend->raz) + ' - ' + alltrim(regiao->dregiao) + ' - ' + alltrim(regiao->repres)
      @3,001 say replicate('*',NCOLS)
      @4,001 say 'REG. VEND DUP/PARC N.GUERRA         EMISSAO   VENCTO  RECEBTO  ATR VALOR S/IPI VALOR C/IPI    IPI    COMISSAO   PEDID   PED.REPR'
    endif
    stor 5 TO lin
    stor space(04) to MVEND, MREGIAO
    do while .not. eof() .and. lin < 64
      if REGIAO <> MREGIAO .or. VEND <> MVEND .or. lin = 5
         if vopcao = 1
            @lin,001 say REGIAO
            @lin,006 say VEND
         endif
         MREGIAO = REGIAO
      endif
      MVEND = VEND
      MDUPL = DUPL
      if vopcao = 1
        @lin,011 say DUPL               pict '999999'
        @lin,018 say PDUP               pict '9'
      endif
      MPDUP = PDUP
      if seek(str(MDUPL,6)+str(MPDUP,1),'dupl')
        MCGC = dupl->CGC
        MVALDP = (dupl->VDP - dupl->VALDEV)
        MVALDEV = dupl->VALDEV
        if seek(MCGC,'clien')
          if vopcao = 1
            @lin,020 say clien->GUE
          endif
        endif
      endif

      if vopcao = 1
        @lin,037 say EMISSAO
        @lin,046 say VENC
        @lin,055 say RECEB
      endif
      TVALOR = (MVALDP + VALIPI)
      MATRASO = RECEB - VENC
      MVALOR    = (VALOR - MVALDEV)  
      if vopcao = 1
        if MATRASO <> 0
           @lin,064 say MATRASO               pict '999'
        endif
        @lin,068 say MVALOR             pict '999,999.99'
        @lin,080 say MVALDP             pict '999,999.99'
        @lin,092 say VALIPI             pict '9,999.99' 
        @lin,101 say COMISSAO           pict '99,999.99'
      endif
      VCOMISSAO = (VCOMISSAO + COMISSAO)
      RCOMISSAO = (RCOMISSAO + COMISSAO)
      GCOMISSAO = (GCOMISSAO + COMISSAO)
      nIpi      = nIpi  + VALIPI
      VSIPI     = VSIPI + MVALOR - VALIPI
      RSIPI     = RSIPI + MVALOR - VALIPI
      GSIPI     = GSIPI + MVALOR - VALIPI
      nGeralIpi = nGeralIpi + VALIPI
      VCIPI     = VCIPI + MVALDP
      RCIPI     = RCIPI + MVALDP
      GCIPI     = GCIPI + MVALDP
      if vopcao = 1
        @lin,111 say PED  pict '999,999'
        @lin,120 say PINT
        if MVALDEV > 0
          @lin,130 say 'C/DEVOL.'
        endif
        stor lin + 1 TO lin
      endif
      skip
      if VEND <> MVEND .or. REGIAO <> MREGIAO .or. eof()
        if vopcao = 1
          @lin,1 say replicate('-',NCOLS)
          lin  = lin + 1
          @lin,010 say 'TOTAL DO VENDEDOR'
          @lin,068 say VSIPI                   pict '999,999.99'
          @lin,080 say VCIPI                   pict '999,999.99'
          @lin,091 say nIpi                    pict '99,999.99'
          @lin,101 say VCOMISSAO               pict '99,999.99'
          lin = lin + 1
          @lin,001 say replicate('=',NCOLS)
          lin = lin + 1
        endif
        stor 0.00 to VCOMISSAO, VSIPI, VCIPI, nIpi
      endif
      if REGIAO <> MREGIAO .or. eof()
        if vopcao = 1
          @lin,113 say 'I.R.(1,5%)  LIQUIDO'
          lin = lin + 1
          @lin,010 say 'TOTAL do REPRESENTANTE'
          @lin,066 say RSIPI                   pict '9,999,999.99'
          @lin,078 say RCIPI                   pict '9,999,999.99'
          @lin,091 say nIpi                    pict '99,999.99'
          @lin,100 say RCOMISSAO               pict '999,999.99'
        endif
        REG_IR = ((RCOMISSAO * 1.5) / 100)
        RREG_IR = ROUND(REG_IR,2)
        if vopcao = 1
          @lin,111 say TRANSFORM( RREG_IR, '99,999.99')
          @lin,122 say (RCOMISSAO - RREG_IR)  pict '999,999.99'
          lin = lin + 2
          @lin,1 say replicate('*',NCOLS)
          lin = lin + 2
        endif
        TOT_IR = (TOT_IR + RREG_IR)
        stor 0.00 to RCOMISSAO, VCOMISSAO, RSIPI, RCIPI, REG_IR, RREG_IR, nTotIpi
      endif
      if vopcao = 1
        if LEFT(REGIAO,2) <> LEFT(MREGIAO,2)
          lin = 70
        endif
      endif
      * Se e Fim de Arquivo e todos os Representantes foram selecionados
      if eof() .AND. ReprSelec == .F.
        PAGCTR = PAGCTR + 1
        @01,001 say MREL
        @01,030 say MODULO
        @01,045 say 'COMISSOES DE REPRESENTANTES DE '+DTOC(dINICIO)+' ATE '+DTOC(dFIM)
        @01,114 say dtoc(date())
        @01,124 say 'PAG '+str(PAGCTR,3)
        @02,001 say replicate('*',NCOLS)
        @03,113 say 'I.R.(1,5%)  LIQUIDO'
        lin = 4
        @lin,010 say 'TOTAL DAS COMISSOES'
        @lin,066 say GSIPI                   pict '9,999,999.99'
        @lin,078 say GCIPI                   pict '9,999,999.99'
        @lin,091 say nGeralIpi               pict '99,999.99'        
        @lin,100 say GCOMISSAO               pict '999,999.99'
        @lin,111 say TOT_IR                  pict '99,999.99'
        @lin,122 say (GCOMISSAO - TOT_IR)   pict '999,999.99'
        lin = lin + 2
        @lin,010 say 'QUANTIDADE DE DUPLICATAS '+str(QTDDUP,4)
        lin = lin + 2
        @lin,001 say replicate('*',NCOLS)
        lin = lin + 1
        @lin,010 say 'F I N A L    D E S T E    R E L A T O R I O'
      endif
    enddo
  enddo
  do IMPSAIDA
  
RETURN 

*================
FUNCTION FINALIZA
*================
  close data 
  if file('arqtemp.dbf')
    erase arqtemp.dbf
    erase arqtemp.idx
  endif

return


*=================
FUNCTION SELECREPR
*=================
  PRIVATE xlixo, Tecla, Dbf

  * Esta rotina seleciona alguns representantes  
  DBF = SELECT() 
  sele 0
  use vend index vend 
  sele ArqTemp
  index on vend + regiao to arqtemp
  @23,01 clear to 23,78
  save screen to xlixo
  @04,00 clear to 21,79
  @18,28 say 'Tecle ESC para cancelar'
  stor space(4) to mregiao, mcvr
  Tecla = 0
  do while Tecla <> 27 .AND. Tecla <> 13
    @12,30 say 'Regiao.......:' get mregiao pict '9999' valid seek(mregiao,'regiao')
    @14,30 say 'Vendedor.....:' get mcvr    pict '9999' valid seek(mcvr,'vend')
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
      IF !seek(mcvr+mregiao)
        append blank
        replace regiao with mregiao
        replace vend   with mcvr
      ELSE
        =MENSAG('Dados j  digitados - ENTER')
        Tecla = 0
      ENDIF
    ENDIF
  enddo
  restore screen from xlixo
  SELECT(DBF)  

RETURN Tecla = 13

*==================
function AV03500faz
*==================

* Esta fun‡„o decide se o vendedor + regiao deverao ser listados
 private mfaz
 mfaz = .t.
 sele ArqTemp
 if reccount() # 0
    if !seek(mvend+mregiao)
       mfaz = .f.
    endif
 endif
 sele CADNF

return mfaz


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
    =MENSAG('N„o houve Duplicatas recebidas no per¡odo - ENTER')
    =INKEY(0)
    close data
    return .F.
  endif
  sele 0
  use COMISSAO
  zap
  sele 0
  use PROD   inde PROD
  sele 0
  use GRUPOS inde GRUPOS
  sele 0
  use CADNF  inde CADNF
  copy stru fields vend, regiao to ArqTemp
  sele 0
  use ArqTemp
  sele 0
  use CLIEN inde CLIEN
  sele 0
  use REGIAO inde REGIAO
  sele 0
  use PEDIC  inde PEDIC

RETURN .T.  
