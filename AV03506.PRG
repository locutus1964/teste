* AV03503 - CALCULO E IMPRESSAO DAS COMISSOES DE VENDEDORES
* SISTEMA ADMINISTRACAO DE VENDAS - 27/02/98
* JAIR GONCALVES
*

PRIVATE APePag, ReprSelec, dInicio, dFim, Tecla, qtddup, NCOLS, AliqIR

DIMENSION APePag(4)
* Porcentagem do Imposto de Renda
AliqIR = 1.5        
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
sele 0
SELECT;
   DUPL.*, NF.CP1, NF.CP2, NF.CP3, NF.CP4, NF.PE1, NF.PE2, NF.PE3, NF.PE4;
 FROM;
   DUPL, CABECNF NF;
 WHERE;
   (DUPL.NDUP = NF.NNF AND NF.CANCELADA = .F.) AND;
   (CTOD(PAG) >= dInicio AND CTOD(PAG) <= dFim);
 INTO TABLE;
   CADWORK  

*--- Remover a linha ao processar a comissao de Fev/98
IF dInicio <= {01/03/98} 
  APPEND FROM DUPL FOR NDUP = 3620
  APPEND FROM DUPL FOR NDUP = 3586
ENDIF
          
CLOSE DATA
  
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
      APEPAG[1] = CADWORK->PE1
      APEPAG[2] = CADWORK->PE2
      APEPAG[3] = CADWORK->PE3
      APEPAG[4] = CADWORK->PE4
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
            *---- Procura a referencia e o grupo
            MREF  = REF
            =seek(mref,'prod')
            mgrupo = prod->grupo
            =seek(mgrupo,'grupos')
            mcomissao = grupos->comissao
            MVALOR  = MVALOR  + VAL
            MVALIPI = MVALIPI + IPI
            if MPGCOMIS .and. MVEND <> '0199' .and. VAL > 0
               VALCOMI  = VALCOMI  + ((VAL * MCOMISSAO) / 100)  &&-- Comissao sobre Valor
               *-- Para nao calcular a comissao sobre o IPI, comentar esta linha
               *VALCOMII = VALCOMII + ((IPI * MCOMISSAO) / 100)  &&-- Comissao sobre o IPI
            endif
            skip
         enddo 
         *--- Fim do Loop de Itens da Nota Fiscal
         
         *===========
         * Duplicatas  
         *===========
                  
         * Obtem a diferenca entre a parte inteira das duplicatas e os centavos
 
         DPAR = 0      
         DPAR = AcumDifDupl( MVALOR, APEPAG[1], APEPAG[2], APEPAG[3], APEPAG[4]) 
 
         DPARIPI = 0
         DPARIPI = AcumDifDupl( MVALIPI, APEPAG[1], APEPAG[2], APEPAG[3], APEPAG[4]) 
 
         MVALDUP = INT(MVALOR * APEPAG[MPDUP] / 100) 
         
         sele PEDIC
         =seek(str(MPED,6))
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
         repl VEND     with MVEND
         repl REGIAO   with MREGIAO
         repl VALIPI   with MIPIDUP
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
  index ON VEND + REGIAO + str(DUPL,6)  TO COMISSAO.IDX
*  index ON REGIAO + VEND + str(DUPL,6)  TO COMISSAO.IDX
  go top
  sele 0
  use vend index vend.IDX
  sele 0
  use REGIAO inde REGIAO.IDX
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
    MVEND = VEND  
    lNovoVen = .T.
    DO WHILE ! EOF() .AND. MVEND = VEND 
      MREGIAO = REGIAO
      lNovaReg = .T.
      STORE 0.00 TO RCOMISSAO, RSIPI, RCIPI, RVALIPI, RIR
      DO WHILE ! EOF() .AND. MVEND = VEND .AND. MREGIAO = REGIAO 
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
      cCliente = SPACE(15)  
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
    @lin, 001 SAY VEND
  ENDIF   
  IF lNovaReg
    @lin, 006 SAY REGIAO
  ENDIF  
  
  @lin,011 say MDUPL       pict '999999'
  @lin,018 say MPDUP       pict '9'
  @lin,020 say cCliente
  @lin,037 say EMISSAO
  @lin,046 say VENC
  @lin,055 say RECEB
     
  if MATRASO <> 0
    @lin,064 say MATRASO pict '999'
  endif

  @lin,068 say MVALOR             pict '999,999.99'
  @lin,080 say MVALDP             pict '999,999.99'
  @lin,092 say VALIPI             pict '9,999.99' 

  IF COMISSAO->DESC <> 0
    @lin, 100 say '*'
  ENDIF
        
  @lin, 101 say COMISSAO pict '99,999.99'
      
  @lin,111 say PED  pict '999,999'
  @lin,120 say PINT
  if MVALDEV > 0
    @lin,130 say 'C/DEVOL.'
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
  use vend index vend.IDX 
  sele ArqTemp
  index on vend + regiao to arqtemp.IDX
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
  use PROD   inde PROD.IDX
  sele 0
  use GRUPOS inde GRUPOS.IDX
  sele 0
  use CADNF  ORDER X_NFREF
  copy stru fields vend, regiao to ArqTemp
  sele 0
  use ArqTemp
  sele 0
  use CLIEN inde CLIEN.IDX
  sele 0
  use REGIAO inde REGIAO.IDX
  sele 0
  use PEDIC  ORDER P_PED

RETURN .T.  

*******************
PROCEDURE IMPTOTREG
*******************

   @lin,1 say replicate('-',NCOLS)
   lin  = lin + 1
   @lin,010 say 'TOTAL DA REGIAO'
   @lin,068 say RSIPI                   pict '999,999.99'
   @lin,080 say RCIPI                   pict '999,999.99'
   @lin,091 say RVALIpi                 pict '99,999.99'
   @lin,101 say RCOMISSAO               pict '99,999.99'
   lin = lin + 1
   @lin,001 say replicate('=',NCOLS)
   lin = lin + 1

RETURN   

*******************
PROCEDURE IMPTOTREP
*******************

  @lin,113 say 'I.R.' + TRANSFORM( AliqIR, [ (99.9 %) ] ) + ' LIQUIDO'
  lin = lin + 1
  @lin,010 say 'TOTAL do REPRESENTANTE'
  @lin,066 say VSIPI                   pict '9,999,999.99'
  @lin,078 say VCIPI                   pict '9,999,999.99'
  @lin,091 say VVALIPI                 pict '99,999.99'
  @lin,100 say VCOMISSAO               pict '999,999.99'
  
  @lin,111 say TRANSFORM( RIR, '99,999.99')
  @lin,122 say (RCOMISSAO - RIR)  pict '999,999.99'
  lin = lin + 2
  @lin,1 say replicate('*',NCOLS)
  lin = lin + 2
 
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
  =seek(str(comissao->dupl,6),'dupl')
  =seek(dupl->cgc,'clien')
  =seek(clien->ven,'vend')
  =seek(clien->regiao,'regiao')
  @2,001 say alltrim(vend->raz) + ' - ' + alltrim(regiao->dregiao) + ' - ' + alltrim(regiao->repres)
  @3,001 say replicate('*',NCOLS)
  @4,001 say 'REP. REG. DUP/PARC N.GUERRA         EMISSAO   VENCTO  RECEBTO  ATR VALOR S/IPI VALOR C/IPI    IPI    COMISSAO   PEDID   PED.REPR'
RETURN

*******************
PROCEDURE IMPCABREG
*******************

  @lin, 001 say VEND
  @lin, 006 say REGIAO

RETURN

*******************
PROCEDURE IMPTOTGER
*******************

  PAGCTR = PAGCTR + 1
  @01,001 say MREL
  @01,030 say MODULO
  @01,045 say 'COMISSOES DE REPRESENTANTES DE '+DTOC(dINICIO)+' ATE '+DTOC(dFIM)
  @01,114 say dtoc(date())
  @01,124 say 'PAG '+str(PAGCTR,3)
  @02,001 say replicate('*',NCOLS)
  @03,113 say 'I.R.' + TRANSFORM( AliqIR, [(99.9 %)] ) + '  LIQUIDO'
  lin = 4
  @lin,010 say 'TOTAL DAS COMISSOES'
  @lin,066 say GSIPI                   pict '9,999,999.99'
  @lin,078 say GCIPI                   pict '9,999,999.99'
  @lin,091 say GValIpi                 pict '99,999.99'        
  @lin,100 say GCOMISSAO               pict '999,999.99'
  @lin,111 say GIR                     pict '99,999.99'
  @lin,122 say (GCOMISSAO - GIR)       pict '999,999.99'
  lin = lin + 2
  @lin,010 say 'QUANTIDADE DE DUPLICATAS '+str(QTDDUP,4)
  lin = lin + 2
  @lin,001 say replicate('*',NCOLS)
  lin = lin + 1
  @lin,010 say 'F I N A L    D E S T E    R E L A T O R I O'

RETURN

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
