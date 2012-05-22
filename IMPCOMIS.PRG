***************************************************************************
* IMPCOMIS - MODULO DE IMPRESSAO DAS COMISSOES DE VENDEDORES
* SISTEMA ADMINISTRACAO DE VENDAS - 05/08/99
* JAIR GONCALVES
***************************************************************************

*------------------ Imprime o Relatorio de comissoes -----------------
******************
PROCEDURE IMPRELAT
******************   
  PRIVATE cCliente

  cCliente = SPACE(15)
    
  sele 0
  use COMISSAO
  index ON FmtRepr(VEND, IDREPR) + FmtRegiao(REGIAO, IDREGIAO) + str(DUPL,6)  TAG VENDX
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
               VCOMFAT, RCOMFAT, GCOMFAT,;
               MVALDP,  MATRASO, MCOMISFAT
      
  do DEFSAIDA
  sele COMISSAO
  ImpDetalhe = vopcao = 1

  DO WHILE ! EOF()
    * Imprimir Cabec Vendedor
    if ImpDetalhe
      =ImprCabec()
    endif  

    STORE 0.00 TO VCOMISSAO, VSIPI, VCIPI, VVALIPI, VIR, VCOMFAT
    MVEND = FMTREPR( VEND, IDREPR )  
    lNovoVen = .T.
    DO WHILE ! EOF() .AND. MVEND = FMTREPR(VEND,IDREPR)
      MREGIAO = FmtRegiao(REGIAO,IDREGIAO)
      lNovaReg = .T.
      STORE 0.00 TO RCOMISSAO, RSIPI, RCIPI, RVALIPI, RIR, RCOMFAT
      DO WHILE ! EOF() .AND. MVEND = FMTREPR(VEND,IDREPR) .AND. MREGIAO = FmtRegiao(REGIAO, IDREGIAO) 
        MCOMISFAT = COMISFAT
        MDUPL     = DUPL
        MPDUP     = PDUP
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
*    IF VIR > ValMinIR
*      GIR  = GIR  + VIR
*    ENDIF
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
  
  =AddLin()

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

RETURN

*******************
PROCEDURE IMPTOTREG
*******************

   @lin,1 say replicate('-',NCOLS)
   =AddLin()
   @lin,010 say 'TOTAL DA REGIAO'
   @lin,076 say RSIPI                   pict '999,999.99'
   @lin,088 say RCIPI                   pict '999,999.99'
   @lin,098 say RVALIpi                 pict '99,999.99'
   @lin,110 say RCOMISSAO               pict '99,999.99'
   =AddLin()
   @lin,001 say replicate('=',NCOLS)
   =AddLin()

RETURN   

*******************
PROCEDURE IMPTOTREP
*******************

  IF lin + 4 > MAXLIN
    =ImprCabec()
  ENDIF  
  @lin+0,010 say 'TOTAL do REPRESENTANTE'
  @lin+0,076 say VSIPI                   pict '999,999.99'
  @lin+0,088 say VCIPI                   pict '999,999.99'
  @lin+0,098 say VVALIPI                 pict '99,999.99'
  @lin+0,110 say VCOMISSAO               pict '99,999.99'

  @lin+1,110 say 'I.R.' + TRANSFORM( AliqIR, [(99.9 %)] ) + ':'
  @lin+1,126 say TRANSFORM( VIR, '9,999.99')  

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
  @0,114 say dtoc(date())
  @0,124 say 'PAG '+str(PAGCTR,3)
  =seek(COMISSAO->IDREPR,'vend')
  =seek(COMISSAO->IDREGIAO,'regiao')
  @1,001 say alltrim(vend->raz)
  @2,001 say replicate('*',NCOLS)
  @3,001 say 'REPR.   REGIAO  DUP/PARC N.GUERRA         EMISSAO   VENCTO   RECEBTO  ATR VALOR NF.   VALOR DUPL.  VALOR IPI  COMISSAO  PED. PED.REPR'
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
  
  @lin+2,001 say 'REPR.   REGIAO  DUP/PARC N.GUERRA         EMISSAO   VENCTO   RECEBTO  ATR VALOR S/IPI VALOR C/IPI      IPI    COMISSAO  PED. PED.REPR'

  @lin+3,001 say replicate('*',NCOLS)  
  
  @lin+4,010 say 'TOTAL DAS COMISSOES'
  @lin+4,076 say GSIPI                   pict '999,999.99'
  @lin+4,088 say GCIPI                   pict '999,999.99'
  @lin+4,098 say GValIpi                 pict '99,999.99'        
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
