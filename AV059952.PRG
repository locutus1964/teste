* AV059942 - CUSTOS FINANCEIROS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06/09/93
*
CLOS DATA
vNOMEPRG = 'AV059952'
sele 0
  use CTR_SENH index CTR_SENH.IDX
sele 0
  use SIGLAS   index SIGLAS.IDX
sele 0
  use TABFIN2   index TABFIN2.IDX
*
? CABTELA(0,0,24,79)
*
MENS = 'Custos Financeiros'
? MENSAGR(MENS)
@ 01,POS SAY MENS
mdel = 0
@ 08,10 say 'C¢digo da Tabela'
@ 10,10 say 'Quant. de Parcelas'
@ 12,10 say 'Dias p/Faturamento'
@ 14,10 say 'Vencimentos'
@ 16,10 say '% de Custo Financeiro'
@ 18,10 say 'Data/Respons vel'
*
mTABFIN = space(05)
*
do while .T.
*
  mPERFIN = 0.00
  mdias_de = 0
  mVENCTOS = SPACE(20)
  mPARCELAS = 0
  
  @ 10,40 SAY mPARCELAS PICT '9' COLOR &CORVER
  @ 12,40 SAY mDIAS_DE PICT '999' COLOR &CORVER
  @ 14,40 SAY mVENCTOS COLOR &CORVER
  @ 16,40 SAY mPERFIN PICT '999.99' COLOR &CORVER
   
  ? MENSAG(ECODIGO)

  msigla  = space(04)
  @ 18,46 SAY SPACE(30)
*
SELE TABFIN2

@ 08,40 get mTABFIN pict '99/99'
  read

    if LASTKEY() = 27
      if mdel = 1
        mc = ' '
        mens = 'Houve cancelamentos. Compacta Arquivos? (S/N) '
        ? MENSAG(mens)
          @ 23,POS say mens get mc pict '!' valid(mc$'SN')
            read
              if mc = 'S'
                SELE TABFIN2
                pack
              endif
      endif
        clos data
        clear
        return
    endif
  
    if mTABFIN = space(03)
      ? MENSAG(calpha)
      =INKEY(0)
      loop
    endif
*
  @ 23,01 SAY SPACE(78)
  @ 10,40 GET mPARCELAS PICT '9' 
    READ
    
  @ 12,40 get mdias_de pict '999'
    read
 
  sele TABFIN2
    if !seek(mTABFIN+STR(mPARCELAS,1)+STR(MDIAS_DE,3))
      MENS = (TESTECAD)
      do pegresp with [SN]
      if mc = 'N'
        loop
      endif
    endif
*
  IF SEEK(mTABFIN + STR(mPARCELAS,1) + STR(mDIAS_DE,3))
    @ 14,40 say  VENCTOS color &corver
      mVENCTOS = VENCTOS
    @ 16,40 say  PERFIN pict '999.99' color &corver
      mPERFIN = PERFIN
    @ 18,30 say dtalt    COLOR &CORVER
    @ 18,40 say sigla    COLOR &CORVER
      msigla = sigla
      sele SIGLAS
      if seek(msigla)
        @ 18,46 say nome COLOR &CORVER
      else  
        @ 18,46 say space(30) COLOR &CORVER
      endif  
*
      MENS = (testeinc)
      DO PEGOPCAO WITH [123]
        if opcao = '1'
          loop
        endif
        
        IF OPCAO = '2' 
          DO SENHA
        ENDIF  
        
        if opcao = '3'
          DO SENHA
          SELE TABFIN2
          dele
          mdel = 1
          LOOP
        endif
  endif
*
*
  @ 23,01 SAY SPACE(78)
  @ 14,40 GET mVENCTOS PICT '@!'
  @ 16,40 get mPERFIN pict '999.99'
    read
*
*
  do limpa23  
  SELE SIGLAS
  do while .t.
    mdtalt = dtoc(HOJE)
    @ 18,30 say mdtalt pict '99/99/99' color &corver
    @ 18,40 get msigla pict '@!'
      read
      if msigla = space(04)
        ?MENSAG('Sigla do Respons vel n„o pode ser brancos')
        =INKEY(0)
        loop
      endif  
      if !seek(msigla)
        ? mensag('Sigla do Funcion rio autorizado n„o cadastrado - ENTER')
        =INKEY(0)
        loop  
      else
        @ 18,46 say nome  COLOR &CORVER
        exit
      endif  
      
  enddo      

  MENS = (DADCORR)
  DO PEGRESP WITH [SN]
      if mc = 'N'
        loop
      endif
*
*
  sele TABFIN2
    if !SEEK(MTABFIN + STR(mPARCELAS,1) + STR(mDIAS_DE,3))
      appe blan
    endif
*
  repl TABFIN with mTABFIN,PARCELAS with mPARCELAS,DIAS_DE with MDIAS_DE, ;
       VENCTOS with mVENCTOS,PERFIN with MPERFIN,dtalt  with mdtalt,sigla with msigla
*
enddo
* ---------------------------------------------------------------------
*enddo
