* AV059943 - DESCONTO SEMANAL
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06/09/93
*
vNOMEPRG = 'AV059943'
sele 0
  use CTR_SENH index CTR_SENH
sele 0
  use SIGLAS   index SIGLAS
sele 0
  use TABSEM   index TABSEM
*
? CABTELA(0,0,24,79)
*
MENS = 'Desconto Semanal'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 08,10 say 'C¢digo da Tabela'
@ 10,10 say 'Dias - de / at‚'
@ 12,10 say '% de Desconto'
mdel = 0
*
do while .T.
*
sele TABSEM
mPERDIAS = 0.00
mTABSEM = space(05)
mdias_de = space(05)
mdias_ate = space(05)
msigla  = space(04)
@ 18,46 SAY SPACE(30)
*
? MENSAG(ECODIGO)
*
@ 08,30 get mTABSEM pict '99/99'
  read

    if LASTKEY() = 27
      if mdel = 1
        mc = ' '
        mens = 'Houve cancelamentos. Compacta Arquivos? (S/N) '
        ? MENSAG(mens)
          @ 23,POS say mens get mc pict '!' valid(mc$'SN')
            read
              if mc = 'S'
                SELE TABSEM
                pack
              endif
      endif
        clos data
        clear
        return
    endif
  
    if mTABSEM = space(03)
      ? MENSAG(calpha)
      =INKEY(0)
      loop
    endif
*
@ 23,01 SAY SPACE(78)
do while .t.
@ 10,30 get mdias_de pict '99/99'
  read
    if mdias_de = SPACE(02)
      ? MENSAG(CNUMER)
      =INKEY(0)
      loop
    endif
  exit    
enddo
*
  sele TABSEM
    if !seek(mTABSEM+MDIAS_DE)
      @ 10,36 say MDIAS_ATE  color &corver
      @ 12,30 say mPERDIAS pict '99.99' color &corver
      MENS = (TESTECAD)
      do pegresp with [SN]
      if mc = 'N'
        loop
      endif
    endif
*
@ 18,10 say 'Data/Resp. p/alt.'
*
IF SEEK(mTABSEM + mDIAS_DE)
  @ 10,36 say  DIAS_ATE color &corver
    mDIAS_ATE = DIAS_ATE
  @ 12,30 say  PERDIAS pict '99.99' color &corver
    mPERDIAS = PERDIAS
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
        if opcao = '3'
          SELE TABSEM
          dele
          mdel = 1
          LOOP
        endif
endif
*
IF OPCAO = '2' .OR. OPCAO = '3'
  DO SENHA
ENDIF  
*
sele TABSEM
@ 23,01 SAY SPACE(78)
@ 10,36 get MDIAS_ATE PICT '99/99'
  READ
  
@ 12,30  get mPERDIAS pict '99.99'
  read
*
do limpa23  
SELE SIGLAS
do while .t.
  mdtalt = dtoc(date())
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
  sele TABSEM
    if !SEEK(MTABSEM + MDIAS_DE))
      appe blan
    endif
*
  repl TABSEM with mTABSEM,DIAS_DE with MDIAS_DE
  repl DIAS_ATE with MDIAS_ATE,	PERDIAS with MPERDIAS
  repl dtalt  with mdtalt,sigla with msigla
*
enddo
