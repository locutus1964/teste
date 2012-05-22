* AV05994 - ATUALIZACAO DOS PRECOS PRODUTOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06/09/93
*
vNOMEPRG = 'AV05994'
sele 0
  use PROD     index PROD.IDX
sele 0
  use CTR_SENH index CTR_SENH.IDX
sele 0
  use SIGLAS   index SIGLAS.IDX
sele 0
  use TABREF   index TABREF.IDX
*
? CABTELA(0,0,24,79)
*
MENS = 'Pre‡os dos Produtos'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 08,10 say 'C¢digo da Tabela'
@ 10,10 say 'C¢digo do Produto'
@ 12,10 say 'Tipo do Produto'
@ 14,10 say 'Descri‡„o'
@ 16,10 say 'Pre‡o Unit rio'
mdel = 0
*
do while .T.
*
sele TABREF
mpreco  = 0
mtabref = space(05)
mref    = space(04)
msigla  = space(04)
*
? MENSAG(ECODIGO)
*
@ 08,30 get mtabref pict '99/99'
  read

    if LASTKEY() = 27
      if mdel = 1
        mc = ' '
        mens = 'Houve cancelamentos. Compacta Arquivos? (S/N) '
        ? MENSAG(mens)
          @ 23,POS say mens get mc pict '!' valid(mc$'SN')
            read
              if mc = 'S'
                SELE TABREF
                pack
              endif
      endif
        clos data
        clear
        return
    endif

    if mtabref = space(03)
      ? MENSAG(calpha)
      =INKEY(0)
      loop
    endif
*
@ 23,01 SAY SPACE(78)

do while .t.

sele PROD

@ 10,30 get mref
  read
    if mref = space(04)
      ? MENSAG(CALPHA)
      =INKEY(0)
      loop
    endif

      if seek(MREF)
         @ 12,30 SAY TIPO color &corver
         @ 14,25 SAY DES color &corver
        exit
      else
        ? mensag(corrija)
      endif
enddo
*
inc = ' '
  sele TABREF
    if !seek(mtabref+mref)
      @ 16,30 say mpreco pict '999,999.9999' color &corver
      MENS = (TESTECAD)
      do pegresp with [SN]
      if mc = 'N'
        loop
      endif
    endif
*
@ 18,10 say 'Data/Resp. p/alt.'
*
IF SEEK(mtabref + mref)
  @ 16,30 say  preco pict '999,999.9999' color &corver
    mpreco = preco
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
          SELE TABREF
          dele
          mdel = 1
          loop
        endif
endif
*
IF OPCAO = '2' .OR. OPCAO = '3'
  DO SENHA
ENDIF
*
sele TABREF
@ 23,01 SAY SPACE(78)
do while .t.
@ 16,30  get mpreco pict '999,999.9999'
  read
    if mpreco > 0
      exit
    endif
      ? MENSAG(CNUMER)
enddo
*
do limpa23
SELE SIGLAS
do while .t.
  mdtalt = HOJE
  @ 18,30 say mdtalt  color &corver
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
  sele TABREF
    if !SEEK(MTABREF + MREF)
      appe blan
    endif
*
  replace tabref with mtabref,ref with mref,preco with mpreco
  replace dtalt  with mdtalt,sigla with msigla
*
enddo

