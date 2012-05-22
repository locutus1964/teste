* AV05993 - ATUALIZACAO DAS TABELAS DE PRECOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 15/05/90 - 26/11/91
*
? CABTELA(0,0,24,79)
vNOMEPRG = 'AV05993'
*
sele 0
  use SIGLAS   index SIGLAS.IDX
sele 0
  use CTR_SENH index CTR_SENH.IDX
sele 0
  use GRUPOS   index GRUPOS.IDX
sele 0
  use TABPRE   index TABPRE.IDX
*
MENS = 'Tabelas de Pre‡os'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 08,04 say 'Grupo'
@ 08,15 say 'Descri‡„o do Grupo'

@ 08,50 say 'Ano'

@ 14,04 say 'Percentuais de Desconto mensal'
@ 15,04 say 'Jan   Fev   Mar   Abr   Mai   Jun   Jul   Ago   Set   Out   Nov   Dez'

@ 18,10 say 'Data/Resp. p/alt.'

mdel = 0
mano = SPACE(02)
*
do while .T.
*
mgrupo = space(02)
mper01  = 0.00
mper02  = 0.00
mper03  = 0.00
mper04  = 0.00
mper05  = 0.00
mper06  = 0.00
mper07  = 0.00
mper08  = 0.00
mper09  = 0.00
mper10  = 0.00
mper11  = 0.00
mper12  = 0.00
msigla  = space(04)
*
? MENSAG(ECODIGO)
*
sele GRUPOS
@ 09,05 get mgrupo pict '@!'
  read

    if LASTKEY() = 27
      if mdel = 1
        mc = ' '
        MENS = 'Houve cancelamentos. Compacta o Arquivo? (S/N) '
        ? MENSAG(MENS)
          @ 23,POS say MENS get mc pict '!' VALID(MC$'SN')
            read
              if mc = 'S'
                sele TABPRE
                pack
              endif
      endif
        clos data
        clear
        return
    endif
    
    if mgrupo = space(02)
      ? MENSAG(CALPHA)
      =INKEY(0)
      loop
    else
      sele grupos
        if !seek(mgrupo)
          ? MENSAG('Grupo n„o cadastrado. - ENTER')
          =INKEY(0)
          loop
          
        else
          mdgrupo = dgrupo
          @ 09,15 say dgrupo   color &corver
        endif
    endif
*
@ 09,51 get mano
  read
  if mano = space(02)
    ? mensag(calpha)
    loop
  endif
  
  sele TABPRE
  if !seek(mgrupo + mano)  
    mens = TESTECAD
    do pegresp with [SN]
      if mc = 'N'
        loop
      endif
  endif   
*
  if seek(mgrupo + mano)
    @ 16,04 say per01 pict '99.99' color &corver
      mper01 = per01
    @ 16,10 say per02 pict '99.99' color &corver
      mper02 = per02
    @ 16,16 say per03 pict '99.99' color &corver
      mper03 = per03
    @ 16,22 say per04 pict '99.99' color &corver
      mper04 = per04
    @ 16,28 say per05 pict '99.99' color &corver
      mper05 = per05
    @ 16,34 say per06 pict '99.99' color &corver
      mper06 = per06
    @ 16,40 say per07 pict '99.99' color &corver
      mper07 = per07
    @ 16,46 say per08 pict '99.99' color &corver
      mper08 = per08
    @ 16,52 say per09 pict '99.99' color &corver
      mper09 = per09
    @ 16,58 say per10 pict '99.99' color &corver
      mper10 = per10
    @ 16,64 say per11 pict '99.99' color &corver
      mper11 = per11
    @ 16,70 say per12 pict '99.99' color &corver
      mper12 = per12
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
    mens = testeinc
    do pegopcao with [123]
      if opcao = '1'
        loop
      endif
        if opcao = '3'
          SELE TABPRE
          delete
          mdel =1
          loop
        endif
  endif
  
  if opcao = '2' .or. opcao = '3'
    do senha
  endif  
*
    @ 16,04 get mper01 pict '99.99'
    @ 16,10 get mper02 pict '99.99' 
    @ 16,16 get mper03 pict '99.99'
    @ 16,22 get mper04 pict '99.99'
    @ 16,28 get mper05 pict '99.99'
    @ 16,34 get mper06 pict '99.99'
    @ 16,40 get mper07 pict '99.99'
    @ 16,46 get mper08 pict '99.99'
    @ 16,52 get mper09 pict '99.99'
    @ 16,58 get mper10 pict '99.99'
    @ 16,64 get mper11 pict '99.99'
    @ 16,70 get mper12 pict '99.99'
    read
*
do limpa23  
SELE SIGLAS
do while .t.
  mdtalt = HOJE
  @ 18,30 say mdtalt 
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
    
*
  mens = dadcorr
  do pegresp with [SN]
    if mc = 'N'
      loop
    endif
*
  sele TABPRE
    if !seek(mgrupo + mano)
      appe blan
    endif
*
  repl grupo with mgrupo
  repl ano with mano
  repl per01 with mper01
  repl per02 with mper02
  repl per03 with mper03
  repl per04 with mper04
  repl per05 with mper05
  repl per06 with mper06
  repl per07 with mper07
  repl per08 with mper08
  repl per09 with mper09
  repl per10 with mper10
  repl per11 with mper11
  repl per12 with mper12
  repl dtalt with DTOC(mdtalt) 
  repl sigla with msigla
*
enddo


