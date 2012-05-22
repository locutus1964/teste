* CONCEITO - CONTROLE DOS CONCEITOS
* SISTEMA DE EMISSAO/CONTROLE DE PGI
* ADAO ELI PEREIRA - V 1.4 - 18/11/90
*
Sele 0
use CONCEITO index CONCEITO.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Atualiza‡„o do Conceito dos Clientes'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'C¢digo do Conceito'
@ 12,15 say 'Descri‡„o'
@ 14,15 say 'Colorir Conceito'
@ 16,15 say 'Conceito SCI'
mconceito = space(02)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    INC = ' '
    mdconceito = SPACE(30)
    mcolorir  = space(01)
    mdcolorir = space(30)
    mconcsci = '00'
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get mconceito pict '@!'
    read
      
    if lastkey() = 27
      if mdel = 1
        pack
      endif
      close databases
      clear
      restore screen from main2_scr
      return
    endif

    if SEEK(mconceito)
       INC = '*'
       @ 12,40 say dconceito color &corver
       mdconceito = dconceito
       @ 14,40 say colorir color &corver
       mcolorir   = colorir
       @ 16,40 say concsci color &corver
       mconcsci   = concsci
    else
       @ 12,40 say mdconceito color &corver
       @ 14,40 say mconcsci color &corver
       @ 16,40 say concsci color &corver
       MENS = TESTECAD
       DO PEGRESP WITH [SN]
       if mc = 'N'
         loop
       endif
     endif

     IF SEEK(MCONCEITO)
        MENS = TESTEINC
        DO PEGOPCAO WITH [123]

        if opcao = '1'
          loop
        endif
        if opcao = '3'
          delete
          mdel = 1
          loop
        endif
     ENDIF

    @ 23,01 say space(78)
    do while .t.
      @ 12,40 get mdconceito
         read
           if mdconceito <> space(10)
             exit
           endif
         ? MENSAG(CALPHA)
    enddo
    
    ? mensag('Preencher com Asterisco se desejar mostrar colorido no Cliente')
    @ 14,40 get mcolorir
      read
    ? mensag('Preencher com o C¢digo de conceito definido pelo SCI')
    @ 16,40 get mconcsci PICT [99]
    read
*
    MENS = DADCORR
    DO PEGRESP WITH [SN]
      IF MC = 'N'
        LOOP
      ENDIF
*
    IF INC = ' '
      appe blan
    endif
*
    repl conceito with mconceito
    repl dconceito with mdconceito
    repl colorir   with mcolorir
    repl concsci   with mconcsci
*
  enddo
  clos data
return
