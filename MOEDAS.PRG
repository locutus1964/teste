* MOEDAS - CADASTRA MOEDAS
* SISTEMA ADMINISTRACAO DE VENDAS - 01/09/93
* ADAO ELI PEREIRA
*
? CABTELA(0,0,22,79)
MENS = 'Atualiza‡„o de Moedas'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 08,15 say 'C¢digo da Moeda'
@ 11,15 say 'Descri‡„o'
@ 14,15 say 'Abreviatura'
*
CLOS DATA
sele 0
  use CADMOEDA  INDE CADMOEDA.IDX
sele 0 
  use NOMMOEDA  INDE NOMMOEDA.IDX
  set rela to str(moeda,3) into cadmoeda
  set skip to cadmoeda
*

sele NOMMOEDA

DO WHILE .T.
mmoeda  = 0
mdmoeda = SPACE(26)
mamoeda = SPACE(08)
@ 08,39 get mmoeda PICT '999'
  ? MENSAG(ECODIGO)
  read
  
  if lastkey() = 27
    clos data
    clear
    return
  endif
  
  if mmoeda = 0
    ? MENSAG(CNUMER)
    loop
  endif
*
  if !seek(str(mmoeda,3))
    @ 11,39 say mdmoeda
    @ 14,39 say mamoeda
      MENS = testecad
      DO PEGRESP WITH [SN]
        if mc = 'N'
          loop
        endif
  else
    @ 11,39 say dmoeda
      stor dmoeda to mdmoeda
    @ 14,39 say amoeda
      stor amoeda to mamoeda
*
    mens = testeinc
    do pegopcao with [123]
    if opcao = '1'
      loop
    endif
    
    if opcao = '3'
       MENS = 'Esta op‡„o exclui as paridades. Continua ?'
       DO PEGRESP WITH [SN]
       if mc = 'S'
         sele CADMOEDA
         IF SEEK(STR(MMOEDA,3))
           do while moeda = mmoeda
             delete
             MDEL = 1
             skip
           enddo
         ENDIF  
       endif
       loop
    endif
  endif
*
  @ 23,01 say space(78)
  do while .t.
    @ 11,39 get mdmoeda
    read
      if mdmoeda <> space (25)
        exit
      endif
    MENS = CALPHA
    ? MENSAG(MENS)
  enddo
*
  @ 23,01 say space(78)
  do while .t.
    @ 14,39 get mamoeda
      read
        if mamoeda <> space (8)
          exit
        endif
      MENS = CALPHA
      ? MENSAG(MENS)
  enddo
*
  MENS = DADCORR
  DO PEGRESP WITH [SN]
      if mc = 'N'
        loop
      endif
*
  sele NOMMOEDA
  if !SEEK(STR(MOEDA,3))
    appe blan
  endif
*
  repl moeda with mmoeda,dmoeda with mdmoeda,amoeda with mamoeda
*
enddo
clos data
return

