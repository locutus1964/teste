* AV05997 - ATUALIZACAO GERAL DE PRECOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 08/07/90
*
? JANELA(08,32,14,76)
? BORDA(08,32,14,76)
*
@ 10,35 say 'C¢digo da Tabela Base'
@ 11,35 say 'C¢digo da Tabela a criar'
@ 12,35 say 'Percentual p/aumento de Pre‡o'

stor space(03) to mtab,ctab
stor 0.00      to mper
*
? MENSAG(ECODIGO)

do while .T.

  @ 10,71 get ctab PICT '@!'
    read
    IF LASTKEY() = 27
      CLOS DATA
      RETURN
    ENDIF
      
    if EMPTY(ctab)
      ? MENSAG(CALPHA)
      loop
    endif
      use tabpre index tabpre
        seek ctab
          if (eof() .or. bof())
            ? MENSAG(CORRIJA)
            =INKEY(0)
            loop
          endif
*
  @ 11,71 get mtab PICT '@!'
    read
    if EMPTY(mtab)
      ? MENSAG(CALPHA)
      loop
    endif
      use tabpre index tabpre
        seek mtab
          if .not. (eof() .or. bof())
            ? MENSAG("Tabela ja est  cadastrada - ENTER para retornar")
            =INKEY(0)
            loop
          endif
*
  @ 12,68 get mper pict '999.99'
    read
    if mper = 0.00
      ? MENSAG('Percentual p/aumento n„o pode ser = Zeros - ENTER')
      =INKEY(0)
      loop
    endif
*
      mc = ' '
      ? MENSAG(DADCORR)
      @ 23,POS say DADCORR get mc pict '!'
        read
          if mc = 'N'
            clos data
            return
          else
            exit
          endif
enddo
*
? MENSAG('Atualizando Precos: Tabela '+mtab+'  Percentual aumento '+str(mper,6,2))
clos data
use tabpre
  copy to cadwork for tab = ctab
  clos data
*
? MENSAG('Criando Tabela '+mtab)
*
  if mper > 0
    stor (1.00 + (mper / 100)) to cper
  else
    stor (mper / 100) to pper
    stor (pper * (-1)) to pper
    stor (1.00 - pper) to cper
  endif
*
use cadwork
  repl all pre with (pre * cper),tab with mtab
  clos data
*
use tabpre
  appe from cadwork
  clos data
  use tabpre
  index on tab + ref to tabpre
*
clos data
return

