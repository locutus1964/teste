* AV05994 - ATUALIZACAO DOS PRECOS PRODUTOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06/09/93
*
? mensag('Programa ainda n„o liberado - ENTER')
=INKEY(0)
return

clos data
  use VENDPARC index VENDPARC.IDX
*
? CABTELA(0,0,24,79)
*
MENS = 'Vendas a Prazo - Em Parcelas'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 08,10 say 'C¢digo da Tabela'
@ 10,10 say 'Prazo M‚dio em meses'
@ 12,10 say 'Desconto s/parte fin'
mtabparc = space(05)
mdel = 0
*
do while .T.
*
mprazmed  = 0.0
mdesparc  = 0.000
*
? MENSAG(ECODIGO)
*
@ 08,40 get mtabparc pict '99/99'
  read

    if LASTKEY() = 27
      if mdel = 1
        mens = 'Houve cancelamentos. Compacta Arquivos ?'
        do pegresp with [SN]
        if mc = 'S'
          pack
        endif
      endif
      clos data
      clear
      return
    endif
  
    if mtabparc = SPACE(05)
      ? MENSAG(cnumer)
      =INKEY(0)
      loop
    endif
*
do while .t.

@ 10,40 get mprazmed pict '99.9'
  read
    if mprazmed = 0
      ? MENSAG(CALPHA)
    else
      exit  
    endif
enddo
*
    if !seek(mtabparc + str(mprazmed,4,1))
      @ 12,40 say mdesparc pict '999.999' color &corver
      MENS = (TESTECAD)
      do pegresp with [SN]
      if mc = 'N'
        loop
      endif
    endif
*
  IF SEEK(mtabparc + str(mprazmed,4,1))
    @ 12,40 say desparc pict '999.999' color &corver
    mdesparc = desparc
*
  MENS = (testeinc)
  DO PEGOPCAO WITH [123]
      if opcao = '1'
        loop
      endif
        if opcao = '3'
          dele
          mdel = 1
          loop
        endif
endif
*
@ 23,01 SAY SPACE(78)
do while .t.
@ 12,40 get mdesparc pict '999.999'
  read
    if mdesparc > 0.00
      exit
    endif
      ? MENSAG(CNUMER)
enddo
*
  MENS = (DADCORR)
  DO PEGRESP WITH [SN]
      if mc = 'N'
        loop
      endif
*
    if !SEEK(mtabparc + str(mprazmed,4,1))
      appe blan
    endif
*
  repl tabparc with mtabparc,prazmed with mprazmed,desparc with mdesparc
*
enddo


