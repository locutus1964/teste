* AV05994 - ATUALIZACAO DOS PRECOS PRODUTOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06/09/93
*
clos data
  use VENDUNIC index VENDUNIC.IDX
*
? CABTELA(0,0,24,79)
*
MENS = 'Vendas a Prazo - Parcela Unica'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 08,10 say 'C¢digo da Tabela'
@ 10,10 say 'N£mero de Dias financ.'
@ 12,10 say 'Desconto s/parte financ.'
mtabunic = space(05)
mdel = 0
*
do while .T.
*
mqtddias  = 0.0

mdesunic  = 0.000
*
? MENSAG(ECODIGO)
*
@ 08,40 get mtabunic pict '99/99'
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
  
    if mtabunic = SPACE(05)
      ? MENSAG(cnumer)
      =INKEY(0)
      loop
    endif
*
@ 10,40 get mqtddias pict '99'
  read
*
    if !seek(mtabunic + str(mqtddias,2))
      @ 12,40 say mdesunic pict '999.999' color &corver
      MENS = (TESTECAD)
      do pegresp with [SN]
      if mc = 'N'
        loop
      endif
    endif
*
  IF SEEK(mtabunic + str(mqtddias,2))
    @ 12,40 say desunic pict '999.999' color &corver
    mdesunic = desunic
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
@ 12,40 get mdesunic pict '999.999'
  read
*
  MENS = (DADCORR)
  DO PEGRESP WITH [SN]
      if mc = 'N'
        loop
      endif
*
    if !SEEK(mtabunic + str(mqtddias,2))
      appe blan
    endif
*
  repl tabunic with mtabunic,qtddias with mqtddias,desunic with mdesunic
*
enddo


