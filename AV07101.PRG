* AV07100 - ATUALIZACAO DO CADASTRO DE TIPOS DE NOTAS FISCAIS - 31/12/86
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 6/5/90  -  08/12/91
*
? CABTELA(0,0,24,79)
*
sele 0
  use TPNF index TPNF.IDX
*
mdel = 0
*
MENS = 'Tipos de Notas Fiscais'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 06,05 say 'Tipo de Nota Fiscal'
@ 07,05 say 'C¢d. Natureza Opera‡„o'
@ 08,05 say 'Descri‡„o Nat. Oper.'
@ 09,05 say 'Tributa‡„o - IPI'
@ 10,05 say '           - ICM'
@ 11,05 say '           - ISS'
@ 12,05 say 'C¢digo da Tributa‡„o'
@ 13,05 say 'Emite Duplicatas? (S/N)'
@ 14,05 say 'Baixa Estoque pela sa¡da? (S/N)'
@ 15,05 say 'Consum. Final c/Inscricao? (S/N)'
@ 16,05 say '              s/Inscricao? (S/N)'
@ 17,05 say 'Inclue na estat.de Vendas? (S/N)'
@ 18,05 say 'Mensagens a imprimir na Nota Fiscal'
*
do while .T.
*
mtip = SPACE(03)
mnope = SPACE(05)
mdesn = SPACE(22)
medup = SPACE(01)
mipit = SPACE(01)
micmt = SPACE(01)
misst = SPACE(01)
mcodt = SPACE(01)
mbest = SPACE(01)
mvenda = 'N'
mpicm = 0.00
missp = 0.00
mcons_ci = SPACE(01)
mcons_si = space(01)
mmnf1 = SPACE(60)
mmnf2 = SPACE(60)
*
? MENSAG(ECODIGO)
@ 06,40 get mtip pict '999'
  read
    
    if lastkey() = 27
      clos data
      clear
      return
    endif
    
    if EMPTY(mtip)
      ? MENSAG(CALPHA)
      =INKEY(0)
      loop
    endif
*
    if !seek(mtip)
      @ 07,40 say mnope color &corver
      @ 08,40 say mdesn color &corver
      @ 09,40 say mipit color &corver
      @ 10,40 say micmt color &corver
      @ 11,40 say misst color &corver
      @ 11,45 say str(missp,5,2) color &corver
      @ 12,40 say mcodt color &corver
      @ 13,40 say medup color &corver
      @ 14,40 say mbest color &corver
      @ 15,40 say mcons_ci color &corver
      @ 16,40 say mcons_si color &corver
      @ 17,40 say mvenda color &corver
      @ 19,10 say mmnf1 color &corver
      @ 20,10 say mmnf2 color &corver
      
      
        MENS = TESTECAD
        DO PEGRESP WITH [SN]
          if mc = 'N'
            loop
          endif
    else       
      @ 07,40 say nope color &corver
        mnope = nope 
      @ 08,40 say desn color &corver
        mdesn = desn
      @ 09,40 say ipit color &corver
        mipit = ipit
      @ 10,40 say icmt color &corver
        micmt = icmt
      @ 11,40 say isst color &corver
        misst = isst
      @ 11,45 say str(issp,5,2) color &corver
        missp = issp
      @ 12,40 say codt color &corver
        mcodt = codt
      @ 13,40 say edup color &corver
        medup = edup
      @ 14,40 say best color &corver
        mbest = best
      @ 15,40 say cons_ci color &corver
        mcons_ci = cons_ci
      @ 16,40 say cons_si color &corver
        mcons_si = cons_si  
      @ 17,40 say venda color &corver
        mvenda = venda  
      @ 19,10 say mnf1 color &corver
        mmnf1 = mnf1
      @ 20,10 say mnf2 color &corver
        mmnf2 = mnf2
*
      MENS = TESTEINC
      DO PEGRESP WITH [123]
*
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
? MENSAG('C¢digo da Natureza da Opera‡„o definido na Legisla‡„o Brasileira')
@ 07,40 get mnope pict '9.999'
  read

? MENSAG('Descri‡„o da Natureza da Opera‡„o a ser impressa na Nota Fiscal')
@ 08,40 get mdesn pict '@!'
  read

? MENSAG('1 = Debita, 2 = Isento, 3 = Outras Debita, 4 = Outras isento')
@ 09,40 get mipit pict '9' valid(mipit$'1234')
  read

@ 10,40 get micmt pict '9' valid(micmt$'1234')
  read

? MENSAG('1 = Debita e 2 = Isento')
@ 11,40 get misst pict '9' valid(misst$'12')
  read

? MENSAG('Se Tributa‡„o do ISS = debitado (1), indicar o percentual')
do while .t.
  if misst = '1'
    @ 11,45 get missp pict '99.99'
      read
      if missp > 0
        exit
      endif
  else
    missp = 0.00
    @ 11,45 say space(05)
    exit      
  endif    
enddo  
  
? MENSAG('C¢digos de Tributa‡„o aceitos compostos da letra + n£mero')
@ 12,40 get mcodt pict '!'
  read
*
? MENSAG('Indicar se o Tipo de Nota Fiscal emite Duplicata (S) ou n„o emite (N)')
do while .t.
@ 13,40 get medup pict '!'
  read
    if medup = 'S'.or. medup = 'N'
      exit
    endif
enddo
*
? MENSAG('Indicar se o Tipo de Nota Fiscal Baixa Estoque (S) ou n„o baixa (N)')
do while .t.
@ 14,40 get mbest pict '!'
  read
    if mbest = 'S'.or. mbest = 'N'
      exit
    endif
enddo

? MENSAG('Indicar se Tipo de Nota Fiscal p/Consumidor Finl com Inscricao (S/N)')
do while .t.
@ 15,40 get mcons_ci pict '!'
  read
    if mcons_ci = 'S'.or. mcons_ci = 'N'
      exit
    endif
enddo
*
? MENSAG('Indicar se Tipo de Nota Fiscal p/Consumidor Final sem Inscricao (S/N)')
do while .t.
@ 16,40 get mcons_si pict '!'
  read
    if mcons_si = 'S'.or. mcons_si = 'N'
      exit
    endif
enddo

? MENSAG('Indica se o tipo de Nota Fiscal ser  inclu¡da nos Relat.de Vendas (S/N)')
do while .t.
@ 17,40 get mvenda pict '!'
  read
    if mvenda $ [SN]
      exit
    endif
enddo

*
? MENSAG('Descrever a mensagem a ser impressa na Nota Fiscal para o Tipo indicado')
@ 19,10 GET MMNF1
@ 20,10 GET MMNF2
  READ
*
  MENS = DADCORR
  DO PEGRESP WITH [SN]
    if mc = 'N'
      loop
    endif
*
  IF (EOF() .OR. BOF())
     appe blan
  ENDIF
*
  repl tipo with mtip,nope with mnope,desn with mdesn
  repl ipit with mipit,icmt with micmt,isst with misst,issp with missp
  repl codt with mcodt,edup with medup,best with mbest
  repl cons_ci with mcons_ci,cons_si with mcons_si
  repl mnf1  with mmnf1,mnf2 with mmnf2
  repl venda with mvenda
*
enddo
