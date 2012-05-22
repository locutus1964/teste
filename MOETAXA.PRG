* MOETAXA - ATUALIZACAO DE TAXAS DE MOEDAS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA
*
? CABTELA(0,0,22,79)
@ 01,25 say '  Atualiza‡„o de Paridades    '
@ 07,15 say 'C¢digo da Moeda'
@ 09,15 say 'Data - (DD/MM/AAAA)'
@ 11,15 say 'Descri‡„o'
@ 13,15 say 'Abreviatura'
@ 15,15 say 'Fator de Convers„o:  Dolar'
@ 17,15 say '                     Cruzeiro'
*
mdata  = ctod('')
mmoeda   = 0
*
CLOS DATA
SELE 0
  USE CADMOEDA INDEX CADMOEDA.IDX
SELE 0
  USE NOMMOEDA INDEX NOMMOEDA.IDX
  SET RELATION TO STR(MOEDA,3) INTO CADMOEDA
  SET SKIP TO CADMOEDA
    
DO WHILE .T.
mdmoeda  = SPACE(26)
mamoeda  = SPACE(08)
mcdolar  = 0.00000
mccz     = 0.00000

SELE NOMMOEDA

? MENSAG(ECODIGO)
@ 7,39 get mmoeda PICT '999'
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

  if !seek(str(mmoeda,3))
    ? MENSAG('Moeda n„o cadastrada - ENTER')
    =INKEY(0)
    loop
  else
    @ 11,39 say dmoeda
      mdmoeda = dmoeda
    @ 13,39 say amoeda
      mamoeda = amoeda
  endif
*
@ 9,39 GET Mdata
READ


SELE CADMOEDA
SEEK STR(MMOEDA,3) + DTOS(MDATA)
if (eof() .or. bof())

  MENS = TESTECAD
  DO PEGRESP WITH [SN]
    if mc = 'N'
      loop
    endif
endif

@ 23,01 say space(78)
*
if .not. (eof() .or. bof())
@ 15,50 say str(cdolar,12,6)
  mcdolar  = cdolar
@ 17,50 say str(ccz,12,6)
  mccz     = ccz
@ 23,10 say space(68)
  stor ' 'to opcao
*
  MENS = TESTEINC
  DO PEGOPCAO WITH [123]
      if opcao = '1'
        loop
      endif
        if opcao = '3'
          delete
          MDEL = 1
          loop
        endif
endif
*
MENS = 'Qual a Paridade de '+rtrim(mdmoeda)+' em rela‡„o ao Dolar?'
? MENSAG(MENS)
@ 15,50 get mcdolar pict '99999.999999'
  read
*
MENS = 'Qual a Paridade de '+rtrim(mdmoeda)+' em rela‡„o ao Cruzeiro?'
? MENSAG(MENS)
@ 17,50 get mccz    pict '99999.999999'
  read
*
  MENS = DADCORR
  DO PEGRESP WITH [SN]
  if mc = 'N'
    loop
  endif
*
if eof() .or. bof()
  appe blan
endif
*
repl moeda with mmoeda
repl cdolar with mcdolar,ccz with mccz,data with mdata
*
enddo
clos data
return

