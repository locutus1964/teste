* AV05100 - ATUALIZACAO DO CADASTRO DE PRODUTOS - 13/11/86
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 15-04-90
*
SAVE SCRE TO MAIN2_SCR
*
? CABTELA(0,0,24,79)
*
MENS = 'Atualiza‡„o de Produtos'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 03,04 say 'Ref'
@ 03,16 say 'Descr'
@ 05,04 say 'Tipo'
@ 05,24 say 'Grupo'
@ 05,36 say 'Conteudo'
@ 05,56 say 'Unid'
@ 05,67 say 'Embal'
@ 07,04 say 'Tabela de Pre‡o'
@ 07,45 say 'Conta do Produto'
@ 09,04 say 'C¢d. Clas. Fiscal'
@ 09,45 say 'Percentual do IPI'
@ 11,04 say 'Linha do Produto'
@ 11,45 say 'Peso L¡quido'
@ 13,04 say 'Lote Suprimento'
@ 13,45 say 'Ponto de Pedido'
@ 14,01 TO 14,78
@ 16,04 say 'Saldo'
@ 16,27 say 'N.Fiscais'
@ 16,53 say 'Vendas Mes'
@ 18,04 say 'Pedidos em Aberto'
@ 18,43 say 'Vendas at‚ o Mes'
@ 20,04 say 'Faturado no Mes'
@ 20,43 say 'Faturado at‚ Mes'
*
mref = SPACE(04)
msal = 0.00
mcom = 0.00
mvam = 0
mvaa = 0
mpea = 0
mfme = 0
mfac = 0
*
  sele 0
    USE GRUPOS   INDEX GRUPOS.IDX
  sele 0
    USE UNIDADES INDEX UNIDADES.IDX
  sele 0
    USE EMBALAGE INDEX EMBALAGE.IDX
  sele 0
    USE CLASFISC INDEX CLASFISC.IDX
  sele 0
    USE LIMPROD INDEX LIMPROD.IDX
  sele 0
    USE SALDOS INDEX SALDOS.IDX
  sele 0
    USE PROD INDEX PROD.IDX
*
do while .T.
sele PROD
*
stor 0 to mori,mpea,mvam,mvaa,memi,mema,mdel,mvme,mfme,mfac,msal
mdes      = SPACE(53)
mdes2     = SPACE(53)
munidade  = space(03)
mlpr      = SPACE(02)
mipi      = 0.0
mpel      = 0.0000
mcon      = SPACE(10)
mclasfisc = SPACE(01)
mcpr      = SPACE(03)
nTipo = SPACE(11)
mconteudo = 0
membalagem = SPACE(03)
mgrupo    = SPACE(02)
*
? MENSAG(ECODIGO)
*
@ 03,08 GET mref
  read
    
  if lastkey() = 27
    if mdel = 1
      MENS = 'Exclui Produtos cancelados ?'
      DO PEGRESP WITH [SN]
        if mc = 'S'
          pack
        endif
    endif
      clos data
      clear
      return
  endif
*
  inc = ' '
  if !seek(MREF)
    @ 03,22 say mdes   color &corver
    @ 04,22 say mdes2  color &corver
    @ 05,09 say nTipo  color &corver
    @ 05,30 say mgrupo color &corver
    @ 05,45 say mconteudo  color &corver
    @ 05,61 say munidade   color &corver
    @ 05,73 say membalagem  color &corver
    @ 07,34 say mcpr   color &corver
    @ 07,67 say mcon   color &corver
    @ 09,25 say mclasfisc   color &corver
    @ 09,73 say mipi pict '99.9'  color &corver
    @ 11,33 say mlpr   color &corver 
    @ 11,69 say mpel pict '999.9999'  color &corver
    @ 13,27 say str(mema,8)  color &corver
    @ 13,69 say str(memi,8)  color &corver
    @ 16,12 say msal pict '9,999,999.99'  color &corver
    @ 16,37 say mcom pict '9,999,999,999' color &corver 
    @ 16,64 say mvam pict '9,999,999,999' color &corver
    @ 18,64 say mvaa pict '9,999,999,999' color &corver
    @ 18,24 say mpea pict '9,999,999,999' color &corver
    @ 20,24 say mfme pict '9,999,999,999' color &corver
    @ 20,64 say mfac pict '9,999,999,999' color &corver
  
      MENS = TESTECAD
      DO PEGRESP WITH [SN]
        if mc = 'N'
          loop
        endif
  endif
*
if seek(mref)
  @ 03,22 say des      color &corver
    stor des to mdes
  @ 04,22 say des2     color &corver
    stor des2 to mdes2  
  @ 05,09 say tipo     color &corver
    stor tipo to nTipo
  @ 05,30 say grupo    color &corver
    stor grupo to mgrupo   
  @ 05,45 say conteudo color &corver
    stor conteudo to mconteudo
  @ 05,61 say uni      color &corver
    stor uni to munidade
  @ 05,73 say embalagem color &corver
    stor embalagem to membalagem
  @ 07,34 say cpr     color &corver
    stor cpr to mcpr
  @ 07,67 say CON     color &corver
    stor CON to MCON
  @ 09,25 say ccl     color &corver
    stor ccl to mclasfisc
  @ 09,73 say ipi pict '99.9'  color &corver
    stor ipi to mipi
  @ 11,33 say lpr     color &corver
    stor lpr to mlpr
  @ 11,69 say pel pict '999.9999'  color &corver
    stor pel to mpel
  @ 13,27 say str(ema,8)  color &corver
    stor ema to mema
  @ 13,69 say str(emi,8)  color &corver
    stor emi to memi
  @ 16,12 say sal pict '9,999,999.99'  color &corver
  @ 16,37 say com pict '9,999,999,999' color &corver
  @ 16,64 say vam pict '9,999,999,999' color &corver
  @ 18,64 say vaa pict '9,999,999,999' color &corver
  @ 18,24 say pea pict '9,999,999,999' color &corver
  @ 20,24 say fme pict '9,999,999,999' color &corver
  @ 20,64 say fac pict '9,999,999,999' color &corver
*
  MENS = TESTEINC
  DO PEGOPCAO WITH [123]
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
@ 23,01 SAY SPACE(78)
do while .t.
@ 03,22 get mdes pict '@!'
  read
    if mdes <> space(40)
      exit
    endif
      ? MENSAG(CALPHA)
enddo
@ 04,22 get mdes2
*
@ 05,09 get nTipo
  read
  
sele GRUPOS    	
do while .t.
  @ 05,30 get mgrupo
    read
    if mgrupo = space(02)
      ? MENSAG(CALPHA)
      loop
    endif
    if seek(mgrupo)  
      exit
    else
      ? MENSAG('Grupo n„o est  cadastrada')  
    endif
ENDDO      
  
@ 05,45 get mconteudo 
  read

@ 23,01 say space(78)
SELE UNIDADES
DO WHILE .T.
  @ 05,61 get munidade pict '@!'
  read
    if munidade = space(03)
      ? MENSAG(CALPHA)
      loop
    endif
    if seek(munidade)  
      exit
    else
      ? MENSAG('Unidade n„o est  cadastrada')  
    endif
ENDDO      
  
@ 23,01 say space(78)  
SELE EMBALAGE
DO WHILE .T.  
  @ 05,73 get membalagem
  read
    if membalagem = space(03)
      ? MENSAG(CALPHA)
      loop
    endif  
    if seek(membalagem)   
      exit
    else
      ? MENSAG('Embalagem n„o est  cadastrada')  
    endif
ENDDO
*  
MENS1 = 'Informe os demais dados sempre que dispon¡veis'
? MENSAG(MENS1)
@ 07,67 get mcon
  read
*
SELE CLASFISC
@ 23,01 SAY SPACE(78)
do while .t.
@ 09,25 get mclasfisc
  read
    if mclasfisc = space(01)
      ? MENSAG(CALPHA)
      loop
    endif
    if seek(mclasfisc)  
      exit
    else
      ? MENSAG('Classifica‡„o Fiscal n„o est  cadastrada')  
    endif
enddo
*
@ 23,01 say space(78)
? MENSAG(MENS1)
@ 09,73 get mipi pict '99.9'
*
SELE LIMPROD
DO WHILE .T.
@ 11,33 get mlpr
  READ
  IF SEEK(MLPR)
    EXIT
  ENDIF
  ? MENSAG('Linha de Produto n„o cadastrada')
ENDDO    
*
@ 11,69 get mpel pict '999.9999'
*
@ 13,27 get mema pict '99999999'
*
@ 13,69 get memi pict '99999999'
  read
*
  MENS = DADCORR
  DO PEGRESP WITH [SN]
    if mc = 'N'
      loop
    endif
*
sele SALDOS
  if !seek(mref)
    appe blan
    repl ref with mref,sali with 0.00
  endif
*
sele PROD
  if !seek(mref)
    appe blan
  endif  
*
  repl ref with mref,con with mcon,des with mdes,des2 with mdes2
  repl tipo with nTipo,conteudo with mconteudo
  repl embalagem with membalagem,grupo with mgrupo
  repl uni with munidade,ori with mori,ccl with mclasfisc
  repl ipi with mipi,lpr with mlpr,pel with mpel
  repl emi with memi,ema with mema,cpr with mcpr
  *REPL CONJUNTO WITH '00000'
*
enddo
return
