* AV06100 - ALTERACAO DE CABECALHOS DE PEDIDOS 05/02/87
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 24-02-90
*
mdel = 0
*
mpcl  = SPACE(13)
CGCT  = SPACE(09)
MCPG  = SPACE(09)
mref  = SPACE(11)
mpint = SPACE(06)
mcgc  = SPACE(20)
icgc  = SPACE(16)
loja  = SPACE(03)
ucgc  = SPACE(20)
maut  = SPACE(01)
STOR SPACE(03) TO utra, mtra, uven, mven, utip, mtip
STOR {} TO mden, mdem
key   = SPACE(15)
STOR 0 TO mpri, qtit, lin, mped, mmped, mmoeda
STOR 0.00 TO mdge, mfre, mpre, mvam, nvam, mvaa, nvaa
STOR 0.00 TO mpea, npea, memb, mdesconto
*
? CABTELA(0,0,24,79)
MENS = 'Altera‡„o de Cabe‡alho do Pedido'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 03,03 say 'C¢d/Loja'
@ 03,34 say 'Vend'
@ 03,44 say 'Ped Sis'
@ 03,60 say 'Emiss„o'
@ 05,03 say 'Ped Int'
@ 05,20 say 'Ped Cli'
@ 05,44 say 'Transpt'
@ 07,03 say 'Descto Cliente'
@ 07,32 say 'Tipo NF'
@ 07,46 say 'C Pag'
@ 08,46 say 'Perc'
@ 07,65 say 'Dias'
@ 09,03 say 'V.Desc'
@ 09,23 say 'Frete'
@ 09,43 say 'Embal'
@ 09,63 say 'Moeda'
*
do while .t.
? MENSAG('Entre n£mero do Pedido (999999) para encerrar altera‡„o')
*
@ 03,52 get mped pict '999999'
  read
    if  mped = 0
      ? MENSAG(CNUMER)
      loop
    endif
      if mped = 999999
        if mdel = 1
          MC = ' '
            MENS = 'Houve exclus”es. Compacta arquivo de Pedidos? (S/N) '
            ? MENSAG(MENS)
            @ 23,POS SAY MENS GET MC PICT '!' VALID(MC$'SN')
              READ
                IF MC = 'S'
                  ? MENSAG('Limpando as exclus”es do arquivo... Aguarde')
                  USE PEDIC INDE PEDIC
                    pack
                  USE PEDID INDE PEDIDM,PEDID
                    pack
                ENDIF
         endif
           clos data
           clear
           return
    endif
*
USE PEDIC INDE PEDIC
*
SEEK STR(MPED,6)
  if (eof() .or. bof())
    ? MENSAG('Pedido n„o est  cadastrado ou n£mero est  errado - ENTER')
    =INKEY(0)
    loop
  else
*
@ 03,12 say subs(cgc,1,16)
  icgc = subs(cgc,1,16)
@ 03,29 say subs(cgc,18,3)
  loja = subs(cgc,18,3)
  stor cgc to mcgc,ucgc
@ 03,39 say ven
  stor ven to mven,uven
@ 03,69 say dem
  stor dem to mdem
@ 05,11 say pint
  stor pint to mpint
@ 05,28 say pcl
  stor pcl to mpcl
@ 05,52 say tra
  stor tra to mtra,utra
@ 07,18 say descto1 pict '99.99'
  stor descto1 to mdescto1
@ 07,24 say descto2 pict '99.99'
  stor descto2 to mdescto2  
@ 07,40 say tip
  stor tip to mtip,utip
@ 07,46 say str(cp1,3)
  stor cp1 to mcp1
@ 07,50 say str(cp2,3)
  stor cp2 to mcp2
@ 07,54 say str(cp3,3)
  stor cp3 to mcp3
@ 07,58 say str(cp4,3)
  stor cp4 to mcp4
@ 07,70 say cpd
  stor cpd to mcpd
@ 08,46 say str(pe1,3)
  stor pe1 to mpe1
@ 08,50 say str(pe2,3)
  stor pe2 to mpe2
@ 08,54 say str(pe3,3)
  stor pe3 to mpe3
@ 08,58 say str(pe4,3)
  stor pe4 to mpe4
@ 09,10 say str(desconto,11,2)
  mdesconto = desconto
@ 09,30 say str(fre,11,2)
  stor fre to mfre
@ 09,50 say str(emb,11,2)
  stor emb to memb
@ 09,70 say str(moeda,3)
  stor moeda to mmoeda
@ 10,01 SAY REPLICATE('-',78)

@ 12,14 clear to 20,70
@ 12,14 to 20,70 double
@ 13,16 SAY 'Observa‡”es a imprimir no Pedido:'
@ 14,16 SAY O1
  MO1 = O1
@ 15,16 SAY O2
  MO2 = O2
@ 16,16 SAY O3
  MO3 = O3
@ 17,16 SAY O4
  MO4 = O4
@ 18,16 SAY O5
  MO5 = O5
@ 19,16 SAY O6
  MO6 = O6

*
ans = ' '
mens = 'Exclui o Pedido do arquivo? (S/N) '
? MENSAG(MENS)
  @ 23,POS say mens get ans pict '!' valid(ans$'SN')
    read
      if ans = 'S'
        dele
        mdel = 1
        USE PEDID INDE PEDIDM,PEDID
        seek STR(mped,6)
          IF !(EOF() .OR. BOF())
            do while ped = mped .and. !eof()
              DELE
              skip
            enddo
          ENDIF
        LOOP
    else
      ans = ' '
      MENS = 'Altera dados do Cabe‡alho do Pedido? (SN) '
      ? MENSAG(MENS)
      @ 23,pos say mens get ans pict '!' valid(ANS$'SN')
        read
    endif
*
    if ans = 'N'
      loop
    else
      do while .t.
        @ 03,12 get icgc pict '@!'
        @ 03,29 get loja pict '@!'
          READ

          if loja <> space(03)
            mcgc = icgc+'/'+loja
          else
            mcgc = icgc+space(04)
          endif

            USE CLIEN ORDER P_CGC
            seek mcgc
              if !(eof() .or. bof())
                stor tra to mtra
                stor ven to mven
                stor ves to mcpg
                  if mcgc <> ucgc
                    USE PEDIC INDE PEDIC
                      SEEK STR(MPED,6)
                      dele
                      appe blan
                      REPL PED WITH MPED,CGC WITH MCGC
                  endif
                exit
              else
                ? MENSAG('Cliente n„o est  cadastrado ou CGC est  errado')
              endif
      enddo
*
? MENSAG('Se o C¢digo do Vendedor n„o for o cadastrado, substitua-o')
do while .t.
  @ 03,39 get mven pict '999'
    read
      if mven = '999'
        clear
        clos data
        return
      endif
    if !empty(mven)
      USE VEND INDE VEND
        seek mven
          if (eof() .or. bof())
            ? MENSAG('N„o cadastrado. Corrija ou entre (999) para encerrar')
            IF LASTKEY() != 27
              ? MOSTRA(12,45,21,78)
              BROWSE FIELDS RAZ:R:H='Descri‡„o' IN WINDOW TELA
            ENDIF
            MVEN = CVR
            MCOMIS = COM
            DO ESCONDE
            loop
          endif
    endif
      exit
enddo
*
? MENSAG('Informe a data da emiss„o do Pedido no formato (DD/MM/AA)')
DO WHILE .T.
@ 03,69 get mdem pict '99/99/99'
  READ
    DO FDATA WITH DTOC(MDEM)
      IF TDATA = ' '
        EXIT
      ENDIF
ENDDO
*
? MENSAG('Informe os n£meros dos Pedidos de controle interno e do Cliente')
@ 05,11 get mpint
@ 05,28 get mpcl
  read
*
? MENSAG('Se o c¢digo da Transportadora n„o for o do cadastro, substitua-o')
do while .t.
@ 05,74 get mtra pict '999'
  read
    if mtra = '999'
      clear
      clos data
      return
    endif
    if !empty(mtra)
      USE TRANS INDE TRANS
      seek mtra
        if (eof() .or. bof())
          ? MENSAG('N„o cadastrada. Corrija ou entre (999) para encerrar')
        endif
    endif
      exit
enddo
*
? MENSAG('Informe percentual de Desconto Geral para este Pedido, se concedido')
@ 07,18 get mdescto1 pict '99.99'
@ 07,24 get mdescto2 pict '99.99'
  read
*
? MENSAG('Informe o Tipo de Nota Fiscal a ser emitido para este Pedido')
do while .t.
  @ 07,40 get mtip pict '999'
    read
      if mtip = space(3)
        DO MENSAG WITH CALPHA
        loop
      endif
*
      if mtip = '999'
        clear
        clos data
        return
      endif
*
        USE TPNF INDE TPNF
          seek mtip
            if (eof() .or. bof())
              ? MENSAG('Tipo n„o cadastrado. Corrija ou entre (999) para encerrar')
              loop
            else
              exit
            endif
enddo
*
? MENSAG('Informe os DIAS para vencimento das parcelas 1 a 4, se parcelado')
@ 07,46 get mcp1 pict '999'
@ 08,46 get mpe1 pict '999'
@ 07,50 get mcp2 pict '999'
@ 08,50 get mpe2 pict '999'
@ 07,54 get mcp3 pict '999'
@ 08,54 get mpe3 pict '999'
@ 07,58 get mcp4 pict '999'
@ 08,58 get mpe4 pict '999'
  read
*
SAVE SCREEN TO TELA
? janela(09,50,19,75)
? borda(09,50,19,75)
@ 10,52 SAY 'Modos aceitos:'
@ 11,52 SAY 'VIS - Pagto a Vista'
@ 12,52 SAY 'APR - Contra Apresent.'
@ 13,52 SAY 'DDD - Dias da Data Liq'
@ 14,52 SAY 'DFM - Dias fora o Mes'
@ 15,52 SAY 'DFQ - Dias fora Quinz.'
@ 16,52 SAY 'SDB - Sem D‚bito'
@ 17,52 SAY 'PER - Permuta'
@ 18,52 SAY 'ANT - Pagto Antecipado'

do while .t.
@ 07,70 get mcpd pict '!!!'
  read
    if mcpd = 'VIS' .or. mcpd = 'APR' .or. mcpd = 'DDD' .or. mcpd = 'DFM' ;
      .or. mcpd = 'DFQ' .OR. MCPD = 'SDB' .OR. MCPD = 'ANT'
      exit
    endif
enddo
REST SCREEN FROM TELA
*
? MENSAG('Informe o valor do Desconto Fixo, se concedido')
@ 09,10 get mdesconto pict '99999999.99'
  read

? MENSAG('Informe o valor do Frete, se PAGO pelo Cliente')
@ 09,30 get mfre pict '99999999.99'
  read

? MENSAG('Informe o valor da Embalagem, se PAGO pelo Cliente')
@ 09,50 get memb pict '99999999.99'
  read
*
? MENSAG('Se o c¢digo da Moeda n„o for o do cadastro, substitua-o')
do while .t.
@ 09,70 get mmoeda pict '999'
  read
    if mmoeda = 0
      clear
      clos data
      return
    endif
      USE CADMOEDA INDE CADMOEDA
      seek str(mmoeda,3)
        if (eof() .or. bof())
          ? MENSAG(corrija)
          =INKEY(0)
          LOOP
        endif
    exit
enddo
*
      DO WHILE .T.
        @ 14,16 GET MO1
        @ 15,16 GET MO2
        @ 16,16 GET MO3
        @ 17,16 GET MO4
        @ 18,16 GET MO5
        @ 19,16 GET MO6
          READ
            MC = ' '
            MENS = 'Observa‡”es corretas? (S/N) '
            ? MENSAG(MENS)
              @ 23,POS SAY MENS GET MC pict '!' VALID(MC$'SN')
                READ
                  IF MC = 'S'
                    EXIT
                  ENDIF
      ENDDO
*
mc = ' '
? mensag(dadcorr)
  @ 23,pos say dadcorr get mc pict '!' valid(MC$'SN')
    read
      if mc = 'N'
        loop
      endif
*
USE PEDIC INDE PEDIC
SEEK STR(MPED,6)
repl cgc with mcgc,ven with mven,dem with mdem,ped with mped,pcl with mpcl
repl den with mden,tra with mtra,dge with mdge,tip with mtip
repl fre with mfre,emb with memb,cp1 with mcp1,cp2 with mcp2
repl cp3 with mcp3,cp4 with mcp4,pe1 with mpe1,pe2 with mpe2,pe3 with mpe3
repl pe4 with mpe4,cpd with mcpd,pint with mpint,O1 with MO1,O2 with MO2
repl O3 with MO3,O4 with MO4,O5 with MO5,O6 with MO6,moeda with mmoeda
repl desconto with mdesconto
  endif
endif
*
@ 10,01 clear to 21,78
@ 10,01 say replicate('-',78)
enddo
