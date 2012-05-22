* AV06150 - ALTERACAO DE ITENS DE PEDIDOS CADASTRADOS - 04/02/87
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 24-04-90 - 02/12/91
*
DO CABTELA WITH 0,0,24,79
*
sele B
  USE PROD   INDEX PROD
sele E
  USE PEDID  INDEX PEDIDM,PEDID
sele C
  USE TABPRE INDEX TABPRE
sele D
  USE CODPRE INDEX CODPRE    
sele A
  USE PEDIC  INDEX PEDIC
*
cgct = SPACE(09)
mcpg = SPACE(09)
mref = SPACE(11)
mnnf = SPACE(06)
MPED = SPACE(06)
STOR 0 TO mdel, mqtd, mqti, mqtit, lin, msal, mped
MCGC = SPACE(20)
ucgc = SPACE(16)
maut = SPACE(01)
mcont = SPACE(01)
mdescricao = SPACE(40)
mdescraux1 = SPACE(38)
mdescraux2 = space(38)
mtab = space(03)
mite = 1
mpipi = 0.0
key  = SPACE(15)
lin  = 12
*
MENS = 'Altera‡„o de Itens do Pedido'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 03,03 say 'C¢d/Loja'
@ 03,34 say 'Vend'
@ 03,44 say 'Ped Sis'
@ 03,60 say 'Emiss„o'
@ 05,03 say 'Ped Int'
@ 05,20 say 'Ped Cli'
@ 05,44 say 'Dat p/Entr'
@ 05,66 say 'Transpt'
@ 07,03 say 'Desc Geral'
@ 07,25 say 'Tipo NF'
@ 07,40 say 'Cond Pagto'
@ 09,03 say 'Frete'
@ 09,22 say 'Embal'
@ 09,63 say 'Moeda'
@ 10,01 say replicate('-',78)
@ 11,02 say 'It'
@ 11,05 say 'Referˆncia'
@ 11,17 say 'Descri‡„o'
@ 11,41 say 'Quantidade'
@ 11,52 say '     Saldo'
@ 11,63 say 'Desct'
@ 11,69 say 'Bonus'
@ 11,75 say 'Tab'
*
DO WHILE .T.
*
? MENSAG('Entre n£mero do Pedido (999999) para encerrar altera‡„o')
*
@ 03,52 get mped pict '999999'
  read
    if  mped = 0
      ? MENSAG(CNUMER)
      loop
    endif
      if mped = 999999
        clos data
        clear
        return
      endif
*
sele A
seek STR(MPED,6)
  if (eof() .or. bof())
    ? MENSAG('Pedido n„o est  cadastrado ou n£mero est  errado - ENTER')
    =INKEY(0)
    loop
  endif
*
@ 03,12 say cgc
  MCGC = CGC
@ 03,39 say ven
@ 03,52 say ped
@ 03,69 say dem
@ 05,11 say pint
@ 05,28 say pcl
@ 05,55 say den
@ 05,74 say tra
@ 07,14 say str(dge,6,2)
@ 07,34 say tip
@ 07,52 say str(cp1,3)
@ 07,57 say str(cp2,3)
@ 07,62 say str(cp3,3)
@ 07,67 say str(cp4,3)
@ 08,52 say str(PE1,3)
@ 08,57 say str(PE2,3)
@ 08,62 say str(PE3,3)
@ 08,67 say str(PE4,3)
@ 07,72 say cpd
@ 09,10 say str(fre,11,2)
@ 09,30 say str(emb,11,2)
@ 09,70 say str(moeda,3)
*
do while .t.
*
? MENSAG('Entre Referˆncia do Produto (99) para encerrar altera‡„o')
mqtd  = 0
cip   = 0
msal  = 0
mdit  = 0.00
mbon  = 0.00
mpref = 0.00
mprev = 0.00
mnnf  = space(06)
ment  = space(08)
mcpr  = space(03)
medi  = space(04)
@ lin,01 say space(78)
@ lin,02 say str(mite,2)

@ lin,05 get mref PICT '@!'
  read
    if empty(mref)
      ? MENSAG(CALPHA)
      loop
    endif

    if mref = '99'
      exit
    endif
*
  sele E
  
  inc = ' '
  KEY = STR(mped,6) + mref
  seek key
    if (eof() .or. bof())
      SELE B
      SEEK MREF
      IF !(EOF() .OR. BOF())
        @ LIN+1,17 SAY DES
        MDESCRICAO = DES
      ELSE
        ? MENSAG('Referˆncia n„o cadastrada no estoque - ENTER')
        =INKEY(0)
        LOOP
      ENDIF
      
      MENS = 'Item n„o cadastrado no Pedido - vai incluir? (S/N)'
      ? MENSAG(MENS)
      ans = ' '
      @ 23,POS SAY MENS get ans pict '!' VALID(ANS$'SN')
        read
          if ans = 'N'
            loop
          else
            inc = '*'
            uqtd = 0                      
          endif
    else
      @ lin+1,17 say descricao
        mdescricao = descricao
      @ lin,41 say str(qtd,10,2)
        mqtd = qtd
        uqtd = qtd
      @ lin,52 say str(sal,10,2)
        msal = sal
      @ lin,63 say str(dit,5,2)
        mdit = dit
      @ lin,69 say str(bon,5,2)
        mbon = bon
      @ lin,75 say tab
        mtab = tab
        mdescraux1 = descraux1
        mdescraux2 = descraux2
    endif
*

do while .t.
  @ lin,41 get mqtd pict '9999999.99'
    read
      if mqtd > 0
        exit
      endif
        ? MENSAG(CNUMER)
enddo
*
do while .t.
  @ lin,52 get msal pict '9999999.99'
    read
      if msal = 0
        ? MENSAG('Saldo n„o pode ser zeros - ENTER')
        =INKEY(0)
      endif
      if msal > mqtd
        ? MENSAG('Saldo n„o pode ser maior que quantidade - ENTER')
        =INKEY(0)
        loop
      endif
        exit
enddo
*
@ lin,63 get mdit pict '99.99'
*
@ lin,69 get mbon pict '99.99'
  read
*
? MENSAG('Informe a Tabela de Pre‡os para o ¡tem do Pedido - (999) encerra')
do while .t.
  @ lin,75 get mtab pict '!!!'
    read
      if mtab = space(3)
        ? MENSAG(CALPHA)
        LOOP
      ENDIF
*
      if mtab = '999'
        clos data
        return
      endif

      if mcpr = space(3)

        SELE C
        seek mtab + mref
        if (eof() .or. bof())
          ? MENSAG('N„o cadastrada. Corrija ou entre (999) para encerrar')
          IF LASTKEY() != 27
            ? MOSTRA(04,49,11,78)
            BROWSE FIELDS TAB:R:H='Tab',REF:R:H='Referˆncia',PRE:R:H='Pre‡o' IN WINDOW TELA
          ENDIF
          MTAB = TAB
          MPRE = PRE
          DO ESCONDE
        else
          mpre = pre
          exit
        endif
      else
        SELE D
        seek mtab + mcpr
        if (eof() .or. bof())
          ? MENSAG('N„o cadastrada. Corrija ou entre (999) para encerrar')
          IF LASTKEY() != 27
            ? MOSTRA(04,55,11,78)
            BROWSE FIELDS TAB:R:H='Tab',CPR:R:H='C¢d',PRE:R:H='  Pre‡o' ;
            FOR CPR = MCPR IN WINDOW TELA
          ENDIF
          MTAB = TAB
          MPRE = PRE
          DO ESCONDE
        else
          mpre = pre
          exit
        endif
      endif
  enddo
*
  @ lin+1,17 get mdescricao
  read
  
  mc = ' '
  mens = 'Inclui Descri‡„o Complementar? (S/N) '
  ? MENSAG(MENS)
  @ 23,pos say mens get mc pict '!' valid(mc$'SN')
    READ
      IF MC = 'S'
        DO WHILE .T.
          SAVE SCREEN TO TELA
          @ 05,08 CLEAR TO 09,50
          @ 05,08 TO 09,50 double
          @ 06,10 say 'Descri‡„o Complementar'
          @ 07,10 GET MDESCRAUX1
          @ 08,10 GET MDESCRAUX2
            READ
            MC = ' '
            MENS = 'Descri‡„o complementar correta? (S/N) '
            ? MENSAG(MENS)
            @ 23,POS SAY MENS GET MC PICT '!' VALID (MC$'SN')           
              READ
              IF MC = 'S'
                REST SCREEN FROM TELA
                EXIT
              ENDIF
        ENDDO
              
      ELSE
        MDESCRAUX1 = SPACE(38)
        MDESCRAUX2 = SPACE(38)
      ENDIF
*
  MC = ' '
  ? MENSAG(DADCORR)
  @ 23,POS SAY DADCORR GET MC PICT '!' VALID(MC$'SN')
    READ
      IF MC = 'N'
        LOOP
      ENDIF  
*  
  sele E
    if inc = '*'
      appe blan
    endif

    repl cgc with mcgc,ped with mped,ite with mite,ref with mref
    repl qtd with mqtd,sal with msal
    repl bon with mbon,dit with mdit,pre with mpre,tab with mtab
    repl descricao with mdescricao,descraux1 with mdescraux1
    repl descraux2 with mdescraux2
    
    IF MQTD <> UQTD
      DQTD = (MQTD - UQTD)
      SELE B
      SEEK MREF
      REPL VAA WITH (VAA + DQTD)
      REPL VAM WITH (VAM + DQTD)
      REPL PEA WITH (PEA + DQTD)
    ENDIF  
    
      LIN = LIN + 2
      if lin > 20
        lin = lin + 1
      endif
    mite = mite + 1
     
enddo
enddo
clos data
return

