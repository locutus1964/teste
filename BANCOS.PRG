* BANCOS - CONTROLE DOS BANCOS
* SISTEMA DE EMISSAO/CONTROLE DE PGI
* ADAO ELI PEREIRA - V 1.4 - 18/11/90
*
Sele 0
  use ESTADOS index ESTADOS.IDX
Sele 0
  use BANCOS index BANCOS.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Atualiza‡„o dos Bancos e Agˆncias'
? mensagr(mens)
@ 01,pos say mens
*
@ 06,15 say 'N£mero do Banco'
@ 08,15 say 'C¢digo da Agˆncia'
@ 10,15 say 'Nome do Banco'
@ 12,15 SAY 'Endere‡o '
@ 14,15 say 'Cidade'
@ 16,15 say 'C¢digo da Cidade'
@ 18,15 say 'Sigla do Estado'
@ 20,15 SAY 'Conta Corrente/Cont bil'
mnbanco = 0
*
SELE BANCOS
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    INCA = ' '
    mdbanco = SPACE(30)
    mender  = space(30)
    mcodcid = SPACE(10)
    mctabco = space(10)
    MCIDADE = space(20)
    MCEST = space(02)
    magencia= 0
    mctacorr = space(15)
*
    ? MENSAG(ECODIGO)
*
    @ 06,40 get mnbanco pict '999'
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
*
        SEEK STR(mnbanco,3)
          if !(eof() .or. bof())
            INC = '*'
            @ 10,40 say dbanco
            mdbanco = dbanco
          else
            @ 10,40 say mdbanco
            @ 12,40 say mender
            @ 14,40 say mcidade
            @ 16,40 say mcodcid
            @ 18,40 say MCEST
            @ 20,40 say mctacorr
            @ 20,56 say mctabco
            @ 23,10 say space(60)
            
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
    do while .t.
      @ 08,40 get magencia pict '9999'
        read
        if magencia > 00000
          exit
        endif
        ? MENSAG(cnumer)
    enddo
*
    INC = ' '
    SEEK STR(MNBANCO,3) + STR(MAGENCIA,4)
      IF !(EOF() .OR. BOF())
        INC = '*'
        @ 10,40 say dbanco
          mdbanco = dbanco
        @ 12,40 say ender
          mender   = ender
        @ 14,40 say cidade
          mcidade  = cidade
        @ 16,40 say codcid
          mcodcid  = codcid
        @ 18,40 say estado
          MCEST  = estado
        @ 20,40 say ctacorr
          mctacorr   = ctacorr
        @ 20,56 say ctabco
          mctabco = ctabco
*
        MENS = TESTEINC
        DO PEGOPCAO WITH [123]
*
          if opcao = '1'
            loop
          endif
            if opcao = '3'
              delete
              mdel = 1
              loop
            endif
      ENDIF
*
    SELE BANCOS
*
    @ 23,10 say space(68)
    do while .t.
      @ 10,40 get mdbanco
         read
           if mdbanco <> space(30)
             exit
           endif
         ? MENSAG(CALPHA)
    enddo
*
    @ 23,10 say space(68)
    do while .t.
      @ 12,40 get mender
        read
          if mender <> space(30)
            exit
          endif
        ? MENSAG(CALPHA)
    enddo
*
    @ 23,10 say space(68)
    do while .t.
      @ 14,40 get mcidade
        read
          if mcidade <> space(20)
            exit
          endif
        ? MENSAG(CALPHA)
    enddo
*
    @ 23,10 say space(68)
    do while .t.
      @ 16,40 get mcodcid
        read
          if mcodcid <> space(10)
            exit
          endif
        ? MENSAG(CALPHA)
    enddo
*
    SELE ESTADOS
    @ 23,10 say space(68)
    do while .t.
      @ 18,40 get MCEST
        read
        
        if lastkey() = 27
          clos data
          return
        endif
        
        if MCEST = space(2)
          ? mensag(calpha)
          loop
        endif
        
        IF SEEK(MCEST)
          EXIT
        ELSE
          ? MENSAG('N„o cadastrado - Corrija ou entre (99) para encerrar')
        ENDIF
    enddo
*
    @ 23,10 say space(68)
    do while .t.
      @ 20,40 get mctacorr
        read
          if mctacorr <> space(15)
            exit
          endif
        ?MENSAG(CALPHA)
    enddo
*
    @ 23,1 say space(78)
    @ 20,56 get mctabco
      read
*
    MENS = DADCORR
    DO PEGRESP WITH [SN]
      IF MC = 'N'
        LOOP
      ENDIF
*
    SELE BANCOS
*
    IF INC = ' '
      appe blan
    endif
*
    repl nbanco with mnbanco,dbanco with mdbanco,agencia with magencia
    repl cidade with mcidade,estado with MCEST,codcid with mcodcid
    repl ctacorr with mctacorr,ender with mender,ctabco with mctabco
*
  enddo
  clos data
return
