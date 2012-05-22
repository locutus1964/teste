* CLASS - CLASSIFICACOES FISCAIS DO IPI
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 04/09/93
*
Sele 0
  use CLASFISC index CLASFISC.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Classifica‡„o Fiscal do IPI'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'C¢digo'
@ 12,15 say 'Classifica‡„o'
@ 14,15 say 'Descri‡„o'
MCLASFISC  = space(01)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    MDCLASFISC    = SPACE(12)
    MDESCRICAO    = SPACE(30)
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get MCLASFISC  pict '@!'
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
          if SEEK(MCLASFISC)
            INC = '*'
            @ 12,40 say DCLASFISC PICT '9999.99.9999' COLOR &CORVER
            MDCLASFISC    = DCLASFISC   
            @ 14,40 say DESCRICAO COLOR &CORVER
            MDESCRICAO    = DESCRICAO
          else
            @ 12,40 say MDCLASFISC PICT '9999.99.9999' COLOR &CORVER
            @ 14,40 say MDESCRICAO COLOR &CORVER
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(MCLASFISC)
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
    @ 23,01 say space(78)
    do while .t.
      @ 12,40 get MDCLASFISC PICT '9999.99.9999'
         read
           if MDCLASFISC <> space(10)
             exit
           endif
         ? MENSAG(CALPHA)
    enddo
*
    @ 14,40 GET MDESCRICAO    PICT '@!'
      READ
*
    MENS = DADCORR
    DO PEGRESP WITH [SN]
      IF MC = 'N'
        LOOP
      ENDIF
*
    IF !SEEK(MCLASFISC)
      appe blan
    endif
*
    repl CLASFISC  with MCLASFISC 
    repl DCLASFISC with MDCLASFISC   
    repl DESCRICAO with MDESCRICAO
*
  enddo
  clos data
return
