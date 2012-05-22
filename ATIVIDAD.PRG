* ATIVIDADES - CONTROLE DAS ATIVIDADES
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 11/07/93
*
Sele 0
  use ATIVIDAD index ATIVIDAD.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Atualiza‡„o das Atividades dos Clientes'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'C¢digo da Atividade'
@ 12,15 say 'Descri‡„o'
matividade = space(04)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    INC = ' '
    mdatividade = SPACE(30)
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get matividade pict '@!'
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
          if SEEK(matividade)
            INC = '*'
            @ 12,40 say datividade
            mdatividade = datividade
          else
            @ 12,40 say mdatividade
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(matividade)
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
      @ 12,40 get mdatividade
         read
           if mdatividade <> space(10)
             exit
           endif
         ? MENSAG(CALPHA)
    enddo
*
    MENS = DADCORR
    DO PEGRESP WITH [SN]
      IF MC = 'N'
        LOOP
      ENDIF
*
    IF INC = ' '
      appe blan
    endif
*
    repl atividade with matividade
    repl datividade with mdatividade
*
  enddo
  clos data
return
