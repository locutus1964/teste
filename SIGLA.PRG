* SIGLA - CONTROLE DAS SIGLAS DOS FUNCIONARIOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 01/08/93
*
Sele 0
  use SIGLAS index SIGLAS.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Atualiza‡„o das Siglas dos Funcion rios autorizados'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'Sigla do Funcion rio'
@ 12,15 say 'Nome completo'
MSIGLA = space(04)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    INC = ' '
    MNOME = SPACE(30)
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get MSIGLA pict '@!'
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
          if SEEK(MSIGLA)
            INC = '*'
            @ 12,40 say NOME
            MNOME = NOME
          else
            @ 12,40 say MNOME
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(MSIGLA)
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
      @ 12,40 get MNOME
         read
           if MNOME <> space(10)
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
    repl SIGLA with MSIGLA
    repl NOME with MNOME
*
  enddo
  clos data
return
