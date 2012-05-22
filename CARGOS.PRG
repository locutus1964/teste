* REGIAO - CONTROLE DOS REGIAO
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 11/07/93
*
Sele 0
  use CARGOS index CARGOS.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Atualiza‡„o de Cargo dos Contatos dos Vend/Repres.'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'Sigla do Cargo'
@ 12,15 say 'Descri‡„o'
MCARGO  = space(02)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    INC = ' '
    MDCARGO    = SPACE(30)
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get MCARGO  pict '@!'
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
          if SEEK(MCARGO)
            INC = '*'
            @ 12,40 say DCARGO   
            MDCARGO    = DCARGO   
          else
            @ 12,40 say MDCARGO
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(MCARGO)
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
      @ 12,40 get MDCARGO   
         read
           if MDCARGO <> space(10)
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
    repl CARGO  with MCARGO 
    repl DCARGO   with MDCARGO   
*
  enddo
  clos data
return
