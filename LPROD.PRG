* LPROD - LINHAS DE PRODUTOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 23/08/93
*
Sele 0
  use LIMPROD index LIMPROD.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Atualiza‡„o de Linhas de Produtos'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,05 say 'Linha de Produto'
@ 12,05 say 'Descri‡„o'
MLPR  = space(02)
*
  do while .T.
*
    @ 08,25 CLEAR TO 21,78
    INC = ' '
    MDLPR    = SPACE(51)
*
    ? MENSAG(ECODIGO)
*
    @ 10,25 get MLPR  pict '@!'
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
          if SEEK(MLPR)
            INC = '*'
           @ 12,25 say DLPR   
            MDLPR    = DLPR   
          else
            @ 12,25 say MDLPR
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(MLPR)
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
      @ 12,25 get MDLPR   
         read
           if MDLPR <> space(10)
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
    repl LPR  with MLPR 
    repl DLPR with MDLPR   
*
  enddo
  clos data
return
