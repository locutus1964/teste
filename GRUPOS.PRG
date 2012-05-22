* GRUPOS - GRUPOS DE PRODUTOS           
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06/09/93
*
Sele 0
  use GRUPOS index GRUPOS.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Grupos de Produtos'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'C¢digo'
@ 12,15 say 'Descri‡„o'
@ 14,15 say '% Comiss„o'
MGRUPO  = space(02)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    MDGRUPO = SPACE(30)
    MCOMISSAO = 0.00
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get MGRUPO  pict '@!'
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
          if SEEK(MGRUPO)
            INC = '*'
            @ 12,40 say DGRUPO COLOR &CORVER
              MDGRUPO    = DGRUPO   
            @ 14,40 say COMISSAO PICT '99.99' COLOR &CORVER
              MCOMISSAO = COMISSAO
          else
            @ 12,40 say MDGRUPO COLOR &CORVER
            @ 14,40 say MCOMISSAO COLOR &CORVER
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(MGRUPO)
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
      @ 12,40 get MDGRUPO
         read
           if MDGRUPO <> space(10)
             exit
           endif
         ? MENSAG(CALPHA)
    enddo
    
    @ 14,40 GET MCOMISSAO PICT '99.99'
      READ
*
    MENS = DADCORR
    DO PEGRESP WITH [SN]
      IF MC = 'N'
        LOOP
      ENDIF
*
    IF !SEEK(MGRUPO)
      appe blan
    endif
*
    repl GRUPO  with MGRUPO 
    repl DGRUPO with MDGRUPO      
    repl COMISSAO with MCOMISSAO
*
  enddo
  clos data
return
