* UNID - UNIDADES DE MEDIDA
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 04/09/93
*
Sele 0
  use UNIDADES index UNIDADES.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Atualiza‡„o de Unidades'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'Unidade'
@ 12,15 say 'Descri‡„o'
MUNIDADE  = space(03)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    MDUNIDADE    = SPACE(30)
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get MUNIDADE  pict '@!'
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
          if SEEK(MUNIDADE)
            INC = '*'
            @ 12,40 say DUNIDADE   COLOR &CORVER
            MDUNIDADE    = DUNIDADE   
          else
            @ 12,40 say MDUNIDADE COLOR &CORVER
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(MUNIDADE)
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
      @ 12,40 get MDUNIDADE   
         read
           if MDUNIDADE <> space(10)
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
    IF !SEEK(MUNIDADE)
      appe blan
    endif
*
    repl UNIDADE  with MUNIDADE 
    repl DUNIDADE with MDUNIDADE   
*
  enddo
  clos data
return
