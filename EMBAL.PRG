* EMBAL - TIPOS DE EMBALAGEM
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 04/09/93
*
Sele 0
  use EMBALAGE index EMBALAGE.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Tipos de Embalagem'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'Embalagem'
@ 12,15 say 'Descri‡„o'
MEMBALAGEM  = space(03)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    MDEMBALAGEM    = SPACE(30)
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get MEMBALAGEM  pict '@!'
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
          if SEEK(MEMBALAGEM)
            INC = '*'
            @ 12,40 say DEMBALAGEM  COLOR &CORVER
            MDEMBALAGEM    = DEMBALAGEM   
          else
            @ 12,40 say MDEMBALAGEM COLOR &CORVER
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(MEMBALAGEM)
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
      @ 12,40 get MDEMBALAGEM   
         read
           if MDEMBALAGEM <> space(10)
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
    IF !SEEK(MEMBALAGEM)
      appe blan
    endif
*
    repl EMBALAGEM  with MEMBALAGEM 
    repl DEMBALAGEM with MDEMBALAGEM   
*
  enddo
  clos data
return
