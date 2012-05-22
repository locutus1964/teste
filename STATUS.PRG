* STATUS - CONTROLE DOS STATUS
* SISTEMA DE EMISSAO/CONTROLE DE PGI
* ADAO ELI PEREIRA - V 1.4 - 18/11/90
*
Sele 0
  use STATUS index STATUS.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Atualiza‡„o do Status dos Clientes'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'C¢digo do Status'
@ 12,15 say 'Descri‡„o'
mstatus = space(01)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    INC = ' '
    mdstatus = SPACE(30)
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get mstatus pict '!'
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
          if SEEK(mstatus)
            INC = '*'
            @ 12,40 say dstatus
            mdstatus = dstatus
          else
            @ 12,40 say mdstatus
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(mstatus)
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
      @ 12,40 get mdstatus
         read
           if mdstatus <> space(10)
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
    repl status with mstatus
    repl dstatus with mdstatus
*
  enddo
  clos data
return
