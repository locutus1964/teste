* VEICULOS - VEICULOS UTILIZADOS NAS ENTREGAS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 19/03/94
*
Sele 0
  use VEICULOS index VEICULOS.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Atualiza‡„o de Ve¡culos'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'Ve¡culo'
@ 12,15 say 'Descri‡„o'
MVEICULO  = space(08)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    MDVEICULO = SPACE(30)
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get MVEICULO  pict '@!'
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
          if SEEK(MVEICULO)
            INC = '*'
            @ 12,40 say DVEICULO   COLOR &CORVER
            MDVEICULO    = DVEICULO   
          else
            @ 12,40 say MDVEICULO COLOR &CORVER
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(MVEICULO)
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
      @ 12,40 get MDVEICULO   
         read
           if MDVEICULO <> space(10)
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
    IF !SEEK(MVEICULO)
      appe blan
    endif
*
    repl VEICULO  with MVEICULO 
    repl DVEICULO with MDVEICULO   
*
  enddo
  clos data
return
