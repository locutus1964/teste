* AV01100 - ATUALIZACAO DO CADASTRO DE ESTADOS (UF) - 09-09/86
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06/04/90 - 23/10/91
*

sele A
use ESTADOS index ESTADOS.IDX
*
SAVE SCREEN TO MAIN2_SCR
*
DO CABTELA WITH 0,0,24,79
@ 01,27 say 'Atualiza‡„o de Estados (UF)'
@ 08,10 say 'Sigla do Estado (UF)  '
@ 11,10 say 'Descri‡„o do Estado'
@ 14,10 say 'Percentual de ICM do Estado'
mdel  = 0
mcest = space(02)
MVAR1 = SPACE(02)
*
do while .T.
*
  mdest = SPACE(20)
  micm  = 0.00
*
  ? MENSAG(ECODIGO)

 
  @ 8,40 get mcest pict '!!'
    read
*
    if lastkey() = 27
      if mdel = 1
        pack
      endif
        close databases
        clear
        rest screen from main2_scr
      return
    endif
  
    if mcest = space(02)
      ? mensag(CALPHA)
      =INKEY(0)
      loop
    endif
*
   
    IF !SEEK(MCEST)
      @ 11,40 say mdest
      @ 14,40 say str(micm,5,2)
      mens = testecad
      do pegresp with [SN]
        if mc = 'N'
          loop
        ENDIF
    ENDIF
*
    IF SEEK(MCEST)
      @ 11,40 say  dest
        mdest = dest
      @ 14,40 say str(icm,5,2)
        micm  = icm
*
      @ 23,10 say space(60)
      mens = testeinc
      do pegopcao with [123]
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
      @ 11,40  get mdest pict '@!'
        read
        if mdest <> space(20)
          exit
        endif
        ? mensag(CALPHA)
    enddo
*
    @ 23,01 say space(78)
    @ 14,40 get micm pict '99.99'
      read
*
      mens = DADCORR
      do pegresp with [SN]
      if mc = 'N'
        loop
      endif
*
      if (eof() .or. bof())
        appe blan
      endif
*
      repl cest with mcest,dest with mdest,icm with micm
*
  enddo
RETURN


