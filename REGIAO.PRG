* REGIAO - CONTROLE DOS REGIAO
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 11/07/93
*
Clos data

Sele 0
  use REGIAO index REGIAO.IDX
*
MDEL = 0
*
? CABTELA(0,0,24,79)

mens = 'Atualiza‡„o das Regi”es'
? mensagr(mens)
@ 01,pos say mens
*
@ 10,15 say 'C¢digo da Regi„o'
@ 12,15 say 'Descri‡„o'
@ 14,15 say 'Representante'
@ 16,15 say 'Paga comiss„o'
MREGIAO  = space(04)
*
  do while .T.
*
    @ 08,40 CLEAR TO 21,78
    INC = ' '
    MDREGIAO    = SPACE(30)
    MREPRES     = SPACE(30)
    MPGCOMIS    = 'N'
*
    ? MENSAG(ECODIGO)
*
    @ 10,40 get MREGIAO  pict '@!'
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
          if SEEK(MREGIAO)
            INC = '*'
            @ 12,40 say DREGIAO   
            MDREGIAO    = DREGIAO   
            @ 14,40 say REPRES
            MREPRES    = REPRES
            @ 16,40 say PGCOMIS
            MPGCOMIS   = IIF(PGCOMIS,'S','N')
          else
            @ 12,40 say MDREGIAO
            @ 14,40 say MREPRES
            @ 16,40 say MPGCOMIS
            MENS = TESTECAD
            DO PEGRESP WITH [SN]
              if mc = 'N'
                loop
              endif
          endif
*
      IF SEEK(MREGIAO)
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
      @ 12,40 get MDREGIAO   
         read
           if MDREGIAO <> space(10)
             exit
           endif
         ? MENSAG(CALPHA)
    enddo
*
    @ 14,40 GET MREPRES
      READ
    DO WHILE .T.   
      @ 16,40 GET MPGCOMIS PICT '!' VALID MPGCOMIS $ [SN]
      READ
      IF MPGCOMIS = 'S' .OR. MPGCOMIS = 'N'
        EXIT
      ENDIF
      ? MENSAG("Aceito: 'S' paga Comiss„o e 'N' n„o paga")  
    ENDDO
          
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
    repl REGIAO  with MREGIAO 
    repl DREGIAO   with MDREGIAO   
    repl REPRES  with MREPRES
    repl PGCOMIS with MPGCOMIS = 'S'
*
  enddo
  clos data
return
