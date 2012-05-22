* CONDPAG - ATUALIZACAO DO CADASTRO DE CONDICOES DE PAGAMENTO
* ADAO ELI PEREIRA - 28/01/92
*
sele 0
use CONDPAG index CONDPAG.IDX
*
SAVE SCREEN TO MAIN2_SCR
*
? CABTELA(0,0,24,79)
MENS = 'Atualiza‡„o das Condi‡”es de Pagamento'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 05,10 say 'C¢digo'
@ 05,50 say 'Tipo'
@ 06,10 say 'Descri‡„o'
@ 08,10 say '     Parcelas    Dias    %    Parcela   Dias     %'
@ 09,10 say '        01                       13'
@ 10,10 say '        02                       14'
@ 11,10 say '        03                       15'
@ 12,10 say '        04                       16'
@ 13,10 say '        05                       17'
@ 14,10 say '        06                       18'
@ 15,10 say '        07                       19'
@ 16,10 say '        08                       20' 
@ 17,10 say '        09                       21'
@ 18,10 say '        10                       22'
@ 19,10 say '        11                       23'
@ 20,10 say '        12                       24'
mdel  = 0
MCONDCPAG = 0
*
do while .T.
*
MDESCCPA1 = SPACE(40)
MTIPOCPAG = SPACE(03)
STOR 0 TO MVENC01, MVENC02, MVENC03, MVENC04, MVENC05, ;
          MVENC06, MVENC07, MVENC08, MVENC09, MVENC10, ;
          MVENC11, MVENC12, MVENC13, MVENC14, MVENC15, ;
          MVENC16, MVENC17, MVENC18, MVENC19, MVENC20, ;
          MVENC21, MVENC22, MVENC23, MVENC24

STOR 0.00 TO MPERC01, MPERC02, MPERC03, MPERC04, MPERC05, ;
             MPERC06, MPERC07, MPERC08, MPERC09, MPERC10, ;
             MPERC11, MPERC12, MPERC13, MPERC14, MPERC15, ;
             MPERC16, MPERC17, MPERC18, MPERC19, MPERC20, ;
             MPERC21, MPERC22, MPERC23, MPERC24
*
? MENSAG(ECODIGO)
*
@ 05,30 get MCONDCPAG PICT '999'
  read
  
    if lastkey() = 27
      if mdel = 1
        pack
      endif
        close data
        clear
        rest screen from main2_scr
      return
    endif
    
    if MCONDCPAG = 0
      ? MENSAG(CNUMER)
      =INKEY(0)
      loop
    endif
*
*
    IF !seek(STR(MCONDCPAG,3))
      @ 05,65 say MTIPOCPAG
      @ 06,27 say MDESCCPA1
      @ 09,27 say STR(MVENC01,3)
      @ 09,31 say STR(MPERC01,6,2)
      @ 10,27 say STR(MVENC02,3)
      @ 10,31 say STR(MPERC02,6,2)
      @ 11,27 say STR(MVENC03,3)
      @ 11,31 say STR(MPERC03,6,2)
      @ 12,27 say STR(MVENC04,3)
      @ 12,31 say STR(MPERC04,6,2)
      @ 13,27 say STR(MVENC05,3)
      @ 13,31 say STR(MPERC05,6,2)
      @ 14,27 say STR(MVENC06,3)
      @ 14,31 say STR(MPERC06,6,2)
      @ 15,27 say STR(MVENC07,3)
      @ 15,31 say STR(MPERC07,6,2)
      @ 16,27 say STR(MVENC08,3)
      @ 16,31 say STR(MPERC08,6,2)
      @ 17,27 say STR(MVENC09,3)
      @ 17,31 say STR(MPERC09,6,2)
      @ 18,27 say STR(MVENC10,3)
      @ 18,31 say STR(MPERC10,6,2)
      
      mens = testecad
      DO PEGRESP WITH [SN]
        if mc = 'N'
          loop
        ENDIF
    endif
*
IF SEEK(STR(MCONDCPAG,3))
*
  @ 05,65 say TIPOCPAG
    MTIPOCPAG = TIPOCPAG
  @ 06,27 say DESCCPA1
    MDESCCPA1 = DESCCPA1
  @ 09,27 say STR(VENC01,3)
    MVENC01 = VENC01
  @ 09,31 say STR(PERC01,6,2)
    MPERC01 = PERC01
  @ 10,27 say STR(VENC02,3)
    MVENC02 = VENC02
  @ 10,31 say STR(PERC02,6,2)
    MPERC02 = PERC02
  @ 11,27 say STR(VENC03,3)
    MVENC03 = VENC03
  @ 11,31 say STR(PERC03,6,2)
    MPERC03 = PERC03
  @ 12,27 SAY VENC04
    MVENC04 = VENC04  
  @ 12,31 say STR(PERC04,6,2)
    MPERC04 = PERC04
  @ 13,27 say STR(VENC05,3)
    MVENC05 = VENC05
  @ 13,31 say STR(PERC05,6,2)
    MPERC05 = PERC05
  @ 14,27 say STR(VENC06,3)
    MVENC06 = VENC06
  @ 14,31 say STR(PERC06,6,2)
    MPERC06 = PERC06
  @ 15,27 say STR(VENC07,3)
    MVENC07 = VENC07
  @ 15,31 say STR(PERC07,6,2)
    MPERC07 = PERC07
  @ 16,27 say STR(VENC08,3)
    MVENC08 = VENC08
  @ 16,31 say STR(PERC08,6,2)
    MPERC08 = PERC08
  @ 17,27 say STR(VENC09,3)
    MVENC09 = VENC09
  @ 17,31 say STR(PERC09,6,2)
    MPERC09 = PERC09
  @ 19,27 say STR(VENC10,3)
    MVENC10 = VENC10
  @ 19,31 say STR(PERC10,6,2)
    MPERC10 = PERC10
*
  mens = testeinc
  DO PEGOPCAO WITH [123]
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
SAVE SCREEN TO TELA
do while .t.
@ 11,49 CLEAR TO 21,76
@ 11,49 TO 21,76
@ 12,50 SAY 'VIS - A Vista'
@ 13,50 SAY 'APR - Contra Apresenta‡„o'
@ 14,50 SAY 'DDL - Dias de Data L¡quido'
@ 15,50 SAY 'DFM - Dias fora o Mes'
@ 16,50 SAY 'DFQ - Dias fora a Quinzena'
@ 17,50 SAY 'DFD - Dias fora a Dezena'
@ 18,50 SAY 'SDB - Sem D‚bito'
@ 19,50 SAY 'PER - Permuta'
@ 20,50 say 'ANT - Pagamento Antecipado'

? mensag('Tipos de Vencimento aceitos')

@ 05,65 get MTIPOCPAG PICT '!!!'
  read

    mcpd = MTIPOCPAG
    if mcpd = 'VIS' .or. mcpd = 'APR' .or. mcpd = 'DDL' .or. mcpd = 'DFM' ;
      .or. mcpd = 'DFQ' .OR. mcpd = 'DFD' .or. mcpd = 'SDB' .OR. MCPD = 'ANT'
      exit
    endif
enddo
*
REST SCRE FROM TELA
@ 05,65 SAY MTIPOCPAG
do while .t.
@ 06,27 get MDESCCPA1
  read
    if MDESCCPA1 <> space(40)
      exit
    endif
      ? MENSAG(CALPHA)
enddo
*
@ 09,27 get MVENC01 PICT '999'
  read
*
do while .t.
@ 09,31 get MPERC01 PICT '999.99'
  read
    if MPERC01 > 0
      exit
    endif
      ? MENSAG('Exigido Percentual para primeiro vencimento')
enddo
*
      @ 10,27 get MVENC02 PICT '999'
      @ 10,31 get MPERC02 PICT '999.99'
      @ 11,27 get MVENC03 PICT '999'
      @ 11,31 get MPERC03 PICT '999.99'
      @ 12,27 get MVENC04 PICT '999'
      @ 12,31 get MPERC04 PICT '999.99'
      @ 13,27 get MVENC05 PICT '999'
      @ 13,31 get MPERC05 PICT '999.99'
      @ 14,27 get MVENC06 PICT '999'
      @ 14,31 get MPERC06 PICT '999.99'
      @ 15,27 get MVENC07 PICT '999'
      @ 15,31 get MPERC07 PICT '999.99'
      @ 16,27 get MVENC08 PICT '999'
      @ 16,31 get MPERC08 PICT '999.99'
      @ 17,27 get MVENC09 PICT '999'
      @ 17,31 get MPERC09 PICT '999.99'
      @ 18,27 get MVENC10 PICT '999'
      @ 18,31 get MPERC10 PICT '999.99'
        READ
          
        MENS = DADCORR
        DO PEGRESP WITH [SN]
          if mc = 'N'
            loop
          endif
*
    if (eof() .or. bof())
      appe blan
    endif
*
  repl CONDCPAG with MCONDCPAG, TIPOCPAG with MTIPOCPAG
  repl DESCCPA1 with MDESCCPA1
  repl VENC01   with MVENC01,   PERC01   with MPERC01
  repl VENC02   with MVENC02,   PERC02   with MPERC02
  repl VENC03   with MVENC03,   PERC03   with MPERC03
  repl VENC04   with MVENC04,   PERC04   with MPERC04
  repl VENC05   with MVENC05,   PERC05   with MPERC05
  repl VENC06   with MVENC06,   PERC06   with MPERC06
  repl VENC07   with MVENC07,   PERC07   with MPERC07
  repl VENC08   with MVENC08,   PERC08   with MPERC08
  repl VENC09   with MVENC09,   PERC09   with MPERC09
  repl VENC10   with MVENC10,   PERC10   with MPERC10
*
enddo
RETURN

