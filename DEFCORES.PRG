* DEFCORES - FUNCAO PARA ESCOLHA DE CORES DOS SISTEMAS
* ADAO ELI PEREIRA - 7/11/92
*
    MENS = 'Altera a composi‡„o das cores ?'
    DO PEGRESP WITH [SN]
      IF MC = 'N'
        DO TELAG
        RETURN
      ENDIF

DO WHILE .T.
    USE ARQCORES
    @ 04,00 CLEAR TO 21,79
    ? JANELA(05,01,22,30)
    ? BORDA(05,01,22,30)
    @ 05,04 SAY 'Cores dispon¡veis: '
    @ 06,08 say 'N   - Preto'
    @ 07,08 say 'B   - Azul'
    @ 08,08 say 'G   - Verde'
    @ 09,08 say 'BG  - Ciano'
    @ 10,08 say 'R   - Vermelho'
    @ 11,08 say 'BR  - Magenta'
    @ 12,08 say 'GR  - Marron'
    @ 13,08 say 'W   - Branco'
    @ 14,08 say 'N+  - Cinza'
    @ 15,08 say 'B+  - Azul claro'
    @ 16,08 say 'G+  - Verde claro'
    @ 17,08 say 'BG+ - Ciano claro'
    @ 18,08 say 'R+  - Vermelho claro'
    @ 19,08 say 'BR+ - Magenta claro'
    @ 20,08 say 'GR+ - Amarelo claro'
    @ 21,08 say 'W+  - Branco intenso'

    MCORTELA    = CORTELA
    BCORTELA    = CORTELAB
    MCORLINHA   = CORLINHA
    BCORLINHA   = CORLINHAB
    MSAYLETRA   = SAYLETRA
    BSAYLETRA   = SAYLETRAB
    MSAYREVERSO = SAYREVERSO
    BSAYREVERSO = SAYREVERSB
    MBORDA1     = BORDA1
    BBORDA1     = BORDA1B
    MBORDA2     = BORDA2
    BBORDA2     = BORDA2B
    MGETLETRA   = GETLETRA
    BGETLETRA   = GETLETRAB
    MGETREVERSO = GETREVERSO
    BGETREVERSO = GETREVERSB
    MMENSAG1    = CORMENSAG1
    BMENSAG1    = CORMENSAB1
    MMENSAG2    = CORMENSAG2
    BMENSAG2    = CORMENSAB2
    MCORSOMB1   = CORSOMB1
    MCORSOMB2   = CORSOMB2
    BCORSOMB1   = BCORSOM1
    BCORSOMB2   = BCORSOM2

    ? JANELA(08,27,21,74)
    ? BORDA(08,27,21,74)
    @ 09,30 SAY 'Cor 1 das sombras dos boxes'
    @ 10,30 SAY 'Cor 2 das sombras dos boxes'
    @ 11,30 SAY 'Cor do texto impresso nos Menus'
    @ 12,30 say 'Cor do fundo da Tela'
    @ 13,30 SAY 'Cor do texto da barra dos menus'
    @ 14,30 SAY 'Cor da barra "highlight" dos menus'
    @ 15,30 SAY 'Primeira cor da Borda da Tela'
    @ 16,30 SAY 'Segunda cor da Borda da Tela'
    @ 17,30 SAY 'Cor dos contornos das janelas'
    @ 18,30 SAY 'Cor do texto dos dados alterados'
    @ 19,30 SAY 'Cor destaque dos textos digitados'
    @ 20,30 SAY 'Cor de fundo dos textos digitados'
    @ 11,68 SAY MCORTELA
    @ 11,71 SAY BCORTELA
    @ 12,68 SAY MCORLINHA
    @ 12,71 SAY BCORLINHA
    @ 13,68 SAY MSAYLETRA
    @ 13,71 SAY BSAYLETRA
    @ 14,68 SAY MSAYREVERSO
    @ 14,71 SAY BSAYREVERSO
    @ 15,68 SAY MBORDA1
    @ 15,71 SAY BBORDA1
    @ 16,68 SAY MBORDA2
    @ 16,71 SAY BBORDA2
    @ 17,68 SAY MGETLETRA
    @ 17,71 SAY BGETLETRA
    @ 18,68 SAY MGETREVERSO
    @ 18,71 SAY BGETREVERSO
    @ 19,68 SAY MMENSAG1
    @ 19,71 SAY BMENSAG1
    @ 20,68 SAY MMENSAG2
    @ 20,71 SAY BMENSAG2
        @ 09,68 GET MCORSOMB1 PICT '@!' VALID(MCORTELA$'N _B _G _BG_R _BR_GR_W _X ')
        @ 09,71 GET BCORSOMB1 VALID(BCORTELA$' +')
        @ 10,68 GET MCORSOMB2 PICT '@!' VALID(MCORLINHA$'N _B _G _BG_R _BR_GR_W _X ')
        @ 10,71 GET BCORSOMB2 VALID(BCORLINHA$' +')
        @ 11,68 GET MCORTELA PICT '@!' VALID(MCORTELA$'N _B _G _BG_R _BR_GR_W _X ')
        @ 11,71 GET BCORTELA VALID(BCORTELA$' +')
        @ 12,68 GET MCORLINHA PICT '@!' VALID(MCORLINHA$'N _B _G _BG_R _BR_GR_W _X ')
        @ 12,71 GET BCORLINHA VALID(BCORLINHA$' +')
        @ 13,68 GET MSAYLETRA PICT '@!' VALID(MSAYLETRA$'N _B _G _BG_R _BR_GR_W _X ')
        @ 13,71 GET BSAYLETRA VALID(BSAYLETRA$' +')
        @ 14,68 GET MSAYREVERSO PICT '@!' VALID(MSAYREVERSO$'N _B _G _BG_R _BR_GR_W _X ')
        @ 14,71 GET BSAYREVERSO VALID(BSAYLETRA$' +')
        @ 15,68 GET MBORDA1 PICT '@!' VALID(MBORDA1$'N _B _G _BG_R _BR_GR_W _X ')
        @ 15,71 GET BBORDA1 VALID(BBORDA1$' +')
        @ 16,68 GET MBORDA2 PICT '@!' VALID(MBORDA2$'N _B _G _BG_R _BR_GR_W _X ')
        @ 16,71 GET BBORDA2 VALID(BBORDA2$' +')
        @ 17,68 GET MGETLETRA PICT '@!' VALID(MGETLETRA$'N _B _G _BG_R _BR_GR_W _X ')
        @ 17,71 GET BGETLETRA VALID(BGETLETRA$' +')
        @ 18,68 GET MGETREVERSO PICT '@!' VALID(MGETREVERSO$'N _B _G _BG_R _BR_GR_W _X ')
        @ 18,71 GET BGETREVERSO VALID(BGETREVERSO$' +')
        @ 19,68 GET MMENSAG1 PICT '@!' VALID(MMENSAG1$'N _B _G _BG_R _BR_GR_W _X ')
        @ 19,71 GET BMENSAG1 VALID(BMENSAG1$' +')
        @ 20,68 GET MMENSAG2 PICT '@!' VALID(MMENSAG2$'N _B _G _BG_R _BR_GR_W _X ')
        @ 20,71 GET BMENSAG2 VALID(BMENSAG2$' +')
          READ

    MENS = DADCORR
    DO PEGRESP WITH [SN]
      IF MC = 'N'
        LOOP
      ELSE
        REPL CORSOMB1   WITH MCORSOMB1,  CORSOMB2   WITH MCORSOMB2
        REPL BCORSOM1   WITH BCORSOMB1,  BCORSOM2   WITH BCORSOMB2
        REPL CORTELA    WITH MCORTELA,   CORTELAB   WITH BCORTELA
        REPL CORLINHA   WITH MCORLINHA,  CORLINHAB  WITH BCORLINHA
        REPL SAYLETRA   WITH MSAYLETRA,  SAYLETRAB  WITH BSAYLETRA
        REPL SAYREVERSO WITH MSAYREVERSO,SAYREVERSB WITH BSAYREVERSO
        REPL BORDA1     WITH MBORDA1,    BORDA1B    WITH BBORDA1
        REPL BORDA2     WITH MBORDA2,    BORDA2B    WITH BBORDA2
        REPL GETLETRA   WITH MGETLETRA,  GETLETRAB  WITH BGETLETRA
        REPL GETREVERSO WITH MGETREVERSO,GETREVERSB WITH BGETREVERSO
        REPL CORMENSAG1 WITH MMENSAG1,   CORMENSAB1 WITH BMENSAG1
        REPL CORMENSAG2 WITH MMENSAG2,   CORMENSAB2 WITH BMENSAG2
      ENDIF

  EXIT
ENDDO
*
    PAR1_P1 = RTRIM(CORTELA)+RTRIM(CORTELAB)
    PAR1_P2 = RTRIM(CORLINHA)+RTRIM(CORLINHAB)
      IF PAR1_P1 + PAR1_P2 <> SPACE(02)
        PAR1 = (PAR1_P1 +'/'+PAR1_P2) + ','
      ELSE
        PAR1 = ','
      ENDIF

    PAR2_P1 = RTRIM(SAYLETRA)+RTRIM(SAYLETRAB)
    PAR2_P2 = RTRIM(SAYREVERSO)+RTRIM(SAYREVERSB)
      IF PAR2_P1 + PAR2_P2 <> SPACE(02)
        PAR2 = (PAR2_P1 +'/'+PAR2_P2) + ','
      ELSE
        PAR2 = ','
      ENDIF

    PAR3_P1 = RTRIM(BORDA1)+RTRIM(BORDA1B)
    PAR3_P2 = RTRIM(BORDA2)+RTRIM(BORDA2B)
      IF PAR3_P2 + PAR3_P1 <> SPACE(02)
        PAR3 = (PAR3_P1 +'/'+PAR3_P2) + ','
      ELSE
        PAR3 = ','
      ENDIF

    PAR4_P1 = RTRIM(GETLETRA)+RTRIM(GETLETRAB)
    PAR4_P2 = RTRIM(GETREVERSO)+RTRIM(GETREVERSB)
      IF PAR4_P2 + PAR4_P1 <> SPACE(02)
        PAR4 = (PAR4_P1 +'/'+PAR4_P2) + ','
      ELSE
        PAR4 = ','
      ENDIF

    PAR5_P1 = RTRIM(CORMENSAG1)+RTRIM(CORMENSAB1)
    PAR5_P2 = RTRIM(CORMENSAG2)+RTRIM(CORMENSAB2)
      IF PAR5_P1 + PAR5_P2 <> SPACE(02)
        PAR5 = (PAR5_P1 +'/'+PAR5_P2)
      ELSE
        PAR5 = ','
      ENDIF

    PAR0_P1 = RTRIM(CORSOMB1)+RTRIM(BCORSOM1)
    PAR0_P2 = RTRIM(CORSOMB2)+RTRIM(BCORSOM2)
      IF PAR0_P1 + PAR0_P2 <> SPACE(02)
        CORSCHE = (PAR0_P1 +'/'+PAR0_P2)
      ELSE
        CORSCHE = ','
      ENDIF
      
     SET COLOR TO

     DEFCORES = PAR1+PAR2+PAR3+PAR4+PAR5
     SET COLOR TO &DEFCORES
     * @ 0,0 TO 24,79
     CLEAR
     DO TELAG

RETURN
* -----------------------------------------------------------------------
