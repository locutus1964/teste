* CARGOS - MENU DE CARGOS
* ADAO ELI PEREIRA - 01/08/93
*
DO WHILE .T.

  NUMIN3 = MENU2R_IN(.T.)
  DO CASE

    CASE NUMIN3 = 1
      DO CARGOS

    CASE NUMIN3 = 2
      DO RCARGOS

    CASE LASTKEY() = 27
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE

  RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
******************* FIM  DO  PROGRAMA  *************************
                    FUNCTION MENU2R_IN
****************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'CARGOS' )
SAVE SCREEN TO MAIN2_SCR
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
  ? JANELA(13,41,19,74)
  ? BORDA(13,41,19,74)
*
@ 15,44 PROMPT "a - Atualiza‡„o dos Cargos  " MESSAGE  INCALTEXC
@ 17,44 PROMPT "b - Emiss„o de Relat¢rios   " MESSAGE  IMPRCODM
*
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
