* MENUEMBA - MENU DE TIPOS DE EMBALAGEM
* ADAO ELI PEREIRA - 04/09/93
*
DO WHILE .T.

  NUMIN3 = MENU2E_IN(.T.)
  DO CASE

    CASE NUMIN3 = 1
      DO EMBAL

    CASE NUMIN3 = 2
      DO REMBAL

    CASE NUMIN3 = 3
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE

  RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
******************* FIM  DO  PROGRAMA  *************************
                    FUNCTION MENU2E_IN
****************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'MENUEMBA' )
SAVE SCREEN TO MAIN2_SCR
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
  ? JANELA(13,41,21,76)
  ? BORDA(13,41,21,76)
*
@ 15,44 PROMPT "a - Atualiza‡„o das Embalagens" MESSAGE  INCALTEXC
@ 17,44 PROMPT "b - Emiss„o de Relat¢rios     " MESSAGE  IMPRCODM
@ 19,44 PROMPT "v - Volta ao Menu anterior    " MESSAGE  ENCMENU
*
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
