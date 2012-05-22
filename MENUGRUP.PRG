* MENUGRUP - MENU DOS GRUPOS DE PRODUTOS    
* ADAO ELI PEREIRA - 23/08/93
*
DO WHILE .T.

  NUMIN3 = MENU2G_IN(.T.)
  DO CASE

    CASE NUMIN3 = 1
      DO GRUPOS

    CASE NUMIN3 = 2
      DO RGRUPOS

    CASE LASTKEY() = 27
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE

  RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
******************* FIM  DO  PROGRAMA  *************************
                    FUNCTION MENU2G_IN
****************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'MENUGRUP' )
SAVE SCREEN TO MAIN2_SCR
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
  ? JANELA(13,41,19,77)
  ? BORDA(13,41,19,77)
*
@ 15,44 PROMPT "a - Atualiza‡„o dos Grupos      " MESSAGE  INCALTEXC
@ 17,44 PROMPT "b - Emiss„o de Relat¢rios       " MESSAGE  IMPRCODM
*
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
