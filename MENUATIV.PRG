* BANCO - MENU DE BANCOS
* ADAO ELI PEREIRA - 11/02/92
*
DO WHILE .T.

  NUMIN3 = MENU2A_IN(.T.)
  DO CASE

    CASE NUMIN3 = 1
      DO ATIVIDAD

    CASE NUMIN3 = 2
      DO RATIVIDA

    CASE LASTKEY() = 27
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE

  RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
******************* FIM  DO  PROGRAMA  *************************
                    FUNCTION MENU2A_IN
****************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'STATUS' )
SAVE SCREEN TO MAIN2_SCR
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
  ? JANELA(13,41,19,77)
  ? BORDA(13,41,19,77)
*
@ 15,44 PROMPT "a - Atualiza��o das Atividades" MESSAGE  INCALTEXC
@ 17,44 PROMPT "b - Emiss�o de Relat�rios     " MESSAGE  IMPRCODM
*
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
