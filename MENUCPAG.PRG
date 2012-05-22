* MCONDPAG - MENU DAS CONDICOES DE PAGAMENTO
* ADAO ELI PEREIRA - VERSAO 1.2 - 23/10/91 - 23/07/93

DO WHILE .T.
   NUMIN2 = MENU_COND(.T.)
   DO CASE
     CASE NUMIN2 = 1
       DO CONDPAG
       
     CASE NUMIN2 = 2
       DO RCONDPAG
       
    CASE LASTKEY() = 27
      CLEAR
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
************************************************************************
FUNCTION MENU_COND
************************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0

=NOMEMOD( 'MENUCPAG' )
SAVE SCREEN TO MAIN2_SCR

IF CLR_SCR
   SET MESSAGE TO 23 CENTER
   ? JANELA(13,41,19,73)
   ? BORDA(13,41,19,73)

   @ 15,44 PROMPT "a - Atualiza‡„o do Cadastro" MESSAGE  INCALTEXC
   @ 17,44 PROMPT "b - Emiss„o de Relat¢rio   " MESSAGE  IMPRCODM

   MENU TO NUMHOLD
   RESTORE SCREEN FROM MAIN2_SCR
ENDIF

RETURN(NUMHOLD)
