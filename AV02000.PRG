* AV02000 - MENU DE TRANSPORTADORAS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 27/09/92
*
DO WHILE .T.
   NUMIN3 = MENU21_IN(.T.)
   DO CASE
      CASE NUMIN3 = 1          &&--- a - Atualiza‡„o do Cadastro
         DO AV02102
      CASE NUMIN3 = 2          &&--- b - Impress„o de Relat¢rios
         DO AV02300
**      CASE LASTKEY() = 2
      case NUMIN3 = 0
         exit
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
RESTORE SCREEN FROM MAIN_SCR
RETURN
******************* FIM  DO  PROGRAMA  *************************
FUNCTION MENU21_IN
****************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'AV02000' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
IF CLR_SCR
   SET MESSAGE TO 23 CENTER
   ? JANELA(10,25,16,57)
   ? BORDA(10,25,16,57)
   @ 12,28 PROMPT "a - Atualiza‡„o do Cadastro" MESSAGE  INCALTEXC
   @ 14,28 PROMPT "b - Impress„o de Relat¢rios" MESSAGE ;
   "Imprime Relat¢rio por Ordem de C¢digo, Alfab‚tica ou Nome de Guerra"
   MENU TO NUMHOLD
ENDIF
RETURN(NUMHOLD)
