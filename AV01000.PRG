* AV01000 - MENU DE ESTADOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - VERSAO 1.2 - 23/10/91
*
DO WHILE .T.
  NUMIN2 = MENU2_IN(.T.)
  DO CASE

    CASE NUMIN2 = 1
      DO AV01100

    CASE NUMIN2 = 2
      DO AV01300
      
    CASE LASTKEY() = 27
      CLEAR
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE
      RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
************************************************************************
FUNCTION MENU2_IN
************************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'AV01000' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
*
IF CLR_SCR
*  DO COR2
  DO JANELA WITH 09,20,15,52
  DO BORDA WITH 09,20,15,52
*
    @ 11,23 PROMPT "a - Atualiza‡„o do Cadastro" MESSAGE(INCALTEXC)
    @ 13,23 PROMPT "b - Impress„o de Relat¢rios" MESSAGE ;
      " Imprime Relat¢rios por Ordem de Sigla ou Alfab‚tica "
      
 
  MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
