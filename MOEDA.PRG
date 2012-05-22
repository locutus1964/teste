* MOEDA - MENU DE MOEDAS
* ADAO ELI PEREIRA - 19/01/92
*
DO WHILE .T.
  NUMIN2 = MENUM_IN(.T.)
  DO CASE
    CASE NUMIN2 = 1
      DO MOEDAS
    
    CASE NUMIN2 = 2
      DO MOETAXA
    
    CASE NUMIN2 = 3
      DO RMOEDAS2
    
    CASE NUMIN2 = 4
      DO RMOETAXA

    CASE LASTKEY() = 27
      CLEAR
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE
    RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
************************************************************************
FUNCTION MENUM_IN
************************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'MOEDA' )
SAVE SCREEN TO MAIN2_SCR
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
*  DO COR2
  DO JANELA WITH 12,42,19,76
  DO BORDA WITH  12,42,19,76
*
  @ 14,45 PROMPT "a - Atualiza‡„o das Moedas   " MESSAGE  INCALTEXC
  @ 15,45 PROMPT "b - Atualiza‡„o das Paridades" MESSAGE INCALTEXC
  @ 16,45 PROMPT "c - Relat¢rio das Moedas     " MESSAGE ;
    "Imprime Relat¢rio dos Nomes das Moedas cadastradas"
  @ 17,45 PROMPT "d - Relat¢rio das Paridades  " MESSAGE ;
    "Imprime Relat¢rio das Paridades em ordem de data"
 
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)


