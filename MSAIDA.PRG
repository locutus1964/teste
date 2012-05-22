*** MSAIDA - EMITE REGISTRO DE SAIDAS DA EMPRESA ***
*** 25/01/93 ***

   GO = .T.
   DO WHILE GO
      NUMIN2 = MENU_IN14(.T.)
      IF NUMIN2 = 1
         DO RELDEF
      ENDIF
      IF NUMIN2 = 2
        DO RESUMO
     ENDIF
      IF NUMIN2 = 3
         DO ALTFOLHA
      ENDIF
      IF LASTKEY() = 27
         clear
         RESTORE SCREEN FROM MAIN_SCR
         RETURN
      ENDIF
      RESTORE SCREEN FROM MAIN2_SCR
   ENDDO
   QUIT
************************
   FUNCTION MENU_IN14

   PARAMETERS CLR_SCR
   NUMHOLD = 0

   @ 02,69 SAY 'MSAIDA'
   SAVE SCREEN TO MAIN2_SCR

   IF CLR_SCR
      SET MESSAGE TO 23 CENTER
      ? JANELA(14,35,20,72)
      ? BORDA(14,35,20,72)
      
      @ 16,38 PROMPT "a - Registro de Saidas          " MESSAGE ;
        "Imprime Relat rio Registro de Saidas"
      @ 17,38 PROMPT "b - Resumo do Registro de Saidas"MESSAGE ;
        "Imprime Relat¢rio Resumo do Registro de Saidas"
      @ 18,38 PROMPT "c - Altera N£mero Ultima Folha  " MESSAGE ;
        "Altera o N£mero da Ultima Folha do Registro de Saidas"

      MENU TO NUMHOLD
   ENDIF

   RETURN(NUMHOLD)