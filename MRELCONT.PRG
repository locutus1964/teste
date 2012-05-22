* MRELCONT - MENU DE RELATORIOS CONTABEIS
* ADAO ELI PEREIRA - 11/02/92

DO WHILE .T.
   NUMin2 = MENU_IN11(.T.)
   DO CASE
    CASE NUMin2 = 1
      DO COMPETEN
    CASE NUMin2 = 2
      DO msaida
    CASE NUMin2 = 3
      DO IPI_ICM
    CASE NUMin2 = 4
      DO RAZAOMES
    CASE NUMin2 = 5
      DO FECHAMES
    CASE LASTKEY() = 27
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT

********************************************
              FUNCTION MENU_IN11
********************************************
PARAMETERS CLR_SCR
NUMHOLD = 0

@ 02,68 SAY 'MRELCONT'
SAVE SCREEN TO MAIN2_SCR

IF CLR_SCR
   ? JANELA(09,20,17,64)
   ? BORDA(09,20,17,64)
   SET MESSAGE TO 23 CENTER
   
   @ 11,23 PROMPT "a - Define Competˆncia para fechamento " MESSAGE ;
   "Define o mes e ano (Competˆncia) para fechamento Cont bil do Sistema"
   @ 12,23 PROMPT "b - Emite o Registro de Saidas         " MESSAGE ;
   "Emite o Registro de Saidas para o mes informado"  
   @ 13,23 PROMPT "c - Resumo do IPI e ICMS               " MESSAGE ;
   "Emite o Resumo do IPI e do ICMS num per¡odo selecionado"  
   @ 14,23 PROMPT "d - Raz„o Geral do Mes por Cliente     " MESSAGE ;
   "Emite o Relat¢rio Raz„o Geral do Mes por Cliente"  
   @ 15,23 PROMPT "e - Encerramento do mes                " MESSAGE ;
   'Encerra mes de Competˆncia. Fecha entrada de documentos com data anterior'
   MENU TO NUMHOLD
*   RESTORE SCREEN FROM MAIN2_SCR
ENDIF

RETURN(NUMHOLD)
