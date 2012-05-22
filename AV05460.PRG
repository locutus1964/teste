* AV05900 - MENU DE CURVA ABC
* SISTEMA ADMINISTRATIVO DE VENDAS
* ADAO ELI PEREIRA - 15-04-90
*
GO = .T.
DO WHILE GO
  NUMIN5 = MENU5_IN(.T.)
    @ 02,68 SAY 'AV05900'
    IF NUMIN5 = 1
      DO AV05910
    ENDIF
    IF NUMIN5 = 2
      DO AV05920
    ENDIF
    IF NUMIN5 = 3
      DO AV05930
    ENDIF
    IF LASTKEY() = 27
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
    ENDIF
      RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
*---------------------------------------------------------------
FUNCTION MENU5_IN
*---------------------------------------------------------------
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'AV05900')
SAVE SCREEN TO MAIN2_SCR
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
  ? JANELA(10,38,18,78)
  ? BORDA(10,38,18,78)
*
  @ 12,40 PROMPT "a - Acumular Vendas do Mes anterior  " MESSAGE ;
    'Sistema acumula as Venda do mes anterior com as dos meses anteriores'
  @ 14,40 PROMPT "b - Relat¢rio Curva ABC - Valor M‚dio" MESSAGE ;
    'Imprime Relat¢rio das Vendas acumuladas no formato ABC'
  @ 16,40 PROMPT "c - Limpar acumulados do ano anterior" MESSAGE ;
    'Limpa acumulados do ano anterior e prepara arquivos para iniciar novo ano'
*
  MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
