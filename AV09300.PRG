* LIMPEZA  - LIMPA OS ARQUIVOS COM DADOS LIQUIDADOS
* SISTEMA DE VENDAS
* ADAO ELI PEREIRA - V 1.0 - 5/04/92
*
GO = .T.
DO WHILE GO
*
  NUMIN2 = MENUL_IN(.T.)
    IF NUMIN2 = 1
      DO AV09310
    ENDIF
    IF NUMIN2 = 2
      DO AV09330
    ENDIF
    IF NUMIN2 = 3
      DO AV09340
    ENDIF
    IF NUMIN2 = 4
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
    ENDIF
      RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
*                   FUNCAO  ESCOLHA                            *
                   FUNCTION MENUL_IN
*
PARAMETERS CLR_SCR
NUMHOLD = 0
*
@ 01,70 SAY 'AV09300'
SAVE SCREEN TO MAINL_SCR
SET MESS TO 23 CENTER
*
IF CLR_SCR
  DO JANELA WITH 08,34,13,76
  DO BORDA WITH 08,34,13,76
*
@ 09,37 PROMPT "a - Limpeza dos acumulados do Estoque" MESSAGE ;
  "Zera os acumulados mensais ou anuais das Vendas"
@ 10,37 PROMPT "b - Exclui Notas Fiscais             " MESSAGE ;
  "Exlui as Notas Fiscais at‚ o n£mero indicado"
@ 11,37 PROMPT "c - Exclui Pedidos com Saldo ZERO    " MESSAGE ;
  "Exclui do Sistema os Pedidos de Clientes com Saldo ZERO"
@ 12,37 PROMPT "v - "+VOLMENU  MESSAGE  ENCMENU
*
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
