* AV08000 - MENU DE DUPLICATAS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06-05-90
*
DO WHILE .T.
  NUMIN2 = MENUDUP_IN(.T.)
  DO CASE
    CASE NUMIN2 = 1         &&--- a-Impress„o de Duplicatas
       DO AV08110
    CASE NUMIN2 = 2         &&--- b-Consulta Situa‡„o do Cliente
       DO AV08121
    CASE NUMIN2 = 3         &&--- c-Registro ou Altera‡ao do Pagamento
       DO AV08132
    CASE NUMIN2 = 4         &&--- d-Consulta pelo Nome de Guerra
       DO AV08140
    CASE NUMIN2 = 5         &&--- e-Cancelamento e Exclusao de Duplicatas
       DO AV08150
    CASE NUMIN2 = 6         &&--- f-Reimpress„o de Duplicatas
       DO AV08160
    CASE NUMIN2 = 7         &&--- g-Sele‡„o/Rela‡„o de Notas Fiscais
       DO AV08170
    CASE NUMIN2 = 8         &&--- h-Duplicatas com Observa‡”es/Problemas
       DO AV08180
    CASE LASTKEY() = 27
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE
      RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
* --------------------------------------------------------------------
                   FUNCTION MENUDUP_IN
* --------------------------------------------------------------------
PARAMETERS CLR_SCR
NUMHOLD = 0
*

SAVE SCREEN TO MAIN2_SCR
=NOMEMOD( 'AV08100' )
SET MESS TO 23 CENTER
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
  DO JANELA WITH 10,32,21,78
  DO BORDA WITH 10,32,21,78
*
@ 12,35 PROMPT "a - Impress„o de Duplicatas              " MESSAGE ;
  "Imprime formul rio de Duplicatas quando exigido"
@ 13,35 PROMPT "b - Consulta Situa‡„o do Cliente         " MESSAGE ;
  "Mostra na tela a situa‡„o das Duplicatas do Cliente"
@ 14,35 PROMPT "c - Registro ou Altera‡ao do Pagamento   " MESSAGE ;
  "Altera dados e valores e Registra o Pagamento pelos Clientes"
@ 15,35 PROMPT "d - Consulta pelo Nome de Guerra         " MESSAGE ;
  "Consulta situa‡„o das Duplicatas do Cliente pelo Nome de Guerra"
@ 16,35 PROMPT "e - Cancelamento e Exclusao de Duplicatas" MESSAGE ;
  "Seleciona e copia as Duplicatas geradas para o Sistema Contas a Pagar"
@ 17,35 PROMPT "f - Reimpress„o de Duplicatas            " MESSAGE ;
  "Reimprime o formul rio de Duplicatas (Triplicatas ou c¢pias)"
@ 18,35 PROMPT "g - Sele‡„o/Rela‡„o de Notas Fiscais     " MESSAGE ;
  "Relaciona as Notas Fiscais emitidas e que geraram Duplicatas"
@ 19,35 PROMPT "h - Duplicatas com Observa‡”es/Problemas " MESSAGE ;
  "Relaciona as Duplicatas com Observa‡”es ou Problemas"
*
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
