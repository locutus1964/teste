* AV05930 - LIMPEZA DO ARQUIVO ACUMULADO PARA EMISSAO DA CURVA ABC
* SISTEMA ADMINISTRATIVO DE VENDAS
* ADAO ELI PEREIRA - 15-04-90
*
  MENS = 'Tem certeza que pode eliminar os acumulados do ano ?'
  DO PEGRESP WITH [SN]
      IF MC = 'S'
        MENS = 'Confirma a elimina‡„o (C) ou n„o confirma (N) ' 
        DO PEGRESP WITH [CN]
            IF MC = 'N'
              RETURN
            ELSE
              USE CURGERAL
              ZAP
            ENDIF
      ELSE
        RETURN
      ENDIF
*
CLOS DATA
RETURN
