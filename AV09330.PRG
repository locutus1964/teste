* AV09500 - LIMPEZA DE NOTAS FISCAIS
*
  STOR 0 TO ULTNF
*
DO WHILE .T.
? MENSAG('Informe at‚ qual numero de NF deve ser excluida')
? JANELA(15,35,19,73)
? BORDA(15,35,19,73)

@ 17,38 SAY 'N£mero da Nota Fiscal 'GET ULTNF PICT '9999999'
  READ
  
  MENS = 'N£mero '+STR(ULTNF,6)+' est  correto ?'
  DO PEGRESP WITH [SN]
    IF MC = 'N'
      RETURN
    ELSE
      EXIT
    ENDIF
ENDDO
*
? MENSAG('Copiando as Notas Fiscais a serem excluidas... Aguarde')
USE CADNF
  COPY TO NCADNF FOR NNF > ULTNF .OR. CVEN = SPACE(1) .OR. ENT = SPACE(8)

? MENSAG('Classificando as Notas Fiscais n„o excluidas... Aguarde')
USE CADNF
  ZAP
  USE CADNF ORDER X_NFREF
  APPE FROM NCADNF
*
ERASE NCADNF.DBF
*
CLEAR
RETURN

