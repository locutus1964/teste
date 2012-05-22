* AV05910 - ACUMULA VALORES DAS VENDAS DO MES ANTERIOR P/EMISSAO DAS CURVAS
* SISTEMA ADMINISTRATIVO DE VENDAS
* ADAO ELI PEREIRA - 15-04-90
*
MMES = space(02)
  ? JANELA(14,15,18,77)
  ? BORDA(14,15,18,77)
*
MENS = 'Informe o mes para sele‡„o das Notas Fiscais de Venda '
@ 16,18 SAY MENS GET MMES PICT '99'
  READ
    IF VAL(MMES) < 1 .OR. VAL(MMES) > 12
      ? MENSAG('Mes inv lido - ENTER')
      =INKEY(0)
      RETURN
    ENDIF  
    
    MENS = DADCORR
    DO PEGRESP WITH [SN]
      IF MC = 'N'
        RETURN
      ENDIF

MENS = 'Selecionando as Notas Fiscais do Mes '+MMES+'... Aguarde'
? MENSAG(MENS)
*
USE CADNF
COPY TO CADWORK FIELDS REF,DEMI,VAL,QTD FOR SUBS(DEMI,4,2) = MMES .AND. ;
       EDUP = 'S'
*
? MENSAG('Classificando e acumulando as Notas de Venda Selecionadas... Aguarde')
*
  USE CADWORK
    INDEX ON REF TO CADWORK.IDX
    TOTAL TO CURTOTAL ON REF FIELDS QTD,VAL
  USE CURGERAL
    INDEX ON REF TO CURGERAL.IDX
*
? MENSAG('Juntando as Notas Selecionadas com as dos meses Anteriores... Aguarde')
*
  USE CURGERAL INDEX CURGERAL.IDX
    REPL ALL QTDMES WITH 0,VALMES WITH 0
    CLOS DATA
*
  SELE B
    USE CURGERAL INDEX CURGERAL.IDX
  SELE A
    USE CURTOTAL
*
  DO WHILE !EOF()
    STOR REF TO MREF
    STOR QTD TO MQTD
    STOR VAL TO MVAL

    SELE B
      SEEK MREF
        IF (EOF() .OR. BOF())
          APPE BLAN
          REPL REF WITH MREF
        ENDIF
        
          REPL QTDMES WITH MQTD,VALMES WITH MVAL,QTDACU WITH (QTDACU+MQTD)
          REPL VALACU WITH (VALACU+MVAL)
          
    SELE A
      SKIP
  ENDDO
*
CLOS DATA
RETURN