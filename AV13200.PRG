* AV13200 - CONSULTA NOTAS FISCAIS POR PRODUTO
* ADAO ELI PEREIRA - 14/11/92

  SELE D
    USE PROD INDEX PROD.IDX 
  SELE C
    USE CADNF
  SELE B
    USE CLIEN ORDER X_GUE
 
  MPEDIDO = 0
  MREF    = SPACE(11)
  MGUE = SPACE(15)
 
DO WHILE .T.
 
DO ABREVGA 
? CABTELA45(0,0,49,79)
MENS = 'Consulta Notas Fiscais por Produto'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 03,02 SAY 'Produto'
@ 05,02 say ' Nota'
@ 05,09 say 'Tip'
@ 05,13 say 'Emiss„o'
@ 05,22 say 'Descri‡„o'
@ 05,60 say 'Pedido'
@ 05,67 say 'Quantidade'
? MENSAG49(ECODIGO)
 
  mped = 0
  SELE D
  @ 03,15 get MREF pict '@!'
    read
    
    IF LASTKEY() = 27
      DO FECHAVGA
      CLOS DATA
      RETURN
    ENDIF  
 
    IF EMPTY(MREF)
      ? MENSAG(CALPHA)
      LOOP
    ENDIF
      STOR LEN(RTRIM(MREF)) TO TAM
 
  lin = 6
 
  SELE C
  go top
do while !EOF()
  IF SUBS(REF,1,TAM) = SUBS(MREF,1,TAM) .AND. CANCELNF = SPACE(08)
    @ lin,02 say str(NNF,6)
    @ LIN,09 say TIP
    @ lin,13 say DEMI
    @ LIN,22 SAY SUBS(DES,1,37)
    @ lin,60 say str(PED,6)
    @ lin,67 say QTD PICT '99999,999.99'
    lin = lin + 1
  ENDIF
      if lin > 46
        lin = 6
        
        MENS = 'Continua a consulta? (S/N) '
        DO PEGRESP WITH [SN]
          IF MC = 'N'
            DO FECHAVGA
            RETURN
          ELSE
            @ 06,01 CLEAR TO 46,78
          ENDIF
      endif
    skip
      if EOF()
        ? MENSAG49('Final do Arquivo - Aperte qualquer tecla para continuar')
        =INKEY(0)
*        DO FECHAVGA
      endif
  enddo
enddo
clos data
return

