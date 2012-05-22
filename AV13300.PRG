* AV13300 - CONSULTA NOTAS FISCAIS POR PEDIDO
* ADAO ELI PEREIRA - 14/11/92

  SELE 0  
    USE PEDID ORDER P_PEDID
  SELE 0
    USE CADNF
  SELE 0
    USE CLIEN ORDER X_GUE
  MPEDIDO = 0
  MREF    = SPACE(11)
  MGUE = SPACE(15)
  
DO ABREVGA 
DO WHILE .T.
 
  ? CABTELA45(0,0,49,79)
  MENS = 'Consulta Notas Fiscais por Pedido'
  ? MENSAGR(MENS)
  @ 01,POS SAY MENS
  @ 03,02 SAY 'Pedido'
  @ 05,02 say ' Nota'
  @ 05,09 say 'Tip'
  @ 05,13 say 'Emiss„o'
  @ 05,22 say 'Descri‡„o'
  @ 05,60 say 'Pedido'
  @ 05,67 say 'Quantidade'
  ? MENSAG49(ECODIGO)
 
  mped = 0
  @ 03,15 get MPED pict '999999'
  read
    
  IF LASTKEY() = 27
    CLOS DATA
    DO FECHAVGA
    RETURN
  ENDIF  
    
  IF MPED = 0
    ? MENSAG49(CNUMER)
    LOOP
  ENDIF
 
  lin = 6
 
  SELE CADNF
  go top
  do while !EOF()
    IF PED = MPED .AND. CANCELNF = SPACE(08)
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
          RETURN
        ELSE
          @ 06,01 CLEAR TO 46,78
        ENDIF
    endif

    skip
    if EOF()
      ? MENSAG49('Final do Arquivo - Aperte qualquer tecla para continuar')
      =INKEY(0)
    endif
  enddo
enddo
clos data
return

