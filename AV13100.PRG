* AV13100 - CONSULTA NOTAS FISCAIS POR CLIENTE
* ADAO ELI PEREIRA - 4/11/92

  SELE C
    USE CADNF
  SELE B
    USE CLIEN ORDER X_GUE

  MPEDIDO = 0
  MGUE = SPACE(15)
 
DO WHILE .T.

SELE B

DO ABREVGA 
? CABTELA45(0,0,49,79)
MENS = 'Consulta Notas Fiscais por Cliente'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 03,02 SAY 'Cliente'
@ 05,02 say ' Nota'
@ 05,09 say 'Tip'
@ 05,13 say 'Emiss„o'
@ 05,22 say 'Descri‡„o'
@ 05,60 say 'Pedido'
@ 05,67 say 'Quantidade'
? MENSAG49(ECODIGO)
 
  mped = 0
  @ 03,13 get MGUE pict '@!'
    read
    
    IF LASTKEY() = 27
      CLOS DATA
      DO FECHAVGA
      RETURN
    ENDIF  
    
    IF EMPTY(MGUE)
      ? MENSAG(CALPHA)
      LOOP
    ENDIF
 
    seek MGUE
      if !(eof() .or. bof())
        @ 03,29 SAY RAZ
      else
        ? MENSAG(CORRIJA)
        =INKEY(0)
        LOOP
      endif

  MCGC = CGC

  lin = 6
 
  SELE C
  go top
do while !EOF()
  IF CGC = MCGC .AND. CANCELNF = SPACE(08)
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
        =MENSAG49('Final do Arquivo - Aperte qualquer tecla para continuar')
        =INKEY(0)
      endif
  enddo
enddo
clos data
return

