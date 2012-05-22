* AV07424 - LISTA OS PEDIDOS COM PENDENCIAS (TELA)
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 17-07-90  -  09/12/91
*
? CABTELA(0,0,24,79)
  MENS = 'Lista dos Pedidos com Pendˆncias'
  ? MENSAGR(MENS)
  @ 01,POS SAY MENS
*
  MCGC  = SPACE(20)
  MREF  = SPACE(11)
  MVIA  = SPACE(10)
  MTRA  = SPACE(03)
  MEST  = SPACE(02)
  MESTT = SPACE(02)
  MRAZ  = SPACE(30)
  MEND  = SPACE(30)
  MCID  = SPACE(20)
  MBAI  = SPACE(20)
  MICM  = 0.00
*
  SELE B
    use PROD index PROD
  SELE C
    use APROVP index APROVP
*
  IF BOF()
    ? MENSAG('N„o existem Pedidos aprovados com Pendˆncia - ENTER')
    =INKEY(0)
    clos data
    return
  endif
*
  pagctr = 0

  do while !EOF()
    @ 04,02 say 'CGC do Cliente'
    @ 04,22 say 'Pedido'
    @ 04,29 say 'Referˆncia'
    @ 04,40 say 'Descri‡„o'
    @ 04,60 say 'Quantid'
    @ 04,69 say 'Pendˆncias'
*
    lin = 5

    do while !EOF()
      if eof()
        ? MENSAG('Final dos itens com pendˆncia - ENTER')
        =INKEY(0)
      ENDIF

      IF CLIE = ' ' .AND. PROD = ' '
        SKIP
        LOOP
      ENDIF

      @ lin,01 say CGC
      @ lin,22 say PED
      @ lin,29 say REF
      MREF = REF
*
      SELE B
        SEEK MREF
          IF !(EOF() .OR. BOF())
            @ LIN,40 SAY SUBS(DES,1,19)
          ENDIF
*
      SELE C
        IF SALE > 0
          @ LIN,61 SAY STR(SALE,8)
        ELSE
          @ LIN,61 SAY STR(QTD,8)
        ENDIF
*
        IF PROD = '1'
          @ LIN,69 SAY 'SEM SALDO'
        ENDIF
*
        IF PROD = '2'
          @ LIN,69 SAY 'NAO CAD.'
        ENDIF
*
        IF PROD = '9'
          @ LIN,69 SAY 'AT.PARC.'
        ENDIF
*
        IF CLIE = '1' .OR. CLIE = '2' .OR. CLIE = '9'
          IF CLIE = '1'
            @ LIN+1,69 SAY 'S/CRED'
          ELSE
            IF CLIE = '2'
              @ LIN+1,69 SAY 'NAO CAD'
            ELSE
              @ LIN+1,69 SAY 'VAL EXC'
            ENDIF
          ENDIF
          LIN = LIN + 1
        ENDIF
*
        IF CLIE <> ' ' .OR. PROD = '1' .OR. PROD = '2'
          DELETE
        ENDIF
*
        LIN = LIN + 1
        IF LIN > 21
          ? MENSAG('Existem mais ¡tens - ENTER para continuar')
          =INKEY(0)
          LIN = 5
          @ 05,01 CLEAR TO 21,78
        ENDIF

        skip
        IF EOF()
          ? MENSAG('Final dos itens com pendˆncias - ENTER')
          =INKEY(0)
        ENDIF

      enddo
    ENDDO

CLOS DATA
RETURN
