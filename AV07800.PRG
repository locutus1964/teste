* AV07800 - TRATAMENTO DAS DEVOLUCOES DE FORNECEDORES
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 6/5/90
*
STOR 0 TO MNDOC
*
? CABTELA(0,0,24,79)
  mens = 'Notas de Devolu‡„o de Clientes'
  ? mensagr(mens)
  @ 01,pos say mens
*
@ 5,1   SAY '                    A  t  e  n  c  a  o : '
@ 7,6   SAY '    As Devolu‡”es dos  CLIENTES  devem ser informadas ao Sis-'
@ 8,6   SAY '    tema para a corre‡„o dos saldos do estoque e a emiss„o do'
@ 9,6   SAY '    Livro Modelo 3.                                         '
@ 11,6  SAY '    A entrada de uma Devolu‡„o no Estoque ou uma corre‡„o, se'
@ 12,6  SAY '    houver um erro, ‚ feita indicando-se:'
@ 14,6  SAY '    Devolu‡„o                  (E) = aumenta saldo do estoque'
@ 15,6  say '    Cancelamento da Devolu‡„o  (S) = diminui saldo do estoque'

? MENSAG('Pressione qualquer tecla para continuar')
=INKEY(0)
@ 05,01 clear to 15,78
*
@ 3,3  say 'N.Guerra:'
@ 5,3 say 'Referˆncia  Descri‡„o do Produto'
@ 5,58 SAY 'N.Doc'
@ 5,64 say 'E/S'
@ 5,70 say 'Quantid'
STOR 6 TO LIN
select 0
USE MOD3 ORDER X_DATA
select 0
USE CLIEN ORDER X_GUE
select 0
USE PROD ORDER P_REF

DO WHILE .T.

  ? MENSAG(ECODIGO)

  STOR SPACE(04) TO MREF
  STOR SPACE(40) TO MDES
  STOR SPACE(1)  TO MES
  STOR 0.00 	 TO MQTD
  STOR 0         TO MNDOC
  STOR SPACE(15) TO MGUE
  STOR SPACE(03) TO LOJA
  STOR SPACE(20) TO MCGC
  SELECT CLIEN
  @ 3,13 GET MGUE
  READ
  
  IF LASTKEY() = 27
    CLOS DATA
    RETURN
  ENDIF  

  IF MGUE = SPACE(8)
    ? MENSAG('C¢digo/Nome de Guerra do Cliente n„o pode ser brancos - ENTER')
    =INKEY(0)
    LOOP
  ENDIF

  IF !SEEK(MGUE)
    ? MENSAG('Cliente n„o cadastrado - ENTER')
    =INKEY(0)
    LOOP
  ENDIF

  MCGC = CGC
  @ 3,30 SAY RAZ
  ? MENSAG(ECODIGO)
  DO WHILE .T.
    SELECT PROD   
    MREF = space(4)
    @ LIN,3 GET MREF PICT [9999]
    READ
  
    IF LASTKEY() = 27
      CLOS DATA
      RETURN
    ENDIF   

    IF EMPTY(MREF)
      ? MENSAG('Referˆncia do Produto nao pode ser brancos - ENTER')
      loop
    ENDIF

    IF SEEK(MREF)
      STOR DES TO MDES
      STOR SAL TO MSAL
      @ LIN, 15 SAY MDES
    ELSE
      ? MENSAG('Produto n„o cadastrado ou Referˆncia errada - ENTER')
      =INKEY(0)
      LOOP
    ENDIF

    @ 23,01 SAY SPACE(78)
    DO WHILE .T.
      @ lin,58 get mndoc pict '999999'
      read
      IF MNDOC > 0
        EXIT
      ENDIF
      ? MENSAG(CNUMER)
    ENDDO      

    @ 23,01 SAY SPACE(78)
    MES = ' '
    @ LIN,65 GET MES PICT '!' VALID(MES$'ES')
    READ

    ? MENSAG(ECODIGO)
    DO WHILE .T.
      @ LIN,67 GET MQTD PICT '99999999.99'
      READ
      
      IF LASTKEY() = 27
        CLOS DATA
        RETURN
      ENDIF  

      IF MES = 'S' .AND. MQTD > MSAL
        ? MENSAG('Quantidade saida n„o pode ser maior que saldo - ENTER')
        =INKEY(0)
        LOOP
      ENDIF
          
      IF MQTD <> 0
        EXIT
      ENDIF
      ? MENSAG('Quantidade n„o pode ser Zeros - ENTER')
    ENDDO

    MENS = DADCORR
    DO PEGRESP WITH [SN]
    IF MC = 'N'
      LOOP
     ENDIF

    IF MES = 'E'
      REPL SAL WITH (SAL + MQTD)
    ELSE
      REPL SAL WITH (SAL - MQTD)
    ENDIF

    IF MES = 'E'
      MTDOC = '04'
    ELSE
      MTDOC = '54'
    ENDIF

    SELECT MOD3
    APPE BLAN
    REPL REF WITH MREF,;
        TDOC WITH MTDOC,;
        DATA WITH DTOC(HOJE),;
        QTD  WITH MQTD,;
        CGC  WITH MCGC,;
        NDOC WITH MNDOC

    STOR LIN + 1 TO LIN
    IF LIN > 21
      @ 08,01 CLEAR TO 22,78
      STOR 8 TO LIN
    ENDIF
  ENDDO
ENDDO
RETURN
