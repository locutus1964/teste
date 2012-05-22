* AV07423 - IMPRIME OS PEDIDOS COM PENDENCIAS - 21/01/87
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA   -   09/12/91 - 20/10/92
*
do TESTIMP
do SELCOR
  IF CP = 'N'
    RETURN
  ENDIF
*
? CABTELA(0,0,24,79)
  MENS = 'Aprova‡„o de Pedidos selecionados'
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
SELE A
  USE CLIEN ORDER P_CGC
SELE B
  use PROD index PROD
SELE C
  use APROVP index APROVP
*
  IF BOF()
    ? MENSAG('N„o existem Pedidos Pendentes - ENTER p/continuar')
    =INKEY(0)
    clos data
    return
  endif
*
  pagctr = 0
  ? MENSAG('Imprimindo Relat¢rio Situa‡„o dos Pedidos Pendentes...Aguarde')
  set DEVICE to print
  set print on
*
  do while .not. EOF()
*
    ? CABEC('AV07423','SITUACAO DOS PEDIDOS PENDENTES P/EMISSAO DE NF',130)
*
    @ 3,001 say '    CGC '
    @ 3,022 say 'CLIENTE'
    @ 3,034 say 'PEDIDO'
    @ 3,042 say 'REFERENCIA'
    @ 3,054 say 'DESCRICAO'
    @ 3,094 say 'QUANTIDADE'
    @ 3,108 say 'PENDENCIAS'
    @ 4,001 SAY REPLICATE('-',130)
*
    lin = 5
    do while !EOF() .and. lin < 56
*
      IF CLIE = ' ' .AND. PROD = ' '
        SKIP
        LOOP
      ENDIF
*
*     PEDIDO COM ATENDIMENTO PARCIAL/EXCLUIDOS
*
      @ lin,01 say CGC
      MCGC = CGC

      SELE A
      SEEK MCGC
        IF !(EOF() .OR. BOF())
          @ LIN,22 SAY GUE
        ENDIF

      SELE C
      @ lin,34 say PED
      @ lin,42 say REF
      MREF = REF

      SELE B
      SEEK MREF
        IF !(EOF() .OR. BOF())
          @ LIN,54 SAY DES
        ENDIF

      SELE C
*
*     SALE = SALDO EXISTENTE NO ESTOQUE QUANDO ATENDIMENTO PARCIAL'
*
      IF SALE > 0
        @ LIN,96 SAY SALE PICT '99,999,999'
      ELSE
        @ LIN,96 SAY QTD  PICT '99,999,999'
      ENDIF
*
      IF CLIE = '1' .OR. CLIE = '2' .OR. CLIE = '9'
        IF CLIE = '1'
          @ LIN,108 SAY 'S/CREDITO'
        ELSE
          IF CLIE = '2'
            @ LIN,108 SAY 'S/CADASTRO'
          ELSE
            @ LIN,108 SAY 'VALOR EXCED'
          ENDIF
        ENDIF
      ENDIF
*
      IF PROD = '1' .OR. PROD = '2' .OR. PROD = '9'
        IF PROD = '1'
          @ LIN,120 SAY 'S/SALDO'
        ELSE
          IF PROD = '2'
            @ LIN,120 SAY 'S/CADASTRO'
          ELSE
            @ LIN,120 SAY 'ATEND.PARC'
          ENDIF
        ENDIF
      ENDIF
*
      IF CLIE <> ' ' .OR. PROD = '1' .OR. PROD = '2'
        DELETE
      ENDIF
*
      LIN = LIN + 1
      skip
    enddo

    eject
  enddo

set print off
set DEVI to scre
close databases
return





