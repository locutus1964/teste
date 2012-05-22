* AV06300 - EMISSAO DE LISTAGEM DAS PREVIAS DOS PEDIDOS DE CLIENTES
* SISTEMA ADMINISTRACAO DE VENDAS - 26/11/86 - 06/12/91
* ADAO ELI PEREIRA
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
NMPED = 0
NUPED = 0
MPED  = 0
UPED  = 0
*
MENS = 'Imprime Relat¢rio Total (T) ou Parcial (P)? '
DO PEGRESP WITH [TP]
*
PC = MC
IF MC = 'P'
  ? JANELA(18,33,21,77)
  ? BORDA(18,33,21,77)
*
  USE PEDIC ORDER P_PED
  DO WHILE .T.

    @ 19,35 SAY 'Informe n£mero do primeiro Pedido ' get mped pict '999999'
      READ
        IF MPED = 0
          ? MENSAG(CNUMER)
          LOOP
        ENDIF
          SEEK MPED
            IF (EOF() .OR. BOF())
              ? MENSAG(CORRIJA)
              =INKEY(0)
              LOOP
            ENDIF
              uped = 0
                @ 20,35 SAY 'Informe £ltimo Pedido a imprimir  ' get uped pict '999999'
                  read
                    MENS = DADCORR
                    DO PEGRESP WITH [SN]
                      IF MC = 'N'
                        CLEAR
                        CLOS DATA
                        RETURN
                      ELSE
                        EXIT
                      ENDIF
  ENDDO
    CLOS DATA
ENDIF
*
IF PC = 'P'
  USE PEDIC
  COPY TO CADWORK FOR PED >= MPED .AND. PED <= UPED
  USE CADWORK
  INDEX ON PED TO CADWORK.IDX
  CLOS DATA
  SELE A
  USE CADWORK INDEX CADWORK.IDX
ELSE
  SELE A
  USE PEDIC ORDER P_PED
  UPED = 999999
ENDIF
*
? MENSAG('Imprimindo Pr‚vias de Pedidos... Aguarde')
*
key = 0
PAGCTR = 0
TAMREL = 2
*
DO DEFSAIDA

  SELE 0
  USE PEDID ORDER P_PEDID
  SELE 0
  USE CLIEN ORDER P_CGC
  SELE A
*
DO WHILE !EOF() .AND. PED <= UPED
*
  ? CABEC('AV06300','PREVIA/LISTA DOS PEDIDOS CADASTRADOS',131)
  @ 3,001 SAY 'PEDIDO'
  @ 3,009 SAY 'COD/LOJA CLIENTE'
  @ 3,031 SAY 'R.SOCIAL'
  @ 3,054 SAY 'VEN'
  @ 3,058 SAY 'TRA'
  @ 3,062 SAY 'D.GER'
  @ 3,071 SAY ' DESCONTO'
  @ 3,084 SAY 'VAL.FRETE'
  @ 3,098 SAY 'EMBALAGEM'
  @ 3,108 SAY 'CONDIC. DE PAGTO'
  @ 3,128 SAY 'TPNF'

  @ 4,032 SAY 'ITEM'
  @ 4,038 SAY 'REFERENCIA'
  @ 4,049 SAY 'QUANTIDADE'
  @ 4,060 SAY '     SALDO'
  @ 4,071 SAY 'PRECO UNITAR'
  @ 4,085 SAY 'D.ITE'
  @ 4,091 SAY 'BONUS'
  @ 4,097 SAY 'TAB'
  @ 4,101 SAY 'ENTREGA'
  @ 4,110 SAY 'N FISC'
  @ 5,1   SAY REPLICATE('-',131)
  LIN = 6
*
  DO WHILE !EOF() .AND. LIN < 59 .AND. PED <= UPED
    STOR PED TO KEY
    @ LIN,1 SAY STR(PED,6)
    @ LIN,9 SAY CGC
    STOR CGC TO MCGC
    SELE CLIEN
    SEEK MCGC
      IF !(EOF() .OR. BOF())
        @ LIN,31 SAY SUBS(RAZ,1,23)
      ENDIF
    SELE A
    @ LIN,054 SAY VEN
    @ LIN,058 SAY TRA 
    @ LIN,068 SAY DESCONTO PICT '99999,999.99'
    @ LIN,081 SAY FRE PICT '99999,999.99'
    @ LIN,095 SAY EMB PICT '99999,999.99'
    @ LIN,108 SAY CP1
    @ LIN,112 SAY CP2
    @ LIN,116 SAY CP3
    @ LIN,120 SAY CP4
    @ LIN,124 SAY CPD
    @ LIN,129 SAY TIP
    LIN = LIN + 1
    
    SELE PEDID
    SEEK STR(KEY,6)
    IF (EOF() .OR. BOF())
      LIN = LIN + 1
    ENDIF
      
    DO WHILE PED = KEY .AND. !EOF() .AND. LIN < 59
      @ LIN,033 SAY STR(ITE,2)
      @ LIN,037 SAY REF
      @ LIN,049 SAY QTD PICT '99,999,999'
      @ LIN,060 SAY SAL PICT '99,999,999'
      @ LIN,071 SAY PRE PICT '99,999,999.99'
      @ LIN,085 SAY STR(DIT,5,2)
      @ LIN,091 SAY STR(BON,5,2)
      @ LIN,110 SAY STR(NNF,6)
      LIN = LIN + 1
      IF LIN > 58
        LIN = 65
*
        ? CABEC('AV06300','PREVIA/LISTA DOS PEDIDOS CADASTRADOS',131)
        @ 3,1 SAY 'PEDIDO'
        @ 3,9 SAY 'CGC DO CLIENTE'
        @ 3,31 SAY 'RAZ SOCIAL'
        @ 3,42 SAY 'VEND'
        @ 3,48 SAY 'P INT'
        @ 3,56 SAY 'PED CLIENTE'
        @ 3,71 SAY 'TRA'
        @ 3,76 SAY 'DESCTO'
        @ 3,86 SAY 'FRETE'
        @ 3,94 SAY 'COND. PAGTO'
*        @ 3,114 SAY 'PR'
        @ 3,122 SAY 'TPNF'
        STOR LIN + 1 TO LIN
        @ 4,034 SAY 'ITE'
        @ 4,041 SAY 'REFERENC'
        @ 4,053 SAY 'QUANT'
        @ 4,061 SAY 'SALDO'
        @ 4,069 SAY 'PR.UNIT'
        @ 4,078 SAY 'TAB'
        @ 4,083 SAY 'D.ITE'
        @ 4,090 SAY 'BONUS'
        @ 4,097 SAY 'EMBAL'
        @ 4,110 SAY 'ENTREGA'
        @ 4,119 SAY 'N FISC'
        @ 5,1   SAY REPLICATE('-',130)
        LIN = 6
*
        @ LIN,1 SAY STR(PED,6)
        @ LIN,9 SAY CGC
        STOR CGC TO MCGC
        
        SELE CLIEN
        SEEK MCGC
        IF .NOT. (EOF() .OR. BOF())
          @ LIN,31 SAY GUE
        ENDIF
        SELE A
        @ LIN,43 SAY VEN
        @ LIN,48 SAY PINT
        @ LIN,56 SAY PCL
        @ LIN,71 SAY TRA
        @ LIN,86 SAY STR(FRE,5,2)
        @ LIN,94 SAY CP1
        @ LIN,98 SAY CP2
        @ LIN,102 SAY CP3
        @ LIN,106 SAY CP4
        @ LIN,110 SAY CPD
*        @ LIN,114 SAY PRI
        @ LIN,118 SAY AUT
        @ LIN,122 SAY TIP
*        @ LIN,127 SAY CONT
        STOR LIN + 1 TO LIN
        SELE PEDID
      ENDIF
      SKIP
      IF PED <> KEY .OR. EOF()
        IF .NOT. EOF()
           STOR LIN + 1 TO LIN
        ENDIF
        EXIT
      ELSE
        LOOP
      ENDIF
  ENDDO
SELE A
  SKIP
    IF EOF()
      STOR LIN + 1 TO LIN
        @ LIN,1 SAY REPLICATE('-',130)
        @ LIN+1,10 SAY 'F  I  N  A  L     D  E  S  T  E     R  E  L  A  T  O  R  I  O'
    ENDIF
*
ENDDO
ENDDO

DO IMPSAIDA

IF FILE("CADWORK.DBF")
  ERASE CADWORK.DBF
ENDIF
RETURN
