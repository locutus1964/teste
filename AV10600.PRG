* AV10600 - IMPRESSAO DO DEMONSTRATIVO DO ESTOQUE
* SISTEMA ADMINISTRATIVO DE VENDAS
* ADAO ELI PEREIRA - 04/08/90
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
    
MENS = 'J  selecionou movimentos p/Demonstrativo ?'
DO PEGRESP WITH [SN]

  IF MC = 'N'
    ? MENSAG('ENTER - Para voltar ao Menu de Sele‡„o')
    =INKEY(0)
  ENDIF
*
CLOS DATA
*
SELE A
  USE PROD INDEX PROD.IDX
SELE B
  USE SALDOS INDE SALDOS.IDX
SELE C
  USE CADWORK4 INDE CADWORK4.IDX
*
PAGCTR = 0
TAMREL = 2
*
? MENSAG('Imprimindo Demonstrativo da Situa‡„o do Estoque... Aguarde')
*
DO DEFSAIDA

SELE A
DO WHILE !EOF()

  ? CABEC('AV10600','DEMONSTRATIVO DA SITUACAO DO ESTOQUE',131)
  @ 3,1 SAY 'CODIGO / DECRICAO'
  @ 3,025 SAY '   SALDO'
  @ 3,036 SAY '  E N T R A D A S   N O   E S T O Q U E'
  @ 3,080 SAY '   S A I D A S    D O    E S T O Q U E'
  @ 3,124 SAY '   SALDO'
  @ 4,025 SAY ' INICIAL'
  @ 4,034 SAY ' COM/PRO'
  @ 4,043 SAY ' AJUSTES'
  @ 4,052 SAY ' CANC NF'
  @ 4,061 SAY 'NF DEVOL'
  @ 4,070 SAY '   TOTAL'
  @ 4,079 SAY ' EXT ENT'
  @ 4,088 SAY ' AJUSTES'
  @ 4,097 SAY 'ENTREGAS'
  @ 4,106 SAY 'CANC DEV'
  @ 4,115 SAY '   TOTAL'
  @ 4,124 SAY '   FINAL'
  @ 5,1   SAY REPLICATE('-',131)
  STOR SPACE(1) TO MCOD
  STOR 5 TO LIN
*
  DO WHILE !EOF() .AND. LIN < 62
    STOR 0 TO PRODUC,EAJUSTE,EDEVOL,ECANC
    STOR 0 TO SAJUSTE,SENTRADA,SENTREGA,SCANC
    STOR REF TO MREF
    STOR DES TO MDES
    STOR SAL TO MSAL,MSALF
    
    IF LIN = 5 .OR. REF <> MCOD
      STOR REF TO MCOD
      STOR LIN + 1 TO LIN
    ENDIF
    
    SELE C
    SEEK SUBS(MREF,1,11)
    
    IF !(EOF() .OR. BOF())
      STOR 0 TO MSALI
      
      DO WHILE SUBS(KEYMOD3,1,11) = MREF .AND. !EOF()
        STOR SUBS(KEYMOD3,12,2) TO TDOC
        IF TDOC = '01'
          STOR (PRODUC+QTD) TO PRODUC
        ENDIF

        IF TDOC = '02'
          STOR (EAJUSTE+QTD) TO EAJUSTE
        ENDIF

        IF TDOC = '03'
          STOR (ECANC + QTD) TO ECANC
        ENDIF

        IF TDOC = '04'
          STOR (EDEVOL + QTD) TO EDEVOL
        ENDIF

        IF TDOC = '51'
          STOR (SENTRADA +QTD) TO SENTRADA
        ENDIF

        IF TDOC = '52'
          STOR (SAJUSTE+QTD) TO SAJUSTE
        ENDIF

        IF TDOC = '53'
          STOR (SENTREGA + QTD) TO SENTREGA
        ENDIF

        IF TDOC = '54'
          STOR (SCANC + QTD) TO SCANC
        ENDIF

        IF VAL(TDOC) < 51
          STOR (MSAL - QTD) TO MSAL
        ELSE
          STOR (MSAL + QTD) TO MSAL
        ENDIF
        SKIP
      ENDDO
      
      @ LIN,1   SAY MREF
      @ LIN,13  SAY SUBS(MDES,1,38)
      STOR LIN + 1 TO LIN
      STOR (PRODUC+EAJUSTE+EDEVOL+ECANC) TO ENTRADAS
      STOR (SAJUSTE+SENTRADA+SENTREGA+SCANC) TO SAIDAS
      @ LIN,025 SAY STR(MSAL,8)
      @ LIN,034 SAY STR(PRODUC,8)
      @ LIN,043 SAY STR(EAJUSTE,8)
      @ LIN,052 SAY STR(ECANC,8)
      @ LIN,061 SAY STR(EDEVOL,8)
      @ LIN,070 SAY STR(ENTRADAS,8)
      @ LIN,079 SAY STR(SENTRADA,8)
      @ LIN,088 SAY STR(SAJUSTE,8)
      @ LIN,097 SAY STR(SENTREGA,8)
      @ LIN,106 SAY STR(SCANC,8)
      @ LIN,115 SAY STR(SAIDAS,8)
      @ LIN,124 SAY STR(MSALF,8)
      STOR LIN + 1 TO LIN
    ELSE
      @ LIN,1   SAY MREF
      @ LIN,13  SAY MDES
        STOR LIN + 1 TO LIN
      @ LIN,025 SAY STR(MSAL,8)
      @ LIN,124 SAY STR(MSAL,8)
        STOR LIN + 1 TO LIN
    ENDIF
    
    SELE A
    SKIP
      IF EOF()
        @ LIN,1 SAY REPLICATE('-',131)
        @ LIN+1,10 SAY "F I N A L   D E S T E   R E L A T O'R I O"
      ENDIF
  ENDDO
ENDDO
*
DO IMPSAIDA

RETURN
