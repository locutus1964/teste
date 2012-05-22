* AV08800 - LISTA A POSICAO FINANCEIRA DE CLIENTES COM PROBLEMAS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 02/03/87 - 10/07/92

DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF

* PREPARACAO DO ARQUIVO COM DUPLICATAS E VENCIMENTOS
  ? MENSAG('Selecionando duplicatas para emissao do relatorio...Aguarde')
  USE DUPL
  COPY TO CADWORK FOR OBS<>SPACE(40).OR.NFCLI > 0 .OR. ! empty(DTNFCLI)

  ? MENSAG('Classificando duplicatas para emissao do relatorio...Aguarde')

  USE CADWORK
  INDEX ON SUBSTR(VENC,7,2)+SUBSTR(VENC,4,2)+SUBSTR(VENC,1,2)+CGC TO CADWORK
  CLOS DATA

  MCGC   = SPACE(16)
  KEY2   = SPACE(08)
  MVENC  = SPACE(08)
  MRAZ   = SPACE(40)
  VALDIA = 0.00
  TVAL   = 0
  PAGCTR = 0
  LIN    = 0

  SELE B
    USE CLIEN ORDER P_CGC
  SELE A
    USE CADWORK INDEX CADWORK

    ? MENSAG('Coloque Papel de 132 Colunas na Impressora e ENTER')
    =INKEY(0)
    SET PRINT ON
    ?? CHR(30) + '3'
    ? MENSAG('Imprimindo Duplicatas com Problemas... Aguarde')
    set devi to print
*
    DO WHILE .NOT. EOF()
      PAGCTR = PAGCTR + 1
      ? CABEC('AV08800','DUPLICATAS C/PROBLEMAS - P/VENCIMENTO',131)
      @ 3,1 SAY 'VENCIMEN'
      @ 3,10 SAY 'RAZAO SOCIAL DO CLIENTE'
      @ 3,49 SAY 'DUPLICATA'
      @ 3,59 SAY 'EMISSAO'
      @ 3,68 SAY 'VALOR DUPLIC'
      @ 3,81 SAY ' VAL DO DIA'
      @ 3,95 SAY 'ACUM ATE DIA'
      @ 3,111 SAY 'OBSERVACOES'
      @ 3,152 SAY 'NF DEV'
      @ 3,159 SAY 'VAL DEVOLUC'
      @ 3,173 SAY 'DUP DEV'
        LIN = 5

      DO WHILE .NOT. EOF() .AND. LIN < 62
        SELE A

        IF PAG <> SPACE(8) .OR. !EMPTY(DTCANCEL)
          SKIP
          LOOP
        ENDIF

        IF VENC <> KEY2 .OR. LIN = 5
          @ LIN,1 SAY VENC
          KEY2 = VENC
        ELSE
          @ LIN,1 SAY '        '
        ENDIF

        MCGC = CGC
        SELE B
        SEEK MCGC

        IF !(EOF() .OR. BOF())
          @ LIN,10 SAY RAZ
        ENDIF

        SELE A
          @ LIN,49 SAY NDUP
          @ LIN,59 SAY EMIS
          @ LIN,67 SAY VDP PICT '999,999,999.99'
          TVAL = (TVAL + VDP - VALDEV)
          VALDIA = (VALDIA + VDP - VALDEV)
          MOBS   = OBS
          MNFCLI = NFCLI
          MDUPDEV= DUPDEV
          SKIP

          IF VENC <> KEY2 .OR. EOF()
            @ LIN,81 SAY VALDIA PICT '999,999,999.99'
            @ LIN,95 SAY TVAL   PICT '999,999,999.99'
            VALDIA = 0.00
            TESTE  = 1
          ELSE
            TESTE  = 0
          ENDIF

          @ LIN,111 SAY MOBS
          @ LIN,152 SAY MNFCLI

          IF VALDEV > 0
            @ LIN,159 SAY VALDEV PICT '999,999,999.99'
          ENDIF

          @ LIN,173 SAY MDUPDEV

          IF TESTE > 0
            LIN = LIN + 2
          ELSE
            LIN = LIN + 1
          ENDIF

          LOOP
      ENDDO
    EJECT
  ENDDO

SET PRINT OFF
SET DEVI TO SCRE
CLOS DATA
ERASE CADWORK.DBF
CLOSE DATA
RETURN

