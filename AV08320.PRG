* CLIBANCO - CALCULA A POSICAO BANCOS POR CLIENTES
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 14/09/92 - FOX 02/04/94
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
@ 15,42 clear to 21,78
@ 15,42 to 21,78 double
@ 17,45 SAY "Data Inicial :"
@ 18,45 SAY "Data Final :"
DTINI = {}
DTFIM = {}
@ 17,58 GET DTINI 
@ 18,58 GET DTFIM 
READ

CLOS DATA
MB      = ' '
MNBANCO = 0
MGUE    = SPACE(15)
MCGC    = SPACE(16)
MENS = 'Seleciona Cliente ?'
DO PEGRESP WITH [SN]
  MB = MC
  IF MB = 'S'
    USE CLIEN ORDER X_GUE
    @ 19,45 SAY 'Nome de Guerra 'GET MGUE PICT '@!'
      READ
      IF SEEK(MGUE)
        MCGC = CGC
        @ 23,20 SAY RAZ
      ELSE
        ? MENSAG('Cliente n„o cadastrado - ENTER')
        =INKEY(0)  
        RETURN
      ENDIF
  ENDIF

* PREPARACAO DO ARQUIVO COM DUPLICATAS E VENCIMENTOS
  ? MENSAG('Classificando duplicatas para emissao do relatorio...Aguarde')

  USE DUPL
  IF MB = 'N'
    COPY TO CADWORK FOR VENC >= DTINI .AND. VENC <= DTFIM .AND. empty(PAG) .AND. EMPTY(DTCANCEL)
  ELSE
    COPY TO CADWORK FOR VENC >= DTINI .AND. VENC <= DTFIM .AND. empty(PAG) .AND. empty(DTCANCEL) .AND. CGC = MCGC
  ENDIF

  USE CADWORK
  INDEX ON CGC + DTOS(VENC) + CGC TO POSFIN.IDX
  CLOSE DATABASES

  MCGC    = SPACE(16)
  MNBANCO = 0
  MAGENCIA = 0
  KEY2    = {}
  MVENC   = {}
  MRAZ    = SPACE(30)
  VALDEV  = 0.00
  VALGER  = 0.00
  VALDIA  = 0.00
  VALBCO  = 0.00
  TVAL    = 0
  TESTE   = 0
  TDESCTO = 0

  SELE 0
    USE BANCOS INDEX BANCOS.IDX
  SELE 0
    USE CLIEN ORDER P_CGC
  SELE 0
    USE CADWORK INDEX POSFIN.IDX

    PAGCTR = 0
    TAMREL = 2
    
    ? MENSAG('Imprimindo Posicao de Clientes por Banco... Aguarde')

    DO DEFSAIDA
    
    DO WHILE !EOF()
      TITULO = 'POSICAO DE BANCOS POR CLIENTE - DE '+ dtoc(DTINI) +' ATE '+ dtoc(DTFIM)
      ? CABEC('CLIBANCO',TITULO,131)

      @ 3,1 SAY 'VENCIMEN   B A N C O'
      @ 3,55 SAY 'DUPLICATA'
      @ 3,67 SAY ' EMISSAO'
      @ 3,80 SAY ' VALOR DUPLIC'
      @ 3,98 SAY ' VALOR ACUMULADO'
      @ 3,117 SAY '  ACUMUL DO DIA'
        LIN = 5

      DO WHILE !EOF() .AND. LIN < 60

        IF LIN = 5 .OR. CGC <> MCGC
          IF LIN <> 5
            LIN = LIN + 1
          ENDIF

          @ LIN,1 SAY CGC
            MCGC     = CGC
            MNBANCO  = NBANCO
            MAGENCIA = AGENCIA

            SELE CLIEN
              SEEK MCGC
                IF !(EOF() .OR. BOF())
                  @ LIN,22 SAY RAZ
                ELSE
                  @ LIN,22 SAY 'CLIENTE NAO CADASTRADO'
                ENDIF
          LIN = LIN + 2
        ENDIF

      SELE CADWORK

        IF ! empty(DTCANCEL) .OR. !empty(PAG)
          SKIP
          LOOP
        ENDIF

        @ LIN,001 SAY VENC
          MVENC    = VENC
          MNBANCO  = NBANCO
          MAGENCIA = AGENCIA
          SELE BANCOS
          IF SEEK(STR(MNBANCO,3) + STR(MAGENCIA,4))
            @ LIN,12 SAY DBANCO
          ELSE
            @ LIN,12 SAY 'NAO CADASTRADO'
          ENDIF

          SELE CADWORK
            @ LIN,55 SAY NDUP

            IF !empty(DTDESCTO) 
              STOR (TDESCTO + VDP) TO TDESCTO
              @ LIN,64 SAY 'D'
              @ LIN,64 SAY 'D'
            ENDIF

            @ LIN,67 SAY EMIS
            @ LIN,78 SAY VDP PICT '9,999,999,999.99'


            STOR (TVAL + VDP - (VALDEV + DESC)) TO TVAL
            STOR (VALDIA + VDP - (VALDEV + DESC)) TO VALDIA
            STOR (VALBCO + VDP - (VALDEV + DESC)) TO VALBCO
            STOR (VALGER + VDP - (VALDEV + DESC)) TO VALGER

              IF VALDEV > 0
                LIN = LIN + 1
                @ LIN,55 SAY 'VALOR DEVOLUCAO'
                @ LIN,78 SAY VALDEV PICT '9,999,999,999.99'
              ENDIF

              IF DESC > 0
                LIN = LIN + 1
                @ LIN,55 SAY 'VALOR DESCONTO'
                @ LIN,78 SAY DESC PICT '9,999,999,999.99'
              ENDIF

            @ LIN,97 SAY TVAL PICT '999,999,999,999.99'
              SKIP

              IF VENC <> MVENC
                @ LIN,115 SAY VALDIA PICT '99,999,999,999.99'
                STOR 0.00 TO VALDIA
                TESTE = 1
              ELSE
                TESTE = 0
              ENDIF

              IF CGC <> MCGC .OR. EOF()
                LIN = LIN + 2
                @ LIN,70 SAY 'TOTAL DO CLIENTE --------------->'
                @ LIN,115 SAY VALBCO PICT '99,999,999,999.99'

                IF EOF()
                  LIN = LIN + 1
                  @ LIN,75 SAY 'TOTAL GERAL --------------->'
                  @ LIN,115 SAY VALGER PICT '99,999,999,999.99'
                  LIN = LIN + 1
                  @ LIN,80 SAY 'DUPLICATAS DESCONTADAS --->'
                  @ LIN,115 SAY TDESCTO PICT '99,999,999,999.99'
                  LIN = LIN + 1
                  @ LIN,85 SAY 'TOTAL LIQUIDO ------------>'
                  @ LIN,115 SAY (VALGER - TDESCTO) PICT '99,999,999,999.99'
                ENDIF

                VALDEV = 0.00
                VALDIA = 0.00
                VALBCO = 0.00
                
              ENDIF

                LIN = LIN + 1
            LOOP
   ENDDO
ENDDO

DO IMPSAIDA

RETURN

