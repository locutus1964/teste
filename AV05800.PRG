* AV05800 - CONSULTA/IMPRESSAO DO RAZAO DO ESTOQUE - LIVRO MODELO 3
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 15-04-90
*
MVALIT = 0.00
MENS = 'Consulta Tela por Item (C) ou imprime Relat¢rio (R) '
DO PEGRESP WITH [CR]
  IF MC = 'C'
    DO AV05810
    RETURN
  ENDIF
*
CLOS DATA
SELE 0
  USE PROD INDE PROD.IDX
SELE 0
  USE SALDOS INDE SALDOS.IDX
SELE 0
  USE MOD3 ORDER X_DATA
*
MVALIT  = 0.00     && VALOR DO SALDO INICIAL * PRECO UNITARIO
MVALI   = 0.00     && VALOR FINAL DO ITEM
TVALENT = 0.00     && VALOR DAS ENTRADAS * PRECO
TVALSAI = 0.00     && VALOR DAS SAIDAS * PRECO
MVALENT = 0.00     && ACUMULADO DO VALOR DAS ENTRADAS
MVALSAI = 0.00     && ACUMULADO DO VALOR DAS SAIDAS
MVALFIN = 0.00     && ACUMULADO DO VALOR FINAL
PAGCTR  = 0        && CONTROLE DE PAGINA
MSALI   = 0        && SALDO INICIAL DO MOD3
MESREF  = SPACE(05)     && MES/ANO DE REFERENCIA
TAMREL  = 2
*
? JANELA(08,40,11,78)
? BORDA(08,40,11,78)
*
MC = ' '
@ 09,42 SAY 'Por C¢digo (C) ou Ordem Alfa (A)? 'GET MC PICT '!' VALID(MC$'AC')
  READ

  @ 10,42 SAY 'Informe MES/ANO de referˆncia 'get mesref pict '99/99'
    READ
        MENS = DADCORR
        DO PEGRESP WITH [SN]
          IF MC = 'N'
            RETURN
          ENDIF
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
TITULO = 'RAZAO GERAL DO ESTOQUE - LIVRO MODELO 3 - REF '+MESREF
*
? MENSAG('Imprimindo o Livro Modelo 3... Aguarde')
*
DO DEFSAIDA

SELE PROD

  DO WHILE .NOT. EOF()
    ? CABEC('AV05800',TITULO,131)
    @ 3,1   SAY 'REF'
    @ 3,06  SAY 'DECRICAO DO PRODUTO'
    @ 3,53  SAY 'UN'
    @ 3,56  SAY 'SALDO INIC'
    @ 3,68  SAY 'TRANSAC'
    @ 3,78  SAY 'NR.DOC'
    @ 3,88  SAY 'DATA'
    @ 3,95  SAY 'QTD ENTRADA'
    @ 3,108 SAY 'QTD SAIDA'
    @ 3,120 SAY 'SAL FINAL'
    @ 4,1   SAY REPLICATE('-',131)
      LIN = 5
*
      DO WHILE !EOF() .AND. LIN < 56
        MREF = REF
        @ LIN,1   SAY MREF
        @ LIN,06  SAY SUBS(DES,1,45)
        @ LIN,53  SAY UNI
          SELE SALDOS
            MSALI = 0
            SEEK MREF
            IF (EOF() .OR. BOF())
              MSALI = 0
            ELSE
              MSALI = SALI
            ENDIF
        @ LIN,56 SAY STR(MSALI,10,2)
*
          SELE MOD3
            SEEK MREF
              IF (EOF() .OR. BOF())
                @ LIN,119 SAY STR(MSALI,10,2)
                LIN = LIN + 1
              ELSE
                DO WHILE MREF = REF
                  IF TDOC = '01'
                    TRANS = 'COMP/PRD'
                  ENDIF

                  IF TDOC = '51'
                    TRANS = 'EST.ENTR'
                  ENDIF

                  IF TDOC = '02' .OR. TDOC = '52'
                    TRANS = 'AJUSTE'
                  ENDIF

                  IF TDOC = '03'
                    TRANS = 'CANC ENTR'
                  ENDIF

                  IF TDOC = '53'
                    TRANS = 'ENTREGA'
                  ENDIF

                  IF TDOC = '04'
                    TRANS = 'NF DEVOL'
                  ENDIF

                  IF TDOC = '54'
                    TRANS = 'CC NF DEV'
                  ENDIF

                  @ LIN,68  SAY TRANS
                  @ LIN,78  SAY NDOC
                  @ LIN,86  SAY DATA
                    IF VAL(TDOC) < 51
                      @ LIN,95 SAY QTD PICT '9,999,999.99'
                      STOR (MSALI + QTD) TO MSALI
                    ELSE
                      @ LIN,107 SAY QTD PICT '9,999,999.99'   
                      STOR (MSALI - QTD) TO MSALI
                    ENDIF
                      SKIP
                        IF REF <> MREF
                          @ LIN,119 SAY MSALI PICT '9,999,999.99'
                          SELE SALDOS
                            IF (EOF() .OR. BOF())
                              APPE BLAN
                                REPL REF WITH MREF
                                REPL SALF WITH MSALI
                            ELSE
                              REPL SALF WITH MSALI
                            ENDIF
                          SELE MOD3
                            MSALI = 0
                        ENDIF
                        
                        LIN = LIN + 1
                          
                        IF EOF()
                          @ LIN,1 SAY REPLICATE('-',130)
                          @ LIN+1,10 SAY 'F I N A L    D E S T E   R E L A T O R I O'
                        ENDIF
                ENDDO
                LIN = LIN + 1
              ENDIF
                SELE PROD
        SKIP
      ENDDO
    ENDDO
*
DO IMPSAIDA
*
? CABTELA(0,0,24,79)
MENS = 'Modelo 3 - Posi‡„o ou Encerramento Mensal'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 06,10 SAY '              A  t  e  n  ‡  „  o'
@ 08,10 SAY 'Este programa emite a Posi‡„o do Estoque at‚ a data da impress„o'
@ 09,10 say 'e tambem processa o encerramento mensal  quando  reinicializa os'
@ 10,10 say 'saldos para o in¡cio do mes seguite.'
@ 12,10 say 'Portanto, se estiver encerrando o mes, e o mesmo estiver correto,'
@ 13,10 say 'responda (S) para processar o encerramento e complemente com (S)'
@ 14,10 say 'para indicar que tem certeza do encerramento do mes. '
@ 16,10 say 'Um erro de encerramento exigir  que seja recarregado  o  BACK-UP'
@ 17,10 say 'com os dados anteriores ao processamento.'

  MENS = 'Encerrou mes (S) ou foi somente Posicao (P)? '
  DO PEGRESP WITH [SP]
      IF MC = 'S'
        MENS = 'Tem certeza que Encerrou correto? (S/N) '
        DO PEGRESP WITH [SN]
            IF MC = 'S'
              MENS ='Preparando os saldos iniciais para o pr¢ximo mes... Aguarde'
              ? MENSAG(mens)
                DO AV05820
              ENDIF
      ENDIF
*
  CLOS DATA
  RETURN
