* AV05995 - LISTA AS TABELAS DE PRECOS CADASTRADAS (POR CODIGO)
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 03/07/90 11/09/93
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF

CLOS DATA
MTABREF = SPACE(5)
*
  ? JANELA(07,39,11,78)
  ? BORDA(07,39,11,78)
*
  USE TABREF INDE TABREF.IDX
  @ 09,42 SAY 'Informe Tabela a Imprimir 'GET MTABREF PICT '99/99'
    READ

    MENS = DADCORR
    DO PEGRESP WITH [SN]
    IF MC = 'S'
      USE TABREF2
      ZAP
      APPE FROM TABREF FOR TABREF = MTABREF
    ENDIF
    CLOS DATA

    SELE 0
      USE TABREF2
    SELE 0
      USE PROD INDE PROD.IDX

    SELE TABREF2
    DO WHILE !EOF()
      MREF = REF
      SELE PROD
      IF SEEK(MREF)
        MDES       = DES
        MTIPO      = TIPO
        MGRUPO     = GRUPO
        MCONTEUDO  = CONTEUDO
        MUNI       = UNI
        MIPI       = IPI
        MEMBALAGEM = EMBALAGEM
        MLPR       = LPR
        SELE TABREF2
        REPLACE DES WITH MDES
        REPLACE TIPO WITH MTIPO
        REPLACE GRUPO WITH MGRUPO
        REPLACE CONTEUDO WITH MCONTEUDO
        REPLACE UNI      WITH MUNI
        REPLACE IPI      WITH MIPI
        REPLACE EMBALAGEM WITH MEMBALAGEM
        REPLACE LPR      WITH MLPR
      ENDIF
      SELE TABREF2      
      SKIP
    ENDDO

    USE TABREF2
    INDEX ON LPR + REF + EMBALAGEM TO CADWORK.IDX
    CLOS DATA

SELE 0
  USE TABREF2  INDE CADWORK.IDX
SELE 0
  USE LIMPROD  INDE LIMPROD.IDX

? MENSAG('Imprimindo a Tabela de Pre‡os... Aguarde')
*
pagctr = 0
tamrel = 2

DO DEFSAIDA

SELE TABREF2
do while !EOF()
  MLPR = LPR
    SELE LIMPROD
    IF SEEK(MLPR)
      TIT = 'LINHA DE PRODUTO '+MLPR+' - '+DLPR
    ELSE
      TIT = 'LINHA NAO CADASTRADA'+ ' - ' + MLPR
    ENDIF
*
    PAGCTR = PAGCTR + 1
    @ 01,01 SAY MREL
    @ 01,30 SAY TIT
    @ 01,112 SAY HOJE
    @ 01,124 SAY 'PAG '+STR(PAGCTR,3)
    @ 02,001 SAY 'AV05995'
    @ 02,030 SAY 'TABELA DE PRECOS '+MTABREF
    @ 03,01 SAY REPLICATE('-',131)


  @ 4,01 say 'DESCRICAO DO PRODUTO'
  @ 4,56 say 'TIPO'
  @ 4,68 say 'GR'
  @ 4,72 say 'REF'
  @ 4,78 say 'CONTEUD'
  @ 4,87 say 'UNI'
  @ 4,92 say 'EMB'
  @ 4,97 say 'PRECO UNITARIO'
  @ 4,112 say '%IPI'
  @ 5,1  SAY REPLICATE('-',131)
*
  stor 6 to lin
  sele TABREF2
  do while .not. EOF() .and. lin < 64
      @ lin,01 say DES
      @ lin,56 say TIPO
      @ lin,68 say GRUPO
      @ lin,72 say REF
      @ lin,78 say CONTEUDO
      @ lin,87 say UNI
      @ LIN,92 say EMBALAGEM
      @ LIN,97 SAY PRECO PICT '999,999.99'
      @ LIN,112 SAY IPI PICT '99.9'
      MLPR = LPR

      stor lin + 1 to lin
      skip
      IF LPR <> MLPR .AND. !EOF()
        lin = 70
      ENDIF

      IF EOF()
        @ LIN,1 SAY REPLICATE('-',131)
        @ LIN+1,10 SAY 'F I M     D E S T E     R E L A T O R I O'
      ENDIF
  enddo
enddo

DO IMPSAIDA

IF FILE("CADWORK.DBF")
  ERASE CADWORK.DBF
ENDIF
return
