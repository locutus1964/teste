* AV05996 - LISTA OS CODIGOS DE PRECOS CADASTRADOS (POR TABELA)
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 03/07/90
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
STAB = SPACE(1)
MTAB = SPACE(3)
MC = SPACE(1)
STAB = ' '
*
  ? JANELA(07,39,12,76)
  ? BORDA(07,39,12,76)
*
  @ 09,42 SAY 'Seleciona Tabela? (S/N)     'GET STAB PICT '!'
    READ
    IF STAB = 'S'
      @ 10,42 SAY 'Informe Tabela a Selecionar 'GET MTAB
        READ
          MC = ' '
          ? MENSAG(DADCORR)
          @ 23,POS SAY DADCORR GET MC PICT '!'
            READ
              IF MC = 'S'
                USE CODPRE INDE CODPRE
                  COPY TO CADWORK FOR TAB = MTAB
              ENDIF
    ENDIF
*
IF STAB = 'S'
  USE CADWORK
ELSE
  USE CODPRE INDE CODPRE
ENDIF
*
? MENSAG('Imprimindo Tabela dos C¢digos de Pre‡os... Aguarde')
*
pagctr = 0
tamrel = 1
*
DO DEFSAIDA

do while !EOF()

  ? CABEC80('AV05996','TABELAS DOS CODIGOS DE PRECOS',80)

  @ 4,06 say 'TABELA'
  @ 4,30 say 'CODIGO '
  @ 4,52 say 'PRECO UNITAR'
  @ 5,1  SAY REPLICATE('-',80)
*
  stor 6 to linha
  do while .not. EOF() .and. linha < 56
    @ linha,10 say TAB
    @ linha,32 say CPR
    @ linha,51 say PRE PICT '99,999,999.99'
      stor linha + 1 to linha
      skip
      IF EOF()
        @ LINHA,1 SAY REPLICATE('-',80)
        @ LINHA+1,10 SAY 'F I M     D E S T E     R E L A T O R I O'
      ENDIF
  enddo
enddo

DO IMPSAIDA

IF FILE("CADWORK.DBF")
  ERASE CADWORK.DBF
ENDIF
return





