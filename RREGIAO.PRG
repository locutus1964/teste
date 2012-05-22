* RREGIAO - RELACAO DE REGIOES
* ADAO ELI PEREIRA - 10/07/93
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
USE REGIAO INDE REGIAO.IDX

pagctr = 0
tamrel = 1
*
? MENSAG('Imprimindo Relat¢rio das Regi”es cadastradas... Aguarde')

DO DEFSAIDA
*
do while .not. EOF()
  ? CABEC80('RREGIAO', 'REGIOES DE CLIENTES',78)
*
  @ 04,01 say 'REGIAO'
  @ 04,13 SAY 'DESCRICAO'
  @ 04,45 SAY 'REPRESENTANTE'
  @ 05,1 SAY REPLICATE('-',78)
*
  lin = 6
  do while .not. EOF() .and. lin < 64
    @ LIN,02 SAY FmtRegiao(REGIAO, IDREGIAO)
    @ LIN,13 SAY DREGIAO
    @ LIN,45 SAY REPRES
    lin = lin + 1
      skip
        if eof()
          @ lin,1 say replicate('-',78)
          @ lin+1,10 say 'F I N A L     D E S T E     R E L A T O R I O'
        endif
  enddo
enddo

DO IMPSAIDA

return
