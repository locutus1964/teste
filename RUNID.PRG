* RUNID - RELACAO DAS UNIDADES
* ADAO ELI PEREIRA - 04/09/93
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
USE UNIDADES INDE UNIDADES.IDX

pagctr = 0
tamrel = 1
*
? MENSAG('Imprimindo Relat¢rio das Unidades... Aguarde')

DO DEFSAIDA
*
do while .not. EOF()
  ? CABEC80('RUNID', 'UNIDADES DE MEDIDA',78)
*
  @ 04,01 say 'UNIDADE'
  @ 04,13 SAY 'DESCRICAO'
  @ 05,1 SAY REPLICATE('-',78)
*
  lin = 6
  do while .not. EOF() .and. lin < 56
    @ LIN,03 SAY UNIDADE
    @ LIN,13 SAY DUNIDADE
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
