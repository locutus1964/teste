* RSTATUS - RELACAO DE STATUS
* ADAO ELI PEREIRA - 10/07/93
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
USE STATUS INDE STATUS.IDX

pagctr = 0
tamrel = 1
*
? MENSAG('Imprimindo Relat¢rio dos STATUS cadastrados... Aguarde')

DO DEFSAIDA
*
do while .not. EOF()
  ? CABEC80('RSTATUS', 'STATUS DE CLIENTES',78)
*
  @ 04,01 say 'STATUS'
  @ 04,10 SAY 'DESCRICAO'
  @ 05,1 SAY REPLICATE('-',78)
*
  lin = 6
  do while .not. EOF() .and. lin < 56
    @ LIN,05 SAY STATUS
    @ LIN,10 SAY DSTATUS
    lin = lin + 1
      skip
        if eof()
          @ lin,1 say replicate('-',131)
          @ lin+1,10 say 'F I N A L     D E S T E     R E L A T O R I O'
        endif
  enddo
enddo

DO IMPSAIDA

return





