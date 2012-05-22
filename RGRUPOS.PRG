* RGRUPOS - RELACAO DOS GRUPOS DE PRODUTOS
* ADAO ELI PEREIRA - 06/09/93
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
USE GRUPOS INDE GRUPOS.IDX

pagctr = 0
tamrel = 1
*
? MENSAG('Imprimindo Relat¢rio das Unidades... Aguarde')

DO DEFSAIDA
*
do while .not. EOF()
  ? CABEC80('RGRUPO', 'GRUPOS DE PRODUTOS',78)
*
  @ 04,01 say 'GRUPO'
  @ 04,13 SAY 'DESCRICAO'
  @ 04,45 SAY '% COMISSAO'
  @ 05,1 SAY REPLICATE('-',78)
*
  lin = 6
  do while .not. EOF() .and. lin < 56
    @ LIN,03 SAY GRUPO  
    @ LIN,13 SAY DGRUPO
    @ LIN,50 SAY COMISSAO PICT '99.99'
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
