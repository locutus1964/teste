* RATIVIDA - RELACAO DE ATIVIDADES
* ADAO ELI PEREIRA - 10/07/93
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
USE atividad INDE atividad.IDX

pagctr = 0
tamrel = 1
*
? MENSAG('Imprimindo Relat¢rio dos atividade cadastrados... Aguarde')

DO DEFSAIDA
*
do while .not. EOF()
  ? CABEC80('RATIVIDA', 'ATIVIDADES DE CLIENTES',78)
*
  @ 04,01 say 'ATIVIDADE'
  @ 04,13 SAY 'DESCRICAO'
  @ 05,1 SAY REPLICATE('-',78)
*
  lin = 6
  do while .not. EOF() .and. lin < 56
    @ LIN,03 SAY atividade
    @ LIN,13 SAY datividade
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
