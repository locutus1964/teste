* RSIGLA - RELACAO DE SIGLAS DE FUNCIONARIOS AUTORIZADOA
* ADAO ELI PEREIRA - 01/08/93
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
USE SIGLAS INDE SIGLAS.IDX

pagctr = 0
tamrel = 1

? MENSAG('Imprimindo Relat¢rio das Siglas dos Funcion rios... Aguarde')

DO DEFSAIDA
*
do while .not. EOF()
  ? CABEC80('RSIGLA', 'SIGLAS DOS FUNCIONARIOS',78)
*
  @ 04,01 say 'SIGLA'
  @ 04,13 SAY 'NOME DO FUNCIONARIO'
  @ 05,1 SAY REPLICATE('-',78)
*
  lin = 6
  do while .not. EOF() .and. lin < 56
    @ LIN,02 SAY SIGLA
    @ LIN,13 SAY NOME
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