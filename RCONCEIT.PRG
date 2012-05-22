* RCONCEIT - RELACAO DE CONCEITOS
* ADAO ELI PEREIRA - 10/07/93
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
USE CONCEITO INDE CONCEITO.IDX

pagctr = 0
tamrel = 1
*
? MENSAG('Imprimindo Relat¢rio dos Conceitos cadastrados... Aguarde')

DO DEFSAIDA
*
do while .not. EOF()
  ? CABEC80('RCONCEIT', 'CONCEITOS DE CLIENTES',78)
*
  @ 04,01 say 'CONCEITO'
  @ 04,10 SAY 'DESCRICAO'
  @ 04,45 SAY 'COLORIR'
  @ 04,60 say 'COD.SCI'
  @ 05,1 SAY REPLICATE('-',78)
*
  lin = 6
  do while .not. EOF() .and. lin < 56
    @ LIN,05 SAY CONCEITO
    @ LIN,10 SAY DCONCEITO
    @ LIN,48 SAY COLORIR
    @ LIN,60 SAY CONCSCI
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