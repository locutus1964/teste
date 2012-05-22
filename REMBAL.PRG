* REMBAL - RELACAO DOS TIPOS DE EMBALAGENS
* ADAO ELI PEREIRA - 04/09/93
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
USE EMBALAGE INDE EMBALAGE.IDX

pagctr = 0
tamrel = 1
*
? MENSAG('Imprimindo Relat¢rio dos Tipos de Embalagem.. Aguarde')

DO DEFSAIDA
*
do while .not. EOF()
  ? CABEC80('REMBAL', 'TIPOS DE EMBALAGEM',78)
*
  @ 04,01 say 'EMBALAGEM'
  @ 04,13 SAY 'DESCRICAO'
  @ 05,1 SAY REPLICATE('-',78)
*
  lin = 6
  do while .not. EOF() .and. lin < 56
    @ LIN,03 SAY EMBALAGEM
    @ LIN,13 SAY DEMBALAGEM
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
