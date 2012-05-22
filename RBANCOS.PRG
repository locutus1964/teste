* RBANCOS - RELACAO DE BANCOS
* ADAO ELI PEREIRA - 18/12/87
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
MENS = 'Relat¢rio por C¢digo (C) ou Ordem Alfa (A)? '
DO PEGRESP WITH [CA]
    IF MC = 'C'
      USE BANCOS INDE BANCOS.IDX
      TIT = 'BANCOS CADASTRADOS - ORDEM DE CODIGO'
    ELSE
      USE BANCOS
        INDEX ON DBANCO + STR(NBANCO,3) + STR(AGENCIA,4) TO CADWORK.IDX
        USE BANCOS INDE CADWORK.IDX
        TIT = 'BANCOS CADASTRADOS - ORDEM ALFA'
    ENDIF
*
pagctr = 0
tamrel = 2
*
? MENSAG('Imprimindo Relat¢rio dos Bancos cadastrados... Aguarde')

DO DEFSAIDA
*
do while .not. EOF()
  ? CABEC('RBANCOS', TIT,131)
*
  @ 03,01 say 'BCO'
  @ 03,05 SAY 'NOME DO BANCO'
  @ 03,36 SAY 'AGEN'
  @ 03,41 SAY 'CIDADE'
  @ 03,62 say 'ENDERECO'
  @ 03,93 SAY 'COD.CID.'
  @ 03,104 SAY 'UF'
  @ 03,107 say 'C.CORRENTE'
  @ 03,123 say 'CONTA BCO'
  @ 4,1 SAY REPLICATE('-',131)
*
  lin = 5
  do while .not. EOF() .and. lin < 56
    @ LIN,01 SAY STR(NBANCO,3)
    @ LIN,05 SAY DBANCO
    @ LIN,36 SAY STR(AGENCIA,4)
    @ LIN,41 SAY CIDADE
    @ LIN,62 SAY ENDER
    @ LIN,93 SAY CODCID
    @ LIN,104 SAY ESTADO
    @ LIN,107 SAY CTACORR
    @ LIN,123 SAY CTABCO
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