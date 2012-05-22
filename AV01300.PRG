* AV01300 - RELATORIO DE ESTADOS DA FEDERACAO
* ADAO ELI PEREIRA - 16/01/92
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
use ESTADOS index ESTADOS.IDX
*
  ? JANELA(14,38,18,78)
  ? BORDA(14,38,18,78)
*
MC = ' '
@ 16,41 SAY 'Por Sigla (S) ou Ordem Alfa (A)? 'GET MC PICT '!' VALID(MC$'AS')
  READ
    IF MC = 'S'
      USE ESTADOS INDE ESTADOS.IDX
      TIT = 'ESTADOS CADASTRADOS - ORDEM DE SIGLA'
    ELSE
      USE ESTADOS
        INDEX ON DEST TO CADWORK.IDX
        USE ESTADOS INDE CADWORK.IDX
        TIT = 'ESTADOS CADASTRADOS - ORDEM ALFA'
    ENDIF
*
pagctr = 0
TAMREL = 1
*
? MENSAG('Imprimindo Relat¢rio dos Estados cadastrados... Aguarde')

 DO DEFSAIDA
*
do while .not. EOF()
*
? CABEC80('RESTADO', TIT,80)
*
@ 4,10 say 'SIGLA DO ESTADO'
@ 4,30 say 'DESCRICAO DO ESTADO (UF)'
@ 4,65 SAY '% DO ICM'
@ 5,1 SAY REPLICATE('-',80)
*
lin = 6
  do while .not. EOF() .and. lin < 58
    @ lin,16 say CEST
    @ lin,30 say DEST
    @ lin,68 say STR(ICM,5,2)
    lin = lin + 1
    skip
     if eof()
       @ lin,1 say replicate('-',80)
       @ lin+1,10 say 'F I N A L     D E S T E     R E L A T O R I O'
     endif
  enddo
enddo
  
  DO IMPSAIDA  
  
return
