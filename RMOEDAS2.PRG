* RMOEDAS2 - RELATORIO DAS PARIDADES
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
close data

*
? MENSAG('Imprimindo Relat¢rio Moedas e Fatores de Convers„o... Aguarde')
*
USE NOMMOEDA INDEX NOMMOEDA.IDX

pagctr = 0
tamrel = 1 

do defsaida

do while !EOF()
*
  ? CABEC80('RMOEDAS2','MOEDAS CADASTRADAS',80)
*
  @ 4,1  say 'COD'
  @ 4,7  say 'DESCRICAO '
  @ 4,30 SAY 'ABREVIAT'
  @ 5,1 SAY REPLICATE('-',80)
  MMOEDA = 0
*
  lin = 6
  do while .not. EOF() .and. lin < 56
*
    @ lin,1 say STR(MOEDA,3)
    @ lin,7 say DMOEDA
    @ LIN,34 SAY AMOEDA
    stor lin + 1 to lin
    
    skip
    
      if eof()
        @ lin,1 say replicate('-',80)
        @ lin+1,10 say 'F I N A L    D E S T E    R E L A T O R I O'
      endif
  enddo
enddo

do impsaida

return





