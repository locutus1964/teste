* RMOEDAS - RELATORIO DAS PARIDADES
* SISTEMA CONTAS A PAGAR
* ADAO ELI PEREIRA
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
INICIO = {}
FIM    = {}
  ? JANELA(6,36,11,78)
  ? BORDA(6,36,11,78)
  @ 8,38 SAY 'Informe DIA/MES/ANO p/Inicio 'get inicio
  @ 9,38 SAY 'Informe DIA/MES/ANO p/Final  'get fim 
    read
      MENS = DADCORR
      DO PEGRESP WITH [SN]
        IF MC = 'N'
          RETURN
        ENDIF
*
? MENSAG('Preparando os Arquivos... Aguarde ')
  USE CADMOEDA
  COPY TO CADWORK FOR DATA >= INICIO .AND. DATA <= FIM
  CLOS DATA
  USE CADWORK
  INDEX ON STR(MOEDA,3)+dtos(DATA) TO CADWORK
*
? MENSAG('Imprimindo Relat¢rio Moedas e Fatores de Convers„o... Aguarde')
*
USE CADWORK INDEX CADWORK

pagctr = 0
tamrel = 1 

do defsaida

do while !EOF()
*
  ? CABEC80('RMOEDA','MOEDAS E FATORES DE CONVERSAO POR DATA',80)
*
  @ 4,1  say 'COD'
  @ 4,7  say 'DESCRICAO '
  @ 4,30 SAY 'ABREVIAT'
  @ 4,46 SAY 'DATA'
  @ 4,56 SAY 'DOLARES'
  @ 4,67 SAY ' ORIGEM'
  @ 5,1 SAY REPLICATE('-',80)
  MMOEDA = 0
*
  lin = 6
  do while .not. EOF() .and. lin < 56
*
    IF ! empty(DATA)
      IF MOEDA <> MMOEDA .OR. LIN = 6
        @ lin,1 say STR(MOEDA,3)
        @ lin,7 say DMOEDA
        @ LIN,34 SAY AMOEDA
      endif
      @ lin,45 say DATA
      @ lin,55 say str(cdolar,12,6)
      @ lin,68 say str(ccz,12,6)
        stor moeda to mmoeda
        stor lin + 1 to lin
    ENDIF
    
    skip
      if .not. eof() .and. ! empty(DATA) .and. moeda <> mmoeda .and. lin > 6
        stor lin + 1 to lin
      endif
      if eof()
        @ lin,1 say replicate('-',80)
        @ lin+1,10 say 'F I N A L    D E S T E    R E L A T O R I O'
      endif
  enddo
enddo

do impsaida

return





