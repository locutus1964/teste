* AV02300 - LISTA AS TRANSPORTADORAS CADASTRADAS - 16/12/86
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 12-04-90 - 04/08/93
*
  do TESTIMP2
    IF CP = 'N'
      RETURN
    ENDIF

  ? JANELA(16,37,21,74)
  ? BORDA(16,37,21,74)
*
MC = ' '
@ 18,40 SAY 'Ordem C¢digo (C), Ordem Alfa (A)'
@ 19,40 say 'ou Nome de Guerra (G)? 'GET MC PICT '!' VALID(MC$'CAG')
  READ
    IF MC = 'C'
      USE TRANS INDE TRANS.IDX
      TIT = 'T R A N S P O R T A D O R A S  (ORDEM DE CODIGO)'
    ELSE 
      IF MC = 'G'
        USE TRANS INDE TRAGUE.IDX
        TIT = 'T R A N S P O R T A D O R A S  (POR NOME DE GUERRA)'
      ELSE
        USE TRANS INDEX TRARAZ.IDX
        TIT = 'T R A N S P O R T A D O R A S  (ORDEM DE RAZAO SOCIAL)'
      ENDIF  
    ENDIF
*
pagctr = 0
tamrel = 2
*
? MENSAG('Imprimindo Transportadoras por Raz„o Social... Aguarde.')
*
DO DEFSAIDA
*
do while .not. EOF()
*
? CABEC('AV02300',TIT,131)
*
@ 3,1 say 'COD'
@ 3,05 say 'RAZAO SOCIAL / ENDERECO'
@ 3,51 say 'CIDADE/BAIRRO'
@ 3,72 say 'N.G./UF/CEP'
@ 3,88 say 'TELEFONES'
@ 3,102 say 'CONTATO/FAX/VIA/LOCALIZ.'

@ 4,1   SAY REPLICATE('-',131)
*
stor 5 to linha
  do while .not. EOF() .and. linha < 63
@ linha,1 say TRA
@ linha,5 say RAZ
@ linha,51 say SUBS(CID,1,20)
@ linha,72 say GUETRA
@ linha,88 say DDD+' '+TEL
@ linha,102 say CONTATO
@ linha,123 say SUBSTR(LOCALIZ,1,10)
  linha = linha + 1
@ linha,05 say END
@ linha,51 say BAI
@ linha,73 say EST
@ linha,77 say CEP
@ linha,88 say DDD2+' '+TEL2
@ linha,102 say DDD_FAXT
@ linha,107 say NUM_FAXT    
@ linha,117 say VIA
  linha = linha + 2
    skip
      if eof()
        @ linha,1 say replicate('-',131)
        @ linha+1,10 say 'F I N A L     D E S T E     R E L A T O R I O'
      endif
  enddo
enddo
  
  DO IMPSAIDA
  
  ERASE CADWORK.NTX
  CLEAR
return
