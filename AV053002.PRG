* AV05995 - LISTA AS TABELAS DE PRECOS CADASTRADAS (POR CODIGO)
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 03/07/90 11/09/93
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
SELE 0
  USE PROD INDE PROD.IDX
      
    ? MENSAG('Imprimindo a Relat¢rio das Referˆncias cadastradas... Aguarde')
*
    pagctr = 0
    tamrel = 2

    DO DEFSAIDA

    SELE PROD
    do while !EOF()
*
      ? CABEC('AV053002','REFERENCIAS CADASTRADAS - SIMPLIFICADO',131)

      @ 4,01 say 'REF'
      @ 4,06 say 'DESCRICAO DO PRODUTO'
      @ 4,62 say 'TIPO'
      @ 4,74 say 'GR'
      @ 4,78 say 'CONTEUD'
      @ 4,87 say 'UNI'
      @ 4,92 say 'EMB'
      @ 4,97 say 'LPR'
      @ 4,102 say '% IPI'
      @ 5,1  SAY REPLICATE('-',131)
*
      stor 6 to lin
      
      do while .not. EOF() .and. lin < 64
        @ lin,01 say REF
        @ lin,06 say DES
        @ lin,62 say TIPO
        @ lin,74 say GRUPO
        @ lin,78 say CONTEUDO
        @ lin,87 say UNI
        @ LIN,92 say EMBALAGEM
        @ LIN,97 say LPR
        @ LIN,103 SAY IPI PICT '99.9'
        MLPR = LPR
      
        stor lin + 1 to lin
        skip
        
        IF EOF()
          @ LIN,1 SAY REPLICATE('-',131)
          @ LIN+1,10 SAY 'F I M     D E S T E     R E L A T O R I O'
        ENDIF
      enddo
    enddo

DO IMPSAIDA

return
