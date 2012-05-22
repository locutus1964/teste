* AV05300 - LISTA OS PRODUTOS CADASTRADOS - 18/11/86
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 15-04-90
*
MGRUPO = SPACE(02)
MENS = '[S] Seleciona Grupo ou [T] imprime Total dos Produtos ?'
DO PEGRESP WITH [ST]
mgru = mc
if empty(mc)
   return
endif
if MC = 'S'
   sele 0
   use grupos inde grupos.IDX
   do while .T.
      @23,MENSAG('Imforme o Grupo a Selecionar ') say ;
      'Imforme o Grupo a Selecionar ' get MGRUPO PICT '@!' valid !empty(mgrupo)
      READ
      if lastkey()= 27
         close data
         return
      endif
      if !seek(MGRUPO)
         =MENSAG('Grupo enexistente - ENTER')
         =INKEY(0)
      else
         MENS = DADCORR
         DO PEGRESP WITH [SN]
         if MC = 'S'
            EXIT
         else
            if empty(mc)
               close data
               return
            endif
         endif
      endif
   ENDDO
   use PROD inde PROD.IDX
   copy to CADWORK for GRUPO = MGRUPO
   use CADWORK
   inde ON REF to CADWORK.IDX
else
   use PROD inde PROD.IDX
endif
DO TESTIMP2
if CP = 'N'
   close data
   return
endif
=MENSAGR(IMPRCOD)
pagctr = 0
tamrel = 2
DO DEFSAIDA
go top
do while !eof()
   TIT = 'PRODUTOS CADASTRADOS - GRUPO '+ GRUPO
   =CABEC('AV05300',TIT,131)
   @3,01 say 'REF'
   @3,06 say 'D E S C R I C A O '
   @3,59 say 'EMBAL'
   @3,64 say 'C'
   @3,66 say 'S A L D O'
   @3,80 say 'NF EMIT'
   @3,91 say 'VEND.MES'
   @3,102 say 'FAT MES'
   @3,113 say 'P.PEDID'
   @3,124 say 'PES LIQ'
*
   @4,26 say 'CONTA'
   @4,37 say 'TIPO'
   @4,49 say 'CONTEUDO'
   @4,58 say 'UNI'
   @4,64 say 'GRUPO'
   @4,70 say '%IPI'
   @4,89 say 'EMPENHO'
   @4,97 say 'V.AT.MES'
   @4,106 say 'F.AT.MES'
   @4,115 say 'L.SUPRI'
   @4,124 say 'LP'
   @5,1   say REPLICATE('-',131)
   lin = 6
   do while !eof() .and. lin < 62
      @lin,01 say REF
      @lin,06 say DES
      @lin,60 say EMBALAGEM
      @lin,64 say CCL
      @lin,66 say SAL pict '99,999,999.99'
      @lin,80 say COM pict '99,999,999'
      @lin,91 say VAM pict '99,999,999'
      @lin,102 say FME pict '99,999,999'
      @lin,113 say EMI pict '99,999,999'
      @lin,124 say PEL pict '9999.999'
      lin = lin + 1
      @lin,26 say CON
      @lin,37 say TIPO
      @lin,49 say CONTEUDO
      @lin,59 say UNI	
      @lin,65 say GRUPO
      @lin,70 say IPI pict '99.9'
      @lin,80 say PEA pict '99,999,999'
      @lin,91 say VAA pict '99,999,999'
      @lin,102 say FAC pict '99,999,999'
      @lin,113 say EMA pict '99,999,999'
      @lin,128 say LPR
      mgrupo = grupo
      lin = lin + 2
      skip
      if !eof() .and. grupo <> mgrupo
         lin = 70
      endif
      if eof()
         @lin-1,1 say replicate('-',131)
         @lin,10 say 'F I N A L    D E S T E    R E L A T O R I O'
      endif
   enddo
enddo
do impsaida
close data
return
