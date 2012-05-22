* AV05300 - LISTA OS PRODUTOS CADASTRADOS - 18/11/86
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 15-04-90
*
MENS = 'Relat¢rio por Grupo, por Referˆncia ou Simplificado ?'
DO PEGRESP WITH [GRS]
if empty(mc)
   return
endif
IF MC = 'G'
   DO AV053001
   RETURN
ENDIF
IF MC = 'S'
   DO AV053002
   RETURN
ENDIF
DO TESTIMP2
IF CP = 'N'
   RETURN
ENDIF
*
=MENSAGR(IMPRCOD)
*
sele 0
     USE PROD
INDEX ON LPR + REF TO CADWORK.IDX
go top
SELE 0
     USE LIMPROD INDE LIMPROD.IDX
*
pagctr = 0
tamrel = 2
*
DO DEFSAIDA

SELE PROD
do while !EOF()
   MLPR = LPR
   SELE LIMPROD
   IF SEEK(MLPR)
      TIT = 'LINHA DE PRODUTO '+MLPR+' - '+DLPR
   ELSE
      TIT = 'LINHA NAO CADASTRADA'
   ENDIF
*
   SELE PROD
   PAGCTR = PAGCTR + 1
   @ 01,01 SAY MREL
   @ 01,30 SAY TIT
   @ 01,112 SAY HOJE
   @ 01,124 SAY 'PAG '+STR(PAGCTR,3)
   @ 02,01 SAY REPLICATE('-',131)
*  =CABEC('AV05300',TIT,131)
   @ 3,01 say 'REF'
   @ 3,06 say 'D E S C R I C A O '
   @ 3,59 say 'EMBAL'
   @ 3,64 say 'C'
   @ 3,66 say 'S A L D O'
   @ 3,80 say 'NF EMIT'
   @ 3,91 say 'VEND.MES'
   @ 3,102 SAY 'FAT MES'
   @ 3,113 SAY 'P.PEDID'
   @ 3,124 SAY 'PES LIQ'
*
   @ 4,26 SAY 'CONTA'
   @ 4,37 SAY 'TIPO'
   @ 4,49 SAY 'CONTEUDO'
   @ 4,59 SAY 'UNI'
   @ 4,64 SAY 'GRUPO'
   @ 4,70 say '%IPI'
   @ 4,89 say 'EMPENHO'
   @ 4,97 SAY 'V.AT.MES'
   @ 4,106 SAY 'F.AT.MES'
   @ 4,115 SAY 'L.SUPRI'
   @ 4,124 SAY 'LP'
*
   @ 5,1   SAY REPLICATE('-',131)
*
   lin = 6

   do while !EOF() .and. lin < 62
      @ lin,01 say REF
      @ lin,06 say DES
      @ lin,59 say EMBALAGEM
      @ lin,64 say CCL
      @ lin,66 say SAL pict '99,999,999.99'
      @ lin,80 say COM pict '99,999,999'
      @ lin,91 say VAM pict '99,999,999'
      @ lin,102 say FME pict '99,999,999'
      @ lin,113 say EMI pict '99,999,999'
      @ lin,124 say PEL pict '9999.999'
      lin = lin + 1
      @ lin,26 say CON
      @ lin,37 say TIPO
      @ lin,49 say CONTEUDO
      @ lin,59 say UNI 	
      @ lin,65 say GRUPO
      @ lin,70 say IPI pict '99.9'
      @ lin,80 say PEA pict '99,999,999'
      @ lin,91 say VAA pict '99,999,999'
      @ lin,102 say FAC pict '99,999,999'
      @ lin,113 say EMA pict '99,999,999'
      @ lin,128 say LPR
      mlpr = lpr
      lin = lin + 2
      skip
      if lpr <> mlpr
         lin = 70
      endif

      if eof()
         @ lin-1,1 say replicate('-',131)
         @ lin,10 say 'F I N A L    D E S T E    R E L A T O R I O'
      endif
   enddo
enddo

do impsaida

return


