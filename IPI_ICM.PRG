*:*********************************************************************
*:
*: Procedure file: C:\ADMV\IPI_ICM.PRG
*:
*:         System: CPG
*:         Author: ADAO ELI PEREIRA
*:      Copyright (c) 1993, ADAO ELI PEREIRA
*:  Last modified: 26/01/94     15:51
*
do testimp2
if cp = 'N'
   return
endif
*
? janela(15,32,21,77)
? borda(15,32,21,77)
*
inicio = space(8)
fim    = space(8)
@ 17,34 say 'Informe DIA/MES/ANO para Inicio 'get inicio pict '99/99/99'
@ 19,34 say 'Informe DIA/MES/ANO para Final  ' get fim pict '99/99/99'
read

mens = dadcorr
do pegresp with [SN]
if mc = 'N'
   return
endif
*
public mmvar
mccz   = 0
pagctr = 0
ipi_dia = 0.00
ipi_ger = 0.00
icm_dia = 0.00
icm_ger = 0.00
*
? mensag('Preparando os Arquivos... Aguarde')
use cadnf
copy to cadwork for ctod(demi)>=ctod(inicio).and.ctod(demi)<=ctod(fim) ;
     .and. left(cancelnf,2) = space(02)
clos data
use cadwork
index on substr(demi,7,2)+substr(demi,4,5)+substr(demi,1,2) to cadwork.IDX
clos data
*
use cadwork index cadwork.IDX
*
tamrel = 1
? mensag('Imprimindo D‚bitos IPI e ICM por data de emissao... Aguarde')

do defsaida
*
do while .not. eof()
   @ 1,1 say 'IPI_ICM    IPI E ICM ACUMULADOS POR DATA DE EMISSAO DA NF'
   @ 1,70 say HOJE
   @ 2,1 say mrel
   @ 2,35 say 'INICIO - '+inicio+'  FIM - '+fim
   pagctr = pagctr + 1
   @ 2,70 say 'PAG '+padl(pagctr,3,'0')
   *
   @ 3,1 say replicate('-',80)
   @ 4,1 say 'DATA DA NOTA FISCAL'
   @ 4,25 say 'VALOR DO IPI'
   @ 4,40 say 'ACUMULADO IPI'
   @ 4,55 say 'VALOR DO ICM'
   @ 4,70 say 'ACUMULADO ICM'
   *
   @ 5,1 say replicate('-',80)
   *
   lin = 7
   *
   do while .not. eof() .and. lin < 58
   
      ipi_dia = (ipi_dia + ipi)
      ipi_ger = (ipi_ger + ipi)
      
      icm_dia = (icm_dia + icm)
      icm_ger = (icm_ger + icm)
      
      mdemi = demi
      
      skip
      *
      if demi <> mdemi .or. eof()
      
         @ lin,1 say mdemi
         @ lin,25 say ipi_dia pict "99,999,999.99"
         @ lin,40 say ipi_ger pict '999,999,999.99'
         @ lin,55 say icm_dia pict '99,999,999.99'
         @ lin,70 say icm_ger pict '999,999,999.99'
         
	     ipi_dia = 0.00
         icm_dia = 0.00
         
         LIN = LIN + 1
    
      endif
      
      if eof()
         lin = lin + 1
         @ lin,1 say replicate('-',80)
         lin = lin + 1
         @ lin,10 say 'F I M    D E S T E    R E L A T O R I O'
      endif
      *
   enddo
enddo

do impsaida

return






*: EOF: CRESUM.PRG
