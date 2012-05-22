* AV06200 - CONSULTA SITUACAO DE PEDIDO
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 24-04-90
*
sele D
use CADNF ORDER X_PEDNF
sele C
use CLIEN ORDER P_CGC
sele B
use PEDID ORDER P_PEDID
sele A
use PEDIC ORDER P_PED
stor SPACE(6) TO MPED
=CABTELA(0,0,24,79)
@01,MENSAGR('Consulta pelo n£mero do Pedido') say 'Consulta pelo n£mero do Pedido'
@03,02 say 'Ped int'
@04,02 say 'It  Ref   Quantid     Saldo   D Ite Bonus  TdP  Emiss„o  Entrega  Num NF'
do while .T.
   @05,01 clear TO 21,78
   =MENSAG(ECODIGO)
   sele PEDIC
   do while .t.
      mped = 0
      @03,10 get mped pict '999999'
      read
      if LASTKEY() = 27
         CLOS DATA
         return
      endif
      if MPED = 0
         =MENSAG(CNUMER)
         loop
      endif
      if seek(mped)
         exit
      else
         =MENSAG(CORRIJA)
         =INKEY(0)
      endif
   enddo
*
   MCGC = CGC
   if SEEK(MCGC,'clien')
      @03,17 say clien->RAZ color &corver
   ELSE
      @03,17 say 'Cliente n„o encontrado'
   endif
      if situacao = 1
         @03,64 say '(1-Pronto)   ' color &corver
      else
         if situacao = 2
            @03,64 say '(2-Expedi‡„o)' color &corver
         else
            if situacao = 3
               @03,64 say '(3-Cancelado)' color &corver
            else
               @03,64 say '(0-Pendente) ' color &corver
            endif
         endif
      endif
   if !seek(str(mped,6),'pedid')
      =MENSAG('Pedido sem ¡tens cadastrados - confira - ENTER')
      =INKEY(0)
   else
      ment = space(08)
      mcancelnf = space(08)
      if seek(str(mped,6),'cadnf')
         ment = cadnf->ent
         mcancelnf = cadnf->cancelnf
      endif
      lin = 5
   endif
   sele PEDID
   if left(mcancelnf,2) <> space(02)
      @04,58 say 'cancelado'
   else
      @04,58 say ' Entrega '
   endif
   do while ped = mped .and. .not. eof()
      @lin,02 say str(ite,2)
      @LIN,06 say ref
      @lin,10 say qtd pict '999,999.99'
      @lin,21 say sal pict '999,999.99'
      @lin,32 say str(dit,5,2)
      @lin,38 say str(bon,5,2)
      @lin,44 say tabref
      @lin,50 say cadnf->demi
      if !empty(mcancelnf)
         @lin,59 say mcancelnf
         @lin,68 say str(nnf,6)
      else
         @lin,59 say ment
         @lin,68 say str(nnf,6)
      endif
      lin = lin + 1
      if lin > 21
         SET ESCA OFF
         lin = 5
         =MENSAG('Aperte qualquer tecla para continuar')
         =INKEY(0)
         @05,01 clear TO 21,78
      endif
      skip
      if ped <> mped
         SET ESCA OFF
         =MENSAG('Final do Pedido - Aperte qualquer tecla para continuar')
         =INKEY(0)
      endif
   enddo
   sele PEDIC
enddo
clos data
return

