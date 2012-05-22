* AV07510 - SELECIONA PEDIDO PARA EMISSAO DE NOTA ON linE
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 09-07-90  -  08/12/91 - 16/06/93
*
=CABTELA(0,0,24,79)
@01,mensagr('Seleo de Pedidos p/emisso de Notas Fiscais')  ;
say 'Seleo de Pedidos p/emisso de Notas Fiscais'
mate = 0
close data
sele 0
use TPNF     index TPNF.IDX
sele 0
use ESTADOS  index ESTADOS.IDX
sele 0
use PROD     index PROD.IDX
sele 0
use BANCOS   index BANCOS.IDX
sele 0
use OBSNOTA  index OBSNOTA.IDX
ZAP
sele 0
use APROVP   index APROVP.IDX
ZAP
sele 0
use PEDIC2   index PEDIC2.IDX
ZAP
sele 0
use CLIEN    ORDER P_CGC
sele 0
use PEDID    ORDER P_PEDID
sele 0
use PEDIC    ORDER P_PED
sele 0
use VEICULOS index VEICULOS.IDX
stor 0 to MQTDAT, MMPED, MPED
MCGC   = space(20)
MAUT   = space(01)

@04,01 clear TO 21,78
@04,03 say 'Nr.Pedido'
@06,02 say 'Item'
@06,07 say 'Ref'
@06,25 say 'Quantidade'
@06,37 say ' S a l d o'
@06,49 say 'D.Ite'
@06,55 say 'Bonus'
@06,61 say 'Entrega'
@06,70 say 'N.Fisc.'
save scre to tela
do while .t.
   sele PEDIC
   set relation to str(ped,6) into pedid, cgc into clien
   set skip to clien, pedid
   =mensag(ECODIGO)
   @04,14 get mped pict '999999' valid mped>0
   read
   if lastkey() = 27
      exit
   endif
   set relation to
   set skip to
   if !seek(mped)
      =mensag(CORRIJA)
      =INKEY(0)
   else
      mcgc      = cgc
      MIDREPR   = IDREPR
      MIDREGIAO = IDREGIAO
      mven      = ven
      mdem      = dem
      mped      = ped
      mtra      = tra
      mfre      = fre
      memb      = emb
      mdesconto = desconto
      mtip      = tip
      mpcl      = pcl
      mcp1      = cp1
      mcp2      = cp2
      mcp3      = cp3
      mcp4      = cp4
      mpe1      = pe1
      mpe2      = pe2
      mpe3      = pe3
      mpe4      = pe4
      mcpd      = cpd
      mpint     = pint
      mnbanco   = nbanco
      magencia  = agencia
      if seek(MTIP,'tpnf')
         MEDUP = tpnf->EDUP
      else
         MEDUP = space(01)
      endif
      if seek(MCGC,'clien')
         @04,23 say clien->RAZ        color &CORVER
         MEST = clien->EST
      else
         @04,23 say 'Cliente no encontrado - ENTER para sair do programa'
         =INKEY(0)
         exit
      endif
      if seek(MEST,'estados')
         MICM = estados->ICM
      else
         MICM = 0.00
      endif
      sele PEDID
      if !seek(str(mped,6))
         =mensag('Pedido sem กtens cadastrados - ENTER')
         =INKEY(0)
      else
         @07,01 clear TO 21,78
         lin = 7
      endif
      do while ped = mped .and. !eof()
         @lin,03 say str(ite,2)              color &corver
         @lin,06 say ref                     color &corver
         @lin,25 say qtd pict '99,999,999'   color &corver
         @lin,37 say sal pict '99,999,999'   color &corver
         @lin,49 say dit pict '99.99'        color &corver
         @lin,55 say bon pict '99.99'        color &corver
         @lin,61 say dent                    color &corver
         @lin,70 say nnf pict '999,999'      color &corver
         lin = lin + 1
         if lin > 21
            lin = 7
            =mensag('Pressione qualquer tecla para continuar o Pedido')
            =INKEY(0)
         endif
         skip
      enddo
      MENS = 'Pedido "OK" para emisso de Nota Fiscal ?'
      do pegresp with [SN]
      if MC = 'N' .or. empty(mc)
         @07,01 clear TO 21,78
         loop
      else
         MAUT = space(01)
         MENS = 'Atendimento Total do Pedido (T) ou Parcial (P)?'
         do pegresp with [TP]
         if MC ='T'
            seek str(MPED,6)
            do while PED = MPED .AND. !eof()
               repl QTDAT with SAL
               skip
            enddo
         else
            @06,61 say space(17)
            @06,61 say 'Qtd a atender'
            seek str(MPED,6)
            @07,01 clear TO 21,78
            lin = 7
            do while ped = mped .and. !eof()
               if sal > 0
                  @lin,03 say str(ite,2)             color &corver
                  @lin,06 say ref                    color &corver
                  @lin,25 say qtd pict '99,999,999'  color &corver
                  MSAL = SAL
                  @lin,37 say msal pict '99,999,999' color &corver
                  @lin,49 say dit  pict '99.99'      color &corver
                  @lin,55 say bon  pict '99.99'      color &corver
                  do while .T.
                     @lin,63 get MQTDAT pict '99,999,999'
                     READ
                     IF LASTKEY() = 27
                       EXIT
                     ENDIF  
                     if MQTDAT > MSAL
                       =mensag('Saldo menor que quantidade atendida - ENTER')
                       =INKEY(0)
                       loop
                     endif
                     MENS = DADCORR
                     do pegresp with [SN]
                     if MC = 'N' .or. empty(mc)
                        loop
                     endif
                     repl QTDAT with MSAL - MQTDAT
                     MQTDAT = 0
                     exit
                  enddo
                  lin = lin + 1
                  if lin > 21
                     lin = 7
                     =mensag('Pressione qualquer tecla para continuar')
                     =INKEY(0)
                  endif
                  skip
                  if ped <> mped
                     =mensag('Final do Pedido - ENTER para continuar')
                     =INKEY(0)
                  endif
               else
                  skip
               endif
            enddo
         endif
         if mdesconto > 0
            do while .t.
               @15,05 clear to 19,74
               @15,05 to 19,74 double
               @17,10 say 'Valor do Desconto Fixo previsto no Pedido'
               @17,60 say MDESCONTO pict '99,999,999.99'
               mens = 'Confirma Valor do Desconto (C) ou altera (A)'
               do pegresp with [CA]
               if mc = 'A'
                  =mensag('Informe o valor correto do Desconto')
                  @17,60 get mdesconto pict '99,999,999.99'
                  read
                  mens = dadcorr
                  do pegresp with [SN]
                  if mc = 'S'
                     exit
                  endif
               endif
               exit
            enddo
         endif
         sele PEDIC2
         appe blan
         repl cgc      with mcgc      ,ven  with  mven  ,dem   with mdem  ,;
              pcl      with mpcl      ,ped  with  mped  ,tra   with mtra  ,;
              desconto with mdesconto ,tip  with  mtip  ,cp1   with mcp1  ,;
              cp2      with mcp2      ,cp3  with  mcp3  ,cp4   with mcp4  ,;
              pe1      with mpe1      ,pe2  with  mpe2  ,pe3   with mpe3  ,;
              pe4      with mpe4      ,cpd  with  mcpd  ,pint  with mpint ,;
              icm      with micm
         REPL IDREGIAO WITH MIDREGIAO
         REPL IDREPR   WITH MIDREPR     
         stor space(60) TO MOBSNF1, MOBSNF2, MOBSNF3, MOBSNF4
         stor space(60) TO MOBSNF5, MOBSNF6, MOBSNF7, MOBSNF8
         stor 0.00      TO MPBRUTO, MPLIQUIDO, MQTDVOL
         MESPECIE = space(17)
         MVEICULO = space(08)
         MENS = 'Inclui Observa”es para este Pedido ?'
         do pegresp with 'NS'
         if mnbanco = 0
            do incluiob with 'AV07510'
         else
            **--- Clientes antigos com bancos nao cadastrados
            do incluiob with 'AV07600'
         endif
      endif
   endif
   sele PEDIC
   rest screen from tela
enddo
clos data
return
*฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
procedure incluiob2
* Pede os valor de Frete, Seguro e Despesas acessorias inerentes a nf.
private vfre_resp, vfre_vlr, vseg_vlr, vout_vlr, varea
stor 0 to vfre_vlr, vseg_vlr, vout_vlr, vfre_resp
varea = select()
save screen
@09,11 say 'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
@10,11 say 'บ  FRETE                                                 บ'
@11,11 say 'บ  =====                                                 บ'
@12,11 say 'บ       Responsabilidade por conta do  1 - Emitente      บ'
@13,11 say 'บ                                      2 - Destinatario  บ'
@14,11 say 'บ       Valor........                                    บ'
@15,11 say 'ฬออออออออออออออออออออออออหอออออออออออออออออออออออออออออออน'
@16,11 say 'บ          SEGURO        บ   OUTRAS DESPESAS ACESSORIAS  บ'
@17,11 say 'บ          ======        บ   ==========================  บ'
@18,11 say 'บ  Valor..               บ        Valor..                บ'
@19,11 say 'ศออออออออออออออออออออออออสอออออออออออออออออออออออออออออออผ'
@12,50 prompt '1 - Emitente    '
@13,50 prompt '2 - Destinatario'
menu to vfre_resp
@14,33 get vfre_vlr pict '999999999.99'
@18,22 get vseg_vlr pict '999999999.99'
@18,53 get vout_vlr pict '999999999.99'
read
sele obsnota
if eof() .and. bof()
   append blank
   replace cgc   with mcgc     , ped      with mped
endif
replace fre_resp with vfre_resp     ,;
fre_vlr  with vfre_vlr      ,;
seg_vlr  with vseg_vlr      ,;
out_vlr  with vout_vlr
select(varea)
restore screen
return

