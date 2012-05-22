* AV03400 - RELACAO DOS CLIENTES POR REPRESENTANTE
* SISTEMA ADMINISTRACAO DE VENDAS - 10/10/87 - 12-04-90
* ADAO ELI PEREIRA
*
MC = ' '
MENS = 'Relat¢rio Total (T) ou Selecionado (S) ?'
do pegresp with [ST]
if empty(mc)
   close data
   return
endif
sele 0
use regiao order x_regiao
MREGIAO = EmptyRegiao
if MC = 'S'
   =JANELA(17,37,21,78)
   =BORDA(17,37,21,78)
   @19,40 say 'C¢digo do Representante (Regi„o)'
   @19,73 get MREGIAO pict '9999.99'
   READ
   if MREGIAO = EmptyRegiao
      =MENSAG('Representante Selecionado n„o pode ser brancos - ENTER')
      =INKEY(0)
      close data
      return
   endif
   sele 0
   use VEND index VEND.IDX
   =MENSAG('Classificando Clientes por Representante... Aguarde')
   sele 0
   use CLIEN
   SET RELATION to VEN INTO VEND 
   COPY FIELDS VEN,CGC,RAZ,CID,EST,CEP,ULC,DDD1,FON1,END,BAI,REGIAO, IDREGIAO;
        TO CADWORK;
        FOR FmtRegiao(REGIAO,IDREGIAO) = MREGIAO  
else
   =MENSAG('Classificando Clientes por Representante... Aguarde')
   use CLIEN
   COPY FIELDS VEN,CGC,RAZ,CID,EST,CEP,ULC,DDD1,FON1,END,BAI,REGIAO,IDREGIAO;
        TO CADWORK
endif
close data
sele 0
use CADWORK
if EOF()
   =MENSAG('Representante n„o cadastrado - ENTER')
   =INKEY(0)
   close data
   return
endif
INDEX ON VEN + CEP to CADWORK.IDX
sele 0
use REGIAO ORDER X_REGIAO
sele 0
use VEND index VEND.IDX
SELE cadwork
SET RELATION TO ven    INTO vend,;
                regiao INTO regiao
go top
PAGCTR = 0
TAMREL = 2
=MENSAG('Imprimindo Relat¢rio Clientes por Representante... Aguarde')
do TESTIMP2
if CP = 'N'
   close data
   return
endif
set cursor off
=mensag('Gerando Relatorio...')
DO DEFSAIDA
stor SPACE(04) to MCVR  , MVEN
stor 0         to PAGCTR, QTVEN, TCLIEN
stor 58        to lin
ch_imp = .f.
do while !eof()
   mven = ven
   do av03400cab
   do while !eof() .and. ven = mven
      if lin >= 58
         do av03400cab
      endif
      @lin,001 say CGC
      @lin,022 say RAZ
      @lin,069 say CID
      @lin,095 say EST
      @lin,098 say CEP
      @lin,108 say ULC
      @lin,117 say DDD1
      @lin,122 say FON1
      QTVEN  = QTVEN + 1
      TCLIEN = TCLIEN + 1
      lin = lin + 1
      @lin,022 say END
      @lin,069 say BAI
      lin = lin + 1
      skip
      if inkey()=27
         ch_imp = .t.
         exit
      endif
   enddo
   if ch_imp .or. inkey()=27
      ch_imp = .t.
      exit
   endif
enddo
@lin+1,001 say replicate('-',130)
if ch_imp
   @lin+2,001 say 'I M P R E S S A O    I N T E R R O M P I D A'
else
   @lin+2,001 say 'F I N A L    D E S T E    R E L A T O R I O'
endif
set cursor on
DO IMPSAIDA
close data
return

*---------------------------------------------------------------
procedure av03400cab
 if pagctr # 0
    @ 0, 0   
  endif
  PAGCTR = PAGCTR + 1
  @prow()+1,001 say  replicate('-',130)
  @prow()+1,001 say MREL
  @prow()  ,pcol()+5 say HOJE
  @prow()  ,123      say 'PAG. '+STR(PAGCTR,3)
  @prow()+1,001      say 'CLIENTES DO REPRESENTANTE -'
  @prow()  ,pcol()+1 say ven
  @prow()  ,pcol()+1 say '-'
  if ! eof('vend')
     @prow()  ,pcol()+1 say vend->RAZ
  else
     @prow()  ,pcol()+1 say 'VENDEDOR NAO CADASTRADO'
  endif
  * =seek(MVEN,'vend')
  @prow()+1  ,001 say 'REGIAO: ' + regiao
  if ! EOF('regiao')
    @prow()  ,pcol()+1 say regiao->DREGIAO
  else
*    @prow()  ,pcol()+1 say 'REPRESENTANTE NAO CADASTRADO'
    @prow()  ,pcol()+1 say 'REGIAO NAO CADASTRADA'
  endif
  @prow()+1,001     say  replicate('-',130)
  @prow()+1,001     say ' CODIGO DO CLIENTE                RAZAO SOCIAL/ENDERECO                  CIDADE / BAIRRO      UF    CEP      ULC      DDD / FONE  '
  @prow()+1,001     say '==================== ============================================== ========================= == ========= ======== =============='
  lin = prow()+1
return

