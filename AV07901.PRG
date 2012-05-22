*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º Programa....: AV07900.prg                                                 º
*º Sistema.....: Administra‡„o de Vendas - Ad„o Eli Pereira                  º
*º Objetivo....: Emiss„o de Notas Fiscais de Entradas                        º
*º               (Devolucao de Vendas                                        º
*º Data........: Sexta, 23 de fevereiro de 1996 …s 21:27 horas               º
*º Autor.......: Manuel In cio Lino de Almeida                               º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
=cabtela(0,0,24,79)
=INICPAG()
mens = 'Emiss„o de Notas Fiscais de Entrada (Retorno Vendas)'
@01,mensagr(mens) say mens
sele 0
use clien  order x_gue
mgue   = space(len(clien->gue ))
mraz   = space(len(clien->raz ))
mcgc   = space(len(clien->cgc ))
mend   = space(len(clien->end ))
mbai   = space(len(clien->bai ))
mcep   = space(len(clien->cep ))
mcid   = space(len(clien->cid ))
mddd1  = space(len(clien->ddd1))
mfon1  = space(len(clien->fon1))
mcest  = space(len(clien->est ))
mins   = space(len(clien->ins ))
sele 0
use siglas index siglas.IDX
SELE 0
use cabecnf ORDER P_NNF
sele 0
use estados index estados.IDX
sele 0
use entr_cab index entr_cab.IDX  alias NFCAB
sele 0
use entr_ite index entr_ite.IDX  alias NFITE
copy stru to arqtmp1
sele 0
use arqtmp1                  alias TM1
index on str(nnf,6) + str(item,2) to arqtmp1.IDX
sele 0
use prod     index prod.IDX
sele 0
use param
mnnf = 1
unnf = 0
if reccount() # 0
   unnf = nnf
   mnnf = nnf + 1
endif
save screen

IF ! ChkDataNf() 
   CLOSE DATA
   RETURN
ENDIF   
   
IF ! GetSigla() 
  CLOSE DATA
  RETURN
ENDIF
  
cSigla = SIGLAS->SIGLA
 
=JANELA(16,37,21,78)
=BORDA(16,37,21,78)
@18,40 say 'Ultima Nota Fiscal emitida   '+PADL(UNNF,7,'0')
@19,40 say 'Pr¢xima Nota Fiscal a emitir '+PADL(MNNF,7,'0')
mens = dadcorr
do pegresp with [SN]
restore screen
set deci to 4
if mc = 'S'
  save screen to vtela
  mens = 'Impressora pronta para imprimir Notas Fiscais ?'
  do pegresp with [SN]
  if mc = 'S'
     stor 0 to tabco    &&--- Tabula‡„o de linha e coluna
     stor 2 to tabli    &&--- Tabula‡„o de linha e coluna
     mdt_entr = HOJE  &&--- Data de Entrada
     stor space(60) to mobsnf1, mobsnf2, mobsnf3, mobsnf4
     do while .t.
       rest screen from vtela
       stor space(60) to mobsnf1, mobsnf2, mobsnf3, mobsnf4
       stor 0 to micm
       sele tm1
       zap
       sele clien
       **------------ Cabecalho da Nota Fiscal -------------------
       =mensag('Tecle ESC para terminar')
       @04,05 say 'Data Entrada....                                 Nota Fiscal N§'
       @04,69 say mnnf pict '999999'
       @05,05 say 'Nome de Guerra..'
       @06,05 say 'Remetente.......'
       @07,05 say 'Ins. CGC / CPF..'
       @08,05 say 'Endere‡o........'
       @09,05 say 'Bairro..........                            CEP.............'
       @10,05 say 'Munic¡pio.......'
       @11,05 say 'Fone/Fax........     -                      Estado..........'
       @12,05 say 'Ins. Estadual...'
       @14,05 say 'Observa‡”es:'
       @18,05 say 'Motivo:'
       @04,22 get mdt_entr pict '@!'                   valid !empty(mdt_entr)
       @05,22 get mgue     pict '@!'                   valid av07900gue()
       @06,22 get mraz     pict '@!' when av07900whe() valid av07900val()
       @07,22 get mcgc     pict '@!' when av07900whe() valid av07900val()
       @08,22 get mend     pict '@!' when av07900whe() valid av07900val()
       @09,22 get mbai     pict '@!' when av07900whe()
       @09,66 get mcep     pict '@!' when av07900whe()
       @10,22 get mcid     pict '@!' when av07900whe() valid av07900val()
       @11,22 get mddd1    pict '@!' when av07900whe()
       @11,27 get mfon1    pict '@!' when av07900whe()
       @11,66 get mcest    pict '@!' when av07900whe() valid av07900val()
       @12,22 get mins     pict '@!' when av07900whe() valid av07900val()
       @15,10 get mobsnf1  pict '@!'
       @16,10 get mobsnf2  pict '@!'
       @19,10 get mobsnf3  pict '@!'
       @20,10 get mobsnf4  pict '@!'
       read
       if lastkey() = 27
         exit
       endif
       **------------ Itens da Nota Fiscal -------------------
       if av07900ite()
         mens = dadcorr
         do pegresp with [SN]
         if mc = 'S'
           **------------------------- Imprime NOTA FISCAL --------------------------
           sele nfcab
           append blank
           replace nnf       with  mnnf       ,    cgc     with     mcgc      ,;
                   dt_entr   with  mdt_entr   ,    demi    with     HOJE    ,;
                   gue       with  mgue       ,    obs1    with     mobsnf1   ,;
                   obs2      with  mobsnf2    ,    obs3      with  mobsnf3    ,;
                   obs4      with  mobsnf4
           =mensag('IMPRIMINDO NOTA FISCAL')
           do av07900imp
           stor space(60) to mobsnf1, mobsnf2, mobsnf3, mobsnf4
           sele param
           replace nnf       with mnnf
           replace DTEMISSAO WITH HOJE
           sele clien
           mnnf = mnnf + 1
           mgue  = space(len(clien->gue ))
           do av07900let
         endif
       else
         =mensag('Nota n„o completada, faltam itens - ENTER')
         =INKEY(0)
       endif
     enddo
     rest screen from vtela
  endif
else
  if mc = 'N'
    =mensag('Atualize Parƒmetro para emiss„o de Nota Fiscal - ENTER')
    =INKEY(0)
  endif
endif

close data
if file('arqtmp1.dbf')
   erase arqtmp1.dbf
   erase arqtmp1.idx
endif
set deci to
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function av07900whe
if empty(mgue)
   return .t.
endif
return .f.
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
procedure av07900let
mraz  = space(len(clien->raz ))
mcgc  = space(len(clien->cgc ))
mend  = space(len(clien->end ))
mbai  = space(len(clien->bai ))
mcep  = space(len(clien->cep ))
mcid  = space(len(clien->cid ))
mddd1 = space(len(clien->ddd1))
mfon1 = space(len(clien->fon1))
mcest = space(len(clien->est ))
mins  = space(len(clien->ins ))
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function av07900gue
vretu = .t.
if !empty(mgue)
   =seek(alltrim(mgue),'clien')
   save screen
   =MOSTRA(12,01,21,78)
   BROWSE FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
   CID:R:H='Cidade',END:R:H='Endere‡o' IN WINDOW TELA
   mgue = gue
   deactivate window tela
   rest screen
   if seek(alltrim(mgue),'clien')
      mraz  = clien->raz
      mcgc  = clien->cgc
      mend  = clien->end
      mbai  = clien->bai
      mcep  = clien->cep
      mcid  = clien->cid
      mddd1 = clien->ddd1
      mfon1 = clien->fon1
      mcest = clien->est
      mins  = clien->ins
      @06,22 say mraz
      @07,22 say mcgc
      @08,22 say mend
      @09,22 say mbai
      @09,66 say mcep
      @10,22 say mcid
      @11,22 say mddd1
      @11,27 say mfon1
      @11,66 say mcest
      @12,22 say mins
      if seek(mcest,'estados')
         micm = estados->icm
      endif
   endif
else
   do av07900let
endif
return vretu
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function av07900ite
private telalixo, vretu
vretu = .t.
save screen to telalixo
rest screen from vtela
sele prod
mitem = 1
@08,02 say 'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@09,02 say '³ RAZAO -                                                    ³ NF.         ³'
@10,02 say 'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄ´'
@11,02 say '³ ITEM -      ³                   D E S C R I C A O                        ³'
@12,02 say 'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄ´                                                            ³'
@13,02 say '³ REF. -      ³                                                            ³'
@14,02 say 'ÃÄÄÄÄÂÄÄÄÄÂÄÄÄÁÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ´'
@15,02 say '³ CF ³ ST ³ UN  ³ QUANT. ³  UNITARIO  ³  T O T A L  ³ ICMS ³ IPI  ³ VLR IPI³'
@16,02 say 'ÃÍÍÍÍÅÍÍÍÍÅÍÍÍÍÍÅÍÍÍÍÍÍÍÍÅÍÍÍÍÍÍÍÍÍÍÍÍÅÍÍÍÍÍÍÍÍÍÍÍÍÍÅÍÍÍÍÍÍÅÍÍÍÍÍÍÅÍÍÍÍÍÍÍÍ´'
@17,02 say '³    ³    ³     ³        ³            ³             ³      ³      ³        ³'
@18,02 say 'ÀÄÄÄÄÁÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ'
@09,12 say mraz
@09,69 say mnnf PICT '999999'
do while .t.
   =mensag('Tecle ESC para terminar')
   mch_item = 'NOVO'
   mdes   = spac(len(prod->des))
   mdes2  = spac(len(prod->des2))
   mccl   = spac(len(prod->ccl))
   mcodt  = spac(len(prod->ccl))
*   muni   = spac(len(prod->uni))
   muni   = spac(len(prod->EMBALAGEM))
   mref   = spac(len(prod->ref))
   stor 0 to mqtd, mpreu, mipi
   @11,12 get mitem pict '99'   valid av07900val()
   @13,11 get mref  pict '9999' valid av07900val()
   @17,09 get mcodt                     pict '!'
   @17,20 get mqtd                      pict '999999'      valid av07900val()
   @17,29 get mpreu                     pict '999999.9999' valid av07900val()
   read
   if lastkey() = 27
      exit
   endif
   @17,42 say round(mpreu * mqtd,2)                         pict '99999999.99'
   @17,63 say mipi                                          pict '99.9'
   @17,70 say round(round(mpreu * mqtd,2) * (mipi / 100),2) pict '999.99'
   mens = dadcorr
   save screen
   do pegresp with [SN]
   rest screen
   if mc = 'S'
      sele tm1
      if mch_item = 'NOVO'
         append blank
      endif
      replace cgc with mcgc  ,  nnf  with mnnf                      ,;
              ref with mref  ,  des  with mdes  , des2 with mdes2   ,;
              ccl with mccl  ,  codt with mcodt , uni  with muni    ,;
              qtd with mqtd  ,  preu with mpreu , icm  with micm    ,;
              ipi with mipi  ,  item with mitem
      sele prod
      mitem = mitem + 1
   endif
enddo
sele tm1
if reccount() = 0
   vretu = .f.
endif
sele prod
rest screen from telalixo
return vretu
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function av07900VAL
private vretu
save screen to lixo
vretu = .t.
if varread() = 'MREF'
   if seek(mref,'prod')
      mdes  = prod->des
      mdes2 = prod->des2
      mccl  = prod->ccl
*     mipi  = prod->ipi
      MIPI = 0 &&---- Regime do Imposto Simples
*     muni  = prod->uni
      muni  = prod->EMBALAGEM
      @12,18 say mdes
      @13,18 say mdes2
      @17,04 say mccl
      @17,14 say muni
      @17,56 say micm                                          pict '99.9'
   else
      =mensag('REFERENCIA INCORRETA   -   Qualquer tecla para continuar')
      =INKEY(0)
      rest screen from lixo
      vretu = .f.
   endif
endif
if varread() = 'MCEST'
   if seek(mcest,'estados')
      micm = estados->ICM
   else
      =mensag('ESTADO INEXISTENTE  -  Qualquer tecla para continuar')
      =INKEY(0)
      rest screen from lixo
      vretu = .f.
   endif
endif
if varread() = 'MRAZ'
   if empty(mraz)
      =mensag('RAZAO SOCIAL ESTA INCORRETA  -   Qualquer tecla para continuar')
      =INKEY(0)
      rest screen from lixo
      vretu = .f.
   endif
endif
if varread() = 'MCGC'
   if empty(mcgc)
      =mensag('CGC INCORRETO  -   Qualquer tecla para continuar')
      =INKEY(0)
      rest screen from lixo
      vretu = .f.
   endif
endif
if varread() = 'MEND'
   if empty(mend)
      =mensag('ENDERECO NAO PODE ESTAR EM BRANCO  -   Qualquer tecla para continuar')
      =INKEY(0)
      rest screen from lixo
      vretu = .f.
   endif
endif
if varread() = 'MCID'
   if empty(mcid)
      =mensag('CIDADE NAO PODE ESTAR EM BRANCO  -  Qualquer tecla para continuar')
      =INKEY(0)
      rest screen from lixo
      vretu = .f.
   endif
endif
if varread() = 'MINS'
   if empty(mins)
      =mensag('INSCRICAO ESTADUAL INCORRETA  -  Qualquer tecla para continuar')
      =INKEY(0)
      rest screen from lixo
      vretu = .f.
   endif
endif
if varread() = 'MITEM'
   if mitem < 1 .and. mitem > 24
      =mensag('INFORMAR ENTRE 1 E 24  -  Qualquer tecla para continuar')
      =INKEY(0)
      rest screen from lixo
      vretu = .f.
   endif
   if seek(str(mnnf,6)+str(mitem,2),'tm1')
      mch_item = 'VELHO'
      mref   = tm1->ref
      mcodt  = tm1->codt
      mqtd   = tm1->qtd
      mpreu  = tm1->preu
      mipi   = tm1->ipi
      mccl   = tm1->ccl
      muni   = tm1->uni
*      micm   = tm1->icm
      mipi   = tm1->ipi
      @13,11 say mref                      pict '9999'
      @17,09 say mcodt                     pict '!'
      @17,20 say mqtd                      pict '999999'
      @17,29 say mpreu                     pict '999999.9999'
      @17,42 say round(mpreu * mqtd,2)     pict '99999999.99'
      @17,63 say mipi                      pict '99.9'
      @17,70 say round(round(mpreu * mqtd,2) * (mipi / 100),2) pict '999.99'
   else
      mch_item = 'NOVO'
      mref   = spac(len(prod->ref))
      mcodt  = spac(len(prod->ccl))
      mdes   = spac(len(prod->des))
      mdes2  = spac(len(prod->des2))
      mccl   = spac(len(prod->ccl))
*      muni   = spac(len(prod->uni))
      muni   = spac(len(prod->EMBALAGEM))
      stor 0 to mqtd, mpreu, mipi
   endif
endif
if varread() = 'MQTD'
   if mqtd <= 0
      =mensag('QUANTIDADE NAO PODE SER 0 (ZERO)  -  Qualquer tecla para continuar')
      =INKEY(0)
      rest screen from lixo
      vretu = .f.
   endif
endif
if varread() = 'MPREU'
   if mpreu <= 0
      =mensag('PRECO UNITARIO NAO PODE SER 0 (ZERO)  -  Qualquer tecla para continuar')
      =INKEY(0)
      rest screen from lixo
      vretu = .f.
   endif
endif
return vretu
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
procedure av07900imp

SET DEVI TO PRINTER
* SET PRINTER TO ARQ.TXT
tamrel = 1
cComp = CHR(30) + '4' &&-- Compactacao 16,7 cpp 
cComp = cComp + chr(27) + 'C' + chr(78) &&-- Tam.da Pagina s¢ funciona em Emilia PC

set deci to 4
sele TM1
go  top
do while .not. eof()
   **----------------------- Dados da Nota Fiscal -------------------------
   @tabli+01,tabco+01 SAY cComp
   @tabli+02,tabco+118 say CHR(27)+'w'+CHR(1)
   @tabli+02,pcol() say mnnf pict '9 9 9 9 9 9'
   @tabli+02,pcol() say CHR(27)+'w'+CHR(0)       &&--- N£mero
   @tabli+03,tabco+098 say 'X'                   &&--- Nf. de Entrada
   @tabli+07,tabco+002 say 'RETORNO DE VENDA'    &&--- Natureza Opera‡„o
   @tabli+07,tabco+045 say '1.31'                &&--- CFOP
   **----------------------- Destinat rio ---------------------------------
   @tabli+10,tabco+002 say mraz                  &&--- Razao social
   @tabli+10,tabco+085 say mcgc                  &&--- Ins. CGC
   @tabli+10,tabco+123 say HOJE                &&--- Data de emiss„o
   @tabli+12,tabco+002 say mend                  &&--- Endere‡o
   @tabli+12,tabco+072 say mbai                  &&--- Bairro
   @tabli+12,tabco+103 say mcep                  &&--- CEP
   @tabli+14,tabco+002 say mcid                  &&--- Cidade
   if !empty(mddd1)                              &&--- DDD e Telefone
      @tabli+14,tabco+052 say '('+alltrim(mDDD1)+')'+ mFON1
   else
      @tabli+14,tabco+052 say mFON1
   endif
   @tabli+14,tabco+075 say mcest                 &&--- Estado
   @tabli+14,tabco+086 say mins                  &&--- Inscricao
   **----------------------- Imprime os itens da Nf. -------------------------
   lin  = 23
   stor 0 to mtot, mtotipi
   do while .not. eof()
      @tabli+lin,tabco+001 say ref              &&- Referencia
      if !empty(des)
         @tabli+lin,tabco+006 say des           &&- Descricao
         if !empty(des2)
            lin = lin + 1
            @tabli+lin,tabco+006 say des2       &&- Descricao
         endif
      endif
      @tabli+lin,tabco+069 say ccl                     &&- Class. Fiscal
      @tabli+lin,tabco+073 say codt                    &&- Class. Fiscal
      @tabli+lin,tabco+077 say uni                     &&- Unidade
      @tabli+lin,tabco+083 say qtd   pict '999999'     &&- Quantidade
      @tabli+lin,tabco+091 say preu   pict '999999.9999'       &&- Vlr. Unit rio
      mvlr = round(qtd * preu,2)
      @tabli+lin,tabco+103 say mvlr   pict '99,999,999.99'     &&- Vlr. Total
      mtot = mtot + mvlr
      if icm > 0
         @tabli+lin,tabco+119 say icm   pict '@z 99.9'         &&- % Icm
      endif
      if ipi > 0
         @tabli+lin,tabco+124 say ipi pict '@z 99.9'            &&- % ipi
         mipi = round(mvlr * (ipi / 100),2)
         @tabli+lin,tabco+128 say mipi  pict '@z 999.99'        &&- Vlr IPI
         mtotipi = mtotipi + mipi
      endif
      sele nfite
      append blank
      replace nnf       with  tm1->nnf      ,    cgc     with     tm1->cgc      ,;
              item      with  tm1->item     ,    ref     with     tm1->ref      ,;
              des       with  tm1->des      ,    des2    with     tm1->des2     ,;
              ccl       with  tm1->ccl      ,    codt    with     tm1->codt     ,;
              uni       with  tm1->uni      ,    qtd     with     tm1->qtd      ,;
              preu      with  tm1->preu     ,    icm     with     tm1->icm      ,;
              ipi       with  tm1->ipi
      lin = lin + 1
      sele TM1
      skip
   enddo
   **---------------------- Outros Dados no Corpo do Pedido -------------------
   if !empty(mobsnf1)
      @tabli+lin,tabco+011 say 'Observacao: ' + mobsnf1 + mobsnf2
      lin = lin + 1
   endif
   if !empty(mobsnf3)
      @tabli+lin,tabco+011 say 'Motivo    : ' + mobsnf3 + mobsnf4
      lin = lin + 1
   endif
   **------------------- Calculo do Imposto --------------------------------------
   if micm > 0
      @tabli+49,tabco+002 say mtot      picture '999,999,999.99'      && base icms
      @tabli+49,tabco+028 say round(mtot * (micm/100),2)  pict '999,999,999.99'      && Vlr icms
   endif
   @tabli+49,tabco+110 say mtot      picture '999,999,999.99'   && Total Produtos
   @tabli+51,tabco+082 say mtotipi   picture '999,999,999.99'   && Vlr IPI
   @tabli+51,tabco+110 say mtot+mtotipi    picture '999,999,999.99'   && Total NF
   @tabli+67,tabco+002 say mgue
   @tabli+67,tabco+047 say cSigla
   @tabli+71,tabco+118 say CHR(27)+'w'+CHR(1)
   @tabli+71,pcol() say mnnf pict '9 9 9 9 9 9'
   @tabli+71,pcol() say CHR(27)+'w'+CHR(0)      &&--- N£mero
   @78,00
   SELE CABECNF
   IF ! SEEK( MNNF, 'CABECNF' )
      append blank
   ENDIF
   repl CGC      WITH MCGC,;
        DEMISSAO WITH HOJE,;
        VBASEICM with 0,;
        NNF      with MNNF,;
        VALPROD  with MTOT,;
        VALIPI   with MTOTIPI,;
        CANCELADA with .F.,;
        DTCANCEL  WITH CTOD(''),;
        TRA       WITH TRA,;
        TIPNF     WITH '1.31'
   SELE TM1     
enddo
=INICPAG()
SET PRINTER TO
set devi to screen

return

*================
FUNCTION GETSIGLA
*================ 
  PRIVATE lRet
  
  lRet = .F.
  do while .t.
    sele siglas
    msigla = space(04)
    @23,01 say space(78)
    @23,10 say 'Informe sua Sigla 'get msigla pict '@!'
    read
    
    if seek(msigla)
      lRet = .T.
      exit
    else
      =mensag('Sigla n„o cadastrada - ENTER')
      =INKEY(0)
    endif
  enddo
  
RETURN lRet  
