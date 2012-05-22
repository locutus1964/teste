* AV074203 - NOTA FISCAL - IMPRESSAO DE NOTAS ON LINE / BATCH
* SISTENA ADMINISTRACAO DE VENDAS - 09-07-90
* ADAO ELI PEREIRA
*
close data
=INICPAG()
mens = 'Impressora pronta para imprimir Notas Fiscais ?'
do pegresp with [SN]
if MC = 'N' .or. empty(mc)
   return
endif

sele 0
USE SIGLAS INDEX SIGLAS.IDX
sele 0
use PARAM
UNNF = 0
MNNF = 1
if !bof()
   UNNF = NNF
   MNNF = NNF + 1
endif
tipoimpr = PADRIMPR
=janela(17,39,22,78)
=borda(17,39,22,78)
@19,41 say 'Ultima Nota Fiscal emitida   '+padl(UNNF,7,'0')
@20,41 say 'Pr¢xima Nota Fiscal a emitir '+padl(MNNF,7,'0')
mens = DADCORR
do pegresp with [SN]
if MC = 'N' .or. empty(mc)
   =mensag('Atualize Parƒmetro para emiss„o de Nota Fiscal - ENTER')
   =INKEY(0)
   close data
   return
endif
sele 0
use PEDIC2  index PEDIC2.IDX
if bof()
   =mensag('N„o existem Pedidos aprovados - ENTER para encerrar')
   =INKEY(0)
   close data
   return
endif


IF ! ChkDataNf()
  CLOSE DATA
  RETURN
ENDIF

IF ! GetSigla() 
   CLOSE DATA
   RETURN
ENDIF

cSigla = SIGLAS->SIGLA
   
=mensag('Preparando a emiss„o das Notas Fiscais... Aguarde')
public VENDUP1,VENDUP2,VENDUP3,VENDUP4,VENDUP5,VENDUP6,VENDUP7,VENDUP8
public VDUP1,VDUP2,VDUP3,VDUP4,VDUP5,VDUP6,VDUP7,VDUP8,MCPD
public MCP1,MCP2,MCP3,MCP4,MPE1,MPE2,MPE3,MPE4

VDUP1 = 0
VDUP2 = 0
VDUP3 = 0
VDUP4 = 0
* Abre as tabelas
sele 0
use CADMOEDA index CADMOEDA.IDX
sele 0
use OBSNOTA index OBSNOTA.IDX
sele 0
use DUPL    index DUPL.IDX, DUPCGC.IDX
sele 0
use PROD    index PROD.IDX
sele 0
use CADNF   index CADNF.IDX, NFREF.IDX, NFPED.IDX, NUMNF.IDX
sele 0
use TRANS   index TRANS.IDX, TRARAZ.IDX
sele 0
use CLIEN   index CLIEN.IDX, GUERRA.IDX, RAZAO.IDX
sele 0
use PEDID   index PEDIDM.IDX, PEDID.IDX
sele 0
use PEDIC   index PEDIC.IDX
sele 0
use TPNF    index TPNF.IDX
sele 0
use CLI_COBR index CLI_COBR.IDX
sele 0
use CLI_ENTR index CLI_ENTR.IDX
sele 0
use TABFIN2 index TABFIN2.IDX
sele 0
use VENDUNIC index VENDUNIC.IDX
sele 0
use VENDPARC index VENDPARC.IDX
sele 0
use CABECNF  index CABECNF.IDX

* Configura a impressora
lp=getenv( "lp" )
if empty(lp)
   lp = "LPT1"
endif   
SET PRINT TO &lp

*--- EMILIA PC
*??? chr(27) + chr(64)
*??? chr(27) + chr(30) + '4'

set device to print
stor 0 to tabco    &&--- Tabula‡„o de linha e coluna
stor 2 to tabli    &&--- Tabula‡„o de linha e coluna
stor 0 to vcontnf  &&--- Contador de notas fiscais emitidas
vfre_resp = 2      &&--- Frete por conta do destinat rio
set deci to 4
fator = 0.0
nIcm  = 0.0
sele PEDIC2
do while .not. eof()
   if seek(str(pedic2->PED,6),'pedic')
      lPesFisica = CLIEN->FIJU = 'F'
      =seek(pedic2->CGC+str(pedic2->ped,6),'obsnota')
      ** mnbanco  = pedic->nbanco
      mnbanco  = obsnota->banco
      ** magencia = pedic->agencia
      magencia = obsnota->agencia
      stor 0 to ivdic, vdic, mpel
      chave=str(pedic->MOEDA,3)+subs(dtoc(date()),7,2)+'/'+subs(dtoc(date()),4,2)+'/'+subs(dtoc(date()),1,2)
      if !seek( chave,'cadmoeda')
         set device to screen
         =mensag('Moeda do pedido n„o cadastrada para hoje')
         =INKEY(0)
         close data
         set deci to
         return
      endif
      if !seek(pedic2->TIP,'tpnf')
         set device to screen
         =mensag('Tipo de Nota Fiscal '+MTIP+' n„o cadastrado - ENTER')
         =INKEY(0)
         close data
         set deci to
         return
      endif
      ** =seek(pedic2->CGC,'obsnota')
      =seek(pedic2->CGC+str(pedic2->ped,6),'obsnota')
      mpliquido = obsnota->pliquido
      =seek(pedic2->CGC,'clien')
      **----------------------- Dados da Nota Fiscal -------------------------
      if vcontnf = 0
         @prow(),pcol() say chr(27) + chr(64)
         @prow(),pcol() say chr(30) + '4'
         @prow(),pcol() say chr(27) + 'C' + chr(78)
      endif
      vcontnf = vcontnf + 1

      @tabli+02,tabco+118 say CHR(27)+'w'+CHR(1)
      @tabli+02,pcol() say padl(MNNF,6,'0')
      @tabli+02,pcol() say CHR(27)+'w'+CHR(0)   &&--- N£mero

      @tabli+03,tabco+083 say 'X'                   &&--- Nf. de sa¡da
      IF lPesFisica 
        cNatOp = tpnf->desn
      ELSE
        cNatOp = 'VENDA CONSUMIDOR'
      ENDIF    
      @tabli+07,tabco+002 say cNatOp                &&--- Natureza Opera‡„o
      @tabli+07,tabco+045 say tpnf->nope            &&--- CFOP
      **  @tabli+07,tabco+058 say '*'                   &&--- Ins. Estad. substituto tribut rio
      **  @tabli+07,tabco+120 say '*'                   &&--- Data limite p/emissao
      **----------------------- Destinat rio ---------------------------------
      @tabli+10,tabco+002 say clien->RAZ            &&--- Razao social
      @tabli+10,tabco+085 say pedic2->CGC           &&--- Ins. CGC
      @tabli+10,tabco+123 say date()                &&--- Data de emiss„o

      @tabli+12,tabco+002 say clien->END            &&--- Endere‡o
      @tabli+12,tabco+072 say clien->BAI            &&--- Bairro
      @tabli+12,tabco+103 say clien->CEP            &&--- CEP
      **     @tabli+12,tabco+120 say '*'                   &&--- Data de Sa¡da
      @tabli+14,tabco+002 say clien->CID
      if !empty(clien->ddd1)                        &&--- DDD e Telefone
         @tabli+14,tabco+052 say '('+alltrim(clien->DDD1)+')'+ clien->FON1
      else
         @tabli+14,tabco+052 say clien->FON1
      endif
      @tabli+14,tabco+075 say clien->EST            &&--- Estado
      @tabli+14,tabco+086 say clien->INS
      **     @tabli+14,tabco+114 say time()                &&--- Hora da Sa¡da
      **----------------------- Fatura ----------------------------------------
      @tabli+17,tabco+002 say date()
      @tabli+17,tabco+025 say padl(MNNF,6,'0')    &&--- N£mero
      **  @tabli+17,tabco+040 say 'Vide abaixo'         &&--- Valor
      @tabli+17,tabco+039 say 'Vide abaixo ('+str(mnbanco,3)+')'     &&--- Valor/Banco
      @tabli+17,tabco+074 say 'Vide abaixo'         &&--- N§ da duplicata
      @tabli+17,tabco+095 say 'Vide abaixo'         &&--- Vencimento
      =seek(subs(pedic2->CGC,1,16),'cli_cobr')
      if !empty(cli_cobr->CIDC)
         @tabli+19,tabco+002 say alltrim(cli_cobr->endc)
         @tabli+19,pcol()+01 say '-'+alltrim(cli_cobr->baic)+'-'+alltrim(cli_cobr->cepc)+'-'+alltrim(cli_cobr->cidc)+'-'+cli_cobr->estc
      else
         @tabli+19,tabco+002 say alltrim(clien->end)
         @tabli+19,pcol()+01 say '-'+alltrim(clien->cid)+'-'+clien->est
      endif
      **----------------------- Atualiza campo da £ltima compra --------------
      sele clien
      repl ULC with date()
      **----------------------- Dados do produto -----------------------------
      lin = 23
      stor 0 to VTOT, GTOT, VIPI, GIPI, BASEIPI, VTNF, TOTSIPI, BASEICM, BASEISS
      stor 0 to CTOTA, TOTIPI, VUNIT, DICM, TOTPES, TPES, MPES, GICM, VICM
      sele PEDID
      =seek(str(pedic2->PED,6))
      do while !eof() .and. ped = pedic2->ped
         if QTDAT # 0
            if seek(pedid->REF,'prod')
               sele PEDID
               VALIQU = QTDAT * (PREBRU * cadmoeda->ccz)
               stor 0 to MDESUNIC
               if DESCGRU > 0
                  VALIQU = VALIQU - ((VALIQU * DESCGRU) / 100)
               endif
               if pedic->DESCTO1 > 0
                  VALIQU = VALIQU - ((VALIQU * pedic->DESCTO1) / 100)
               endif
               if pedic->DESCTO2 > 0
                  VALIQU = VALIQU - ((VALIQU * pedic->DESCTO2) / 100)
               endif
               if pedid->DIT > 0
                  VALIQU = VALIQU - ((VALIQU * pedid->DIT) / 100)
               endif
               if pedid->BON > 0
                  VALIQU = VALIQU - ((VALIQU * pedid->BON) / 100)
               endif
               if pedic->DESCSEM > 0
                  VALIQU = VALIQU - ((VALIQU * pedic->DESCSEM) / 100)
               endif

               nDesp = DespFinanc( VALIQU, pedic->CFINANC,;
                                           pedic->CP1, PEDIC->PE1,; 
                                           pedic->CP2, PEDIC->PE2,; 
                                           pedic->CP3, PEDIC->PE3,;
                                           pedic->CP4, PEDIC->PE4 )          
               FATOR  = ((VALIQU + nDesp) / VALIQU) - 1
               VALIQU = VALIQU * (1 + FATOR) 
               valiqu = round(valiqu,2)
               VTOTA = valiqu
               if tpnf->IPIT = '1' .or. tpnf->IPIT = '3'
                  if prod->IPI > 0
                     VIPI = round((VTOTA * (prod->IPI/100)),2)
                     GIPI = (VIPI + GIPI)
                     BASEIPI = (BASEIPI + VTOTA)
                  endif
               endif
               if tpnf->ICMT = '1' .or. tpnf->ICMT = '3'
                  * Aliquota de 18.0 % para pessoas fisicas
                  IF lPesFisica
                    nICM = 18.0
                  ELSE  
                    nICM = pedic2->ICM
                  ENDIF  
                  if tpnf->CONS_CI = 'S'
                     VICM = round(((VTOTA + VIPI) * (nICM/100)),2)
                     GICM = (VICM + GICM)
                     BASEICM = (BASEICM + VTOTA + VIPI)
                  else
                     VICM = round((VTOTA * (nICM/100)),2)
                     GICM = (VICM + GICM)
                     BASEICM = (BASEICM + VTOTA)
                  endif
               endif
               sele PEDID
               @tabli+lin,tabco+001 say prod->REF               &&- Referencia
               @tabli+lin,tabco+006 say prod->DES               &&- Descricao
               @tabli+lin,tabco+069 say prod->CCL               &&- Class. Fiscal
               @tabli+lin,tabco+073 say tpnf->CODT              &&- Class. Fiscal
               * @tabli+lin,tabco+074 say '*'                   &&- Sit. Tribut ria
               * @tabli+lin,tabco+077 say prod->uni             &&- Unidade
               @tabli+lin,tabco+077 say prod->EMBALAGEM         &&- Unidade
               @tabli+lin,tabco+083 say qtdat  pict '999999'    &&- Quantidade
               @tabli+lin,tabco+091 say (VALIQU / ;
               pedid->QTDAT) pict '999999.9999'                 &&- Vlr. Unit rio
               VTOTAAR = (pedid->QTDAT * (VALIQU / pedid->QTDAT))
               CTOTA   = (VTOTA + CTOTA)
               VTOTA   = round(VTOTAAR,2)
               @tabli+lin,tabco+103 say vtota     ;
               pict '99,999,999.99'                              &&- Vlr. Total
               @tabli+lin,tabco+119 say nIcm ;
               pict '@z 99.9'                                    &&- % Icm
               @tabli+lin,tabco+124 say prod->ipi ;
               pict '@z 99.9'                                    &&- % ipi
               @tabli+lin,tabco+128 say vipi      ;
               pict '@z 999.99'                             &&- Vlr IPI
               if val(clien->DIC) > 0
                  VDICAR = (VTOT * (val(clien->DIC) / 100))
                  IVDIC  = ROUND(VDICAR,2)
                  VDIC   = (IVDIC + VDIC)
                  VTOT   = (VTOT - IVDIC)
                  GTOT   = (GTOT - IVDIC)
               endif
               if obsnota->valoricm > 0
                  VICM = obsnota->valoricm
               endif
               repl SAL with SAL - QTDAT
               do GravItemNota
               stor 0.00 to VIPI, IVDIC, VTOTA, MVISS, VICM, VISSP
               lin = lin + 1
               sele PROD
               repl COM    with    COM + pedid->QTDAT    ,;
                    PEA    with    PEA - pedid->QTDAT
               MPLIQUIDO = (MPLIQUIDO + (pedid->QTDAT * MPEL))
               sele PEDID
               repl QTDAT  with    0    ,  NNF with MNNF
            endif
         endif
         skip
      enddo
      **-------------------- Outros Dados no Corpo do Pedido ----------------
      **      if pedic2->DESCONTO > 0.00
      **         @tabli+lin,tabco+049 say 'Desconto Especial:'
      **         @tabli+lin,tabco+107 say pedic2->DESCONTO  ;
      **         pict '99,999,999.99'                             &&- Desc. Especial
      **         CTOTA = (CTOTA - pedic2->DESCONTO)
      **         lin = lin + 1
      **      endif
      sele PEDIC2
      if val(clien->DIC) > 0
         @tabli+lin,tabco+10 say 'INSCR.SUFR.: '+clien->ISU
         @tabli+lin,tabco+40 say 'MUNICIPIO: '+clien->CMU
         @tabli+lin,tabco+60 say '% ICM: '+clien->DIC
         @tabli+lin,tabco+70 say 'VAL DESC ICM: '
         @tabli+lin,tabco+85 say VDIC                  picture "9999,999,999.99"
      endif
      =av074201ob(obsnota->obsnf1,obsnota->obsnf2)
      =av074201ob(obsnota->obsnf3,obsnota->obsnf4)
      lin = lin + 1
      if seek(subs(pedic2->CGC,1,16),'cli_entr')
         @tabli+lin,tabco+002 say 'Local Entrega:'
         ** @tabli+lin,pcol()+1 say alltrim(cli_entr->ENDE)+' '+alltrim(cli_entr->BAIE)
         ** @tabli+lin,pcol()+1 say ' - ' + alltrim(cli_entr->CEPE)+' / '+alltrim(cli_entr->cide)+'-'+cli_entr->ESTE
         @tabli+lin,pcol()+1 say alltrim(cli_entr->ende) +' '+ alltrim(cli_entr->cide)
         @tabli+lin,pcol()+1 say alltrim(cli_entr->baie) +' '+ alltrim(cli_entr->este) +' '+ alltrim(cli_entr->cepe)
         if !empty(cli_entr->ddd1e)
            @tabli+lin,pcol()+1 say '('+ alltrim(cli_entr->ddd1e) +')'+ alltrim(cli_entr->fon1e)
         else
            @tabli+lin,pcol()+1 say alltrim(cli_entr->fon1e)
         endif
      endif
      **------------------- Calculo do Imposto --------------------------------------
      @tabli+49,tabco+002 say baseicm   picture '999,999,999.99'      && base icms
      @tabli+49,tabco+028 say GICM      picture '999,999,999.99'      && Vlr icms
      **     @tabli+49,tabco+054 say '*'                              && Base Icms substituicao
      **     @tabli+49,tabco+082 say '*'                              && Vlr. Icms substituicao
      @tabli+49,tabco+110 say CTOTA     picture '999,999,999.99'      && Total Produtos
      **      @tabli+51,tabco+002 say obsnota->fre_vlr pict '999999999.99'    && Vlr frete
      **      @tabli+51,tabco+028 say obsnota->seg_vlr pict '999999999.99'    && Vlr Seguro
      **      @tabli+51,tabco+054 say obsnota->out_vlr pict '999999999.99'    && Outras despesas
      @tabli+51,tabco+082 say GIPI      picture '999,999,999.99'      && Vlr IPI
      TOTNF = (CTOTA + GIPI)
      @tabli+51,tabco+110 say TOTNF     picture '999,999,999.99' && Total NF
      BGIPI = GIPI
      **------------------- Transportador / Volumes Transportados ------------------
      =seek(pedic2->TRA,'trans')
      @tabli+54,tabco+002 say trans->raz                   && Razao social
      ** @tabli+54,tabco+079 say obsnota->fre_resp pict '9' && Frete por conta do emitente
      @tabli+54,tabco+077 say vfre_resp pict '9'         && Frete por conta do emitente
      @tabli+54,tabco+087 say obsnota->veiculo           && Placa ve¡culo
      @tabli+54,tabco+100 say trans->est                 && Estado
      if !empty(trans->cgc)
         @tabli+54,tabco+110 say trans->cgc pict '@R 99.999.999/9999-99' && CGC
      endif
      @tabli+56,tabco+002 say trans->end                 && Endereco
      @tabli+56,tabco+065 say trans->cid                 && Cidade
      @tabli+56,tabco+100 say trans->est                 && UF
      @tabli+56,tabco+110 say trans->ins                 && Inscricao

      @tabli+58,tabco+010 say obsnota->QTDVOL  picture '99,999'    && Qtd Volumes
      @tabli+58,tabco+023 say obsnota->ESPECIE                     && Especie
      **     @tabli+58,tabco+045 say '*'                                  && Marca
      **     @tabli+58,tabco+065 say '*'                                  && Numero
      @tabli+58,tabco+095 say obsnota->PBRUTO  picture '99,999.99' && Peso Bruto
      @tabli+58,tabco+120 say MPLIQUIDO        picture '99,999.99' && Peso Liquido
      **---------------------------------------------------------------------------
      **                             Dados Adicionais
      **                 C A L C U L A     V E N C I M E N T O S
      **---------------------------------------------------------------------------
      sele CLIEN
      seek pedic2->CGC
      MVES = VES
      MDEB = 0
      MDEB = DEB
      GTOT = CTOTA
      IF TPNF->EDUP = 'S'
        do CalcVencim
      ENDIF   
      if VDUP1 > 0
         VDUP1 = VDUP1 + GIPI
         @tabli+62,tabco+002 say padl(MNNF,6,'0')+'/1'
         @tabli+62,tabco+013 say VDUP1                  picture "99999,999,999.99"
         @tabli+62,tabco+035 say VENDUP1
         =CriarDupl( MNNF, 1, VDUP1, VENDUP1, ROUND((VDUP1 / cadmoeda->ccz),4))
      endif
      if VDUP2 > 0
         @tabli+63,tabco+002 say padl(MNNF,6,'0')+'/2'
         @tabli+63,tabco+013 say VDUP2   picture "99999,999,999.99"
         @tabli+63,tabco+035 say VENDUP2
         =CriarDupl( MNNF, 2, VDUP2, VENDUP2, ROUND((VDUP2 / cadmoeda->ccz),4))
      endif
      if VDUP3 > 0
         @tabli+64,tabco+002 say padl(MNNF,6,'0')+'/3'
         @tabli+64,tabco+013 say VDUP3                  picture "99999,999,999.99"
         @tabli+64,tabco+035 say VENDUP3
         =CriarDupl( MNNF, 3, VDUP3, VENDUP3, ROUND((VDUP3 / cadmoeda->ccz),4))
      endif
      if VDUP4 > 0
         @tabli+65,tabco+002 say padl(MNNF,6,'0')+'/4'
         @tabli+65,tabco+013 say VDUP4                  picture "99999,999,999.99"
         @tabli+65,tabco+035 say VENDUP4
         =CriarDupl( MNNF, 4, VDUP4, VENDUP4, ROUND((VDUP4 / cadmoeda->ccz),4))
      endif
      **---------------------------------------------------------------------------
      **				                   DADOS DO CLIENTE
      **---------------------------------------------------------------------------
      @tabli+67,tabco+002 say clien->gue
      @tabli+67,tabco+024 say pedic->pcl
      @tabli+67,tabco+040 say str(pedic2->ped,6) + ' / ' + cSigla
      @tabli+67,tabco+060 say clien->regiao + ' / ' + pedic2->ven
      @tabli+69,tabco+002 say trans->localiz  && Localizacao no Guia 
      @tabli+71,tabco+118 say CHR(27)+'w'+CHR(1)
      @tabli+71,pcol() say padl(MNNF,6,'0')
      @tabli+71,pcol() say CHR(27)+'w'+CHR(0)      &&--- N£mero
      @ 78, 00                                

      sele CABECNF
      IF ! SEEK( STR(MNNF,6) )
        append blank
      ENDIF
      repl CGC      WITH CLIEN->CGC,;
           DEMISSAO WITH DATE(),;
           VBASEICM with BASEICM         ,;
           NNF      with MNNF            ,;
           VEN      with pedic2->VEN     ,;
           REGIAO   with clien->REGIAO   ,;
           PBRUTO   with obsnota->PBRUTO ,;
           PLIQUIDO with MPLIQUIDO       ,;
           VEICULO  with obsnota->VEICULO,;
           QTDVOL   with obsnota->QTDVOL ,;
           ESPECIE  with obsnota->ESPECIE,;
           VALPROD  with CTOTA,;
           VALIPI   with GIPI,;
           VALICM   with GICM,;
           CANCELADA with .F.,;
           DTCANCEL  WITH {},;
           TRA       WITH PEDIC2->TRA,;
           TIPNF     WITH PEDIC2->TIP,;
           PED       WITH PEDIC2->PED,;
           CP1       WITH PEDIC2->CP1,;
           CP2       WITH PEDIC2->CP2,;
           CP3       WITH PEDIC2->CP3,;                                
           CP4       WITH PEDIC2->CP4,;
           PE1       WITH PEDIC2->PE1,;                      
           PE2       WITH PEDIC2->PE2,;                      
           PE3       WITH PEDIC2->PE3,;                      
           PE4       WITH PEDIC2->PE4                                                       
      stor 0 to MPLIQUIDO
      sele PARAM
      repl NNF       with MNNF
      repl DTEMISSAO with DATE()
      sele PEDIC2
      skip
      MNNF = MNNF + 1
   else
      set device to screen
      =mensag('Pedido '+str(pedic2->PED,6)+' sem Cabe‡alho cadastrado - ENTER')
      =INKEY(0)
      set device to print
      skip
   endif
enddo
*if vcontnf # 0
*   eject
*endif
zap
sele PARAM
* repl NNF with MNNF
close data
* do RESEIMPR
=INICPAG()
set device to screen
set deci to
return

*==================
procedure AV074201Y
*==================
parameters vpar
private vpar, y
y = 'VER VENCTOS'
if vpar = 'VIS'
   y = 'A VISTA'
endif
if vpar = 'APR'
   y = 'CONTRA APRES'
endif
if vpar = 'SDB'
   y = 'SEM DEBITO'
endif
if vpar = 'ANT'
   y = 'ANTECIPADO'
endif
return y

*===================
procedure AV074201OB
*===================
parameters vpar1, vpar2
  if !empty(vpar1)
    lin = lin + 1
    @tabli+lin,tabco+011 say vpar1 + vpar2
  endif
return

*=====================
PROCEDURE GravItemNota
*=====================
  sele CADNF
  append blank
  replace ;
    CGC  with pedic2->CGC , TIP  with pedic2->TIP, REF  with prod->REF   ,;
    QTD  with pedid->QTDAT, IPI  with VIPI       , VAL  with VTOTA       ,;
    NNF  with MNNF        , CP1  with pedic2->CP1, CP2  with pedic2->CP2 ,;
    CP3  with pedic2->CP3, CP4  with pedic2->CP4 , EMBALAGEM  with prod->EMBALAGEM
  replace ;
    PE1  with pedic2->PE1,  PE2  with pedic2->PE2   , PE3  with pedic2->PE3 ,;
    PE4  with pedic2->PE4,  CPD  with pedic2->CPD   , VEN  with pedic2->VEN ,;
    PED  with pedic2->PED,  ICM  with VICM          , DICM with IVDIC       ,;
    DEMI with dtoc(date()), PINT with pedic2->PINT  , PCL  with pedic2->PCL
  replace ;
    NOPE with tpnf->NOPE  , DES  with prod->DES     , EDUP with tpnf->EDUP  ,;
    TRA  with pedic2->TRA , CCL  with prod->CCL     , BEST with tpnf->BEST  ,;
    BASEICM  with obsnota->BASEICM, DESCONTO with pedic2->DESCONTO
  replace PREU with (pedid->PREBRU * (1 + FATOR) * cadmoeda->ccz) 
return

*=================
FUNCTION CRIARDUPL 
*=================
PARAMETER nDUPL, nPARC, nVALOR, dVENC, nQTD_MOEDA
 
  SELECT DUPL
  APPEND BLANK
  REPLACE NDUP      WITH nDUPL,;
          PDUP      WITH nPARC,;
          VDP       WITH nValor,;
          VENC      WITH dVENC,;
          QTD_MOEDA WITH nQTD_MOEDA,;
          CGC       WITH PEDIC2->CGC,;
          EMIS      WITH DTOC(DATE()),;
          NBANCO    WITH MNBANCO,;
          AGENCIA   WITH MAGENCIA,;
          MOEDA     WITH PEDIC->MOEDA,;    
          VAL_MOEDA WITH CADMOEDA->CCZ,;
          TIPOCART  WITH 'CS'

RETURN

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
              