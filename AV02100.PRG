*
* AV02100 -  ATUALIZACAO DE TRANSPORTADORAS - 11/11/86 - 24/10/91
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA
*
SAVE SCREEN TO MAIN2_SCR
mdel = 0
mtra = SPACE(03)
vNOMEPRG = 'AV02100 '
do CABTELA with 0,0,24,79
@01,25 say 'Atualiza‡„o das Transportadoras'
@03,02 say 'C¢digo...........:   -                  Nome de Guerra.....:'
@05,02 say 'Raz„o Social.....:'
@07,02 say 'C. G. C. ........:                         Insc. Estad...:'
@09,02 say 'Endere‡o.........:'
@10,02 say 'Bairro/Cidade/UF.:                       -                             - '
@11,02 say 'CEP..............:                  FAX...:'
@13,02 say 'Telefone DDD/N§..:'
@15,02 say 'Via de Transp....:            Coleta:          Contato:'
@17,02 say 'Data/Resp. p/alt.:'
sele 0
use ctr_senh index ctr_senh
sele 0
use CLIEN   index CLITRANS,CLITRAOP
sele 0
use SIGLAS  index SIGLAS
sele 0
use ESTADOS index ESTADOS
sele 0
use TRANS index TRANS,TRAGUE,TRARAZ
DO WHILE .T.
   stor SPACE(02) to  mcest
   stor SPACE(04) to  mddd, mddd2, mddd_faxt, msigla
   stor SPACE(08) to  mdtalt, mcole
   MTEL = SPACE(09)
   MTEL2 = SPACE(09)
   MNUM_FAXT = SPACE(09)
   stor SPACE(09) to  mcep
   stor SPACE(10) to  mvia
   stor SPACE(15) to  mguetra, nguetra
   stor space(17) to  mins
   stor SPACE(20) to  mbai, mcontato, mcgc
   stor SPACE(24) to  mcid
   stor SPACE(45) to  mraz, nraz, mend
   ? MENSAG(ECODIGO)
   SELE TRANS
   GO BOTT
   PRITRA = VAL(TRA) + 1001
   MTRA = SUBS(STR(PRITRA,4),2,3)
   SET ORDER TO 2
   GO TOP
   do MONTATRA2
   @23,01 say space(78)
   @3,21 get mtra pict '@!' valid !empty(mtra) mess 'Campo nao pode estar em Branco'
   read
   if lastkey() = 27
      if mdel = 1
         pack
         mdel = 0
      endif
      close databases
      return
   endif
   SET ORDER TO 1
   if !seek(mtra)
      DEFINE WINDOW TRANS FROM 16,01 TO 21,78 COLOR &CORARQ
      @03,63 say mguetra    color &corver
      @05,21 say mraz       color &corver
      @07,21 say mcgc       pict '@r 99.999.999/9999-99' color &corver
      @07,61 say mins       color &corver
      @09,21 say mend       color &corver
      @10,21 say mbai       color &corver
      @10,46 say mcid       color &corver
      @10,76 say mcest      color &corver
      @11,21 say mcep       color &corver
      @11,46 say mddd_faxt  color &corver
      @11,51 say mnum_faxt  color &corver
      @13,21 say mddd       color &corver
      @13,26 say mtel       color &corver
      @13,39 say mddd2      color &corver
      @13,45 say mtel2      color &corver
      @15,21 say mvia       color &corver
      @15,39 say mcole      color &corver
      @15,58 say mcontato   color &corver
      @17,21 say mdtalt     COLOR &CORVER
      @17,37 say msigla     COLOR &CORVER
      @17,44 say space(30)  COLOR &CORVER
      mc = ' '
      MENS = testecad
      DO PEGRESP WITH [SN]
      if mc = 'N' .or. lastkey() = 27
         loop
      endif
   endif
   IF SEEK(MTRA)
      DEFINE WINDOW TRANS FROM 18,01 TO 22,78 COLOR &corarq
      MODI MEMO Observacao WINDOW TRANS NOWAIT
      DO MONTATRA1
      DO MONTATRA2
      mens =  testeinc
      do pegopcao with [123]
      if opcao = '1'
         loop
      endif
      if opcao = '2' .or. opcao = '3'
         do senha
      endif
      if opcao = '3'
         sele CLIEN
         if seek(mtra)
            mguecli = gue
            mens = 'Transportadora no Cliente '+mguecli+' - N„o pode excluir'
            ? MENSAG(mens)
            =INKEY(0)
            loop
         endif
         SET ORDER TO 2
         if seek(mtra)
            mguecli = gue
            mens = 'Transportadora opcional no Cliente '+mguecli+' - N„o pode excluir'
            ? MENSAG(mens)
            =INKEY(0)
            set order to 1
            loop
         endif
         sele TRANS
         delete
         MDEL = 1
         loop
      endif
   ENDIF
   @23,01 say space(78)
   sele TRANS
   close memo observacao
   @03,63 get mguetra pict '@!' valid !empty(mguetra) mess 'Campo nao pode estar em Branco'
   read
   if lastkey() = 27
      loop
   endif
   set order to 2
   seek mguetra
   if seek(mguetra) .and. tra <> mtra
      @03,21 say tra
      mtra = tra
      DO MONTATRA1
      DO MONTATRA2
      mens =  testeinc
      do pegopcao with [123]
      if opcao = '1'
         loop
      endif
      if opcao = '3'
         delete
         MDEL = 1
         loop
      endif
   endif
   @23,01 say space(78)
   @05,21 get mraz pict '@!' valid !empty(mraz) messa calpha
   @07,21 get mcgc pict '@r 99.999.999/9999-99'
   @07,61 get mins pict '@!'
   @09,21 get mend pict '@!'
   @10,21 get mbai pict '@!'
   @10,46 get mcid pict '@!'
   @10,76 get mcest pict '@!' valid AV02100est() messa calpha
   @11,21 get mcep pict '@!'
   @11,46 get mddd_faxt
   @11,51 get mnum_faxt
   @13,21 get mddd
   @13,26 get mtel
   @13,39 get mddd2
   @13,45 get mtel2
   @15,21 get mvia pict '@!'
   @15,39 get mcole pict '@!'
   @15,58 get mcontato pict '@!'
   @17,37 get msigla pict '@!' valid AV02100sig() messa 'Sigla do Respons vel n„o pode ser brancos'
   read
   do limpa23
   set order to 1
   sele trans
   if lastkey() = 27
      loop
   endif
   mens = dadcorr
   do pegresp with [SN]
   if mc = 'N' .or. lastkey() = 27
      loop
   endif
   =MENSAG('Sair sem alterar (ESC), Gravar/alterar conte£do (Crtl+W)')
   DEFINE WINDOW TRANS FROM 18,01 TO 22,78 COLOR &corarq
   MODI MEMO Observacao WINDOW TRANS SAVE
   if seek(mtra)
      if nguetra <> mguetra .or. nraz <> mraz
         dele
      endif
   endif

   if !seek(mtra)
      appe blan
   endif
   mdtalt = dtoc(date())
   replace    tra       with   mtra       ,    raz      with    mraz
   replace    end       with   mend       ,    cid      with    mcid
   replace    est       with   mcest      ,    via      with    mvia
   replace    bai       with   mbai       ,    tel      with    mtel
   replace    cep       with   mcep       ,    ddd      with    mddd
   replace    ddd_faxt  with   mddd_faxt  ,    ddd2     with    mddd2
   replace    tel2      with   mtel2      ,    contato  with    mcontato
   replace    num_faxt  with   mnum_faxt  ,    guetra   with    mguetra
   replace    dtalt     with   mdtalt     ,    sigla    with    msigla
   replace    cgc       with   mcgc       ,    ins      with    mins
   replace    cole      with   mcole
enddo
close databases
clear
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
PROCEDURE MONTATRA1
stor guetra to mguetra, nguetra
stor raz    to mraz, nraz
mcgc = cgc
mins = ins
mend = end
mcid = cid
mbai = bai
mcest = est
mcep = cep
mddd_faxt = ddd_faxt
mnum_faxt = num_faxt
mddd = ddd
mtel = tel
mddd2 = ddd2
mtel2 = tel2
mvia = via
mcole = cole
mcontato = contato
mdtalt = dtalt
msigla = sigla
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
PROCEDURE MONTATRA2
@03,63 say mguetra color &corver
@05,21 say mraz color &corver
@07,21 say mcgc pict '@r 99.999.999/9999-99' color &corver
@07,61 say mins color &corver
@09,21 say mend color &corver
@10,21 say mbai color &corver
@10,46 say mcid color &corver
@10,76 say mcest color &corver
@11,21 say mcep color &corver
@11,46 say mddd_faxt color &corver
@11,51 say mnum_faxt color &corver
@13,21 say mddd color &corver
@13,26 say mtel color &corver
@13,39 say mddd2 color &corver
@13,45 say mtel2 color &corver
@15,21 say mvia color &corver
if !empty(mcole)
   @15,32 say 'Coleta' color &corpisca
   @15,39 say mcole     color &corpisca
else
   @15,32 say 'Coleta'
   @15,39 say mcole    color &corver
endif
@15,58 say mcontato color &corver
@17,21 say mdtalt COLOR &CORVER
@17,37 say msigla COLOR &CORVER
sele SIGLAS
seek msigla
@17,44 say nome COLOR &CORVER
SELE TRANS
@23,01 say space(78)
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function AV02100est
if empty(mcest)
   return .f.
endif
sele ESTADOS
if !seek(mcest)
   =mensag(corrija)
   return .f.
endif
sele TRANS
return .t.
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function AV02100sig
if empty(msigla)
   =mensag('Campo nao pode estar em branco')
   return .f.
endif
sele SIGLAS
if !seek(msigla)
   =mensag('Sigla do Funcion rio autorizado n„o cadastrado - ENTER')
   return .f.
else
   @17,44 say nome  COLOR &CORVER
endif
sele TRANS
return .t.

