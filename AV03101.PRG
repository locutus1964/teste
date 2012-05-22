* AV03101 - ATUALIZACAO DO CADASTRO DE VENDEDORES
*           REFORMULADO EM 31/07/93
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 31/07/93
*
Sele 0
use CARGOS   index CARGOS.IDX
Sele 0
use CONCEITO index CONCEITO.IDX
Sele 0
use REGIAO   index REGIAO.IDX
Sele 0
use STATUS   index STATUS.IDX
Sele 0
use ESTADOS  index ESTADOS.IDX
Sele 0
use VEND     index VEND.IDX,VENGUE.IDX,VENRAZ.IDX
SAVE SCREEN TO MAIN2_SCR
=CABTELA(0,0,24,79)
MENS = 'Atualiza‡„o de Vendedores/Representantes'
=mensagr(mens)
@01,pos say mens
@03,03 say 'Vend/Repr'
@03,15 say 'F/J'
@03,21 say 'CPF/CGC'
@03,42 say 'Nome de Guerra'
@03,61 say 'Regi„o'
@03,70 say 'Status'
@05,03 say 'Raz„o Social'
@05,51 say 'Cidade'
@07,03 say 'Endere‡o'
@07,51 say 'Bairro'
@09,03 say 'Est'
@09,09 say 'CEP'
@09,23 say 'DDD1 Telef 1'
@09,41 say 'DDD2 Telef 2'
@09,60 say 'FAX (DDD/N£mero)'
@11,03 say 'C Postal'
@11,14 say 'Banco'
@11,54 say 'Agencia'
@11,64 say 'Cta Corrente'
@13,03 say 'Contato 1'
@13,24 say 'Cargo'
@13,32 say 'Nasct' 	
@13,42 say 'Contato 2'
@13,63 say 'Cargo'
@13,71 say 'Nasct'
@15,03 say 'Inclus„o'
@15,17 say 'Conceito'
@15,31 say 'Area de A‡„o'
@17,31 say 'Obs.:'
MDEL = 0
MCVR = SPACE(04)
do while  .T.
   SELE VEND
   mcgc      = space(16)
   mcid      = SPACE(25)
   mbai      = SPACE(20)
   mraz      = SPACE(45)
   mend      = SPACE(45)
   MCEST     = SPACE(02)
   mcep      = space(09)
   mbox      = SPACE(06)
   mddd1     = SPACE(04)
   mfon1     = SPACE(09)
   mddd2     = SPACE(04)
   mfon2     = SPACE(09)
   mccor     = SPACE(12)
   mddd_faxv = SPACE(04)
   mnum_faxv = SPACE(09)
   mcontat1  = SPACE(20)
   m_cargo1  = SPACE(02)
   mnasct1   = SPACE(05)
   mcontat2  = SPACE(20)
   m_cargo2  = SPACE(02)
   mnasct2   = SPACE(05)
   mregiao   = SPACE(04)
   mfiju     = SPACE(01)
   mconceito = SPACE(02)
   mestatus  = SPACE(01)
   minic     = space(08)
   mCVR      = SPACE(04)
   mvengue   = SPACE(15)
   macao     = SPACE(45)
   mbanco    = SPACE(38)
   magencia  = SPACE(04)
   mctacorr  = SPACE(12)
   @04,05 say mcvr        COLOR &CORVER
   @04,16 say mfiju       COLOR &CORVER
   @04,21 say mcgc        COLOR &CORVER
   @04,42 say mvengue     COLOR &CORVER
   @04,62 say mregiao     COLOR &CORVER
   @04,72 say mestatus    COLOR &CORVER
   @06,03 say mraz        COLOR &CORVER
   @06,51 say mcid        COLOR &CORVER
   @08,03 say mend        COLOR &CORVER
   @08,51 say mbai        COLOR &CORVER
   @10,04 say MCEST        COLOR &CORVER
   @10,09 say mcep        COLOR &CORVER
   @10,23 say mddd1       COLOR &CORVER
   @10,28 say mfon1       COLOR &CORVER
   @10,41 say mddd2       COLOR &CORVER
   @10,46 say mfon2       COLOR &CORVER
   @10,60 say mddd_faxv   COLOR &CORVER
   @10,67 say mnum_faxv   COLOR &CORVER
   @12,03 say mbox        COLOR &CORVER
   @12,14 say mbanco      COLOR &CORVER
   @12,56 say magencia    COLOR &CORVER
   @12,64 say mctacorr    COLOR &CORVER
   @14,03 say mcontat1    COLOR &CORVER
   @14,26 say m_cargo1     COLOR &CORVER
   @14,31 say mnasct1     COLOR &CORVER
   @14,42 say mcontat2    COLOR &CORVER
   @14,65 say m_cargo2     COLOR &CORVER
   @14,71 say mnasct2     COLOR &CORVER
   @16,03 say minic       COLOR &CORVER
   @16,19 say mconceito   COLOR &CORVER
   @16,31 say macao       COLOR &CORVER
   @18,31 say mobservavao COLOR &CORVER
   DEFINE WINDOW Vend_Repres FROM 17,01 TO 21,78 COLOR &CORARQ
   escolha = space(01)
   MENS = 'Quer selecionar Vendedor/Representante j  cadastrado ?'
   DO PEGRESP WITH [SN]
   IF MC = 'S'
      SELE VEND
      MENS = 'Seleciona por Nome de Guerra ou Raz„o Social ?'
      DO PEGRESP WITH [GR]
      IF MC = 'R'
         SET ORDER TO 3
         MENS = 'Informe Raz„o Social ou iniciais dela'
         =MENSAG(MENS)
         @06,03 GET MRAZ PICT '@!'
         READ

         IF MRAZ = SPACE(10)
            =MENSAG(CALPHA)
            =INKEY(0)
            LOOP
         ENDIF
         IF !SEEK(RTRIM(MRAZ))
            MENS = 'N„o ha Vendedor/Representante cadastrado com esta Raz„o Social - ENTER'
            =INKEY(0)
            LOOP
         ENDIF
      ELSE
         SET ORDER TO 2
         MENS = 'Informe Nome de Guerra ou iniciais dele'
         =MENSAG(MENS)
         @04,42 GET MVENGUE PICT '@!'
         READ
         IF MVENGUE = SPACE(10)
            =MENSAG(CALPHA)
            =INKEY(0)
            LOOP
         ENDIF
         IF !SEEK(RTRIM(mvengue))
            MENS = 'N†o ha Vendedor/Representante cadastrado com este Nome de Guerra - ENTER'
            =INKEY(0)
            LOOP
         ENDIF
      ENDIF
      DO WHILE .T.      &&  SUBS(VENGUE,1,LEN(MVENGUE)) = MVENGUE
         @23,15 SAY 'Anterior [], Pr¢ximo [] ou Confirma [ESC] ?' COLOR &LINTELA
         @23,24 SAY '[]' COLOR &CORARQ
         @23,37 SAY '[]' COLOR &CORARQ
         @23,53 SAY '[ESC]' COLOR &CORARQ
*         DO TELASVEN
         MCVR     = CVR
         MFIJU    = FIJU
         MVENGUE  = VENGUE
         MRAZ     = RAZ
         MCGC     = CGC
         FN       = INKEY(0)
         * Seta para cima 
         IF FN = 5  
            SKIP - 1
            IF BOF()
               =MENSAG('In¡cio do Arquivo - ENTER')
               =INKEY(0)
               LOOP
            ENDIF
         ELSE
            * Seta para baixo
            IF FN = 24 
               SKIP
               IF EOF()
                  =MENSAG('Fim do Arquivo - ENTER')
                  =INKEY(0)
                  LOOP
               ENDIF
            ELSE
               ESCOLHA = '*'
               SET ORDER TO 1
               EXIT
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   SELE VEND
   SET ORDER TO 1
   @04,05 GET MCVR
   READ
   IF LASTKEY() = 27
      EXIT
   ENDIF
   IF MCVR = SPACE(04)
      =MENSAG('C¢digo do Representante n„o pode ser brancos - ENTER')
      LOOP
   ENDIF
   IF SEEK(MCVR)
      DO TELASVEN
   ELSE
      MENS = TESTECAD
      DO PEGRESP WITH [SN]
      if mc = 'N'
         LOOP
      endif
   ENDIF

   IF SEEK(MCVR)
      mens = TESTEINC
      DO PEGOPCAO WITH [123]
      if opcao = '1'
         loop
      endif
      if opcao = '3'
         dele
         mdel = 1
         loop
      endif
   endif
* -------------------
   SELE VEND
   SET ORDER TO 1
   DO TESTACGC
   Sele REGIAO
   DO LIMPA23
   DO WHILE .T.
      @04,62 get mregiao
      read
      if lastkey() = 27
         clos data
         return
      endif
      if mregiao <> space(04)
         if !seek(mregiao)
            =mensag('Regi„o n„o cadastrada - ENTER')
            =INKEY(0)
            loop
         endif
      endif
      exit
   ENDDO
   SELE STATUS
   do while .t.
      @04,72 get mestatus
      read
      if mestatus <> space(1)
         if !seek(mestatus)
            =mensag('Status n„o cadastrado - ENTER')
            loop
         endif
      endif
      exit
   enddo
   @23,01 SAY SPACE(78)
   do while .t.
      @06,03 get mraz pict '@!'
      read
      if mraz <> space (30)
         exit
      endif
      DO MENSAG WITH CALPHA
   enddo
   @23,01 SAY SPACE(78)
   do while .t.
      @06,51  get mcid pict '@!'
      read
      if mcid <> space(20)
         exit
      endif
      DO MENSAG WITH CALPHA
   enddo
   @23,01 SAY SPACE(78)
   do while .t.
      @08,03 get mend pict '@!'
      read
      if mend <> space (30)
         exit
      endif
      DO MENSAG WITH CALPHA
   enddo
   MENSP = 'Preencher os campos a seguir sempre que poss¡vel...'
   DO MENSAG WITH MENSP
   @08,51 get mbai pict '@!'
   read
   sele ESTADOS
   @23,01 SAY SPACE(78)
   do while .t.
      @10,04 get MCEST pict '!!'
      read
      if !seek(MCEST)
         DO MENSAG WITH CORRIJA
      else
         exit
      endif
   enddo
   DO MENSAG WITH MENSP
   @10,09 get mcep
   READ
   DO MENSAG WITH MENSP
   @10,23 get mddd1
   @10,28 get mfon1
   @10,41 get mddd2
   @10,46 get mfon2
   @10,60 get mddd_faxv
   @10,67 get mnum_faxv
   @12,03 get mbox
   @12,14 get mbanco pict '@!'
   @12,54 get magencia
   @12,64 get mctacorr
   @14,03 get mcontat1 pict '@!'
   read
   sele CARGOS
   do while .t.
      @14,26 get m_cargo1  pict '@!'
      read
      if m_cargo1 <> space(02)
         if !seek(m_cargo1)
            =mensag('Cargo do primeiro Contato n„o cadastrado - ENTER')
            =INKEY(0)
            loop
         endif
      endif
      exit
   enddo
   @14,31 get mnasct1  pict '99/99'
   @14,42 get mcontat2 pict '@!'
   read
   do while .t.
      @14,65 get m_cargo2  pict '@!'
      read
      if m_cargo2 <> space(02)
         if !seek(m_cargo2)
            =mensag('Cargo do segundo Contato n„o cadastrado - ENTER')
            =INKEY(0)
            loop
         endif
      endif
      exit
   enddo
   @14,71 get mnasct2  pict '99/99'
   @16,03 get minic pict '99/99/99'
   read
   do limpa23
   SELE CONCEITO
   do while .t.
      @16,19 get mconceito pict '@!'
      read
      if mconceito <> space(2)
         if !seek(mconceito)
            =mensag('Conceito n„o cadastrado - ENTER')
            loop
         endif
      endif
      exit
   enddo
   @16,31 get macao pict '@!'
   @18,31 get mobservacao 
   read
   MENS = DADCORR
   DO PEGRESP WITH [SN]
   if mc = 'N'
      limp = 'N'
      loop
   else
      limp = 'S'
   endif
   SELE VEND
   SET ORDER TO 1
   if !seek(MCVR)
      appe blan
   endif
*   =MENSAG('Sair sem alterar (ESC), Gravar/alterar conte£do (Crtl+W)')
*   DEFINE WINDOW Vend_Repres FROM 17,01 TO 21,78 COLOR &CORARQ
*   MODI MEMO OBSERVACAO WINDOW Vend_Repres SAVE
   repl CVR with mCVR,fiju with mfiju,cgc with mcgc,vengue with mvengue
   repl regiao with mregiao,estatus with mestatus
   repl raz with mraz,end with mend
   repl cid with mcid,bai with mbai,est with MCEST,cep with mcep
   repl ddd1 with mddd1,fon1 with mfon1,ddd2 with mddd2,fon2 with mfon2
   repl contat1 with mcontat1,cargo1 with m_cargo1,nasct1 with mnasct1
   repl contat2 with mcontat2,cargo2 with m_cargo2,nasct2 with mnasct2
   repl box with mbox,ddd_faxv with mddd_faxv,num_faxv with mnum_faxv
   repl inic with minic,conceito with mconceito
   repl banco with mbanco,agencia with magencia,ctacorr with mctacorr
   repl acao  with macao
enddo
RESTORE SCREEN FROM MAIN2_SCR
CLOSE DATA
return

*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
PROCEDURE TELASVEN        && MOSTRA OS DADOS DO VENDEDOR CADASTRADO
  MODI MEMO OBSERVACAO WINDOW Vend_Repres NOWAIT
  @04,05 say cvr     COLOR &CORVER
  mcvr = cvr
  @04,16 say fiju    COLOR &CORVER
  mfiju = fiju
  @04,21 say space(16) COLOR &CORVER
  if mfiju = 'J'
     @04,21 say cgc pict '99999999/9999-99' COLOR &CORVER
  else
     @04,21 say cgc pict '999.999.999/99' COLOR &CORVER
  endif
  mcgc = cgc
  @04,42 say vengue    COLOR &CORVER
  mvengue = vengue
  @04,62 say regiao    COLOR &CORVER
  mregiao  = regiao
  @04,72 say estatus COLOR &CORVER
  mestatus = estatus
  @06,03 say raz COLOR &CORVER
  mraz = raz
  @06,51 say cid COLOR &CORVER
  mcid = cid
  @08,03 say end COLOR &CORVER
  mend = end
  @08,51 say bai  COLOR &CORVER
  mbai = bai
  @10,04 say est COLOR &CORVER
  MCEST = est
  @10,09 say cep  COLOR &CORVER
  mcep = cep
  @10,23 say ddd1  COLOR &CORVER
  mddd1 = ddd1
  @10,28 say fon1  COLOR &CORVER
  mfon1 = fon1
  @10,41 say ddd2  COLOR &CORVER
  mddd2 = ddd2
  @10,46 say fon2  COLOR &CORVER
  mfon2 = fon2
  @10,60 say ddd_faxv  COLOR &CORVER
  mddd_faxv = ddd_faxv
  @10,67 say num_faxv  COLOR &CORVER
  mnum_faxv = num_faxv
  @12,03 say box  COLOR &CORVER
  mbox = box
  @12,14 say banco  COLOR &CORVER
  mbanco = banco
  @12,54 say agencia COLOR &CORVER
  magencia = agencia
  @12,64 say ctacorr COLOR &CORVER
  mctacorr = ctacorr
  @14,03 say contat1  COLOR &CORVER
  mcontat1 = contat1
  @14,26 say cargo1   COLOR &CORVER
  m_cargo1  = cargo1
  @14,31 say nasct1   COLOR &CORVER
  mnasct1  = nasct1
  @14,42 say contat2  COLOR &CORVER
  mcontat2 = contat2
  @14,65 say cargo2   COLOR &CORVER
  m_cargo2  = cargo2
  @14,71 say nasct2   COLOR &CORVER
  mnasct2  = nasct2
  @16,03 say inic COLOR &CORVER
  minic = inic
  @16,19 say conceito  COLOR &CORVER
  mconceito = conceito
  @16,31 say acao    COLOR &CORVER
  macao = acao
  @18,31 say observacao    COLOR &CORVER
  mobservacao = observacao
  
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
procedure LIMPAR                    && limpa as variaveis
mcgc      = SPACE(16)
mcid      = SPACE(25)
mbai      = SPACE(20)
mraz      = SPACE(45)
mend      = SPACE(45)
MCEST     = SPACE(02)
mcep      = space(09)
mbox      = SPACE(05)
mddd1     = SPACE(04)
mfon1     = SPACE(09)
mddd2     = SPACE(04)
mfon2     = SPACE(09)
mccor     = SPACE(12)
mddd_faxv = SPACE(04)
mnum_faxv = SPACE(09)
mcontat1  = SPACE(20)
m_cargo1   = SPACE(02)
mnasct1   = SPACE(05)
mcontat2  = SPACE(20)
m_cargo2   = SPACE(02)
mnasct2   = SPACE(05)
mregiao   = SPACE(04)
mfiju     = SPACE(01)
mconceito = SPACE(02)
mestatus  = SPACE(01)
minic     = space(08)
mCVR      = SPACE(04)
mvengue   = SPACE(15)
macao     = SPACE(45)
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
PROCEDURE TESTACGC
DO WHILE .T.
   =mensag('Vendedor/Representante: F¡sico [F], Jur¡dico, [J], encerrar [ESC]')
   if lastkey() = 27
      if mdel = 1
         MC = ' '
         MENS = 'Exclui Vendedores/Representantes cancelados ?'
         DO PEGRESP WITH [NS]
         IF MC = 'S'
            sele VEND
            pack
            exit
         ENDIF
      endif
      * close data
      return
   endif
   @04,16 get mfiju pict '!' valid(mfiju$'FJ')
   read
   if mfiju = 'F'
      @04,21  get MCGC pict '999.999.999/99'
      read
      if lastkey() = 27
         loop
      endif
      if MCGC = space(03)
         =MENSAG('Pessoa F¡sica com CPF exigido - ENTER')
         =INKEY(0)
         loop
      endif
      CPFT = SUBS(MCGC,1,3)+SUBS(MCGC,5,3)+SUBS(MCGC,9,3)+SUBS(MCGC,13,2)
      DO FCPF WITH CPFT
      if mc = 'N'
         =MENSAG('CPF incorreto. ENTER para digitar novamente')
         =INKEY(0)
         loop
      endif
   endif
   IF MFIJU = 'J'
      @04,21 get mcgc pict '99999999/9999-99'
      read
      if lastkey() = 27
         loop
      endif
      if MCGC = SPACE(08)
         =MENSAG('Pessoa Jur¡dica com CGC exigido')
         =INKEY(0)
         loop
      endif
      CGCT = SUBS(MCGC,1,8)+SUBS(MCGC,10,4)+SUBS(MCGC,15,2)
      DO FCGC WITH CGCT
      IF MC = 'N'
         =MENSAG('CGC incorreto. ENTER para digitar novamente')
         =INKEY(0)
         LOOP
      ENDIF
   ENDIF
   @04,42 GET MVENGUE
   READ
   IF MVENGUE = SPACE(15)
      =MENSAG('Nome de Guerra n„o pode ser brancos - ENTER')
      =INKEY(0)
      LOOP
   ENDIF
   EXIT
*    ENDIF
ENDDO
IF MDEL = 1
   SELE VEND
   =MENSAG('Compactando o Arquivo... Aguarde')
   PACK
endif
* close data
RETURN

