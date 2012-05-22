* AV041001 - ATUALIZACAO DO CADASTRO DE CLIENTES - LOCAL DE ENTREGA
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA -25/07/93
*
SAVE SCREEN TO TELA
@07,00 clear to 24,79
=CABTELAC(07,00,24,79)
MENS = 'Atualiza‡„o de Endere‡os de Entrega'
=MENSAGR(MENS)
@08,POS SAY MENS
@10,03 say 'CGC/C¢digo'
@10,22 say 'Raz„o Social'
@13,03 say 'Endere‡o'
@13,51 say 'Cidade'
@16,03 say 'Bairro'
@16,26 say 'Est'
@16,31 say 'CEP'
@16,43 say 'Inscr Estadual'
@16,61 say 'FAX (DDD/N£mero)'
@19,03 say 'DDD1 Telef 1'
@19,18 say 'DDD2 Telef 2'
@19,34 say 'Contato 1'
@19,56 say 'Contato 2'
MDEL = 0
MCGCE = SPACE(16)
mcide  = SPACE(25)
mbaie  = SPACE(20)
mraze  = SPACE(45)
mende  = SPACE(45)
meste  = SPACE(02)
mcepe  = space(09)
minse  = SPACE(17)
mddd1e = SPACE(04)
mfon1e = SPACE(09)
mddd2e = SPACE(04)
mfon2e = SPACE(09)
mddd_faxe = SPACE(04)
mnum_faxe = SPACE(09)
mcontat1e = SPACE(20)
mcontat2e = SPACE(20)
mfijue    = SPACE(01)
*
do while  .T.
   @11,03 say mcgce       COLOR &CORVER
   @11,22 say mraze       COLOR &CORVER
   @14,03 say mende       COLOR &CORVER
   @14,51 say mcide       COLOR &CORVER
   @17,03 say mbaie       COLOR &CORVER
   @17,26 say meste       COLOR &CORVER
   @17,31 say mcepe       COLOR &CORVER
   @17,43 say minse       COLOR &CORVER
   @17,63 say mddd_faxe   COLOR &CORVER
   @17,68 say mnum_faxe   COLOR &CORVER
   @20,03 say mddd1e      COLOR &CORVER
   @20,08 say mfon1e      COLOR &CORVER
   @20,19 say mddd2e      COLOR &CORVER
   @20,24 say mfon2e      COLOR &CORVER
   @20,34 say mcontat1e    COLOR &CORVER
   @20,56 say mcontat2e    COLOR &CORVER
   ESCOLHA = SPACE(01)
   SELE CLI_ENTR
   IF SEEK(MCGC)
      DO WHILE .T.      &&  EXISTE ENDERECO DE COBRANCA CADASTRADO
         @23,01 SAY SPACE(78)
         @23,15 SAY 'Anterior [], Pr¢ximo [] ou Sai [ESC] ?' COLOR &LINTELA
         @23,24 SAY '[]' COLOR &CORARQ
         @23,37 SAY '[]' COLOR &CORARQ
         @23,48 SAY '[ESC]' COLOR &CORARQ
         DO TELASENT
         MCGCE  = CGCE
         MCGC   = CGC
         MFIJUE = FIJUE
         FN     = INKEY(0)
         if lastkey() = 27
            escolha = '*'
            exit
         endif
         IF FN = 5
            SKIP - 1
            IF BOF() .OR. CGC <> MCGC
               =MENSAG('Inicio dos Endere‡os para o Cliente - ENTER')
               SKIP
               =INKEY(0)
               LOOP
            ENDIF
         ELSE
            IF FN = 24
               SKIP
               IF EOF() .OR. CGC <> MCGC
                  =MENSAG('Final dos Endere‡os para o Cliente - ENTER')
                  SKIP -1
                  =INKEY(0)
                  LOOP
               ENDIF
            ELSE
               MCGCE  = CGCE
               MCGC   = CGC
               MFIJUE = FIJUE
               FN     = INKEY(0)
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   DO WHILE .T.
      SELE CLI_ENTR
      IF !SEEK(MCGC)
         MENS = 'Sem Endere‡o de Entrega cadastrado. Inclui ?'
         DO PEGRESP WITH [SN]
         IF MC = 'N' .or. empty(mc)
            RETURN
         ENDIF
         DO INCLUI
      ELSE
         mens = 'Altera [ A ], Exclui [ E ], Inclui [ I ] ou Sai [ ESC ]'
         DO PEGOPCAO WITH [AEI]
         if lastkey() = 27
            return
         endif
         if opcao = 'E'
            dele
            mdel = 1
            loop
         endif
         if opcao = 'A'
            DO MANUENDE
         endif
         if opcao = 'I'
            DO INCLUI
         endif
      ENDIF
      MENS = DADCORR
      DO PEGRESP WITH [SN]
      if mc = 'N' .or. empty(mc)
         loop
      endif
      SELE CLI_ENTR
      if !SEEK(MCGC + MCGCE)
         appe blan
      endif
      repl cgc with mcgc,  cgce with mcgce, fijue with mfijue, raze with mraze
      repl ende with mende,cide with mcide, baie with mbaie,este with meste
      repl cepe with mcepe,inse with minse,ddd1e with mddd1e,fon1e with mfon1e
      repl ddd2e with mddd2e,fon2e with mfon2e,contat1e with mcontat1e
      repl contat2e with mcontat2e
      repl ddd_FAXE with mddd_FAXE,num_FAXE with mnum_FAXE
      mens = 'Continua a Manuten‡„o de Endere‡os de Entrega ?'
      do pegresp with [SN]
      IF MC = 'N' .or. empty(mc)
         RETURN
      ELSE
         EXIT
      ENDIF
   enddo
enddo
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
PROCEDURE MANUENDE
IF MCGCE = SPACE(10)
   =MENSAG('CGC/C¢digo de Entrega n„o pode ser brancos - ENTER')
   RETURN
ENDIF
@23,01 SAY SPACE(78)
do while .t.
   @11,22 get mraze pict '@!'
   read
   if mraze <> space (30)
      exit
   endif
   DO MENSAG WITH CALPHA
enddo
@14,03 get mende valid !empty(mende) pict '@!'
@14,51 get mcide valid !empty(mcide) pict '@!'
@17,03 get mbaie                     pict '@!'
@17,26 get meste valid seek(meste,'estados') pict '!!'
@17,31 get mcepe
@17,43 get minse
@17,63 get mddd_faxe
@17,68 get mnum_faxe
@20,03 get mddd1e
@20,08 get mfon1e
@20,19 get mddd2e
@20,24 get mfon2e
@20,34 get mcontat1e pict '@!'
@20,56 get mcontat2e pict '@!'
read
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
PROCEDURE TELASENT
@11,03 say CGCE
MCGCE = CGCE
@11,22 say raze COLOR &CORVER
mraze = raze
@14,03 say ende COLOR &CORVER
mende = ende
@14,51 say cide COLOR &CORVER
mcide = cide
@17,03 say baie  COLOR &CORVER
mbaie = baie
@17,26 say este COLOR &CORVER
meste = este
@17,31 say cepe  COLOR &CORVER
mcepe = cepe
@17,43 say inse  COLOR &CORVER
minse = inse
@17,63 say ddd_faxe  COLOR &CORVER
mddd_faxe = ddd_faxe
@17,68 say num_faxe  COLOR &CORVER
mnum_faxe = num_faxe
@20,03 say ddd1e  COLOR &CORVER
mddd1e = ddd1e
@20,08 say fon1e  COLOR &CORVER
mfon1e = fon1e
@20,19 say ddd2e  COLOR &CORVER
mddd2e = ddd2e
@20,24 say fon2e  COLOR &CORVER
mfon2e = fon2e
@20,34 say contat1e  COLOR &CORVER
mcontat1e = contat1e
@20,56 say contat2e  COLOR &CORVER
mcontat2e = contat2e
RETURN
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
PROCEDURE INCLUI
mens = 'Endere‡o:  Jur¡dico [J], F¡sico [F], Indefinido [I], encerrar [ESC]'
do pegresp with [JFI]
if lastkey() = 27 .or. empty(mc)
   if mdel = 1
      MC = ' '
      MENS = 'Exclui Endere‡os de Entrega cancelados ?'
      DO PEGRESP WITH [NS]
      IF MC = 'S'
         sele CLI_COBR
         pack
      ENDIF
   endif
*   clos data

   return
endif
MFIJUE = MC
do while .t.
   if mfijue = 'F'
      @11,03  say space(18)
      @11,03  get MCGCE pict '999.999.999/99'
      read
      if lastkey() = 27
         exit
      endif
      if MCGCE = space(03)
         =MENSAG('Fornecedor Pessoa F¡sica com CPF exigido - ENTER')
         =INKEY(0)
         loop
      endif
      CPFT = SUBS(MCGC,1,3)+SUBS(MCGCE,5,3)+SUBS(MCGCE,9,3)+SUBS(MCGCE,13,2)
      DO FCPF WITH CPFT
      if mc = 'N'
         =MENSAG('CPF do Fornecedor incorreto. ENTER para entrar novamente')
         =INKEY(0)
         loop
      endif
   endif
   IF MFIJUE = 'J'
      @11,03 get MCGCE pict '99999999/9999-99'
      read
      if lastkey() = 27
         exit
      endif
      if MCGCE = SPACE(08)
         =MENSAG('Fornecedor Pessoa Jur¡dica com CGC exigido')
         =INKEY(0)
         loop
      endif
      CGCT = SUBS(MCGCE,1,8)+SUBS(MCGCE,10,4)+SUBS(MCGCE,15,2)
      DO FCGC WITH CGCT
      IF MC = 'N'
         =MENSAG('CGC do Fornecedor incorreto. ENTER para entrar novamente')
         =INKEY(0)
         LOOP
      ENDIF
   ENDIF
   IF MFIJUE = 'I'
      @11,03 GET MCGCE PICT '@!'
      read
      if lastkey() = 27
         exit
      endif
      IF MCGCE = SPACE(16)
         =MENSAG('C¢digo do Fornecedor n„o pode ser brancos - ENTER')
         =INKEY(0)
         LOOP
      ENDIF
   ENDIF
   DO MANUENDE
   exit
enddo
return

