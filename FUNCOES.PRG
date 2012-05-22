* --------------------------------------------------------------------
*   ADMVFUNC - FUNCOES DO SISTEMA ADMINISTRACAO DE VENDAS
* --------------------------------------------------------------------
PROC pegresp
*
PARA x_tudo
@ 23,01 SAY SPAC(78)
x_aqui = (80-(LEN(mens)+(LEN(x_tudo)*2)+1))/2
x_lax = x_aqui+LEN(mens)
SET MESS TO
x_N = 1
DO WHIL x_N <= LEN(x_tudo)
   x_sy = LTRI(STR(x_N))
   op&x_sy = SUBS(x_tudo,x_N,1)
   x_N = x_N + 1
ENDD
x_N = 1
@ 23,x_aqui SAY mens+[ (]+REPL([ /],LEN(x_tudo)-1)+[ )] 
DO WHIL x_N <= LEN(x_tudo)
   x_sy = LTRI(STR(x_N))
   @ 23,x_lax+(x_N*2) PROM op&x_sy
   x_N = x_N + 1
ENDD
MENU TO x_peg
MC = SUBS(x_tudo,x_peg,1)
RETU
* --------------------------------------------------------------
PROC pegresp49
*
PARA x_tudo
@ 48,01 SAY SPAC(78)
x_aqui = (80-(LEN(mens)+(LEN(x_tudo)*2)+1))/2
x_lax = x_aqui+LEN(mens)
SET MESS TO
x_N = 1
DO WHIL x_N <= LEN(x_tudo)
   x_sy = LTRI(STR(x_N))
   op&x_sy = SUBS(x_tudo,x_N,1)
   x_N = x_N + 1
ENDD
x_N = 1
@ 48,x_aqui SAY mens+[ (]+REPL([ /],LEN(x_tudo)-1)+[ )] 
DO WHIL x_N <= LEN(x_tudo)
   x_sy = LTRI(STR(x_N))
   @ 48,x_lax+(x_N*2) PROM op&x_sy
   x_N = x_N + 1
ENDD
MENU TO x_peg
MC = SUBS(x_tudo,x_peg,1)
RETU
* --------------------------------------------------------------
PROC pegopcao
PARA x_tudo
@ 23,01 SAY SPAC(78)
x_aqui = (80-(LEN(mens)+(LEN(x_tudo)*2)+1))/2
x_lax = x_aqui+LEN(mens)
SET MESS TO
x_N = 1
DO WHIL x_N <= LEN(x_tudo)
   x_sy = LTRI(STR(x_N))
   op&x_sy = SUBS(x_tudo,x_N,1)
   x_N = x_N + 1
ENDD
x_N = 1
@ 23,x_aqui SAY mens+[ (]+REPL([ /],LEN(x_tudo)-1)+[ )] 
DO WHIL x_N <= LEN(x_tudo)
   x_sy = LTRI(STR(x_N))
   @ 23,x_lax+(x_N*2) PROM op&x_sy
   x_N = x_N + 1
ENDD
MENU TO x_peg
OPCAO = SUBS(x_tudo,x_peg,1)
RETU
* -------------------------------------------------------------
* FUNCAO DE BROWSE DE ARQUIVOS
    FUNCTION TESTE 
    PRIVATE CAMPO
    CAMPO  = VARREAD()
    DO CASE
      
      CASE CAMPO = "MESTATUS" 
        ? MOSTRA(03,47,21,78) 
        BROWSE FIELDS DSTATUS:R:H='Descri‡„o' IN WINDOW TELA
        MESTATUS = STATUS 
        
      CASE CAMPO = "MDESCSEM"  
        ? MOSTRA(10,47,21,78)
        BROWSE FIELDS TABSEM:R:H='Desc.Sem.',DIAS_DE:R:H='De',DIAS_ATE:R:H='Ate',PERDIAS:R:H='Percent' IN WINDOW TELA  
        MTABSEM = TABSEM
        MDIAS_DE = DIAS_DE
        MDESCSEM = PERDIAS

      CASE CAMPO = "MCFINANC"
        ? MOSTRA(10,47,21,78)
        BROWSE FIELDS TABFIN:R:H='C.Financ.',DIAS_DE:R:H='De',DIAS_ATE:R:H='Ate',PERFIN:R:H='Percent' IN WINDOW TELA
        MTABFIN = TABFIN
        MDIAS_DE = DIAS_DE  
        MDIAS_ATE = DIAS_ATE
        MCFINANC = PERFIN 
        
      CASE CAMPO = "MCFIN2"
        ? MOSTRA(10,47,21,78)
        BROWSE FIELDS TABFIN:R:H='C.Financ.',DIAS_DE:R:H='De',DIAS_ATE:R:H='Ate',PERFIN:R:H='Percent' IN WINDOW TELA
        MTABFIN = TABFIN
        MDIAS_DE = DIAS_DE  
        MDIAS_ATE = DIAS_ATE
        MCFIN2 = PERFIN 
        
      CASE CAMPO = "MCONCEITO" 
        ? MOSTRA(03,42,21,78) 
        BROWSE FIELDS CONCEITO:R:H='C¢d',DCONCEITO:R:H='Descri‡„o' IN WINDOW TELA
        MCONCEITO = CONCEITO 
        
      CASE CAMPO = "MREGIAO" 
        ? MOSTRA(13,07,21,78) 
        BROWSE FIELDS REGIAO:H='C¢d',DREGIAO:H='Descri‡„o',REPRES:H= ;
              'Representante' NOEDIT IN WINDOW TELA
        MREGIAO = FmtRegiao(REGIAO, IDREGIAO)
        MIDREGIAO = IDREGIAO
        
      CASE CAMPO = "MATIVIDADE" 
        ? MOSTRA(03,47,21,78) 
        BROWSE FIELDS DATIVIDADE:R:H='Descri‡„o' IN WINDOW TELA
        MATIVIDADE = ATIVIDADE
        
      CASE CAMPO = "MSIGLA" 
        ? MOSTRA(03,47,21,78) 
        BROWSE FIELDS NOME:H='Nome do Respons.' NOEDIT IN WINDOW TELA
        MSIGLA = SIGLA
        
      CASE CAMPO = "MCARGO" 
        ? MOSTRA(03,47,21,78) 
        BROWSE FIELDS DCARGO:H='Cargo do Contato' NOEDIT IN WINDOW TELA
        MCARGO = CARGO
        
      CASE CAMPO = "MLPR" 
        ? MOSTRA(03,40,21,78) 
        BROWSE FIELDS LPR:H='C¢d',DLPR:H='Linha de Produto' NOEDIT IN WINDOW TELA
        MLPR = LPR
        
      CASE CAMPO = "MCEST"  .OR. CAMPO = "ESTENT"
        ? MOSTRA(03,51,21,78) 
        BROWSE FIELDS CEST:H='UF',DEST:H='Descri‡„o' NOEDIT IN WINDOW TELA
        MCEST = CEST 
        
      CASE CAMPO = "MTRA"   
        ? MOSTRA(03,32,21,78)
        BROWSE FIELDS TRA:H='C¢d',GUETRA:H='N.Guerra',RAZ:H='Raz„o Social' NOEDIT IN WINDOW TELA
        MTRA = TRA
     
      CASE CAMPO = "TTRA_OPC"   
        ? MOSTRA(03,32,21,78)
        BROWSE FIELDS TRA:R:H='C¢d',RAZ:R:H='Raz„o Social' IN WINDOW TELA
        TTRA_OPC = TRA
        
     CASE CAMPO = "MCVR"        
       ? MOSTRA(03,40,21,78)
       BROWSE FIELDS CVR:R:H='C¢d',IDREPR:R:H='Seq.',RAZ:R:H='Raz„o Social/Nome' IN WINDOW TELA
       MCVR = CVR
       MIDREPR = IDREPR
       
    CASE CAMPO = "MREF"
      ? MOSTRA(06,20,21,78)
      BROWSE FIELDS REF:R:H='C¢digo',DES:R:H='Descri‡„o' IN WINDOW TELA
      MREF = REF

    CASE CAMPO = "MCPR"
      ? MOSTRA(13,60,21,78)
      BROWSE FIELDS CPR:R:H='C¢d',PRE:R:H='   Pre‡o' IN WINDOW TELA
      MCPR = CPR

    CASE CAMPO = "MNBANCO"
      ? MOSTRA(06,50,21,78)
      BROWSE FIELDS NBANCO:R:H='Bco',AGENCIA:R:H='Agˆn',DBANCO:R:H= ;
            'R.Social' IN WINDOW TELA
      MNBANCO = NBANCO
      MAGENCIA = AGENCIA

    CASE CAMPO = "MMOEDA"
      SET UNIQUE ON
      ? MOSTRA(06,50,21,78)
      BROWSE FIELDS MOEDA:R:H='C¢d',DMOEDA:R:H='Descri‡„o' IN WINDOW TELA
      MMOEDA = MOEDA
      SET UNIQUE OFF

    CASE CAMPO = "MCGC" 
      ? MOSTRA(12,01,21,78)
      BROWSE FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
                    CID:R:H='Cidade',END:R:H='Endere‡o' IN WINDOW TELA
      MCGC  = SUBS(CGC,1,16)
      MLOJA = SUBS(CGC,18,3)
      KEYCGC = MCGC+'/'+MLOJA
      MFIJU = FIJU
      MGUE = GUE
      MVEN = VEN
      MTRA = TRA
      MTRA_OPC = TRA_OPC

    CASE CAMPO = "MGUE" 
      ? MOSTRA(12,01,21,78)
      BROWSE FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
                    CID:R:H='Cidade',END:R:H='Endere‡o' IN WINDOW TELA
      MCGC  = SUBS(CGC,1,16)
      MLOJA = SUBS(CGC,18,3)
      KEYCGC = MCGC+'/'+MLOJA
      MFIJU = FIJU
      MGUE = GUE
      MVEN = VEN
      MTRA = TRA
      MTRA_OPC = TRA_OPC
      
    CASE CAMPO = "MTIP" .OR. CAMPO = "MTIPO"
      ? MOSTRA(10,33,22,78)
      BROWSE FIELDS TIPO:R:H='Tipo',NOPE:R:H='N.Op',DESN:R:H='Descri‡„o', ;
              IPIT:R:H='IPI',ICMT:R:H='ICM',ISST:R:H='ISS' IN WINDOW TELA
      MTIPO = TIPO
      MTIP  = TIPO
      MNOPE = NOPE
      MDESN = DESN
      MIPIT = IPIT
      MICMT = ICMT
      MISST = ISST
      MISSP = ISSP
      MCODT = CODT
      MEDUP = EDUP
      MBEST = BEST
      MMNF1 = MNF1
      MMNF2 = MNF2
    
    CASE CAMPO = "MITE" 
      ? MOSTRA(03,01,10,79)
      BROWSE FIELDS ITE:R:H='It',REF:R:H='Referˆncia',IPI:R:H='%IPI', ;
                   QTD:R:H='Quantidade',DIT:R:H='%Ite',BON:R:H='%Bon', ;
                   PRE:R:H='Pre‡o Unit' FOR PED = MPED IN WINDOW TELA
      MITE = ITE
      MREF = REF
      MPED = PED
      DO ESCONDE
      @ LIN,02 SAY MITE PICT '99' color &corver
      @ LIN,05 SAY MREF color &corver
      @ LIN,10 say tabref pict '99/99' color &corver
      @ lin,16 say prebru pict '99,999,999.99' color &corver
      @ lin,30 say descgru pict '99.99' color &corver
      @ LIN,36 SAY QTD PICT '99999999.99' color &corver
      @ LIN,45 SAY DIT PICT '99.99'  color &corver
      @ LIN,51 SAY BON PICT '99.99'  color &corver
      @ LIN,67 SAY PRE PICT '99999999.99'  color &corver
      @ LIN+1,05 SAY DESCRICAO    color &corver
      @ lin,61 say ipi pict '99.9' color &corver
                
     CASE CAMPO = "MUNIDADE"        
       ? MOSTRA(03,50,21,78)
       BROWSE FIELDS UNIDADE:R:H='Unid',DUNIDADE:R:H='Descri‡„o' IN WINDOW TELA
       MUNIDADE = UNIDADE
      
     CASE CAMPO = "MEMBALAGEM"        
       ? MOSTRA(03,50,21,78)
       BROWSE FIELDS EMBALAGEM:R:H='Tipo',DEMBALAGEM:R:H='Descri‡„o' IN WINDOW TELA
       MEMBALAGEM = EMBALAGEM
       
     CASE CAMPO = "MCLASFISC"        
       ? MOSTRA(03,50,21,78)
       BROWSE FIELDS CLASFISC:R:H='C¢digo',DCLASFISC:R:H='Class. do IPI' IN WINDOW TELA
       MCLASFISC = CLASFISC
       
     CASE CAMPO = "MGRUPO"        
       ? MOSTRA(03,45,21,78)
       BROWSE FIELDS GRUPO:H='C¢d',DGRUPO:H='Descri‡„o',COMISSAO:H= ;
             '% Com' NOEDIT IN WINDOW TELA
       MGRUPO = GRUPO
       
    CASE CAMPO = "MTABREF"
      ? MOSTRA(04,50,21,78)
      BROWSE FIELDS TABREF:R:H='Tab',REF:R:H='Ref',PRECO:R:H='Pre‡o' IN WINDOW TELA
      MTABREF = TABREF
      MPRECO = PRECO
      MREF   = REF
  
    CASE CAMPO = "MTABSEM"
      ? MOSTRA(04,48,21,78)
      BROWSE FIELDS TABSEM:R:H='Desc.Sem.',DIAS_DE:R:H='De',DIAS_ATE:R:H='Ate',PERDIAS:R:H='Percent' IN WINDOW TELA
      MTABSEM = TABSEM
      MDIAS_DE = DIAS_DE
      MDIAS_ATE= DIAS_ATE
      MPERDIAS = PERDIAS

    CASE CAMPO = "MTABFIN"
      ? MOSTRA(04,48,21,78)
      BROWSE FIELDS TABFIN:R:H='C.Financ.',DIAS_DE:R:H='De',DIAS_ATE:R:H='Ate',PERFIN:R:H='Percent' IN WINDOW TELA
      MTABFIN = TABFIN
      MDIAS_DE = DIAS_DE
      MDIAS_ATE = DIAS_ATE
      MPERFIN = PERFIN 
      
    ENDCASE    
      DO ESCONDE 
    RETURN .T.    

FUNCTION TESTIMP2 && - DEFINE A IMPRESSAO EM TELA OU NA IMPRESSORA
* SET PRINT OFF

MENS = 'Relat¢rio selecionado est  correto para impress„o ?'
DO PEGRESP WITH [SN]
  CP = MC
  IF CP = 'N'
    RETURN
  ENDIF  

IF FILE("GRAREL80.TXT")
  ERASE GRAREL80.TXT
ENDIF

IF FILE("GRAREL10.TXT")
  ERASE GRAREL10.TXT
ENDIF    
*
  CP   = SPACE(01)
  TREL = SPACE(08)
  SREL = SPACE(01)
  DEFREL = ' '

  MENS = 'Relat¢rio em Tela ( T ) ou Impresso ( I ) ?'
  DO PEGRESP WITH [TI]
    DEFREL = MC
    IF DEFREL = 'I'
     
      MENS = 'Impressora est  ligada e com Papel dispon¡vel ?'
      DO PEGRESP WITH [SN]
          CP = MC
          IF CP = 'N'
            RETURN
          ENDIF
      ENDIF
  RETURN
* --------------------------------------------------------------------------
PROCEDURE COMPIMPR      && COMPACTA CARACTERES POR POLEGADA
  SET PRINT ON
  IF TIPOIMPR = 'R'
    ?? CHR(30) + '4'
  ELSE
    IF TIPOIMPR = 'E'  
      ?? CHR(15)
    ELSE
      ?? CHR(27)+"&l7.27C"+chr(27)+"(s0p16.66H"
    ENDIF    
  ENDIF
  SET PRINT OFF
  RETURN
* -------------------------------------------------------------------------
PROCEDURE DEFSAIDA  && - DEFINE SE SAIDA EM 80 OU 132 COLUNAS

  IF DEFREL = 'I'
    DO RESEIMPR
    IF DEFREL = 'I' .AND. TAMREL = 2
      MENS = 'Papel de 80 (1) ou de 132 (2) colunas? '
      DO PEGRESP WITH [12]
      CI = MC        
      IF CI = '1'
        DO COMPIMPR
      ENDIF  
    ENDIF  
    
    SET DEVI TO PRINT
    
  ELSE
  
    IF TAMREL = 1
      SET DEVI TO FILE GRAREL80.TXT
    ELSE
      SET DEVI TO FILE GRAREL10.TXT
    ENDIF
  ENDIF        
 
  RETURN

* -------------------------------------------------------------------------
PROCEDURE IMPSAIDA && - DEFINE SE SAIDA NA IMPRESSORA OU NA TELA
  SET DEVI TO SCRE  
  
    IF DEFREL = 'T'
      SAVE SCREEN TO TELA
      SET DISPLAY TO VGA50
      CLOS DATA
*     
        IF TAMREL = 1
          USE ARQREL80
          ZAP
          APPE FROM GRAREL80.TXT SDF
          GO TOP
        ELSE
          USE ARQREL10
          ZAP
          APPE FROM GRAREL10.TXT SDF
          GO TOP
        ENDIF    
        
      DEFINE WINDOW CELULA FROM 0,0 TO 49,79 GROW FLOAT ZOOM CLOSE NONE
      BROWSE WINDOW CELULA NOMODIFY NODELETE NORMAL
      SET DISPLAY TO VGA25
      SET MESSAGE TO 23 CENTER
      REST SCREEN FROM TELA

      MENS = 'Quer imprimir o Relat¢rio ?'
      DO PEGRESP WITH [SN]
        IF MC = 'S'
          SET PRINT ON
          _PEJECT = "NONE"        
          IF TAMREL = 2
            MENS = 'Papel de 80 (1) ou de 132 (2) colunas? '
            DO PEGRESP WITH [12]
          ENDIF    
 
          CI = MC    
          IF CI = '1'
            DO COMPIMPR
            _PEJECT = "NONE"
            RUN TYPE GRAREL10.TXT > PRN
            
          ELSE
            _PEJECT = "NONE"
            RUN TYPE GRAREL80.TXT > PRN
          ENDIF
          
          EJECT  
        ENDIF
    ELSE
      IF DEFREL = 'I'
        EJECT
      ENDIF      
    ENDIF           
      CLOS DATA
      SET PRINT OFF
      SET DEVI TO SCRE        
      RETURN
* -----------------------------------------------------------
* RESEIMPR - RESETA A IMPRESSORA E VOLTA PARA 132 COLUNAS

  PROCEDURE RESEIMPR
  SET PRINT ON
  IF TIPOIMPR = 'R'
    ?? CHR(30) + '0'
  ELSE
    IF TIPOIMPR = 'E'
      ?? CHR(18) 
    ELSE
      ?? CHR(27)+"&l7.27C"+chr(27)+"(s0p10H"
    ENDIF    
  ENDIF
  SET PRINT OFF
  RETURN
* -------------------------------------------------------
PROCEDURE ABREVGA       && TELA PARA 45 LINHAS
  SET DISPLAY TO VGA50
  RETURN
* -------------------------------------------------------
PROCEDURE FECHAVGA      && VOLTA TELA PARA 25 LINHAS
  SET DISPLAY TO VGA25
  RETURN    
* -------------------------------------------------------
* CABTELA - TELA DE INICIALIZACAO DAS VARIAVEIS DO SISTEMA
FUNCTION CABTELA
  PARAMETERS LI,CI,LF,CF
  PRIVATE LI,CI,LF,CF
  LINTELA = PAR0_P1+'/'+PAR1_P2
  @ LI,CI FILL TO LF,CF
  @ LI,CI TO LF,CF DOUBLE COLOR &LINTELA
  @ LI+1,CI+2 SAY DTOC(DATE()) COLOR &LINTELA
  @ LI+2,CI TO LI+2,CF COLOR &LINTELA
  @ LI+2,CI SAY CHR(199) COLOR &LINTELA
  @ LI+2,CF SAY CHR(182) COLOR &LINTELA
  @ LI+(22-LI),CI TO LI+(22-LI),CF COLOR &LINTELA
  @ LI+(22-LI),CI SAY CHR(199) COLOR &LINTELA
  @ LI+(22-LI),CF SAY CHR(182) COLOR &LINTELA
RETURN.T.
* CABTELA - TELA DE INICIALIZACAO DAS VARIAVEIS DO SISTEMA
FUNCTION CABTELAC
  PARAMETERS LI,CI,LF,CF
  PRIVATE LI,CI,LF,CF
  LINTELA = PAR1_P1+'/'+PAR1_P2
  @ LI,CI FILL TO LF,CF
  @ LI,CI TO LF,CF DOUBLE COLOR &LINTELA
  @ LI+1,CI+2 SAY DTOC(DATE()) COLOR &LINTELA
  @ LI+2,CI TO LI+2,CF COLOR &LINTELA
  @ LI+2,CI SAY CHR(199) COLOR &LINTELA
  @ LI+2,CF SAY CHR(182) COLOR &LINTELA
  @ LI+(22-LI),CI TO LI+(22-LI),CF COLOR &LINTELA
  @ LI+(22-LI),CI SAY CHR(199) COLOR &LINTELA
  @ LI+(22-LI),CF SAY CHR(182) COLOR &LINTELA
RETURN.T.
* --------------------------------------------------------  
* CABTELA45 - TELA DE INICIALIZACAO DAS VARIAVEIS DO SISTEMA
FUNCTION CABTELA45
  PARAMETERS LI,CI,LF,CF
  PRIVATE LI,CI,LF,CF
  LINTELA = PAR0_P1+'/'+PAR1_P2
  @ LI,CI FILL TO LF,CF
  @ LI,CI TO LF,CF DOUBLE COLOR &LINTELA
  @ LI+1,CI+2 SAY DTOC(DATE()) COLOR &LINTELA
  @ LI+2,CI TO LI+2,CF COLOR &LINTELA
  @ LI+2,CI SAY CHR(199) COLOR &LINTELA
  @ LI+2,CF SAY CHR(182) COLOR &LINTELA
  @ LI+47,CI TO LI+47,CF COLOR &LINTELA
  @ LI+47,CI SAY CHR(199) COLOR &LINTELA
  @ LI+47,CF SAY CHR(182) COLOR &LINTELA
RETURN.T.
* -------------------------------------------------------------------------
PROCEDURE TELAI
CLEAR ALL
SET DEVI TO SCRE
*
PUBLIC VOLMENU,IMPRCODM,IMPRALPHAM,TITULO,NAOCAD,MC,TESTPRIN
PUBLIC MAIN_SCR,MAIN2_SCR,BOXFRAME,RELALPHA,RELCOD,ATUCAD,ENCMENU,INCALTEXC
PUBLIC MREL,MIMPRES,MMVAR,MFATURA,MSIS,CORRIJA,ENCERRAR,IMPRALPHA,IMPRCOD
PUBLIC CNUMER,CALPHA,CALPHAN,ECODIGO,EMENS,TESTEINC,TESTECAD,DADCORR,USESETAS
PUBLIC MTPRG,MTATR,MIMPOR,MINTERNO,MINTERNA,MEMBALAG,MVARVTOT,MVALTOT
PUBLIC MVARVAL,LIN,CFRETE,RLINHA,V,POS,IMPRESS,PAG,PAGINA,TIMP,CP
PUBLIC SELCOR,TDATA,TIPOIMPR,TREL,SREL,CORSCHE,DEFREL,TAMREL,MPLIQUIDO,MICM
PUBLIC VDUP1,VDUP2,VDUP3,VDUP4,VDUP5,VDUP6,MEDUP
PUBLIC VENDUP1,VENDUP2,VENDUP3,VENDUP4
PUBLIC MCP1,MCP2,MCP3,MCP4,MPE1,MPE2,MPE3,MPE4,MCPD,INICIO,MEST
PUBLIC CORARQ,CORVER,OPCAO,MC,RET,LINTELA,VNOMEPRG
*
OPCAO = SPACE(01)
MC    = SPACE(01)
TESTPRIN = 'Impressora desligada ou falta papel... Retorna sem imprimir ?'
SELCOR   = 'Relat¢rio selecionado est  correto ?'
ATUCAD   = 'Atualiza‡„o do Cadastro      '
RELCOD   = 'Relat¢rio em ordem de C¢digo '
RELALPHA = 'Relat¢rio em ordem Alfab‚tica'
VOLMENU  = 'Volta ao Menu anterior       '
ENCMENU  = 'Encerra Menu atual e volta ao Menu anterior'
INCALTEXC= 'Inclui, Altera, Consulta ou Exclui dados do Cadastro'
CNUMER   = 'Campo n„o pode ser zeros'
CALPHA   = 'Campo n„o pode ser brancos'
CALPHAN  = 'Campo n„o pode ser brancos ou zeros'
ECODIGO  = '[ESC] para encerrar/sair e [F1] para consultar Arquivos'
EMENS    = 'encerra a atualiza‡„o'
TESTEINC = 'J  Cadastrado - Retorna (1), Altera (2) ou Exclui (3)? '
TESTECAD = 'N„o cadastrado - Inclui ?'
DADCORR  = 'Dados est„o corretos ?'
CORRIJA  = 'N„o cadastrado. ENTER para corrigir ou informar novamente'
ENCERRAR = 'p/encerrar'
IMPRALPHA= 'Imprimindo Relat¢rio em ordem Alfab‚tica... Aguarde'
IMPRCOD  = 'Imprimindo Relat¢rio em ordem de C¢digo... Aguarde'
IMPRALPHAM='Imprime Relat¢rio do Cadastro em ordem Alfab‚tica'
IMPRCODM = 'Imprime Relat¢rio do Cadastro em ordem de C¢digo'
NAOCAD   = 'N„o cadastrado... Confira e re-entre com a informa‡„o...'
TIMP     = 'I'
*
*************** IDENTIFICACAO  DA  EMPRESA  USUARIA / SISTEMA **************
MSIS     = 'SISTEMA ADMINISTRA€ŽO VENDAS'
*
MREL = MRAZAOSOC

RETURN
*-----------------------------------------------------------------------
function TELAG
clear

 TESTE1="ÉÍ»º¼ÍÈº"

@ 00,00,03,11 BOX TESTE1
@ 00,13,03,66 BOX TESTE1
@ 00,68,03,79 BOX TESTE1
@ 22,00,24,79 BOX TESTE1
@ 22,05 SAY ' F1 para Pesquisar 'color R+/b
@ 22,60 SAY ' ESC para Sair 'color R+/b
*
@ 01,01 say ' deltacom'
@ 01,14 SAY '          SISTEMA ADMINISTRA€ŽO DE VENDAS'
@ 02,14 SAY '           deltacom comunica‡”es s/c ltda'
*@ 02,14 say '                 Ad†o Eli Pereira'
@ 02,02 SAY DATE()
=NOMEMOD( 'AV')
RETURN .T.
* -------------------------------------------------------------------------
function janela
parameters li,ci,lf,cf
PUBLIC i

  @ li,ci clear to lf,cf
  LINTELA = PAR4_P1+'/'+PAR1_P2
  @ li,ci to lf,cf double  COLOR &LINTELA
  @ li,ci + 2 say chr(001) COLOR &LINTELA   && c rosto
  @ li,ci say chr(024)     COLOR &LINTELA   && seta cima
  @ li,cf say chr(025)     COLOR &LINTELA   && seta baixo
  @ li,ci say chr(024)     COLOR &LINTELA   && seta baixo
  @ li,cf say chr(025)     COLOR &LINTELA   && seta baixo
    for i = (li + 1) to (lf -1)
      @ i,ci say chr(186) COLOR &LINTELA   && quadrado cheio
      @ i,cf say chr(186) COLOR &LINTELA  && quadrado cheio
    endfor
  return .T.
* -------------------------------------------------------------------------
  function borda
  parameters li,ci,lf,cf
  private c1,c2,l1,l2
  SET COLOR OF SCHEME 1 TO &CORSCHE
  @ li-1,ci+1 say replicate(chr(223),cf-ci)
    for l1=li-1 to lf-1
      @ l1,cf+1 say chr(222)
    endfor
    SET COLOR OF SCHEME 1 TO
  return(.t.)
*---------------------------------------------------------------------------
* CENTRALIZA E IMPRIME MENSAGENS - LINHA 23
FUNCTION MENSAG
  PARAMETERS MENS
    @ 23,01 SAY SPACE(78)
    POS = 40 - INT(LEN(MENS) / 2)
    LINTELA = PAR4_P1+'/'+PAR1_P2
    @ 23,POS SAY MENS color &LINTELA
  RETURN POS
*-------------------------------------------------------------
* CENTRALIZA E IMPRIME MENSAGENS - LINHA 49
FUNCTION MENSAG49
  PARAMETERS MENS
    @ 48,01 SAY SPACE(78)
    POS = 40 - INT(LEN(MENS) / 2)
    @ 48,POS SAY MENS color &LINTELA
  RETURN POS
*-------------------------------------------------------------
* CENTRALIZA E IMPRIME MENSAGENS - LINHA 22
FUNCTION MENSAG22
  PARAMETERS MENS22
  @ 22,01 SAY SPACE(78)
    POS = 40 - INT(LEN(MENS22) / 2)
  @ 22,POS SAY MENS22 COLOR &LINTELA
*--------------------------------------------------------------
* LIMPA A LINHA 23
FUNCTION LIMPA23
  @ 23,01 SAY SPACE(78)
*---------------------------------------------------------------------------
* FSENHA - FUNCAO PARA EXIGIR SENHA DO SISTEMA
FUNCTION FSENHA
*
SET INTENSITY ON
? JANELA (18,56,21,77)
? BORDA (18,56,21,77)
*
DO WHILE .T.
*
*  USE PARAM
*  MSENHA = SCTR
SELE 0
  USE CTR_SENH INDEX CTR_SENH
*
  CSENHA = 0
*
  DO WHILE .T.
    ? MENSAG ("Informe sua £ltima SENHA cadastrada...")
      @ 19,59 SAY "Digite a Senha : "
      S=""
      DO WHILE LEN(S) < 6
        R = 0
        DO WHILE R = 0
          R=INKEY()
        ENDDO
        S=S+CHR(R)
      ENDDO
      CSENHA = CSENHA + 1
*      IF UPPER(S) <> MSENHA
      MSENHA = UPPER(S)
      
      IF !SEEK('AV      ' + RTRIM(MSENHA))
        IF CSENHA < 3
          ? MENSAG ("Senha incorreta. ENTER e repita por favor...")
          =INKEY(0)
          LOOP
        ELSE
          ? MENSAG ("Funcion rio n„o Autorizado - ENTER")
          =INKEY(0)
          CANCEL
        ENDIF
      ELSE
        EXIT
      ENDIF
    ENDDO
*
    DTLANC = CTOD(space(8))
    MDATE  = DATE()
*
    DO WHILE .NOT. DTLANC = MDATE
      @ 20,59 say "Data : "GET DTLANC PICTURE "99/99/99"
        READ
          IF DTLANC = MDATE
            RETURN
          ELSE
            ? MENSAG ('Data do Sistema ‚ '+dtoc(mdate)+' - ENTER e reinforme')
            DTLANC = CTOD("0")
            =INKEY(0)
          ENDIF
    ENDDO
ENDDO
RETURN
* --------------------------------------------------------------------------
* CABEC - IMPRIME LINHA BASICA DO CABECALHO DOS RELATORIOS
*
FUNCTION CABEC
PARAMETERS PR,TIT,HIF
  PAGCTR = PAGCTR + 1
    @ 01,01 SAY MREL
    @ 01,30 SAY PR
    @ 01,42 SAY TIT
    @ 01,112 SAY DATE()
    @ 01,124 SAY 'PAG '+STR(PAGCTR,3)
    @ 02,01 SAY REPLICATE('-',HIF)
  RETURN .T.
* --------------------------------------------------------------------------
* CABEC80 - IMPRIME LINHA BASICA DO CABECALHO DOS RELATORIOS (80 COLUNAS)
*
FUNCTION CABEC80
PARAMETERS PR,TIT,HIF
  PAGCTR = PAGCTR + 1
    @ 01,01 SAY TIT
    @ 01,69 SAY 'PAG '+STR(PAGCTR,3)
    @ 02,01 SAY MREL
    @ 02,49 SAY PR
    @ 02,59 SAY TIME()
    @ 02,69 SAY DATE()
    @ 03,01 SAY REPLICATE('-',HIF)
  RETURN
* --------------------------------------------------------------------------
* CENTRALIZA E IMPRIME MENSAGENS EM LINHAS ESPECIFICADAS
FUNCTION MENSAGR
  PARAMETERS MENS
  POS = 40 - INT(LEN(MENS) / 2)
  RETURN POS
* --------------------------------------------------------------------------
* FCGC - CALCULA OS DIGITOS DE CONTROLE DO CGC
*
FUNCTION FCGC
PARAMETERS CGCT
*
CONTADOR = 1
  DO WHILE CONTADOR <= 12
    D=IIF(CONTADOR < 10, '0'+STR(CONTADOR,1),STR(CONTADOR,2))
    D1&D = VAL(SUBS(CGCT,CONTADOR,1))
    CONTADOR = CONTADOR + 1
  ENDDO
*
DF1=5*D101+4*D102+3*D103+2*D104+9*D105+8*D106+7*D107+6*D108+5*D109+4*D110+3*D111+2*D112
DF2 = DF1 / 11
DF3 = INT(DF2) * 11
RESTO1 = DF1 - DF3
*
  IF RESTO1 = 0 .OR. RESTO1 = 1
    PRIDIG = 0
  ELSE
    PRIDIG = 11 - RESTO1
  ENDIF
* ----------------------------------------------------------------------------
*               SEGUNDO DIGITO
* ----------------------------------------------------------------------------
CONTADOR = 1
  DO WHILE CONTADOR <= 12
    D=IIF(CONTADOR < 10, '0'+STR(CONTADOR,1),STR(CONTADOR,2))
    D2&D = VAL(SUBS(CGCT,CONTADOR,1))
    CONTADOR = CONTADOR + 1
  ENDDO
*
DF4=6*D101+5*D102+4*D103+3*D104+2*D105+9*D106+8*D107+7*D108+6*D109+5*D110+4*D111+3*D112+2*PRIDIG
DF5 = DF4 / 11
DF6 = INT(DF5) * 11
RESTO2 = DF4 - DF6
*
  IF RESTO2 = 0 .OR. RESTO2 = 1
    SEGDIG = 0
  ELSE
    SEGDIG = 11 - RESTO2
  ENDIF
*
IF PRIDIG <> VAL(SUBS(CGCT,13,1)) .OR. SEGDIG <> VAL(SUBS(CGCT,14,1))
  MC = 'N'
ELSE
  MC = 'S'
ENDIF
RETURN
* --------------------------------------------------------------------------
* FCPF - CALCULA O DIGITO DE CONTROLE DO "C P F"
*
FUNCTION FCPF
PARAMETERS CPFT
*
* ----------------------------------------------------------------------------
*                PRIMEIRO DIGITO
* ----------------------------------------------------------------------------
               CONTADOR = 1
                 DO WHILE CONTADOR <= 9
                   D = '0' + STR(CONTADOR,1)
                   D2&D = VAL(SUBS(CPFT,CONTADOR,1))
                   CONTADOR = CONTADOR + 1
                 ENDDO
*
                   DF4=10*D201+9*D202+8*D203+7*D204+6*D205+5*D206+4*D207+3*D208+2*D209
                   DF5 = DF4 / 11
                   DF6 = INT(DF5) * 11
                   RESTO1 = DF4 - DF6
*
                 IF RESTO1 = 0 .OR. RESTO1 = 1
                   PRIDIG = 0
                 ELSE
                   PRIDIG = 11 - RESTO1
                 ENDIF
* ----------------------------------------------------------------------------
*               SEGUNDO DIGITO
* ----------------------------------------------------------------------------
              CONTADOR = 1
                DO WHILE CONTADOR <= 9
                  D = '0' + STR(CONTADOR,1)
                  D2&D = VAL(SUBS(CPFT,CONTADOR,1))
                  CONTADOR = CONTADOR + 1
                ENDDO
*
                  DF4=11*D201+10*D202+9*D203+8*D204+7*D205+6*D206+5*D207+4*D208+3*D209+2*PRIDIG
                  DF5 = DF4 / 11
                  DF6 = INT(DF5) * 11
                  RESTO1 = DF4 - DF6
*
                IF RESTO1 = 0 .OR. RESTO1 = 1
                  SEGDIG = 0
                ELSE
                  SEGDIG = 11 - RESTO1
                ENDIF
*
                  PRIDIG = STR(PRIDIG,1)
                  SEGDIG = STR(SEGDIG,1)
*
                IF PRIDIG <> SUBS(CPFT,10,1) .OR. SEGDIG <> SUBS(CPFT,11,1)
                  MC = 'N'
                ELSE
                  MC = 'S'
                ENDIF
*
RETURN
* --------------------------------------------------------------------------
FUNCTION FDATA
*
PARAMETERS MDATA
  TDATA = ' '
   stor val(subs(mdata,7,2)) to ano
   stor (ano - 60) to ta1
   stor int(ta1 / 4) to ta2
     if (ta2 * 4) = ta1
       qtddias =  '312931303130313130313031'
     else
       qtddias =  '312831303130313130313031'
     endif
*
   STOR VAL(SUBS(MDATA,4,2)) TO MMES
     if mmes < 1 .or. mmes > 12
       ? MENSAG('Mes inv lido - s¢ pode ser de 01 a 12 - ENTER')
       TDATA = '*'
       =INKEY(0)
     endif
   stor ((2 * mmes) - 1) to pmes
   STOR SUBS(QTDDIAS,PMES,2) TO ULTDIA
   STOR VAL(SUBS(MDATA,1,2)) TO DIA
     if dia < 1 .or. dia > VAL(ultdia)
       ? MENSAG('Dia inv lido - Sistema checa final de mes e ano bissexto - ENTER')
       TDATA = '*'
       =INKEY(0)
     endif
     if ano < 90
       ? MENSAG('Ano inv lido - N„o ‚ aceito menor que 93')
       TDATA = '*'
       =INKEY(0)
     endif
return .T.
* --------------------------------------------------------------------------
FUNCTION EXTENSOP
*           - ESCREVE VALORES POR EXTENSO E PASSA AO PROGRAMA SOLICITANTE
*             PARA DETERMINAR TAMANHO DE LINHA E IMPRIMIR
*
NUMERO = M.VDP
N01 = 'UM '
N02 = 'DOIS '
N03 = 'TRES '
N04 = 'QUATRO '
N05 = 'CINCO '
N06 = 'SEIS '
N07 = 'SETE '
N08 = 'OITO '
N09 = 'NOVE '
N00 = ' '
N10 = 'DEZ '
N11 = 'ONZE '
N12 = 'DOZE '
N13 = 'TREZE '
N14 = 'QUATORZE '
N15 = 'QUINZE '
N16 = 'DEZESSEIS '
N17 = 'DEZESSETE '
N18 = 'DEZOITO '
N19 = 'DEZENOVE '
N20 = 'VINTE '
N30 = 'TRINTA '
N40 = 'QUARENTA '
N50 = 'CINCOENTA '
N60 = 'SESSENTA '
N70 = 'SETENTA '
N80 = 'OITENTA '
N90 = 'NOVENTA '
N000 = ' '
N100 = 'CENTO '
N200 = 'DUZENTOS '
N300 = 'TREZENTOS '
N400 = 'QUATROCENTOS '
N500 = 'QUINHENTOS '
N600 = 'SEISCENTOS '
N700 = 'SETECENTOS '
N800 = 'OITOCENTOS '
N900 = 'NOVECENTOS '
MIL = 'MIL '
MILHAO = 'MILHAO '
MILHOES = 'MILHOES '
MOEDA    = 'CRUZEIRO '
MOEDA1  = 'CRUZEIROS '
CENTAVO = 'CENTAVO '
CENTAVOS = 'CENTAVOS '
*
NUMERO1 = STR(NUMERO,12,2)
CENTS   = SUBS(NUMERO1,11,2)
NUMERO  = INT(NUMERO)
CONTADOR = 1
P1 = SUBS(NUMERO1,1,3)
P2 = SUBS(NUMERO1,4,3)
P3 = SUBS(NUMERO1,7,3)
P4 = CENTS
TEXTO1 = ''
TEXTO2 = ''
TEXTO3 = ''
TEXTO4 = ''
LINHA1 = ''
LINHA2 = ''
LINHA3 = ''
*
IF VAL(P1) > 0
  STATUS1 = IIF(VAL(P1) > 1, '2','1')
ELSE
  STATUS1 = ' '
ENDIF
*
DO WHILE CONTADOR <= 4
  ITEM = STR(CONTADOR,1)
  GRUPO = 'P'+STR(CONTADOR,1)
  EXTENSO = IIF(&GRUPO='000','',LTRIM(&GRUPO))
    DO CASE
      CASE LEN(EXTENSO) = 3

        IF EXTENSO = '100'
          TEXTO&ITEM = TEXTO&ITEM + 'CEM '
          CONTADOR = CONTADOR + 1
          LOOP
        ENDIF

        IF SUBS(EXTENSO,2,2) = '00'
          TAB = 'N' + SUBS(EXTENSO,1,1) + '00'
          TEXTO&ITEM = TEXTO&ITEM + &TAB
          CONTADOR = CONTADOR + 1
          LOOP
        ENDIF

        TAB = 'N' + SUBS(EXTENSO,1,1) + '00'
        TEXTO&ITEM = &TAB + 'E '
        DEZENA = VAL(SUBS(EXTENSO,2,2))

          IF DEZENA < 20
            TAB = 'N' + SUBS(EXTENSO,2,2)
            TEXTO&ITEM = TEXTO&ITEM + &TAB
            CONTADOR = CONTADOR + 1
            LOOP
          ELSE
            TAB = 'N' + SUBS(EXTENSO,2,1) + '0'
            TEXTO&ITEM = TEXTO&ITEM + &TAB
            TAB = 'N0' + SUBS(EXTENSO,3,1)
            UNIDADE = VAL(SUBS(EXTENSO,3,1))

              IF UNIDADE > 0
                TEXTO&ITEM = TEXTO&ITEM + IIF(VAL(EXTENSO) > 10,'E ','')
              ENDIF

            TEXTO&ITEM = TEXTO&ITEM + IIF(TAB = 'N00',' ',&TAB)
          ENDIF

    CASE LEN(EXTENSO) = 2
      DEZENA = VAL(SUBS(EXTENSO,1,2))

        IF DEZENA < 20
          TAB = 'N' + SUBS(EXTENSO,1,2)
          TEXTO&ITEM = TEXTO&ITEM + &TAB
          CONTADOR = CONTADOR + 1
          LOOP
        ELSE
          TAB = 'N' +SUBS(EXTENSO,1,1) + '0'
          TEXTO&ITEM = TEXTO&ITEM + &TAB
          TAB = 'N0' + SUBS(EXTENSO,2,1)
          UNIDADE1 = VAL(SUBS(EXTENSO,2,1))

            IF UNIDADE1 > 0
              TEXTO&ITEM = TEXTO&ITEM + 'E '
            ENDIF

          TEXTO&ITEM = TEXTO&ITEM + IIF(TAB = 'N00',' ',&TAB)
        ENDIF

    CASE LEN(EXTENSO) = 1
      TAB = 'N0' + SUBS(EXTENSO,1,1)
      TEXTO&ITEM = TEXTO&ITEM + IIF(TAB = 'N00',' ',&TAB)
    OTHERWISE
      TEXTO&ITEM = ''
    ENDCASE

      CONTADOR = CONTADOR + 1

ENDDO
*
IF VAL(P1 + P2 + P3) = 0 .AND. VAL(P4) # 0
  FINAL = TEXTO4 + (IIF(VAL(P4) = 1,CENTAVO,CENTAVOS))
*  LOOP
ENDIF
*
FINAL=IIF(LEN(TEXTO1)=0,'',TEXTO1+IIF(STATUS1='1',MILHAO,MILHOES))+IIF (LEN(TEXTO2)=0,'', TEXTO2+MIL)+TEXTO3+IIF(VAL(P2+P3)=0,'DE ','')

FINAL = FINAL + IIF(VAL(P1+P2+P3)=0,'',IIF(VAL(P1+P2+P3)=1,MOEDA,MOEDA1))

FINAL=FINAL+IIF(VAL(P4)=0,'',IIF(VAL(P1+P2+P3)=0,'','E ') +TEXTO4+(IIF(VAL(P4)=1,CENTAVO,CENTAVOS)))

*
* PASSA VARIAVEL EXTENSO PARA IMPRESSAO
*
VREXT = FINAL
*
RETURN
* --------------------------------------------------------------------------
* INCLUIOB - INCLUSAO DE OBSERVACOES, BANCO E BASE DE ICM REDUZIDO EM N.F.
*
PROCEDURE INCLUIOB

  STOR SPACE(70) TO MOBSNF1, MOBSNF2, MOBSNF3, MOBSNF4
  STOR SPACE(70) TO MOBSNF5, MOBSNF6, MOBSNF7, MOBSNF8
  MESPECIE = SPACE(17)
  MVEICULO = SPACE(08)
  MQTDVOL  = 0
  MPBRUTO  = 0.00
  MPLIQ    = 0.00

  MENS = 'Inclui Observa‡”es para imprimir na Nota Fiscal ?'
  DO PEGRESP WITH [SN]
        IF MC = 'S'
          ? CABTELA(0,0,24,79)
          MENS = 'Dados complementares para imprimir na Nota Fiscal'
          ? MENSAGR(MENS)
          @ 01,POS SAY MENS
          @ 04,01 SAY '                        A t e n ‡ „ o'
          @ 05,01 say '  As observa‡”es informadas destinam-se a um Cliente, representado pelo seu'
          @ 06,01 say '  C¢digo e ser„o impressas na primeira Nota Fiscal do Cliente, caso  houver'
          @ 07,01 say '  mais de uma. Portanto, se necess rio imprimir observa‡”es diferentes para'
          @ 08,01 say '  observa‡”es para cada Nota Fiscal, selecionar os Pedidos para cada Nota e'
          @ 09,01 say '  imprimi-la antes de continuar a sele‡„o de Pedidos'

          MENS = 'Continua com a inclus„o das Observa‡”es ?'
          DO PEGRESP WITH [SN]
            IF MC = 'S'
              @ 04,01 CLEAR TO 21,76
              @ 04,03 TO 15,76 double
            
              @ 06,05 GET MOBSNF1
              @ 07,05 GET MOBSNF2
              @ 08,05 GET MOBSNF3
              @ 09,05 GET MOBSNF4
              @ 10,05 GET MOBSNF5
              @ 11,05 GET MOBSNF6
              @ 12,05 GET MOBSNF7
              @ 13,05 GET MOBSNF8
                READ
            ENDIF     
        ENDIF     

          SELE BANCOS
            @ 17,01 CLEAR TO 22,78
            @ 17,01 TO 22,78
            @ 18,07 SAY 'Qtd.Vol.'
            @ 18,17 SAY 'Especie'
            @ 18,36 say '  P.Bruto'
            @ 18,46 say 'P.L¡quido'

            @ 20,09 GET MQTDVOL PICT '99999'
            @ 20,17 GET MESPECIE
            @ 20,36 GET MPBRUTO PICT '99999.99'
              READ
            @ 20,46 GET MPLIQ   PICT '99999.99'
              READ

          IF MEDUP = 'S'
            DO WHILE .T.
              @ 18,56 SAY 'Bco/Agˆn'
              @ 20,56 GET MNBANCO PICT '999'
              @ 20,60 GET MAGENCIA PICT '9999'
                READ
                ? MENSAG(CORRIJA)

                IF SEEK(STR(MNBANCO,3) + STR(MAGENCIA,4))
                  EXIT
                ENDIF  
            ENDDO
          ENDIF  
            
          @ 18,66 SAY 'Ve¡culo'
          @ 20,66 GET MVEICULO PICT '@!'
            READ
            
          MVALORICM = 0.00
          MBASEICM  = 0.00
          
          MENS = 'Destaca ICM reduzido ?'
          DO PEGRESP WITH [NS]
            IF MC = 'S'
              @ 21,05 SAY 'Valor do ICM reduzido 'GET MVALORICM PICT '999999999.99'
              @ 21,45 SAY 'Base do ICM 'GET MBASEICM PICT '9999999999.99'
                READ
            ENDIF

            SELE OBSNOTA
            SEEK MCGC
            IF (EOF() .OR. BOF())
              APPE BLAN
            ENDIF
              REPL CGC    WITH MCGC
              REPL OBSNF1 WITH MOBSNF1,OBSNF2 WITH MOBSNF2
              REPL OBSNF3 WITH MOBSNF3,OBSNF4 WITH MOBSNF4
              REPL OBSNF5 WITH MOBSNF5,OBSNF6 WITH MOBSNF6
              REPL OBSNF7 WITH MOBSNF7,OBSNF8 WITH MOBSNF8
              REPL QTDVOL WITH MQTDVOL,ESPECIE WITH MESPECIE
              REPL PBRUTO WITH MPBRUTO,PLIQUIDO WITH MPLIQ
              REPL BANCO  WITH MNBANCO,AGENCIA  WITH MAGENCIA
              REPL VALORICM WITH MVALORICM,BASEICM WITH MBASEICM
              REPL VEICULO WITH MVEICULO

    RETURN
* -------------------------------------------------------
FUNCTION MOSTRA
PARAMETERS B1,B2,B3,B4
DEFINE WINDOW TELA FROM B1,B2 TO B3,B4 COLOR &CORARQ
ACTIVATE WINDOW TELA
RETURN
* ------------------------------------------------------
PROCEDURE ESCONDE
DEACTIVATE WINDOW TELA
SET ESCA OFF
RETURN
* ------------------------------------------------------
