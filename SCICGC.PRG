CLEAR
SET CLOCK OFF
SET PROCEDURE TO ADMVFUNC
do incluir
MC = 'N'
MENS = 'Deseja Imprimir ?' 
DO PEGRESP WITH [SN]
IF MC = 'S' 
  =Imprimir()
ENDIF  
CLOSE DATA
SET CLOCK ON
RETURN

*****************
PROCEDURE INCLUIR
*****************
  PRIVATE cTela, lFim, Janela, MREF, MQTD, Exibir, lAchou

  DEFINE WINDOW Janela FROM 00, 01 TO 03, 79 DOUBLE TITLE " Digite o CGC dos Clientes "
  DEFINE WINDOW Exibir FROM 04, 01 TO 24, 79 DOUBLE TITLE "Clientes" && COLOR +W/N,+N/W,GR+/N,+GR/N,+GR/N
  lFim = .F.
  SELE 0
  USE CLIEN ORDER P_CGC
  IF ! FILE( 'LISTACGC.DBF' )
    COPY STRU EXTE FIELDS CGC,GUE,CID,EST,ESTATUS,INIC,ULC TO XNOVO 
    CREA LISTACGC FROM XNOVO
    SELE 0
    USE LISTACGC
    INDEX ON CGC TAG P_CGC
    USE
  ENDIF
  SELE 0
  USE LISTACGC ORDER P_CGC  
  MC = 'N'
  MENS = 'Limpar os Itens lan‡ados ?'
  DO PEGRESP WITH [NS]
  IF MC = 'S'
    zap
  ENDIF  
  SAVE SCREEN TO cTela
  CLEAR
  ACTIVATE WINDOW Janela
  @ 00, 00 SAY "C.G.C.              Nome de Guerra      Cidade        Est." 
  SELE LISTACGC
  DO WHILE ! lFim
    =FazExibir() 
    MCGC  = SPACE(16)
    MCONF = 'S'
    @ 01, 00 GET MCGC PICT [@!]
    READ
    IF LASTKEY() = 27
      lFim = .T.
      LOOP
    ENDIF
    IF SEEK( MCGC, 'CLIEN' ) 
      lAchou = .T.
    ELSE
      =MENSAG('C.G.C. nao encontrado !!!' )
      =INKEY(0)
      lAchou = .F.
    ENDIF  
    
    IF lAchou       
      MGUE = CLIEN->GUE
      MCid = CLIEN->CID
      MEst = CLIEN->EST
      SET ESCAPE OFF
      @ 01, 19 SAY MGUE
      @ 01, 39 SAY MCID
      @ 01, 54 SAY MEST
      @ 01, 70 GET MCONF PICT [@!] VALID MCONF $ [SVE]      
      READ
      DO CASE
        CASE MCONF = 'S'
          IF ! SEEK( MCGC )
            APPEND BLANK
          ENDIF  
          REPL CGC      WITH MCGC
          REPL GUE      WITH MGUE
          REPL CID      WITH MCID
          REPL EST      WITH MEST
          REPL ULC      WITH CLIEN->ULC
          REPL ESTATUS  WITH CLIEN->ESTATUS
          REPL INIC     WITH CLIEN->INIC
        CASE MCONF = 'E'
          IF lAchou
            DELETE
            PACK
          ENDIF   
      ENDCASE
      SET ESCAPE ON          
    ENDIF  
  ENDDO
  DEACTIVATE WINDOW Janela
  DEACTIVATE WINDOW Exibir
  RESTORE SCREEN FROM cTela 
  
RETURN

*******************
PROCEDURE FAZEXIBIR
*******************

  ACTIVATE WINDOW EXIBIR
  GO TOP
  BROWSE FIELDS CGC:H="C.G.C.",GUE:H="Nome de Guerra",CID:H="Cidade",EST:H="Est.";
         IN WINDOW Exibir NOCLEAR NOEDIT NOWAIT
  ACTIVATE WINDOW JANELA

RETURN 

******************
PROCEDURE IMPRIMIR
******************
  PRIVATE LIN
  
  do defsaida
  LIN = 00
  
  DO IMPSAIDA
  CLOSE DATA
  
RETURN