PRIVATE Tela, Maxlin, Cont, lin, tamlinha

SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Verificacao de integridade de Notas Fiscais/Duplicatas'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )
dInicio = CTOD('')
dFim    = CTOD('')
@ 10, 25 SAY "DATA INICIAL:" GET dInicio
@ 12, 25 SAY "DATA FINAL..:" GET dFim
READ
IF LASTKEY() = 27
   =Finaliza()
   RETURN
ENDIF   

*------- Preparando as Tabelas 

SELE 0
USE DUPL
? MENSAG( 'Indexando Duplicatas...' )
INDEX ON NDUP TO TMPDUP.IDX

? MENSAG( 'Indexando Clientes...')
SELE 0
USE CLIEN 
INDEX ON CGC TO TMPCLI.IDX

*-------------- Checagem das Duplicatas com CGC Incorreto 
=MENSAG( 'Iniciando...' )
=MENSAG( 'Checando Duplicatas com CGC Invalido...' )
SELE 0
  SELECT; 
    NNF,DEMI,CGC;
  FROM; 
    cadnf;
  WHERE; 
    EDUP='S' AND EMPTY(CANCELNF);
  GROUP BY;
    NNF;
  INTO TABLE ;
    WORK1
SELE WORK1       
GO TOP
lin = 0
maxlin = 55
PAGCTR = 0
tamrel = 2
tamlinha = 80
cont = 0

* Seleciona Dispositivo de Saida
DO TESTIMP2
IF cp = 'N'
   =Finaliza()
   RETURN
ENDIF   

=MENSAG( 'Preparando Relatorio...')
DO DEFSAIDA
DO WHILE ! EOF()
   =CABEC80( 'CHKGERAL', 'Relacao de Duplicatas com CGC Incorreto', TAMLINHA )
   @04, 01 SAY 'CGC.NF               CGC.DUP              N.FISCAL  EMISSAO'
 
   @05, 01 SAY REPLICATE('-', TAMLINHA)
   lin = 6
   DO WHILE !EOF() .AND. lin <= maxlin
      * Verifica as NF nao canceladas, com parametro de emissao de duplicatas
      * habilitado
      IF SEEK( NNF, 'DUPL' ) .AND. DUPL->CGC <> CGC
         @ lin, 01 SAY cgc
         @ lin, 22 SAY dupl->cgc
         @ lin, 43 SAY nnf
         @ lin, 53 SAY demi
         lin = lin + 1
         cont = cont + 1
      ENDIF   
      SKIP
   ENDDO  
   IF EOF()
      @ lin, 01 SAY REPLICATE( '-', TAMLINHA )
      @ lin + 1, TAMLINHA-29 SAY 'Total de Notas Fiscais: ' + STR( Cont, 5, 0 )
   ENDIF   
ENDDO
DO IMPSAIDA

*---------- Checagem das Notas Fiscais sem Duplicatas -------------*

SELE 0
USE DUPL INDEX TMPDUP.IDX

=MENSAG( 'Checando Notas Fiscais sem Duplicatas...' )

SELECT NNF,DEMI,TIP,CP1,CP2,CP3,CP4;
       FROM cadnf;
       WHERE EDUP='S' AND CANCELNF=SPACE(8);
       GROUP BY NNF;
       INTO TABLE TMP
SELE TMP
COPY TO WORK2;
     FOR (CTOD(DEMI)+CP1 >= dInicio .AND. CTOD(DEMI)+CP1 <= dFim) .OR.;
         (CP2 <> 0 .AND. (CTOD(DEMI)+CP2 >= dInicio .AND. CTOD(DEMI)+CP2 <= dFim)) .OR.;
         (CP3 <> 0 .AND. (CTOD(DEMI)+CP3 >= dInicio .AND. CTOD(DEMI)+CP3 <= dFim)) .OR.;
         (CP4 <> 0 .AND. (CTOD(DEMI)+CP4 >= dInicio .AND. CTOD(DEMI)+CP4 <= dFim))
USE WORK2      
GO TOP
lin = 0
maxlin = 55
PAGCTR = 0
tamrel = 2
tamlinha = 80
cont = 0
periodo = DTOC(dInicio) + ' - ' + DTOC(dFim)

* Seleciona Dispositivo de Saida
DO TESTIMP2
IF cp = 'N'
   =Finaliza()
   RETURN
ENDIF   
=MENSAG( 'Preparando Relatorio...')
DO DEFSAIDA
DO WHILE ! EOF()
   =CABEC80( 'CHKGERAL', 'Relacao da Notas Fiscais sem Duplicatas - ' + Periodo, TAMLINHA )
   @04, 01 SAY 'N.FISCAL  EMISSAO TIPO Venc1    Venc2    Venc3    Venc4'
   @05, 01 SAY REPLICATE('-', TAMLINHA)
   lin = 6
   DO WHILE !EOF() .AND. lin <= maxlin
      * Verifica as NF nao canceladas, com parametro de emissao de duplicatas
      * habilitado
      IF ! SEEK( NNF, 'DUPL' )
         @ lin, 02 SAY nnf
         @ lin, 10 SAY demi
         @ lin, 19 SAY tip
         @ lin, 24 SAY CTOD(demi) + CP1
         IF CP2 > 0
            @ lin, 33 SAY CTOD(demi) + CP2
         ENDIF   
         IF CP3 > 0   
            @ lin, 42 SAY CTOD(demi) + CP3
         ENDIF  
         IF CP4 > 0
            @ lin, 51 SAY CTOD(demi) + CP4
         ENDIF   
         lin = lin + 1
         cont = cont + 1
      ENDIF   
      SKIP
   ENDDO  
   IF EOF()
      @ lin, 01 SAY REPLICATE( '-', TAMLINHA )
      @ lin + 1, TAMLINHA-29 SAY 'Total de Notas Fiscais: ' + STR( Cont, 5, 0 )
   ENDIF   
ENDDO
DO IMPSAIDA

*----------  Checagem das Duplicatas sem Nota Fiscal ----------*

SELE 0
USE CLIEN INDEX TMPCLI.IDX

SELE 0
USE CADNF
? MENSAG( 'Indexando Notas Fiscais...' )
INDEX ON NNF TO TMPNF.IDX

? MENSAG( 'Checando Duplicatas sem Nota Fiscal...' )
SELE 0
USE DUPL 
COPY FIELDS CGC, NDUP, PDUP, EMIS, VDP, VENC, PAG, DTCANCEL;
     TO WORK3;
     FOR ! EMPTY(PAG) .AND.;
           EMPTY(dtcancel) .AND.;
           PAG >= dInicio .AND. PAG <= dFim
USE WORK3                 
INDEX ON DTOS(EMIS)+STR(NDUP,5) TO TMPWORK.IDX
SET RELATION TO CGC INTO CLIEN
GO TOP
lin = 0
maxlin = 55
PAGCTR = 0
tamrel = 2
tamlinha = 80
cont = 0
periodo = DTOC(dInicio) + ' - ' + DTOC(dFim)

* Seleciona Dispositivo de Saida
DO TESTIMP2
IF cp = 'N'
   =Finaliza()
   RETURN
ENDIF   

=MENSAG( 'Preparando Relatorio...' )                 
DO DEFSAIDA
DO WHILE ! EOF()
   =CABEC80( 'CHKGERAL', 'Relacao de Duplicatas sem Nota Fiscal - ' + periodo, TAMLINHA )
   @04, 01 SAY 'DUPL'
   @04, 10 SAY 'RAZAO SOCIAL'
   @04, 36 SAY 'EMISSAO'
   @04, 45 SAY 'VALOR'
   @04, 56 SAY 'VENC'
   @04, 65 SAY 'PAGTO'
   @05, 01 SAY REPLICATE('-', TAMLINHA)
   lin = 6
   DO WHILE !EOF() .AND. lin <= maxlin
      * Verifica as DUPLS nao canceladas, constam das Notas Fiscais
      IF ! SEEK( NDUP, 'CADNF' )
         @ lin, 01 SAY str( ndup, 5 ) + '/' + str(pdup,1)
         @ lin, 10 SAY SUBSTR(clien->raz,1,25)
         @ lin, 36 SAY emis
         @ lin, 45 SAY vdp PICT [999,999.99]
         @ lin, 56 SAY venc
         @ lin, 65 SAY pag
         lin = lin + 1
         cont = cont + 1
      ENDIF   
      SKIP
   ENDDO  
   IF EOF()
      @ lin, 01 SAY REPLICATE( '-', TAMLINHA )
      @ lin + 1, TAMLINHA-29 SAY 'Total de Duplicatas: ' + STR( Cont, 5, 0 )
   ENDIF   
ENDDO
DO IMPSAIDA
=FINALIZA()

*-------------------------------------------------------------------------
FUNCTION FINALIZA
  RESTORE SCREEN FROM Tela
  CLOSE DATA
  ERASE TMPDP.IDX
  ERASE TMPNF.IDX
  ERASE TMPCLI.IDX
  ERASE WORK1.DBF   
  ERASE WORK2.DBF
  ERASE WORK3.DBF
RETURN   
