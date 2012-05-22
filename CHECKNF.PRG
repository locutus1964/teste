PRIVATE Tela, Maxlin, Cont, lin, tamlinha

SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Verificacao de integridade de Notas Fiscais'
@ 01, Centratxt(MSG,80) SAY MSG
=MENSAG( '<ESC> para sair' )
dInicio = {00/00/00}
dFim    = {00/00/00}
@ 10, 25 SAY "DATA INICIAL:" GET dInicio
@ 12, 25 SAY "DATA FINAL..:" GET dFim
READ
IF LASTKEY() = 27
   =Finaliza()
   RETURN
ENDIF   

DO TESTIMP2
IF cp = 'N'
   =Finaliza()
   RETURN
ENDIF   

SELE 0
USE DUPL
? MENSAG( 'Indexando Duplicatas...' )
INDEX ON NDUP TO TMPDP

? MENSAG( 'Iniciando checagem...' )
SELECT NNF,DEMI,TIP,CP1,CP2,CP3,CP4;
       FROM cadnf;
       WHERE EDUP='S' AND CANCELNF=SPACE(8);
       GROUP BY NNF;
       INTO TABLE WORK
SELE WORK       
SET FILTER TO CTOD(DEMI) >= dInicio .AND. CTOD(DEMI) <= dFim
GO TOP
lin = 0
maxlin = 55
PAGCTR = 0
tamrel = 2
tamlinha = 80
cont = 0
periodo = DTOC(dInicio) + ' - ' + DTOC(dFim)
DO DEFSAIDA
DO WHILE ! EOF()
   =CABEC80( 'CHECKNF', 'Relacao da Notas Fiscais sem Duplicatas - ' + Periodo, TAMLINHA )
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
=FINALIZA()

*-------------------------------------------------------------------------
FUNCTION FINALIZA
  RESTORE SCREEN FROM Tela
  CLOSE DATA
  ERASE TMPDP.IDX
  ERASE TMPNF.IDX
  ERASE WORK.DBF   
RETURN   



