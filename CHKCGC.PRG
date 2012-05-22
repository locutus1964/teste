PRIVATE Tela, Maxlin, Cont, lin, tamlinha

SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Verificacao de integridade de Notas Fiscais'
* @ 01, Centratxt(MSG,80) SAY MSG
DO TESTIMP2
IF cp = 'N'
   =Finaliza()
   RETURN
ENDIF   

SELE 0
USE DUPL
? MENSAG( 'Indexando Duplicatas...' )
INDEX ON NDUP TO TST1

? MENSAG( 'Iniciando checagem...' )
SELECT NNF,DEMI,CGC FROM cadnf;
       WHERE EDUP='S' AND CANCELNF=SPACE(8);
       GROUP BY NNF;
       INTO TABLE WORK
SELE WORK       
GO TOP
lin = 0
maxlin = 55
PAGCTR = 0
tamrel = 2
tamlinha = 80
cont = 0
=MENSAG( 'Pesquisando...')
DO DEFSAIDA
DO WHILE ! EOF()
   =CABEC80( 'CHECKNF', 'Relacao de Duplicatas com CGC Incorreto', TAMLINHA )
   @04, 01 SAY 'CGC NF               CGC DUP          N.FISCAL  EMISSAO'
   @05, 01 SAY REPLICATE('-', TAMLINHA)
   lin = 6
   DO WHILE !EOF() .AND. lin <= maxlin
      * Verifica as NF nao canceladas, com parametro de emissao de duplicatas
      * habilitado
      IF SEEK( NNF, 'DUPL' ) .AND. DUPL->CGC <> CGC
         @ lin, 02 SAY cgc
         @ lin, 24 SAY dupl->cgc
         @ lin, 46 SAY nnf
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
=FINALIZA()

*-------------------------------------------------------------------------
FUNCTION FINALIZA
  RESTORE SCREEN FROM Tela
  CLOSE DATA
  ERASE TMPDP.IDX
  ERASE TMPNF.IDX
  ERASE WORK.DBF   
RETURN   



