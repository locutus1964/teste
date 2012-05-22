PRIVATE Tela, Maxlin, Cont, lin, tamlinha

SAVE SCREEN TO Tela
CLEAR
DO CABTELA WITH 00,00,24,79
MSG = 'Verificacao de integridade de Duplicatas'
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

? MENSAG( 'Indexando Clientes...')
SELE 0
USE CLIEN 
INDEX ON CGC TO TMPCLI

SELE 0
USE CADNF
? MENSAG( 'Indexando Notas Fiscais...' )
INDEX ON NNF TO TMPNF

? MENSAG( 'Iniciando checagem...' )
SELE 0
USE DUPL
COPY FIELDS CGC, NDUP, PDUP, EMIS, VDP, VENC, PAG, DTCANCEL;
     TO WORK FOR ! EMPTY(CTOD(PAG)) .AND.;
                 EMPTY(DTCANCEL) .AND.;
                 CTOD(PAG) >= dInicio .AND. CTOD(PAG) <= dFim
? MENSAG( 'Preparando Relatorio...' )                 
SELE 0
USE WORK                 
INDEX ON DTOS(CTOD(EMIS))+STR(NDUP,5) TO TMPWORK
SET RELATION TO CGC INTO CLIEN
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
   =CABEC80( 'CHECKDUP', 'Relacao de Duplicatas sem Nota Fiscal - ' + periodo, TAMLINHA )
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
  ERASE WORK.DBF   
RETURN   



