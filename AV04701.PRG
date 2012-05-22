************************************************************************
* AV04701 - ALTERA CLIENTE PARA INATIVO BASEADO NO MES/ANO DA ULT.COMPRA
* SISTEMA ADMINISTRACAO DE VENDAS
* JAIR GONCALVES - 23/04/97
************************************************************************
PRIVATE Tela, Msg, MesAno, DataLimite, MaxLin, Lin, TamLinha, TotCli,;
        Condicao, MODULO, NovoStatus

MODULO = PROGRAM()
Lin = 0
MaxLin = 56
TamLinha = 132
SAVE SCREEN TO Tela
CLEAR
SELE 0
USE CLISTAT 
ZAP
SELE 0
USE ESTADOS ORDER P_CEST
SELE 0
USE clien order x_raz
SET FILTER TO ESTATUS = 'A' .OR. ESTATUS = 'I' .OR. ESTATUS = 'P' 
SELE CLIEN
go top

DO CABTELA WITH 00, 00, 24, 79
=MENSAG( ECODIGO )
MSG = 'Inativa Clientes por Estado e Mes da Ult.Compra'
@ 01, CentraTxt(Msg,80) SAY Msg
MESANO = SPACE(05)
MCEST  = SPACE(2)

@ 10, 25 SAY 'ESTADO.:' GET MCEST PICT [!!] VALID ! EMPTY(MCEST) .AND. ExibEstado()
@ 12, 25 SAY 'MES/ANO:' GET MESANO  PICT [99/99]
READ

IF LASTKEY() = 27
   =FINALIZA()
   RETURN
ENDIF   
CP = ''
DataLimite = CTOD('01/' + MesAno)
MENS = 'Confirma os dados'
DO PEGRESP WITH 'SN'
IF MC = 'S' THEN
  IF CP = 'N' THEN
    =FINALIZA()
    RETURN
  ENDIF   
  =MENSAG( 'Selecionando clientes...' )
  UltCompra = {}
  NovoStatus = ''
  SELE CLIEN
  GO TOP
  DO WHILE !EOF()
    * So processa os clientes com data de ultima compra existente 
    * e que esteja ativo
    IF ChkCliente()
      SELE CLISTAT
      APPEND BLANK
      REPLACE CGC        WITH CLIEN->CGC
      REPLACE RAZ        WITH CLIEN->RAZ
      REPLACE EST        WITH CLIEN->EST
      REPLACE ULC        WITH UltCompra
      REPLACE ESTATUS    WITH CLIEN->ESTATUS
      REPLACE NOVOSTATUS WITH M->NovoStatus
    ENDIF  
    SELE CLIEN
    SKIP
  ENDDO
  SELE CLISTAT
  IF RECCOUNT() = 0 THEN
    =MENSAG('Nenhum cliente precisa ser alterado !!!')
    =INKEY(0)
  ELSE  
    =Imprimir()  
  ENDIF  
ENDIF   
    
=FINALIZA()

*****************
PROCEDURE IMPRIMIR
*****************
  PRIVATE TotCli
  
  TotCli = 0
  CLOSE DATA
  CP = 'N'
  tamrel = 2
  PAGCTR = 0
  DO TESTIMP2 
  IF CP = 'N'
    RETURN
  ENDIF  
  SELE 0
  USE STATUS INDEX STATUS.IDX
  SELE 0
  USE ESTADOS ORDER P_CEST  
  SELE 0
  USE CLISTAT  
  set relation to EST INTO estados
  DO DEFSAIDA    
  GO TOP
  DO WHILE !EOF()
    =CABEC(MODULO, 'Relacao de Clientes a serem inativados', TamLinha )
    @03, 01 SAY 'UF: ' + ESTADOS->DEST
    @04, 01 SAY 'Razao Social'
    @04, 47 SAY 'Ult.Compra'
    @04, 59 SAY 'Status Atual'
    @04, 90 SAY 'Novo Status'    
    @05, 01 SAY REPLICATE('-', TamLinha)
    Lin = 6
    DO WHILE !EOF() .AND. lin <= Maxlin
      @Lin, 01 SAY RAZ
      @Lin, 47 SAY EST
      @Lin, 50 SAY ULC
      =SEEK(ESTATUS, 'STATUS')
      @Lin, 59 SAY STATUS->DSTATUS
      =SEEK(NOVOSTATUS, 'STATUS')      
      @Lin, 90 SAY STATUS->DSTATUS      
      Lin = Lin + 1
      TotCli = TotCli + 1
      SKIP
    ENDDO
  ENDDO  
  IF EOF()
    @Lin+0, 001 SAY REPLICATE('-', TamLinha)
    @Lin+1, 050 SAY 'Total de Clientes: ' + STR(TOTCLI,6,0)
    @Lin+2, 001 SAY REPLICATE('-', TamLinha)
  ENDIF
  DO IMPSAIDA
  IF TOTCLI > 0
    CLOSE DATA
    * Altera definitivamente o arquivo de clientes
    MENS = 'Efetivar as alteracoes'
    DO PEGRESP WITH 'NS'
    IF MC = 'S'
      =MENSAG('Atualizando clientes...')    
      SELE 0
      USE CLIEN ORDER P_CGC
      SELE 0
      USE CLISTAT
      DO WHILE !EOF()
        IF SEEK(CGC, 'CLIEN') .AND. CLIEN->ESTATUS <> NOVOSTATUS then
          REPLACE CLIEN->ESTATUS WITH CLISTAT->NOVOSTATUS
        ENDIF
        SKIP
      ENDDO
    ENDIF
  ENDIF  
RETURN

*****************
FUNCTION FINALIZA
*****************
  CLOSE DATA
  RESTORE SCREEN FROM Tela
RETURN

*******************
FUNCTION ChkCliente
*******************
PRIVATE Seleciona

  Seleciona = .F.
  NovoStatus = ESTATUS
  UltCompra = IIF( ULC > ULCSP, ULC, ULCSP )
  IF EMPTY(UltCompra) THEN
    IF EST = MCEST
      IF EMPTY(INIC) THEN
        Seleciona = .T.
        NovoStatus = 'P'
      ENDIF
    ENDIF  
  ELSE  
    IF UltCompra < DataLimite THEN
      IF EST = MCEST THEN 
        Seleciona = .T.
        NovoStatus = 'I'
      ENDIF  
    ENDIF  
  ENDIF
  Seleciona = ESTATUS <> NovoStatus 

RETURN Seleciona
 
*******************
FUNCTION ExibEstado
*******************
  PRIVATE lAchou

  =MENSAG('')
  lAchou = SEEK( MCEST, 'ESTADOS') 
  IF lAchou
    @ 10, 40 SAY ESTADOS->DEST
  ELSE 
    =MENSAG('Estado n∆o encontrado')
  ENDIF    
  
RETURN lAchou
 
