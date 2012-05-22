************************************************************************
* AV04600 - ALTERA CLIENTE PARA INATIVO BASEADO NO MES/ANO DA ULT.COMPRA
* SISTEMA ADMINISTRACAO DE VENDAS
* JAIR GONCALVES - 23/04/97
************************************************************************
PRIVATE Tela, Msg, MesAno, DataCompra, MaxLin, Lin, TamLinha, TotCli,;
        Condicao, MODULO, NovoStatus

MODULO = PROGRAM()
NovoStatus = ''
Lin = 0
MaxLin = 56
TamLinha = 132
SAVE SCREEN TO Tela
CLEAR
SELE 0
USE ESTADOS INDEX ESTADOS.IDX
SELE 0
USE clien order x_raz
SELE 0
USE STATUS INDEX STATUS.IDX
SELE CLIEN
SET RELATION TO ESTATUS INTO STATUS

DO CABTELA WITH 00, 00, 24, 79
=MENSAG( ECODIGO )
MSG = 'Inativa Clientes por Estado e Mes da Ult.Compra'
@ 01, CentraTxt(Msg,80) SAY Msg
MESANO  = SPACE(05)
MCEST = SPACE(2)

@ 10, 25 SAY 'ESTADO.:' GET MCEST PICT [!!] VALID ! EMPTY(MCEST) .AND. ExibEstado()
@ 12, 25 SAY 'MES/ANO:' GET MESANO  PICT [99/99]
READ

IF LASTKEY() = 27
   =FINALIZA()
   RETURN
ENDIF   

DataCompra = CTOD('01/' + MesAno)
MENS = 'Confirma os dados'
DO PEGRESP WITH 'SN'
IF MC = 'S'
   
   SELE CLIEN
   tamrel = 2
   PAGCTR = 0
   DO TESTIMP2 
   IF CP = 'N'
      =FINALIZA()
      RETURN
   ENDIF   
   =MENSAG( 'Selecionando clientes...' )
   TotCli = 0
   Ultima = {}
   GO TOP
   DO DEFSAIDA
   DO WHILE !EOF()
      =CABEC(MODULO, 'Relacao de Clientes a serem inativados', TamLinha )
      @03, 01 SAY 'UF: ' + ESTADOS->DEST
      @04, 01 SAY 'Razao Social'
      @04, 47 SAY 'Ult.Compra'
      @04, 59 SAY 'Status Atual'
      @05, 01 SAY REPLICATE('-', TamLinha)
      Lin = 6
      DO WHILE !EOF() .AND. lin <= Maxlin
         * So processa os clientes com data de ultima compra existente 
         * e que esteja ativo
         IF ChkCliente()
           @Lin, 01 SAY RAZ
           @Lin, 47 SAY EST
           @Lin, 50 SAY ULTIMA
           @Lin, 62 SAY STATUS->DSTATUS
           Lin = Lin + 1
           TotCli = TotCli + 1
         ENDIF  
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
      * Altera definitivamente o arquivo de clientes
      MENS = 'Efetivar as alteracoes'
      DO PEGRESP WITH 'SN'
      IF MC = 'S'
         USE CLIEN
         =MENSAG('Atualizando clientes...')
         REPLACE ALL ESTATUS WITH NovoStatus FOR ChkCliente()
      ENDIF
    ENDIF  
ENDIF   
    
=FINALIZA()

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
  ULTIMA = IIF( ULC > ULCSP, ULC, ULCSP )
  IF EMPTY(ULTIMA)
    IF EST = MCEST
      NovoStatus = 'P'
      Seleciona  = .T.
    ENDIF  
  ELSE  
    Seleciona = Ultima < DataCompra .AND. EST = MCEST
    IF Seleciona 
      NovoStatus = 'I'    
    ELSE
      IF ESTATUS = 'I'
        NovoStatus = 'A'
        Seleciona = .T.
      ENDIF  
    ENDIF  
  ENDIF
  
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
 
