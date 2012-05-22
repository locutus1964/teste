************************************************************************
* AV04600 - ALTERA CLIENTE PARA INATIVO BASEADO NO MES/ANO DA ULT.COMPRA
* SISTEMA ADMINISTRACAO DE VENDAS
* JAIR GONCALVES - 23/04/97
************************************************************************
PRIVATE Tela, Msg, MesAno, UltCompra, MaxLin, Lin, TamLinha, TotCli,;
        Condicao

Lin = 0
MaxLin = 56
TamLinha = 80
SAVE SCREEN TO Tela
CLEAR
SELE 0
USE regiao ORDER X_REGIAO
SELE 0
USE clien ORDER X_RAZ
SELE 0
USE STATUS INDEX STATUS.IDX
SELE CLIEN
SET RELATION TO ESTATUS INTO STATUS

DO CABTELA WITH 00, 00, 24, 79
=MENSAG( ECODIGO )
MSG = 'Inativa Clientes por Regiao e Mes da Ult.Compra'
@ 01, CentraTxt(Msg,80) SAY Msg
MESANO  = SPACE(05)
MREGIAO = EmptyRegiao

@ 10, 20 SAY 'REGIAO.:' GET MREGIAO PICT [9999.99] VALID ExibRegiao()
@ 12, 20 SAY 'MES/ANO:' GET MESANO  PICT [99/99]
READ

IF LASTKEY() = 27
   =FINALIZA()
   RETURN
ENDIF   

UltCompra = CTOD('01/' + MesAno)
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
      =CABEC80('AV04600', 'Relacao de Clientes a serem inativados', TamLinha)
      @03, 01 SAY 'Regiao: ' + MREGIAO
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
           @Lin, 47 SAY Ultima
           @Lin, 59 SAY STATUS->DSTATUS
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
        SELE 0
        USE REGIAO ORDER X_REGIAO
        SELE 0
        USE CLIEN 
        =MENSAG('Atualizando clientes...')
        DO WHILE ! EOF()
          IF ChkCliente()
            REPLACE ESTATUS   WITH 'I' 
            SELE REGIAO
            IF SEEK(Clien->REGIAO + str(Clien->IDREGIAO,2))
              REPLACE DT_INATIV WITH UltCompra
            ENDIF
          ENDIF  
          SELE CLIEN
          SKIP
        ENDDO  
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

  Ultima = IIF( CLIEN->ULC > CLIEN->ULCSP, CLIEN->ULC, CLIEN->ULCSP )
  Seleciona = ((! EMPTY(CLIEN->ULC) .OR. ! EMPTY(CLIEN->ULCSP)) .AND. Ultima < UltCompra) .AND.;
              CLIEN->ESTATUS = 'A' .AND. ((FmtRegiao(CLIEN->REGIAO, CLIEN->IDREGIAO) = MREGIAO) .OR. (MREGIAO = EmptyRegiao))
  
RETURN Seleciona
 
*******************
FUNCTION ExibRegiao
*******************
  PRIVATE lAchou
  
  =MENSAG('') 
  lAchou = .F.
  IF MREGIAO = EmptyRegiao
    lAchou = .T.
    @ 10, 40 SAY '(Todas)'
  ELSE  
    lAchou = ChkRegiao(MREGIAO, .T.)
    IF lAchou
      @ 10, 40 SAY REGIAO->DREGIAO 
    ENDIF   
  ENDIF  
  
RETURN lAchou
 
