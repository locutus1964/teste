************************************************************************
* AV04800 - RELATORIO DE CLIENTES RESUMIDO 
* SISTEMA ADMINISTRACAO DE VENDAS
* JAIR GONCALVES - 22/06/97
************************************************************************
PRIVATE Tela, Msg, MesAno, UltCompra, MaxLin, Lin, TamLinha, TotCli,;
        Condicao, MODULO

MODULO = PROGRAM()
Lin = 0
MaxLin = 64
TamLinha = 219
SAVE SCREEN TO Tela
CLEAR
SELE 0
USE ESTADOS INDEX ESTADOS.IDX
SELE 0
USE regiao INDEX regiao.IDX
SELE 0
USE STATUS INDEX STATUS.IDX
SELE 0 
USE VEND INDEX VEND.IDX
SELE 0 
USE ATIVIDAD INDEX ATIVIDAD.IDX

DO CABTELA WITH 00, 00, 24, 79
=MENSAG( ECODIGO )
MSG = 'Relatorio de Clientes Resumido' 
@ 01, CentraTxt(Msg,80) SAY Msg
MREGIAO  = SPACE(4)
MCVR     = EmptyRepr
MCEST    = SPACE(02)
MCEP1    = SPACE(5)
MCEP2    = SPACE(5)
MSTATUS = SPACE(1)
MATIVIDADE = SPACE(04)

@ 08, 25 SAY 'REGIAO.:' GET MREGIAO PICT [9999] 
@ 10, 25 SAY 'REPRES.:' GET MCVR    PICT [9999.99];
                        VALID MCVR = EmptyRepr .OR. ChkRepr(MCVR, .T.)
@ 12, 25 SAY 'CEP....:' GET MCEP1   PICT [99999]
@ 12, 41 GET MCEP2 PICT [99999]
@ 14, 25 SAY 'ESTADO.:' GET MCEST    PICT [!!]
@ 16, 25 SAY 'STATUS.:' GET MSTATUS PICT [!]
@ 18, 25 SAY 'ATIVID.:' GET MATIVIDADE PICT [@!]
READ

IF LASTKEY() = 27
   =FINALIZA()
   RETURN
ENDIF   

MENS = 'Confirma os dados'
DO PEGRESP WITH 'SN'
IF MC = 'S'
   DO TESTIMP2 
   IF CP = 'N'
      =FINALIZA()
      RETURN
   ENDIF   
   =MENSAG( 'Selecionando clientes...' )   
   =Seleciona()
   SELE CADWORK
   tamrel = 2
   PAGCTR = 0
   TotCli = 0
   GO TOP
   DO DEFSAIDA
   DO WHILE !EOF()
      =ImprCabec()
      DO WHILE !EOF() .AND. lin <= Maxlin
        IF ULC >= ULCSP 
          UltCompra = DTOC(ULC) 
        ELSE
          UltCompra = DTOC(ULCSP) +'S'
        ENDIF    
        @Lin,  01 SAY REGIAO
        @Lin,  06 SAY VEN
        @Lin,  11 SAY RAZ
        @Lin,  57 SAY SUBSTR(CID,1,19)
        @Lin,  82 SAY EST
        @Lin,  85 SAY CEP
        @Lin,  95 SAY END
        @Lin, 141 SAY SUBSTR(BAI,1,19)
        @Lin, 161 SAY DDD1
        @Lin, 166 SAY FON1
        @LIN, 176 SAY CONTATO 
        @Lin, 197 SAY ESTATUS
        @Lin, 199 SAY CONCEITO
        @Lin, 202 SAY INIC
        @Lin, 211 SAY UltCompra
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
ENDIF   
    
=FINALIZA()

*****************
FUNCTION FINALIZA
*****************
  CLOSE DATA
  RESTORE SCREEN FROM Tela
RETURN

*******************
PROCEDURE SELECIONA
*******************
  PRIVATE cFiltro
    
  cFiltro = ''
  * Regiao
  IF ! EMPTY(MREGIAO)
    cFiltro = 'C.REGIAO = "' + MREGIAO + '"'
  ENDIF   
  * Representante  
  IF MCVR <> EmptyRepr
     cTexto = 'C.IDREPR = ' + SUBSTR(MCVR,6,2) 
     IF ! EMPTY(cFiltro)
       cFiltro = cFiltro + ' AND ' + cTexto
     ELSE  
       cFiltro = cTexto
     ENDIF  
  ENDIF     
  * CEP
  IF ! EMPTY(MCEP1) .AND. !EMPTY(MCEP2)
     cTexto = 'C.CEP >= "' + MCEP1 + '" AND  C.CEP <= "' + MCEP2 + '"'
     IF ! EMPTY(cFiltro)
       cFiltro = cFiltro + ' AND ' + cTexto
     ELSE
       cFiltro = cTexto
     ENDIF  
  ENDIF     
  * ESTADO
  IF ! EMPTY(MCEST)
     cTexto = 'C.EST = "' + MCEST + '"'
     IF ! EMPTY(cFiltro)
       cFiltro = cFiltro + ' AND ' + cTexto
     ELSE
       cFiltro = cTexto
     ENDIF  
  ENDIF     
  * STATUS
  IF ! EMPTY(MSTATUS)
     cTexto = 'C.ESTATUS = "' + MSTATUS + '"'
     IF ! EMPTY(cFiltro)
       cFiltro = cFiltro + ' AND ' + cTexto
     ELSE
       cFiltro = cTexto
     ENDIF  
  ENDIF     
  * ATIVIDADE
  IF ! EMPTY(MATIVIDADE)
     cTexto = 'C.ATIVIDADE = "' + MATIVIDADE + '"'
     IF ! EMPTY(cFiltro)
       cFiltro = cFiltro + ' AND ' + cTexto
     ELSE
       cFiltro = cTexto
     ENDIF  
  ENDIF     

  IF EMPTY(cFiltro)
    cFiltro = '.T.'
  ENDIF

  SELECT;
    C.REGIAO, C.VEN, C.RAZ, C.CID, C.CEP, C.EST, C.END, C.BAI, C.DDD1, C.FON1,;
    C.ESTATUS, C.CONCEITO, C.INIC, C.ULC, C.ULCSP, C.CONTATO;
  FROM;
    CLIEN C;
  WHERE &cFiltro;
  ORDER BY;
    C.CEP, C.END;
  INTO TABLE;
    CADWORK
 
RETURN 

*******************
PROCEDURE IMPRCABEC
*******************
  
  =CABEC219( MODULO, 'Relacao de Clientes Resumida' )
  @05,  01 SAY 'REG'
  @05,  06 SAY 'REP'
  @05,  11 SAY 'RAZAO SOCIAL'
  @05,  57 SAY 'CIDADE'
  @05,  82 SAY 'UF'
  @05,  85 SAY 'CEP'
  @05,  95 SAY 'ENDERECO'
  @05, 141 SAY 'BAIRRO'
  @05, 161 SAY 'DDD1'
  @05, 166 SAY 'FONE1' 
  @05, 175 SAY 'COMP.'
  @05, 196 SAY 'ST'
  @05, 199 SAY 'CO'
  @05, 202 SAY 'INIC'
  @05, 211 SAY 'ULC'
  @06, 01 SAY REPLICATE('-', TamLinha)
  LIN = 7
RETURN 

