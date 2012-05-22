* AV11300 - IMPRESSAO DE COPIA DE NOTA FISCAL
* SISTENA EMISSAO DE NOTAS FISCAIS - ABRIL
* ADAO ELI PEREIRA   -   28/10/93
*
DO TESTIMP2
IF CP = 'N'
  RETURN
ENDIF
*
NFINI = 0
NFFIM = 0
lPesFisica = .F.
MVEICULO = SPACE(08)
MQTDVOL  = 0
MESPECIE = " "
MPESO    = 0.00
MPLIQUIDO = 0
MPBRUTO   = 0
MBASEIPI  = 0
MBASEICM  = 0
*
  MC = ' '
  ? JANELA(13,34,18,78)
  ? BORDA(13,34,18,78)
  @ 15,37 SAY 'Primeira Nota Fiscal a imprimir 'GET NFINI PICT '999999'
  @ 16,37 SAY 'Ultima Nota Fiscal a imprimir   'GET NFFIM PICT '999999'
  READ

  MENS = DADCORR
  DO PEGRESP WITH [SN]
    IF MC = 'N'
      CLOS DATA
      RETURN
    ENDIF
*
? MENSAG('Preparando a impress„o das C¢pias das Notas Fiscais... Aguarde')
  CLOS DATA
  USE CADNF
  COPY TO CADNF2 FOR NNF >= NFINI .AND. NNF <= NFFIM

CLOS DATA
*
SELE 0
  USE PROD    INDEX PROD.IDX
SELE 0
  USE DUPL    ORDER xdupano
SELE 0
  USE CADNF2
SELE 0
  USE TRANS   INDEX TRANS.IDX, TRARAZ.IDX
SELE 0
  USE CLIEN   ORDER P_CGC
SELE 0
  USE TPNF    INDEX TPNF.IDX
SELE 0
  USE CONDPAG   INDEX CONDPAG.IDX
SELE 0
  USE PEDID   ORDER X_PEDREF
SELE 0
  USE PEDIC   ORDER P_PED
SELE 0
  USE CLI_COBR ORDER P_CGC
SELE 0
  USE CLI_ENTR ORDER P_CGC
SELE 0
  USE VEND     INDEX VEND.IDX
SELE 0
  USE ESTADOS  INDEX ESTADOS.IDX
SELE 0
  USE CABECNF  ORDER P_NNF

  SELE CADNF2
    IF BOF()
      ? MENSAG('N„o existem Notas Selecionadas - ENTER para encerrar')
      =INKEY(0)
      CLOS DATA
      RETURN
    ENDIF

PAGCTR = 0
TAMREL = 2

? MENSAG('Imprimindo C¢pia de Notas Fiscais... Aguarde')

DO DEFSAIDA

SELE CADNF2
DO WHILE !EOF()
*
  MNNF   = NNF
  SELE CABECNF
  IF SEEK(MNNF)
     MQTDVOL   = QTDVOL
     MESPECIE  = ESPECIE
     MPBRUTO   = PBRUTO
     MPLIQUIDO = PLIQUIDO
     MVEICULO  = VEICULO
  ENDIF

  SELE CADNF2

  MPESO = 0
  MVOLUME = 0
  STOR 0.00 TO GVAL, MVAL, MDIC
  MCGC   = CGC
  MPINT  = SPACE(06)
  MPCL   = SPACE(13)
  INSCLI = SPACE(13)
  MREF   = 0
  MGUE   = SPACE(15)
  MDESN  = SPACE(18)
  MPED   = PED
  MVEN   = VEN

  SELE CLIEN
  lPesFisica = .F.
  IF SEEK( CADNF2->CGC, 'CLIEN' )  
    lPesFisica = CLIEN->FIJU = 'F'
  ENDIF

  SELE CADNF2
  STOR 0.00 TO MICM, MEMB, MFRE, TOTISS, VTOTA
  STOR 0.00 TO TOTIPI, IVDIC, VDIC, MISSP, VUNIT
  PIPI  = 0.0
  MIPI  = 0.00
  ENDENT1 = " "
  ENDENT2 = " "
  MMNF1   = " "
  MMNF2   = " "
  MINS    = " "
  MEST    = " "

  IF !EOF()
    MCGC      = CGC
    MTIP      = TIP
    MNOPE     = NOPE
    MDEMI     = DEMI
    MTRA      = TRA
    MCPG      = CPD
    MEMB       = EMB
    MFRE       = FRE
    MDESCONTO  = DESCONTO
    MCANCELNF  = CANCELNF
  ENDIF

  SELE TPNF
  IF SEEK(MTIP)
    MMNF1 = MNF1
    MMNF2 = MNF2
    MDESN = DESN
    MCODT = CODT
  ENDIF
*
  @ 05,10 SAY 'Copia de Nota Fiscal '+STR(MNNF,6)
  @ 05,10 SAY 'Copia de Nota Fiscal '+STR(MNNF,6)

  IF MCANCELNF <> SPACE(08)
    @ 05,80 SAY 'NOTA FISCAL CANCELADA EM: '+MCANCELNF
    @ 05,80 SAY 'NOTA FISCAL CANCELADA EM: '+MCANCELNF
  ENDIF

  @ 07,001 SAY 'Vendedor : '+MVEN
    SELE VEND
    IF SEEK(MVEN)
      @ 07,20 SAY 'Regiao : '+FmtRegiao(CABECNF->REGIAO,CABECNF->IDREGIAO)
    ENDIF

  @ 07,065 SAY 'Codigo Fiscal     : '+MNOPE

  SELE PEDIC
  IF SEEK(MPED)
    @ 08,001 SAY 'Ped Cliente: '+PCL+'  Ped Repres: '+PINT+'  Ped Fornec: '+str(MPED,6)
    IF lPesFisica 
      cDescNATOP = 'VENDA CONSUMIDOR'
    ELSE
      cDescNATOP =  MDESN
    ENDIF  
    @ 08,065 say 'Natureza Operacao : '+cDescNATOP
  ENDIF

  SELE TRANS
  IF SEEK(MTRA)
    @ 09,065 SAY 'Via de Transporte : '+VIA
  ENDIF

  SELE CLIEN
  IF SEEK(MCGC)
    @ 10,065 SAY 'Data da Emissao   : '+MDEMI
    @ 11,001 SAY 'Cliente  : '+RAZ
    @ 12,001 SAY 'Endereco : '+RTRIM(END)+' - CEP : '+CEP
    @ 13,001 SAY 'Cidade   : '+RTRIM(CID)+' - Bairro : '+RTRIM(BAI)+' - Estado : '+EST
      MCGC = CGC
    @ 14,001 SAY 'C.G.C.   : '+MCGC
      MINS = INS
    @ 14,035 SAY 'Inscr.Est. : '+MINS
      MISU   = ISU
      MCMU   = CMU
      MEST   = EST

      SELE CLI_ENTR
      IF SEEK(SUBS(MCGC,1,16))
        ENDENT1 = RTRIM(ENDE)+', '+RTRIM(CIDE)+', '+RTRIM(BAIE)
        ENDENT2 = ESTE+', ' +'CEP '+CEPE+', '+'CGC '+CGCE
      ENDIF
  ENDIF

*
      SELE CLI_COBR
      IF SEEK(MCGC)
        ENDCOB1 = RTRIM(ENDC)+', '+RTRIM(CIDC)+', '+RTRIM(BAIC)
        ENDCOB2 = ESTC+', ' +'CEP '+CEPC+', '+'CGC '+CGCC+ ', '+'INSC '+INSC
      ELSE
        ENDCOB1 = SPACE(50)
        ENDCOB2 = SPACE(50)
      ENDIF
*
    LIN = 18
    STOR 0.00  TO VTOT,    GTOT,  VIPI,  GIPI,GICM, VTNF
    STOR 0.00  TO TOTSIPI, CTOTA, VALDICM
    STOR 0.00  TO VUNIT
    MUNI = SPACE(03)
*
  SELE CADNF2

  @ 15,001 say replicate('-',137)
  @ 16,001 say 'Quantid'
  @ 16,009 say '|Emb'
  @ 16,013 say '|Ref'
  @ 16,019 say '|Descricao dos Produtos'
  @ 16,079 say '|CT/CF'
  @ 16,085 say '|Preco Unit'
  @ 16,101 say '| T o t a l'
  @ 16,114 say '|%IPI'
  @ 16,123 say '|Valor do IPI'
  @ 17,001 say replicate('-',137)

    DO WHILE NNF = MNNF .AND. !EOF()

      MDESCRAUX1 = SPACE(10)
      MDESCRAUX2 = SPACE(10)
      MPED       = PED
      MREF       = REF

      SELE PROD
      IF SEEK(MREF)
        MEMBALAGEM = EMBALAGEM
        PIPI = IPI
      ELSE
        MEMBALAGEM = ""
        PIPI = 0
      ENDIF

      SELE PEDID
      IF SEEK(STR(MPED,6) + MREF)
        MUNI       = UNI
        PIPI       = IPI
      ENDIF

      SELE CADNF2

        MENT = ENT
        @ LIN,001 SAY QTD PICT '999,999'
        @ LIN,010 SAY MEMBALAGEM
        @ LIN,014 SAY REF
        @ LIN,20 SAY DES
        @ LIN,081 SAY CCL+MCODT
*        @ LIN,083 SAY PREU PICT '99,999,999.99'
@ LIN,083 SAY PREU PICT '999,999.9999'
          VTOTA   = VAL
          CTOTA   =  (VTOTA + CTOTA)
        @ LIN,098 SAY VTOTA PICT "999,999,999.99"
        @ LIN,115 SAY PIPI PICT '99.9'
        @ LIN,122 SAY IPI PICT "999,999,999.99"
          GIPI = (GIPI + IPI)
          GICM = (GICM + ICM)
          VALDICM = (VALDICM + DICM)

      IF IPI > 0
        MBASEIPI = (MBASEIPI + VAL)
      ENDIF

      IF ICM > 0
        MBASEICM = (MBASEICM + VAL)
      ENDIF

      STOR 0.00 TO VIPI, VTOTA

*
      LIN = LIN + 1

      SELE CADNF2
        SKIP

* ---------------------------------------------------------------------------
*                  QUEBRA DE FORMULARIO
*--------------------------------------------------------------------------
      IF NNF <> MNNF .OR. EOF()
        EXIT
      ENDIF

    ENDDO
*  ENDIF

  IF MDESCONTO > 0.00
    @ LIN,40 SAY 'Desconto Especial'
    @ LIN,97 SAY MDESCONTO PICT '99,999,999,999.99'
      CTOTA = (CTOTA - MDESCONTO)
      LIN = LIN + 1
  ENDIF

  IF VALDICM > 0
      @ LIN,20 SAY 'INSCR.SUFR.: '+MISU
      @ LIN,50 SAY 'MUNICIPIO: '+MCMU
      @ LIN,70 SAY '% ICM: '+STR(MDIC,1)
      @ LIN,80 SAY 'VALOR DESC ICM: '
      @ LIN,97 SAY VALDICM PICT "99,999,999,999.99"
        LIN = LIN + 1
  ENDIF

    IF MEMB > 0.00
      @ LIN,40 SAY 'Valor da Embalagem'
      @ LIN,97 SAY MEMB PICT '99,999,999,999.99'
    ENDIF

    IF ENDCOB1 <> SPACE(20)
      @ 37,001 SAY 'COBRANCA: '+ ENDCOB1 + ENDCOB2
    ENDIF

    @ 38,001 say replicate('-',137)
    @ 39,005 say 'Frete'
    @ 39,018 say 'Seguro'
    @ 39,032 say 'Total'
    @ 39,047 say '%ICMS'
    @ 39,057 say 'Valor do ICMS'
    @ 39,076 say 'Valor da Merc.'
    @ 39,098 say 'Valor do IPI'
    @ 39,122 say 'Valor Total da NF'

    IF MFRE > 0
      @ 40,001 SAY MFRE PICT '9999,999.99'
    ENDIF

    @ 40,027 SAY MFRE  PICT '9999,999.99'
    IF lPesFisica 
      MPICM = 18.0
    ELSE  
      SELE ESTADOS
      IF SEEK(MEST) 
        MPICM = ICM
      ELSE
        MPICM = 0.00
      ENDIF  
    ENDIF

    @ 40,047 SAY MPICM PICT '99.99'
    SELE CADNF2
    @ 40,053 SAY GICM  PICT '99,999,999,999.99'
    @ 40,073 SAY CTOTA PICT '99,999,999,999.99'
    @ 40,094 SAY GIPI  PICT '99999,999,999.99'

    TOTNF = (CTOTA + GIPI + MEMB + MFRE)
    @ 40,121 SAY TOTNF PICT '99,999,999,999.99'

    @ 41,001 say replicate('-',137)
* -------------------------------------------------------------------------
*    I M P R I M E      V A L O R      P O R      E X T E N S O
* -------------------------------------------------------------------------
  M.VDP = TOTNF

  VREXT = SPACE(244)
  ? EXTENSOP()
    AUX = SUBSTR(VREXT,1,122)
    IF LEN(RTRIM(SUBSTR(VREXT,123,10))) > 0
        A = 122
        DO WHILE A > 1
            IF SUBSTR(AUX,A,1) = " " .OR. SUBSTR(AUX,A,1) = ","
                A = 0
            ELSE
                AUX = SUBSTR(AUX,1,A-1)
            ENDIF
            A = A - 1
        ENDDO
    ENDIF
    @ 42,001 SAY 'Valor por'
    @ 42,015 SAY AUX
    A = LEN( RTRIM( SUBSTR(VREXT,LEN(AUX)+1,122 ) ) )
    IF A < 1
        @ PROW(),PCOL()+1 SAY REPLICATE("*",122-LEN(AUX))
    ENDIF
    @ 43,001 SAY 'Extenso'
    @ 43,015 SAY RTRIM(SUBS(VREXT,LEN(AUX)+1,121))+" "+REPLICATE("*",121-A)
    @ 44,001 say replicate('-',85)
    @ 44,088 say 'Duplicata'
    @ 44,100 say 'Vencimento'
    @ 44,122 say '    Valor'
*---------------------------------------------------------------------------
*
      SELE TRANS
      IF SEEK(MTRA)
        MRAZTRA = RAZ
        MENDTRA = END
        MCIDTRA = CID
        MESTTRA = EST
        MCEPTRA = CEP
      ELSE
        MRAZTRA = SPACE(40)
        MENDTRA = SPACE(40)
        MCIDTRA = SPACE(20)
        MESTTRA = SPACE(02)
        MCEPTRA = SPACE(09)
      ENDIF

    SELE CLIEN
      SEEK MCGC
*
    SELE DUPL

      PDUP1 = 1
      PDUP2 = 2
      PDUP3 = 3
      PDUP4 = 4

      IF SEEK(STR(MNNF,6)+STR(PDUP1,1))
        MDUP1 = STR(NDUP,6)+'/1'
        VENC1 = VENC
        MVDP1  = VDP
      ELSE
        MDUP1 = " "
        VENC1 = CTOD('')
        MVDP1 = 0
      ENDIF

      IF SEEK(STR(MNNF,6)+STR(PDUP2,1))
        MDUP2 = STR(NDUP,6)+'/2'
        VENC2 = VENC
        MVDP2 = VDP
      ELSE
        MDUP2 = " "
        VENC2 = CTOD('')
        MVDP2 = " "
      ENDIF

      IF SEEK(STR(MNNF,6)+STR(PDUP3,1))
        MDUP3 = STR(NDUP,6)+'/3'
        VENC3 = VENC
        MVDP3 = VDP
      ELSE
        MDUP3 = " "
        VENC3 = CTOD('')
        MVDP3 = " "
      ENDIF

      IF SEEK(STR(MNNF,6)+STR(PDUP4,1))
        MDUP4 = STR(NDUP,6)+'/4'
        VENC4 = VENC
        MVDP4 = VDP
      ELSE
        MDUP4 = " "
        VENC4 = CTOD('')
        MVDP4 = " "
      ENDIF

      @ 45,001 say 'Transportador'
      @ 45,015 SAY MRAZTRA

      @ 45,088 SAY MDUP1
      @ 45,100 SAY VENC1
      @ 45,121 SAY MVDP1 PICT "99,999,999,999.99"
*
      @ 46,001 SAY 'Endereco'
      @ 46,015 SAY MENDTRA
      @ 46,065 SAY MCEPTRA

      @ 46,088 SAY MDUP2
      @ 46,100 SAY VENC2
      @ 46,121 SAY MVDP2 PICT "99,999,999,999.99"
*
      @ 47,001 SAY 'Pl.Veiculo'
      @ 47,015 SAY MVEICULO
      @ 47,029 SAY 'Municipio'
      @ 47,040 SAY MCIDTRA
      @ 47,070 SAY 'Est'
      @ 47,076 SAY MESTTRA

      @ 47,088 SAY MDUP3
      @ 47,100 SAY VENC3
      @ 47,121 SAY MVDP3 PICT "99,999,999,999.99"

      @ 48,001 SAY 'Entrega'
      IF ENDENT1 <> SPACE(20)
        @ 48,009 SAY ENDENT1 + ENDENT2
      ENDIF

      @ 48,088 SAY MDUP4
      @ 48,100 SAY VENC4
      @ 48,121 SAY MVDP4 PICT "99,999,999,999.99"

      @ 49,001 SAY REPLICATE('-',137)
      @ 50,001 SAY 'Marca'
      @ 50,014 SAY 'Numero'
      @ 50,035 SAY 'Quantidade'
      @ 50,050 SAY 'Especie'
      @ 50,081 SAY 'Peso Liquido'
      @ 50,099 SAY 'Peso Bruto'
      @ 50,122 SAY 'Numero controle'
      @ 51,035 SAY MQTDVOL PICT '99,999'
      @ 51,050 SAY MESPECIE
      @ 51,083 SAY MPLIQUIDO PICT '99,999.99'
      @ 51,100 SAY MPBRUTO PICT '99,999.99'
      @ 51,126 SAY PADL(MNNF,6,'0')

      @ 52,001 SAY REPLICATE('-',137)
      @ 53,002 SAY 'Emissao'
      @ 53,015 SAY 'CGC do Emitente'
      @ 53,045 SAY 'Inscr. Emitente'
      @ 53,069 SAY 'Est'
      @ 53,080 SAY 'CGC Destinatario'
      @ 53,115 SAY 'Inscr.Destinatar'
      @ 53,135 SAY 'Est'

      @ 54,002 SAY MDEMI
      @ 54,015 SAY '96.160.650/0001-73'
      @ 54,045 SAY '113.756.369.115'
      @ 54,069 SAY MEST
      @ 54,080 SAY MCGC
      @ 54,115 SAY MINS
      @ 54,135 SAY MEST

      @ 55,001 SAY REPLICATE('-',137)
      @ 56,001 SAY 'Nr. N.F.'
      @ 56,016 SAY 'Base Calc.IPI'
      @ 56,043 SAY 'Valor do IPI'
      @ 56,072 SAY 'Base Calc. ICMS'
      @ 56,094 SAY '%ICMS'
      @ 56,108 SAY 'Valor do ICMS'
      @ 56,125 SAY 'Data Saida'
      @ 57,002 SAY PADL(MNNF,6,'0')
*
      IF MBASEIPI > 0
        @ 57,012 SAY MBASEIPI PICT '99,999,999,999.99'
        @ 57,038 SAY GIPI     PICT '99,999,999,999.99'
        MBASEIPI = 0.00
      ENDIF

      IF MBASEICM > 0.00
        @ 57,070 SAY MBASEICM PICT '99,999,999,999.99'
        @ 57,095 SAY MPICM    PICT '99.99'
        @ 57,104 SAY GICM    PICT '99,999,999,999.99'
        MBASEICM = 0.00
      ENDIF

      @ 57,125 SAY MENT

      @ 58,001 SAY REPLICATE('-',137)
      @ 59,001 SAY 'Classificacao Fiscal'
      @ 59,082 SAY MMNF1
      @ 60,082 SAY MMNF2

      @ 61,001 SAY REPLICATE('-',137)
*
      SELE CADNF2
      IF EOF()
 *       ZAP
        CLOS DATA
        DO IMPSAIDA
        RETURN
      ENDIF

ENDDO
