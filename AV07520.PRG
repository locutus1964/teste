* AV07520 - APROVACAO DE PEDIDOS PARA EMISSAO DE NOTAS FISCAIS ON LINE
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 09-07-90  - 09/12/91 - 19/10/92
*
*
? MENSAG('Processando a aprova‡„o dos Pedidos selecionados... Aguarde')
*
PUBLIC MTOL
  MTOL = 0.00
USE PARAM
  IF !(EOF() .OR. BOF())
    MTOL = TOL
  ELSE
    ? MENSAG('Parƒmetro de Notas Fiscais n„o encontrado... Favor Atualizar - ENTER')
    =INKEY(0)
    APPE BLAN
  ENDIF
*
USE APROVP INDEX APROVP
ZAP
CLOS DATA
*
STOR SPACE(01) TO mprod, mclie, mtrib, maut 
STOR SPACE(08) TO mden,  mest,  muni,  mccl 
STOR SPACE(03) TO mtip,  mven,  mcpd
STOR SPACE(04) TO mtra
mpint = SPACE(06)
mref  = space(10)
mpcl  = space(13)
mcgc  = space(20)
mdes  = space(40)
mtabp = space(07)
STOR 0    TO mped, mqtd, mlap, mcre, mcp1, mcp2, mcp3, mcp4, mqtdat 
STOR 0.0  TO mipi, tipi
STOR 0.00 TO mpre, mdge, mbon, memb, tdit, tbon, tfre, tdge, tval 
STOR 0.00 TO mdit, mfre, micm, mvemb,mvembar, mpreuar, temb, mpreu 
mpel  = 0.0000
STOR 0    TO teste, imprime, mped 
*
SELE G
 USE ESTADOS INDEX ESTADOS
SELE F
 USE TPNF INDEX TPNF
SELE D
 USE APROVP INDEX APROVP
SELE E
 USE PEDID INDEX PEDID
SELE C
 USE PROD INDEX PROD
SELE B
 USE CLIEN ORDER P_CGC
SELE A
 USE PEDIC2 INDEX PEDIC2
*
DO WHILE !EOF()
*
mden = den
mped = ped
mtra = tra
mcpd = cpd
mcp1 = cp1
mcp2 = cp2
mcp3 = cp3
mcp4 = cp4
mpe1 = pe1
mpe2 = pe2
mpe3 = pe3
mpe4 = pe4
mcgc = cgc
mven = ven
mtip = tip
mpcl = pcl
mfre = fre
memb = emb
mdge = dge
maut = aut
mpint = pint
mdesconto = desconto
*
SELE F
 seek mtip
   if !(eof() .or. bof())
     MIPIT = IPIT
     MICMT = ICMT
     MISST = ISST
     medup = edup
     mbest = best
   endif
*
SELE E
  SEEK STR(MPED,6)
    IF (EOF() .OR. BOF())
      LOOP
    ENDIF
*
   DO WHILE PED = MPED

     if qtdat < 0.01
       skip
       loop
     endif

     mref   = ref
     mqtdat = qtd
     mdit   = dit
     mpre   = pre
     mbon   = bon
     mclie  = space(01)
     mprod  = space(01)
     mipi = ipi
     muni = uni
     mdes = descricao
     mccl = ccl
*
     tbon = 1.00
     tdit = 1.00
     tdge = 1.00
*
     if mdit > 0
       tdit = (1 - (mdit/100))
     endif
*
     if mbon > 0
       tbon = (1 - (mbon/100))
     endif
*
     if mdge <> 0
       stor (1 - (mdge/100)) to tdge
     endif
*
*------------------- ATENDIMENTO PARCIAL DE PEDIDO --------------------
*
 IF MBEST = 'S'
   IF SEEK(MREF)

     IF MSAL < MQTD
       IF MSAL > 0
         MQTD    = MSAL
         MPROD   = '9'
         IMPRIME = 1
       ENDIF
     ENDIF

       TSAP = sap + mqtd
         if tsap > msal
           mprod   = '1'
           IMPRIME = 1
         else
           TSAP = SAP
         endif
         
      msal = (sal - com)
      
    ENDIF 
  else
     mprod   = '2'
     IMPRIME = 1
  ENDIF         
*
  if mipi > 0
    TIPI = (1 - (mipi/100))
  else
    tipi = 1.0
  endif
*
MPREUAR = (mpre * tdge * tdit)
  MPREU = round (mpreuar,2)
MVEMBAR = (mpre * mqtd)
  MVEMB = round (mvembar,2)
   TVAL = ((mpreu * tbon * tipi) * mqtd)
*
* --------------------- APROVACAO DE CREDITO ------------------------------
*
IF MAUT = 'S'
  SELE B
  SEEK MCGC
    if !(eof() .or. bof())
      mest = est
      mcre = ((cre * 1000) - deb)
      mlap = lap
        if (mlap + tval) > (mcre + (mcre * (mtol/100))) .and. maut <> 'S'
          mclie   = '1'
          IMPRIME = 1
        else
          repl lap with (mlap + tval)
        endif
    else
      mclie   = '2'
      IMPRIME = 1
    endif
ENDIF    
*
SELE G
  SEEK MEST
    IF !(EOF() .OR. BOF())
      MICM = ICM
    ELSE
      MICM = 0.00
    ENDIF
*
SELE D
  APPE BLAN
    repl cgc with mcgc,tip with mtip,ped with mped,ref with mref
    repl qtd with mqtd,ccl with mccl,cp1 with mcp1,cp2 with mcp2
    repl cp3 with mcp3,cp4 with mcp4,pe1 with mpe1,pe2 with mpe2
    repl pe3 with mpe3,pe4 with mpe4,bon with mbon,preu with mpreu
    repl prod with mprod,clie with mclie,val with tval
    repl ven with mven,tra with mtra,pcl with mpcl,pint with mpint
    repl estc with mest,cpd with mcpd,icm with micm
*
SELE E
SKIP
  LOOP
    ENDDO
*
SELE A
SKIP
  LOOP
    ENDDO
CLOS DATA
*
IF IMPRIME = 1
*  SAVE SCREEN TO TELA
  @ 15,05 CLEAR TO 19,74
  @ 15,05 TO 19,74 DOUBLE
  OPCAO = ' '
  MENS = 'Existem Pedidos com Pendˆncias p/emiss„o de Notas Fiscais:'
  ? MENSAGR(MENS)
  @ 17,POS SAY MENS
  MENS = 'Digite: (I) p/imprimir Relat¢rio ou (M) p/mostrar na tela '
  ? MENSAG(MENS)
  @ 23,POS SAY MENS GET OPCAO PICT '!' VALID(OPCAO$'IM')
    READ
      IF OPCAO = 'M'
        DO AV07424
      ELSE
        DO AV07423
      ENDIF
ELSE
  ? MENSAG('Sem Pedidos com ¡tens Pendentes - ENTER P/continuar')
  =INKEY(0)
ENDIF
*
CLOS DATA
RETURN

