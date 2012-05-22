* AV03300 - LISTA DOS VENDEDORES/REPRESENTANTES - ORDEM RAZAO SOCIAL
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 12-04-90
*
do TESTIMP2
IF CP = 'N'
   RETURN
ENDIF
=JANELA(14,40,21,78)
=BORDA(14,40,21,78)
MC = ' '
MS = ' '
MEST = SPACE(02)
MREG = SPACE(04)
MCID = SPACE(25)
MCVR = SPACE(04)
@15,45 SAY 'Selecionar por:'
@16,42 SAY 'C¢digo (C) ou Ordem Alfa (A) ='GET MC PICT '!' VALID(MC$'AC')
READ
@17,42 SAY '[C]idade, [E]stado, [R]egi„o'
@18,42 say ' ou [V]endedor ?'GET MS PICT '!' VALID(MS$'CERV ')
READ
USE VEND
IF MS <> SPACE(01)
   IF MS = 'C'
      @19,42 SAY 'Cidade ?'GET MCID PICT '@!'
      READ
      COPY TO CADWORK FOR CID = MCID
      USE CADWORK
      INDEX ON CID + CEP TO CADWORK.IDX
      TIT = 'VENDEDORES/REPRES. DA CIDADE '+MEST
   ELSE
      IF MS = 'E'
         @19,42 SAY 'Sigla do Estado ?'GET MEST PICT '@!'
         READ
         COPY TO CADWORK FOR EST = MEST
         USE CADWORK
         INDEX ON EST + CVR + CID + CEP TO CADWORK.IDX
         TIT = 'VENDEDORES/REPRESENTANTES DO ESTADO '+MEST
      ELSE
         IF MS = 'R'
            @19,42 SAY 'C¢digo da Regi„o ?'GET MREG PICT '@!'
            READ
            COPY TO CADWORK FOR REGIAO = MREG
            USE CADWORK
            INDEX ON REGIAO + CVR + EST + CID + CEP TO CADWORK.IDX
            TIT = 'VENDEDORES/REPRESENTANTES DA REGIAO '+MREG
         ELSE
            IF MS = 'V'
               @19,42 SAY 'C¢digo do Vendedor ?'GET MCVR PICT '@!'
               READ
               COPY TO CADWORK FOR CVR = MCVR
               USE CADWORK
               INDEX ON REGIAO + EST + CID + CEP TO CADWORK.IDX
               TIT = 'VENDEDORES/REPRESENTANTE '+MCVR
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   USE CADWORK INDE CADWORK.IDX
ELSE
   IF MC = 'C'
      USE VEND INDEX VEND.IDX
      TIT = 'VENDEDORES/REPRESENTANTES POR CODIGO'
   ELSE
      USE VEND
      INDEX ON RAZ TO CADWORK.IDX
      USE VEND INDEX CADWORK.IDX
      TIT = 'VENDEDORES/REPRESENTANTES PELA RAZAO SOCIAL'
   ENDIF
ENDIF

pagctr = 0
TAMREL = 2
*
=mensag('Imprimindo Vendedores/Representantes cadastrados... Aguarde')
*
DO DEFSAIDA

do while .not. EOF()
*
   =cabec('AV03300',TIT,131)
   @3,1    say 'VEND'
   @3,6    say 'F/J'
   @3,10   say 'CGC/CPF REPRES'
   @3,28   say 'NOME/RAZAO SOCIAL/ENDERECO'
   @3,60   say 'CIDADE/EST/CEP'
   @3,82   say 'DDD/TELEF'
   @3,92  SAY 'CONTA/AREA'
   @4,1    SAY REPLICATE('-',131)
*
   stor 5 to linha
   do while .not. EOF() .and. linha < 64
      @linha,2   say CVR
      @linha,7   say FIJU
      @linha,10  say CGC
      @linha,28  say LEFT(RAZ,30)
      @linha,60  say LEFT(CID,20)
      @linha,82  say DDD1
      @linha,92 say CTACORR
      STOR LINHA + 1 TO LINHA
      @linha,10 say END
      @linha,60 say EST
      @linha,65 say CEP
      @linha,82 say FON1
      @linha,92 say ACAO
      stor linha+2 to linha
      skip
      if eof()
         @linha-1,1 say replicate ('-',131)
         @linha,10 say 'F I N A L    D E S T E    R E L A T O R I O'
      endif
   enddo
enddo
DO IMPSAIDA
close data
return
