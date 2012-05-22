* AV07000 - MENU DE NOTAS FISCAIS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06-05-90  -  08/12/91
*
DO WHILE .T.
   NUMIN2 = MENUZ_IN(.T.)
   DO CASE
      CASE NUMIN2 = 1     &&-----  a-Atualiza‡„o dos Tipos de Notas Fiscais
         DO AV07101
      CASE NUMIN2 = 2     &&-----  b-Relat¢rio dos Tipos de Notas Fiscais
         DO AV07200
      CASE NUMIN2 = 3     &&-----  c-Atualiza‡„o dos Parƒmetros para Notas Fiscais
         DO AV07300
      CASE NUMIN2 = 4     &&-----  d-Emiss„o de Notas Fiscais - Com Pedido
         DO AV07500
      CASE NUMIN2 = 5     &&-----  e-                         - Sem Pedido
         DO AV07600
      CASE NUMIN2 = 6     &&-----  f-Embarques e Cancelamentos de Notas Fiscais
         DO AV07703
      CASE NUMIN2 = 7     &&-----  g-Devolu‡„o de Clientes - Entrada no Estoque
         DO AV07800
      CASE NUMIN2 = 8     &&-----  h-Consulta Notas Fiscais emitidas
         DO AV13000
      CASE NUMIN2 = 9     &&-----  i-Pedidos atendidos com Notas Fiscais
         DO PEDNOTA
      CASE NUMIN2 = 10    &&-----  j-Imprime C¢pia de Notas Fiscais emitidas
         DO AV11300
*      CASE NUMIN2 = 11    &&-----  k-Imprime Notas Selecionadas - De / At‚
*         DO AV08600
      CASE NUMIN2 = 11    &&-----  l-Imprime Notas Fiscais de Entradas
         do av07901
*      CASE NUMIN2 = 12    &&-----  m-Imprime Relat¢rio de Embarque
*         do av07990       
*         do av07992
      CASE LASTKEY() = 27 &&-----  Sair da sub-rotina
         RESTORE SCREEN FROM MAIN_SCR
         RETURN
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
* --------------------------------------------------------------------
FUNCTION MENUZ_IN
* --------------------------------------------------------------------
PARAMETERS CLR_SCR
PRIVATE LIN, COL
NUMHOLD = 0
=NOMEMOD( 'AV07000' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
IF CLR_SCR
   LIN = 06
   COL = 20
   SET MESSAGE TO 23 CENTER
*  DO COR2
   =JANELA(LIN,COL,LIN+13,COL+54)
   =BORDA(LIN,COL,LIN+13,COL+54)
   @LIN+1,COL+3 PROMPT "a - Atualiza‡„o dos Tipos de Notas Fiscais       " MESSAGE ;
   INCALTEXC
   @LIN+2,COL+3 PROMPT "b - Relat¢rio dos Tipos de Notas Fiscais         " MESSAGE ;
   IMPRCODM
   @LIN+3,COL+3 PROMPT "c - Atualiza‡„o dos Parƒmetros para Notas Fiscais" MESSAGE ;
   "Atualiza os parƒmetros utilizados para emiss„o das Notas Fiscais"
   @LIN+4,COL+3 PROMPT "d - Emiss„o de Notas Fiscais - Com Pedido        " MESSAGE ;
   "Seleciona, aprova e imprime Notas Fiscais para Pedidos/Produtos codificados"
   @LIN+5,COL+3 PROMPT "e -                          - Sem Pedido        " MESSAGE ;
   "Imprime Notas Fiscais diversas e para Produtos n„o codificados"
   @LIN+6,COL+3 PROMPT "f - Embarques e Cancelamentos de Notas Fiscais   " MESSAGE ;
   "Registra o embarque ou o cancelamento das Notas Fiscais emitidas"
   @LIN+7,COL+3 PROMPT "g - Devolu‡„o de Clientes - Entrada no Estoque   " MESSAGE ;
   "Registra as devolu‡”es dos Clientes e as Notas Fiscais n„o entregues"
   @LIN+8,COL+3 PROMPT "h - Consulta Notas Fiscais emitidas              " MESSAGE ;
   "Consulta Notas Fiscais por Cliente, por Produto e por Pedido"
   @LIN+9,COL+3 PROMPT "i - Pedidos atendidos com Notas Fiscais          " MESSAGE ;
   "Relaciona as Pedidos e as respectivas Notas Fiscais"
   @LIN+10,COL+3 PROMPT "j - Imprime C¢pia de Notas Fiscais emitidas      " MESSAGE ;
   "Emite C¢pia de Notas Fiscais emitidas pelo n£mero solicitado"
*  @LIN+11,COL+3 PROMPT "k - Imprime Notas Selecionadas - De / At‚        " MESSAGE ;
*  "Emite Relat¢rio de Notas Fiscais Selecionadas De / At‚"
   @LIN+11,COL+3 PROMPT "k - Emissao de Notas Fiscais de Entrada          " MESSAGE ;
   "Imprime Notas Fiscais de Entrada (Retorno de Vendas)"
 *  @LIN+12,COL+3 PROMPT "l - Relat¢rio de Embarque de Notas Fiscais" MESSAGE ;
 *  "Imprime a Rela‡Æo de Notas Fiscais com Data de Embarque Lan‡ada"
   
   MENU TO NUMHOLD
ENDIF
RETURN(NUMHOLD)
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
procedure av07000SAI
* DEFINE SE SAIDA NA IMPRESSORA OU NA TELA
set devi to screen
if defrel = 'T'
   save screen to tela
   clear
   set display to vga50
   use arqrel80
   if flock()
      zap
      unlock
   endif
   appe from GRAREL80.TXT sdf
   go top
   define window celula from 0,0 to 49,79     && GROW FLOAT ZOOM CLOSE NONE
   browse window celula nomodify nodelete nomenu color +W/N,+N/W,GR+/N,+GR/N,+GR/N
   set display to vga25
   set message to 23 center
   rest screen from tela
   mens = 'Quer imprimir o Relat¢rio ?'
   do pegresp with [NS]
   if mc = 'S'
      set print on
      run type GRAREL80.txt >prn
   endif
   set print off
   set devi to screen
endif
return

