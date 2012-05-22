*
* Este programa consiste o CGC dos clientes Juridicos
* do cadastro de clientes
*
* Manual Almeida
* Ter‡a, 26 de setembro de 1995 …s 08:59 horas
*
set talk off
set esca on
clear
mc = ' '
stor 0 to pridig, segdig
use clien ORDER P_CGC
ch = .f.
set devi to print
@prow(),001 say chr(15)
do while !eof()
   if fiju = 'J'
      =fcgc(tira(cgc))
      if mc = 'N' .or. !fcgc(tira(cgc))
         ch = .t.
         @prow()+1,001      say cgc
         @prow()  ,pcol()+1 say raz
         @prow()  ,pcol()+1 say gue
         @prow()  ,pcol()+2 say est
         @prow()  ,pcol()+1 say FmtRegiao(regiao, IdRegiao)
      endif
   endif
   skip
enddo
if ch
   @prow(),pcol() say chr(12)
endif
set devi to screen
set talk on
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
procedure tira
parameters vcgc
private vreto, xx, yy
vreto = ""
vcgc  = alltrim(vcgc)
if !empty(vcgc)
   yy = ""
   for xx = 1 to len(vcgc)
      if subs(vcgc,xx,1) = ' ' .or. subs(vcgc,xx,1) = '/' .or.subs(vcgc,xx,1) = '-'
      else
         vreto = vreto + subs(vcgc,xx,1)
      endif
   next xx
endif
return vreto
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function FCGC
PARAMETERS CGCT
CONTADOR = 1
DO WHILE CONTADOR <= 12
   D=IIF(CONTADOR < 10, '0'+STR(CONTADOR,1),STR(CONTADOR,2))
   D1&D = VAL(SUBS(CGCT,CONTADOR,1))
   CONTADOR = CONTADOR + 1
ENDDO
DF1 = 5*D101+4*D102+3*D103+2*D104+9*D105+8*D106+7*D107+6*D108+5*D109+4*D110+3*D111+2*D112
DF2 = DF1 / 11
DF3 = INT(DF2) * 11
RESTO1 = DF1 - DF3
PRIDIG = 11 - RESTO1
IF RESTO1 = 0 .OR. RESTO1 = 1
   PRIDIG = 0
ENDIF
* ----------------------------  SEGUNDO DIGITO
CONTADOR = 1
DO WHILE CONTADOR <= 12
   D=IIF(CONTADOR < 10, '0'+STR(CONTADOR,1),STR(CONTADOR,2))
   D2&D = VAL(SUBS(CGCT,CONTADOR,1))
   CONTADOR = CONTADOR + 1
ENDDO
DF4=6*D101+5*D102+4*D103+3*D104+2*D105+9*D106+8*D107+7*D108+6*D109+5*D110+4*D111+3*D112+2*PRIDIG
DF5 = DF4 / 11
DF6 = INT(DF5) * 11
RESTO2 = DF4 - DF6
SEGDIG = 11 - RESTO2
IF RESTO2 = 0 .OR. RESTO2 = 1
   SEGDIG = 0
ENDIF
MC = 'S'
IF PRIDIG <> VAL(SUBS(CGCT,13,1)) .OR. SEGDIG <> VAL(SUBS(CGCT,14,1))
   MC = 'N'
ENDIF
IF PRIDIG <> VAL(SUBS(CGCT,13,1)) .OR. SEGDIG <> VAL(SUBS(CGCT,14,1))
   return .f.
ENDIF
RETURN .t.

