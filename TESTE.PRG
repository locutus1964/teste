clear
d1 = {01/01/1999}
d2 = {05/05/1999}
? d1, d2, NumMeses(d1, d2)
d1 = {01/05/1999}
d2 = {01/12/1999}
? d1, d2, NumMeses(d1, d2)
d1 = {01/01/1999}
d2 = {05/05/2000}
? d1, d2, NumMeses(d1, d2)

*****************
FUNCTION NUMMESES
*****************
PARAMETER MDATA1, MDATA2
PRIVATE Num, m1, m2, a1, a2

  m1 = month(mdata1)
  a1 = year(mdata1)
  m2 = month(mdata2)
  a2 = year(mdata2)

  if a1 = a2 then
    Num = m2 - m1 + 1
  else
    Num = (12 - m1) + (a2 - a1 - 1) * 12 + m2
  endif
  
RETURN Num
