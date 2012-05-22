parameter n
set talk off
clear
calls = 0
t = seconds()
=fibon(n)
? seconds() - t, calls




function fibon
parameter n

  if n < 2 then
    Return 1
  endif  
  calls = calls + 1
return (fibon(n - 1) + fibon(n - 2))


