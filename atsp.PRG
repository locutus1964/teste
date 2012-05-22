close data
sele 0
use clien.dbf alias cli 
index on trim(gue) tag x_gue

sele 0
use e:\sistdos\avsp\cabpedsp.dbf alias pedsp 

sele pedsp
go top
do while ! eof()
  if seek(trim(pedsp->guerra), 'cli')
    if empty(cgc) then
      replace cgc with cli->cgc
    endif  
  endif  
  sele pedsp
  skip
enddo

