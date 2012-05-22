clear
clear all

set print to lpt1
set devi to print

@prow(),pcol() say chr(27)+chr(64)

for x = 1 to 10

    @prow()+1,001 say prow()

next

@prow()+1,001 say prow() + chr(12)

set devi to screen


