use trans1
MTRA = ''
do while .not. eof() 
  IF SUBSTR(TRA, 4, 1) = ' ' THEN
    mtra = '0' + substr(tra, 1, 3)
    tra = mtra
  END  
  
  skip
end
