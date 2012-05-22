* MOSTRCLI - MOSTRA DADOS DO CLIENTE PARA PEDIDOS
* SISTEMA ADMINISTRACAO DE VENDAS - 06/09/93
*
@03,03 say 'CGC/C¢digo         Nome de Guerra    Regi„o    Vend/Repr  Status  Desctos'
@04,72 say '+'
@05,03 say 'Raz„o Social                                    Cidade'
@07,03 say 'Endere‡o                                        Bairro'
@09,03 say 'Est     CEP          Inscr Estadual       Inscr Sufr      C Postal  D.ICM'
@11,03 say 'DDD1 Telef 1      DDD2 Telef 2      CELULAR (DDD/Num)    FAX (DDD/N£mero)'
@13,03 say 'Contato 1              Contato 2               Ativid  Transport   Cl ABC'
@15,03 say 'Inclus„o    Ult Compra   Lim Cr‚dito    Saldo Deved  Conceito  Aceita‡(+/-)'
@17,03 say 'Venctos Especiais     Dt Alter    Resp Nome Resp. p/£ltima alter.'
@19,03 say 'N§ Banco  Agencia'
@04,03 say  subs(cgc,1,16)              color &corver
@04,22 say  MGUE                  		color &corver
@04,41 say  FmtRegiao(regiao,idregiao)  color &corver
@04,53 say  ven                  		color &corver
@04,63 say  estatus               color &corver
@04,68 say  descto1 pict '99.9'   color &corver
@04,73 say  descto2 pict '99.9'   color &corver
@06,03 say  raz                   color &corver
@06,51 say  cid                   color &corver
@08,03 say  end                   color &corver
@08,51 say  bai                   color &corver
@10,04 say  est                   color &corver
@10,11 say  cep                   color &corver
@10,24 say  ins                   color &corver
@10,45 say  isu                   color &corver
@10,62 say  box                   color &corver
@10,73 say  dic                   color &corver
@12,03 say  ddd1                  color &corver
@12,07 say  fon1                  color &corver
@12,21 say  ddd2                  color &corver
@12,25 say  fon2                  color &corver
@12,40 say  ddd_celu              color &corver
@12,45 say  num_celu              color &corver
@12,62 say  ddd_faxc              color &corver
@12,67 say  num_faxc              color &corver
@14,03 say  contato               color &corver
@14,26 say  contato2              color &corver
@14,51 say  atividade             color &corver
@14,59 say  tra                   color &corver
@14,64 say  tra_opc               color &corver
@14,72 say  clc                   color &corver
@16,03 say  inic                  color &corver
@16,16 say  ulc                   color &corver
@16,28 say  str( cre,11)          color &corver
@16,43 say  str( deb,11)          color &corver
@16,58 say  conceito              color &corver
@16,62 say  conceito              color R+/B
@16,67 say  aceitama pict '99.9'  color &corver
@16,72 say  aceitame pict '99.9'  color &corver
@18,03 say  ves                   color &corver
@18,25 say  dtalt                 color &corver
@18,37 say  sigla                 color &corver
@18,42 say  space(30)             color &corver
@20,05 say  nbanco   pict '999'   color &corver
@20,14 say  agencia  pict '9999'  color &corver
return

