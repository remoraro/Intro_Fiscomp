program variables ! o código sempre inicia com "program (nome do programa")

  implicit none ! naturalmente é implicito (como python)
  ! é boa prática declarar sempre, como em C


  ! para declarar é <variable_type> :: <variable_name>
  integer :: amount ! números inteiros, +-1
  real :: pi, e ! números de ponto flutuante, 2e2 = 4
  complex :: frequency ! números complexos, 3 +2i
  character(3) :: initial ! caracteres de texto (precisa especificar o tamanho), "a"
  logical :: isOkay ! booleano, .true ou .false

  amount = 10
  pi = 3.1415927
  e = 2.7182818
  frequency = (1.0, -0.5)
  initial = 'ABCD'
  isOkay = .false. 

  ! para printar é print *,
  print *, 'The value of amount (integer) is: ', amount
  print *, 'The value of pi (real) is: ', pi
  print *, 'The value of frequency (complex) is: ', frequency
  print *, 'The value of initial (character) is: ', initial
  print *, 'The value of isOkay (logical) is: ', isOkay
  
end program variables ! o código sempre encerrra com "end program (nome do programa")