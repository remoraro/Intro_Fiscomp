program read_values
  implicit none
  real :: x, y

  print *, 'Please enter two numbers. '
  read(*,*) x, y

  print *, 'The sum and product of the numbers are ', x+y, x*y

  ! tomar cuidado com divisão, pq se os 2 forem inteiros vai retornar um inteiro
  ! então integer 3 / integer 2 = 1, para dar 1.5 os 2 tem que ser real

end program read_values