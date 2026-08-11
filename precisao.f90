program precisao
  implicit none
  ! é usado o "kind" para especificar a precisão (número de bits)
  ! tipo (kind = valor) :: nome, exemplo integer(kind=8) :: distancia

  integer, parameter :: a = 4
  integer, parameter :: b = 8
  integer, parameter :: c = 16

  ! aqui vem a especificação
  real (kind=4) :: valor1
  real (kind=b) :: valor2
  real (kind=c) :: valor3
  ! não precisamos necessariamente escrever o kind também
  real (8) :: valor4

  print *, 'kind da variavel 1: ', kind(valor1)
  print *, 'kind da variavel 2: ', kind(valor2)
  print *, 'kind da variavel 3: ', kind(valor3)
  print *, 'kind da variavel 4: ', kind(valor4)
  print *, 'precisao da variavel 1: ', precision(valor1)
  print *, 'precisao da variavel 2: ', precision(valor2)
  print *, 'precisao da variavel 3: ', precision(valor3)




  ! use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64

  ! real(sp) :: float32
  ! real(dp) :: float64

  ! float32 = 1.0_sp  ! Explicit suffix for literal constants
  ! float64 = 1.0_dp
  ! print*, float32, float64
end program precisao