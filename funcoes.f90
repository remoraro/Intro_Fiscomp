program funcoes
    implicit none

    real :: mat(5, 10), i
    integer, parameter :: numero = 4
    mat(:,:) = 0.0
    mat(4:,6:) = 1.0

    ! chamamos subrotinas com "call"
    call print_matrix(5, 10, mat)
    i = dez_vezes(numero)
    i = doze_vezes(numero)
    i = doze_vezes(numero, 2)


    contains ! dentro do seu código tem que ter o "contains" no fim

    ! para criar funções é parecido com C
    ! (tipo de dado de retorno da funcao) function (nome da funcao)(dados que ela recebe) result(variavel)
    integer function dez_vezes(valor) result(res)
        implicit none !precisa dentro
        integer, intent(in) :: valor ! intent fala para não mexer

        res = valor * 10
        print *, res
    end function dez_vezes

    ! também podemos definir o valor do resultado, dentro da função
    function doze_vezes(valor, elevado) result(res)
        integer, intent(in) :: valor ! intent fala para não mexer
        integer, intent(in), optional :: elevado 
        integer :: res

        if (present(elevado)) then
            res = (valor * 12) ** elevado
        else
            res = valor * 12
        end if
        print *, res
    end function doze_vezes

    ! Para "funções" que não retornam nada usamos subroutines
    subroutine print_matrix(n,m,A)
        implicit none !precisa dentro
        integer, intent(in) :: n
        integer, intent(in) :: m
        real, intent(in) :: A(n, m)

        integer :: i

        do i = 1, n
            print *, A(i, 1:m)
        end do

    end subroutine print_matrix
end program funcoes