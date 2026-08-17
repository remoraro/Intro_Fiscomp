program funcoes
    implicit none
    contains ! dentro do seu código tem que ter o "contains" no fim

    ! para criar funções é parecido com C
    integer function dez_vezes(valor) result(res)
        integer, intent(in) :: valor ! intent fala para não mexer

        res = valor * 10
    end function dez_vezes



end program funcoes