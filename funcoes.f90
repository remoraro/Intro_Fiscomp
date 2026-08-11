program funcoes
    implicit none

    ! para criar funções é parecido com C
    integer function dez_vezes(valor) result(res)
        integer, intent(in) :: valor

        res = val * 10
    end function dez_vezes



end program funcoes