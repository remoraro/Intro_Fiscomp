program cosseno
    implicit none

    real(4), parameter :: eps = 1.0e-5, pi = 3.14159265
    real(4) :: x, fat, valor, dif, sinal, expo, incremento, coss

    print *, "Digite o número X para calcular seu cosseno com Taylor: "
    read(*,*) x
    x = abs(x)
    do while (x > (2.0*pi))
        x = x - (2.0 * pi)
    end do

    valor = 1.0
    fat = 2.0
    expo = 2.0
    sinal = -1.0
    incremento = 3.0
    coss = cos(x)
    do while (abs(abs(valor) - abs(coss)) >= eps)
        valor = valor + ((x ** expo) * sinal / fat)

        expo = expo + 2.0
        sinal = sinal *(-1.0)

        fat = fat * incremento * (incremento + 1.0)
        incremento = incremento + 2.0     
    end do

    ! Pela precisão dupla, só funciona até x = 8
    dif = abs(abs(valor) - abs(coss))

    print *, "O valor da série é", valor, ". O valor da função do FORTRAN é ", coss, ". A diferença é", dif

end program cosseno