program exerA
    implicit none

    integer :: num_it_simples, num_it_dupla, num_it_quadrupla
    real(4):: a
    real(8):: aa
    real(16) :: aaa
    real(4) :: simples
    real(8) :: dupla
    real(16) :: quadrupla

    a = 1.0e0
    simples = 1.0e0 + a
    num_it_simples = 0

    print *, "PRECISAO SIMPLES"
    do while (simples /= 1.0e0)
        print *, "Valor de a:", a, "E o valor de 1 + a:", simples
        num_it_simples = num_it_simples + 1
        a = a / 2.0e0
        simples = 1.0e0 + a
    end do


    aa = 1.0d0
    dupla = 1.0d0 + aa
    num_it_dupla = 0

    print *, "PRECISAO DUPLA"
    do while (dupla /= 1.0d0)
        print *, "Valor de aa:", aa, "E o valor de 1 + aa:", dupla
        num_it_dupla = num_it_dupla + 1
        aa = aa / 2.0d0
        dupla = 1.0d0 + aa
    end do


    aaa = 1.0_16
    quadrupla = 1.0_16 + aaa
    num_it_quadrupla = 0

    print *, "PRECISAO QUADRUPLA"
    do while (quadrupla /= 1.0_16)
        print *, "Valor de aaa:", aaa, "E o valor de 1 + aaa:", quadrupla
        num_it_quadrupla = num_it_quadrupla + 1
        aaa = aaa / 2.0_16
        quadrupla = 1.0_16 + aaa
    end do

    print *, "O numero de bits para a precisao SIMPLES foi de", num_it_simples, &
    "E o valor da precisao foi de", 2.0e0 * a
    print *, "O numero de bits para a precisao DUPLA foi de", num_it_dupla, &
    "E o valor da precisao foi de", 2.0d0 * aa
    print *, "O numero de bits para a precisao QUADRUPLA foi de", num_it_quadrupla, &
    "E o valor da precisao foi de", 2.0_16 * aaa

end program exerA