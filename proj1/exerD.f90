program exerD
    implicit none

    real(4) :: contador_dentro, contador_geral, resultado, x, y, distancia
    integer :: num_iteracoes, i
    
    integer, allocatable :: semente(:)
    integer :: tamanho
    call random_seed(size=tamanho)
    allocate(semente(tamanho))
    call system_clock(count=i)
    semente = i
    call random_seed(put=semente)


    read(*,*) num_iteracoes

    contador_geral = 0.0
    contador_dentro = 0.0
    do i = 1, num_iteracoes
        call random_number(x)
        call random_number(y)

        distancia = (x - 0.5)**2.0 + (y - 0.5)**2.0
        if (distancia < 0.25) contador_dentro = contador_dentro + 1.0
        contador_geral = contador_geral + 1.0
    end do

    resultado = 4.0 * contador_dentro / contador_geral

    print '(F10.4)', resultado

    deallocate(semente)



end program exerD