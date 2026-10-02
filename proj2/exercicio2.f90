program exercicio2
    implicit none

    integer :: i, j, num_N, N, unidade_in, unidade_out
    integer :: melhor_N_trapezio, melhor_N_simpsons, melhor_N_bode
    integer, allocatable :: Ns(:)

    real(8) :: h, x_menos1, x0, x1, x2, x3, x4
    real(8) :: integral_trapezio, integral_simpsons, integral_bode
    real(8) :: exata, erro_trapezio, erro_simpsons, erro_bode
    real(8) :: menor_erro_trapezio, menor_erro_simpsons, menor_erro_bode


    exata = 5.0d0 * (cos(1.0d0) + 2.0d0*sin(1.0d0) - 2.0d0)
    !print *, exata
    !print *, sin(1.0)
    !print *, sin(1.0d0)

    unidade_in = 10
    unidade_out = 11

    open(unit=unidade_in, file="tabela2_in.dat", status="old")
    open(unit=unidade_out, file="tabela2_out.dat", status="replace")

    read(unidade_in,*) num_N
    allocate(Ns(num_N))
    read(unidade_in,*) (Ns(i), i=1,num_N)

    write(unidade_out,'(A)') &
    "N      h                  erro_trapezio      erro_simpson       erro_bode"

    menor_erro_trapezio = 100000000
    menor_erro_simpsons = 100000000
    menor_erro_bode = 100000000

    do j = 1, num_N

        N = Ns(j)
        h = 1.0d0 / N


        integral_trapezio = 0.0d0
        integral_simpsons = 0.0d0
        integral_bode = 0.0d0


        do i = 1, N-1, 2

            x0 = i*h
            x1 = x0 + h
            x_menos1 = x0 - h

            integral_trapezio = integral_trapezio + h/2.0d0 * (f(x1) + 2.0d0*f(x0) + f(x_menos1))
            integral_simpsons = integral_simpsons + h/3.0d0 * (f(x1) + 4.0d0*f(x0) + f(x_menos1))

        end do

        do i = 0, N-4, 4

            x0 = i*h
            x1 = x0 + h
            x2 = x0 + 2.0d0*h
            x3 = x0 + 3.0d0*h
            x4 = x0 + 4.0d0*h

            integral_bode = integral_bode + 2.0d0*h/45.0d0 * (7.0d0*f(x0) + &
            32.0d0*f(x1) + 12.0d0*f(x2) + 32.0d0*f(x3) + 7.0d0*f(x4))

        end do

        ! Erros

        erro_trapezio = integral_trapezio - exata
        erro_simpsons = integral_simpsons - exata
        erro_bode = integral_bode - exata

        write(unidade_out,123) N, " |",  h, " |",erro_trapezio, " |" &
        , erro_simpsons, " |", erro_bode
        123 format(I4,1A, ES17.10, 1A, ES17.10, 1A, ES17.10, 1A, ES17.10)
        !write(unidade_out, *) N, h, erro_trapezio, erro_simpsons, , erro_bode


        if (abs(erro_trapezio) < abs(menor_erro_trapezio)) then
            menor_erro_trapezio = erro_trapezio
            melhor_N_trapezio = N
        end if

        if (abs(erro_simpsons) < abs(menor_erro_simpsons)) then
            menor_erro_simpsons = erro_simpsons
            melhor_N_simpsons = N
        end if

        if (abs(erro_bode) < abs(menor_erro_bode)) then
            menor_erro_bode = erro_bode
            melhor_N_bode = N
        end if

    end do

    close(unidade_in)
    close(unidade_out)

    print *, "A justificativa da melhor escolha de N para cada caso é o menor erro possível"
    print *, "No Bode apenas com N = 256 já é o suficiente, por isso ele é menor, nos outros &
    & precisar ter mais para atingir a mesma precisão"
    print *, "Melhores valores de N:"
    print *, "Trapezio: ", melhor_N_trapezio, menor_erro_trapezio
    print *, "Simpson:  ", melhor_N_simpsons, menor_erro_simpsons
    print *, "Bode:     ", melhor_N_bode, menor_erro_bode

contains

    real(8) function f(x)
        real(8), intent(in) :: x

        f = 5.0d0 * sin(x) * x**2.0

    end function f

end program exercicio2