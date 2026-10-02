program exercicio3
    implicit none

    integer :: i, num_it, unidade_out

    real(8) :: dir1, dir2, dir3, nr1, nr2, nr3, sec1, sec2, sec3, novo1, novo2, novo3
    real(8) :: a1, a2, a3, b1, b2, b3, sec1_ant, sec2_ant, sec3_ant

    read *, num_it

    unidade_out = 10

    open(unit=unidade_out, file="tabela3_out.dat", status="replace")

    write(unidade_out,'(A)') &
    "iter  dir1            dir2            dir3            NR1 &
    &            NR2             NR3             sec1 &
    &           sec2            sec3"

    a1 = -4.4d0
    a2 = 0.2d0
    a3 = 2.1d0
    b1 = -4.3d0
    b2 = 0.3d0
    b3 = 2.2d0
    nr1 = -4.4d0
    nr2 = 0.2d0
    nr3 = 2.2d0
    sec1_ant = -4.4d0
    sec2_ant = 0.2d0
    sec3_ant = 2.1d0
    sec1 = -4.3d0
    sec2 = 0.3d0
    sec3 = 2.2d0

    do i = 1, num_it
        call busca_direta(a1, b1, dir1)
        call busca_direta(a2, b2, dir2)
        call busca_direta(a3, b3, dir3)

        nr1 = nr1 - f(nr1) / f_linha(nr1)
        nr2 = nr2 - f(nr2) / f_linha(nr2)
        nr3 = nr3 - f(nr3) / f_linha(nr3)

        ! Tava dando erro de NaN pra N > 8 pq tava dividndo 0/0
        if (abs(f(sec1) - f(sec1_ant)) < 1.0d-14) then
            novo1 = sec1
        else
            novo1 = sec1 - f(sec1) * (sec1 - sec1_ant) / (f(sec1) - f(sec1_ant))
        end if
        if (abs(f(sec2) - f(sec2_ant)) < 1.0d-14) then
            novo2 = sec2
        else
            novo2 = sec2 - f(sec2) * (sec2 - sec2_ant) / (f(sec2) - f(sec2_ant))
        end if
        if (abs(f(sec3) - f(sec3_ant)) < 1.0d-14) then
            novo3 = sec3
        else
            novo3 = sec3 - f(sec3) * (sec3 - sec3_ant) / (f(sec3) - f(sec3_ant))
        end if
        !novo1 = sec1 - f(sec1) * (sec1 - sec1_ant) / (f(sec1) - f(sec1_ant))
        !novo2 = sec2 - f(sec2) * (sec2 - sec2_ant) / (f(sec2) - f(sec2_ant))
        !novo3 = sec3 - f(sec3) * (sec3 - sec3_ant) / (f(sec3) - f(sec3_ant))

        sec1_ant = sec1
        sec1 = novo1
        sec2_ant = sec2
        sec2 = novo2
        sec3_ant = sec3
        sec3 = novo3

        write(unidade_out,'(I4,9ES16.8)') i, dir1, dir2, dir3, &
        nr1, nr2, nr3, sec1, sec2, sec3
    end do

    close(unidade_out)

contains

    real(8) function f(x)
        real(8), intent(in) :: x

        f = x**3 + 2.0d0*x**2 - 10.0d0*x + 2.0d0
    end function f

    real(8) function f_linha(x)
        real(8), intent(in) :: x

        f_linha = 3.0d0*x**2 + 4.0d0*x - 10.0d0
    end function f_linha


    subroutine busca_direta(a, b, raiz)
        implicit none

        real(8), intent(inout) :: a, b
        real(8), intent(out) :: raiz
        real(8) :: passo, x_atual, x_proximo, fa, fb
        integer :: i

        passo = (b - a) / 2.0d0
        ! diferença entre os 2 limites

        x_atual = a !começa em a
        fa = f(a) !funcao avaliada em a

        do i = 1, 2
            x_proximo = a + i*passo !o prox valor a tentar é a + metade da diferença
            fb = f(x_proximo)


            if (fa * fb < 0.0d0) then !encontrou a diferença

                a = x_atual !substitui o valor inferior 
                b = x_proximo !substitui o valor superior

                ! melhor estimativa = ponto medio
                raiz = (a + b) / 2.0d0
                return
            end if

            x_atual = x_proximo !se não achar a dif, vai pro prox
            fa = fb !se não achar a dif, vai pro prox
        end do
    end subroutine busca_direta

end program exercicio3