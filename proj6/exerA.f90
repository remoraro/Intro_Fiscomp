program exerA
    implicit none

    real(8), parameter :: pi = 3.14159265358979323846d0
    real(8), parameter :: G_Ms = 4.0d0 * pi*pi
    real(8) :: r, v0, deltat, periodo_quadrado, x0, y0, x1, y1, x2, y2, t
    real(8) :: velocidade, t_antes
    integer :: i, unidade_out1, unidade_out2

    character(8), parameter :: nomes(9) = (/ "Mercurio", "Venus   ", &
        "Terra   ", "Marte   ", "Jupiter ", "Saturno ", "Urano   ", & 
        "Netuno  ", "Plutao  " /)

    real(8), parameter :: raio(9) = (/ 0.39d0, 0.72d0, 1.00d0, &
        1.52d0, 5.20d0, 9.24d0, 19.19d0, 30.06d0, 39.53d0 /)

    read(*,*) r
    read(*,*) v0
    read(*,*) deltat

    unidade_out1 = 10
    open(unit=unidade_out1, file="trajA1_out.dat", status="replace")

    x0 = r
    y0 = 0.0d0
    write(unidade_out1, *) 0.0d0, x0, y0

    x1 = r
    y1 = 0.0d0 + v0 * deltat
    write(unidade_out1, *) deltat, x1, y1


    t = deltat
    t_antes = deltat

    do

        t = t + deltat

        r = sqrt(x1*x1 + y1*y1)

        x2 = 2.0d0 * x1 - x0 - (G_Ms * x1 / r**3) * deltat**2
        y2 = 2.0d0 * y1 - y0 - (G_Ms * y1 / r**3) * deltat**2

        ! chuta/interpola quando termina o primeiro ciclo / período
        if (y1 < 0.0d0 .and. y2 >= 0.0d0) then

            y2 = y1 + (y2 + y1) / 2.0d0
            x2 = x1 + (x2 + x1) / 2.0d0
            t = t_antes + (t-t_antes) / 2.0d0

            write(unidade_out1, *) t, x2, y2

            exit

        end if

        write(unidade_out1, *) t, x2, y2

        x0 = x1
        y0 = y1
        x1 = x2
        y1 = y2
        t_antes = t

    end do

    close(unidade_out1)

    unidade_out2 = 11
    open(unit=unidade_out2, file="tabA1_out.dat", status="replace")

    write(unidade_out2, *) "Planeta", "velocidade inicial v0", "razão T²/R³"
    do i = 1, 9

        velocidade = sqrt(G_Ms / raio(i))
        periodo_quadrado = (2.0d0 * pi * raio(i) / velocidade) ** 2
        
        write(unidade_out2, *) nomes(i), velocidade, periodo_quadrado/raio(i)**3

    end do

    close(unidade_out2)

    print *, "Escolhi deltat pequeno o suficiente para que a energia e a orbita variem pouco;"
    print *, "dimensione deltat pela escala orbital e reduza-o ate a solucao convergir."

end program exerA