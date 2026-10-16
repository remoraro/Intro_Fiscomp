program exer2
    implicit none

    real(8) :: vy, vx, y, x, angulo, t
    real(8), parameter :: deltat = 0.1d0, v = 500.0d0, g = 9.8d0
    integer :: unidade_out, unidade_out2

    read *, angulo

    unidade_out = 11
    unidade_out2 = 12
    open(unit=unidade_out, file="output2.txt", status="replace")
    open(unit=unidade_out2, file="trajexata.txt", status="replace")
    
    vy = v * sin(angulo * 3.141592653593d0 / 180.0d0)
    vx = v * cos(angulo * 3.141592653593d0 / 180.0d0)
    x = 0.0d0
    y = 0.0d0
    do 

        write(unidade_out, *) x, y
        
        x = x + vx * deltat
        y = y + vy * deltat
        vy = vy - g * deltat
        
        if (y <= 0.0d0) then
            write(unidade_out, *) vx * (2.0d0 * v * &
            & sin(angulo * 3.141592653593d0 / 180.0d0)) / g, 0.0d0
            exit
        end if
    
    end do

    vy = v * sin(45.0d0 * 3.141592653593d0 / 180.0d0)
    vy = v * cos(45.0d0 * 3.141592653593d0 / 180.0d0)
    x = 0.0d0
    y = 0.0d0
    t = deltat
    do

        write(unidade_out2, *) x, y
        
        x = vx * t
        y = vy * t - g * t * t * 0.5d0
        ! s = s0 + vot - gt^2 / 2

        if (y <= 0.0d0) then
            exit
        end if

        t = t + deltat
    
    end do

    write(unidade_out2, *) vx * (2.0d0 * vy) / g, 0.0d0

    close(unidade_out)
    close(unidade_out2)

end program exer2