program exer3B
    implicit none

    real(8) :: w, m, l, theta, deltat, tesao, t, energia, constante1, constante2
    real(8), parameter :: g = 9.8d0, pi = 3.14159265358979323846d0
    integer :: unidade_out!, unidade_energia

    read(*,*) m
    read(*,*) l
    read(*,*) theta
    read(*,*) deltat
    read(*,*) tesao

    unidade_out = 10
    !unidade_energia = 11
    open(unit=unidade_out, file="exer3B_out.dat", status="replace")
    !open(unit=unidade_energia, file="energia3B.dat", status="replace")
    
    theta = theta * pi / 180.0d0
    
    w = 0.0d0
    t = 0.0d0
    constante1 = m * l * l * 0.5d0
    constante2 = m * g * l * 0.5d0
    do while (t <= tesao)

        do
            if (theta > pi) then
                theta = theta - 2.0d0 * pi
            else if (theta < -pi) then
                theta = theta + 2.0d0 * pi
            else
                exit
            end if
        end do
        
        write(unidade_out, *) t, theta

        energia = constante1 * w * w  + constante2 * theta * theta 
        !write(unidade_energia, *) t, energia
        
        w = w - g * theta * deltat / l
        theta = theta + w * deltat

        t = t + deltat 

    end do


    close(unidade_out)

end program exer3B