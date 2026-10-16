program exer1
    implicit none

    real(8) :: v, tesao, deltat, t
    real(8), parameter :: m = 80.0d0, pot = 400.0d0
    integer :: i, unidade_out

    read *, tesao, deltat, v

    unidade_out = 11
    open(unit=unidade_out, file="vel1_out.dat", status="replace")
    

    t = 0.0d0
    do i = 0, int(tesao / deltat)

        write(unidade_out, *) t, v
        v = v + pot * deltat / (m * v)
        t = t + deltat

    end do


    close(unidade_out)

end program exer1