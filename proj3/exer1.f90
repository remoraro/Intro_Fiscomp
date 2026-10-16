program exer1
    implicit none

    real(8) :: v, tesao, deltat
    real(8), parameter :: m = 80.0d0, pot = 400.0d0
    integer :: i, unidade_out

    read *, tesao, deltat, v

    unidade_out = 11
    open(unit=unidade_out, file="vel1_out.dat", status="replace")
    
    do i = 0, int(tesao) - 1, int(deltat)

        write(unidade_out, *) i, v
        v = v + pot * deltat / (m * v)

    end do


    close(unidade_out)

end program exer1